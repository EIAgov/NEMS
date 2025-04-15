"""Submodule for calculating Alaskan well production.

Summary
-------
This submodule takes field size classes, known projects and undiscovered projects and uses these datasets and equations
to project Alaskan production. The Module operates as follows:

    1. The module is initialized in the alaska class *__init__* method, with class dataframes and variables being declared here.

    2. The *setup* method is called, which reads in the necessary input files via .csv or .pkl (see **intermediate_var_pickle.py**) format.

    3. The *run* method is called, which kicks off the main model functions.

    4. In the first model run year the *setup_production* method is run, which determines the initial production rate for currently producing
       projects, and applies the drilling algorithm located in **drilling_equations.py** to project future production for these projects.

    5. Run *load_projects*, which loads announced projects into the main project dataframe by model year. Producing projects
       are concatenated to the master "projects" dataframe in the first model year.

    6. Calculate crude oil, natural gas, and ngpl prices by region based on price values from LFMM and NGMM in *load_prices*.

    7. Run *calculate_drilling*, where project drilling is determined based on a static drilling schedule

    8. Run *calculate production*, which calculates Alaska production based on production profile equations in **drilling_equations.py**.

    9. Calculate capital costs, abandon rate and operating costs by project size based on static cost tables.

    10. Load and run cash flow in *load_cashflow* and *run_cashflow*. Rank projects by project net present value.

    11. Select profitable projects in *select_projects* and write production volumes to output tables.

    12. Calculate NGPL volumes based on a ratio of crude production.

    13. Run *write_intermediate_variables* to store iterative calculation tables for the next model run year. These tables are written out to .pkl files in
        **intermediate_var_pickle.py**.

    14. *report_results_unf* is called from **module_unf.py** to report results to debug files and the restart file.


Input Files
-----------

    * ak_dev_drill_schedule.csv - Development wells drilled by year assigned to region number
    * ak_exp_drill_schedule.csv - Exploratory wells drilled by year assigned to region number
    * ak_facility_costs.csv - Capital expenses cost rate by oil resources
    * ak_field_size_classes.csv - Field Size Class, Minimum Resources BOE, Production Rate Factors, Ramp Up Years,
      Decline Rates, Predecline Production Factors, Hyper Decline Coefficients
    * ak_mapping.csv - Matches crude tariff price, natural gas tariff price, exploratory drill cost rate, and developmental drill cost rate to region numbers
    * ak_opex.csv - Operating costs (e.g., fees for oil and water hauling, facility electricity, etc.)
    * ak_projects_known.csv - Oil & Gas Resources, Production Rates, Region Numbers, Production Start Years,
      Oil Decline Rate, Onshore%, OffSTAT%, OffFED%, Historical Production
    * ak_projects_known_10x.csv - Different 2020 numbers than for ak_projects_known.csv
    * ak_projects_undiscovered.csv - Region Numbers, Oil & Gas Resources


Model Functions and Class Methods
_________________________________

class Alaska(sub.Submodule) - Alaska submodule for HSM

    * __init__ - Constructor to initialize Alaska submodule
    * setup - Method to set up Alaska submodule for HSM; parameter "setup_filename" - path to offshore setup file
    * _setup_production - Method to determine NA drilling and production
    * run - Method to run Alaska Submodule for HSM
    * _load_projects - Method to load known and unknown projects
    * _load_prices - Method to calculate average prices for Offshore regions; averages prices over past years set in averaging_years
    * _calculate_drilling - Method to calculate drilling
    * _calculate_production - Method to calculate production
    * _calculate_kap_cost - Method to calculate other expected cap. costs
    * _calculate_operating_cost - Method to calculate operating cost
    * _load_cash_flow - Method to load cash flow
    * _run_cash_flow - Method for running through CashFlow
    * _select_projects - Method to get production from cash flow; selection mask based on profitability
    * report_results_unf/hdf - Report results to restart variables


Output Debug Files
__________________
    * hsm_ak_crude_projects.csv - Crude Oil production by project
    * hsm_ak_ngpls_projects.csv - NGPL production by project


Output Restart Variables
________________________
**Crude Oil Restart Variables**
    * pmmout_rfqtdcrd - Total crude production by HSM region
    * pmmout_rfqdcrd - Total crude oil production by HSM region (not including EOR)
    * ogsmout_ogqcrrep - Crude oil production by oil category
    * ogsmout_ogcrdprd - Crude oil production by HSM region and crude type
    * ogsmout_ogcruderef - Crude oil production by LFMM crude oil type and region


**Natural Gas Restart Variable**
    * ogsmout_ogqngrep - Natural gas production by natural gas type


**Well Restart Variables**
    * ogsmout_ogogwells - Total wells
    * ogsmout_ognowell - Total completed wells


**NGPL Production Restart Variables**
    * ogsmout_ogngplprd - NGPL production by HSM district
    * ogsmout_ogngplet - Ethane production by HSM district
    * ogsmout_ogngplpr - Propane production by HSM district
    * ogsmout_ogngplbu - Butane production by HSM district
    * ogsmout_ogngplis - Isobutane production by HSM district
    * ogsmout_ogngplpp - Pentanes production by HSM district


Alaska Submodule Class Methods
______________________________
"""

import pandas as pd
import numpy as np
import names as nam
import submodule as sub
import common as com
import drilling_equations as drill_eq
import warnings
import cash_flow as cf


class Alaska(sub.Submodule):
    """Alaska submodule for HSM.

    Parameters
    ----------
    parent : str
        Module_unf.Module (Pointer to parent module)
    """

    def __init__(self, parent):

        super().__init__(parent, submodule_name='alaska')
        #input tables
        self.field_size_classes     = pd.DataFrame()  # DataFrame of Alaska field size classes and properties
        self.mapping                = pd.DataFrame()  # DataFrame of Alaska region mapping, transportation costs and drilling costs
        self.projects_known         = pd.DataFrame()  # DataFrame of known projects
        self.projects_undiscovered  = pd.DataFrame()  # DataFrame of undiscovered projects
        self.exp_drill_schedule     = pd.DataFrame()  # DataFrame of exploratory drilling schedule
        self.dev_drill_schedule     = pd.DataFrame()  # DataFrame of developmental drilling schedule
        self.facility_costs         = pd.DataFrame()  # DataFrame of facility costs
        self.opex                   = pd.DataFrame()  # Dataframe of operating expenses

        #tables for internal calculations
        self.projects           = pd.DataFrame()  # DataFrame of projects
        self.projects_selected  = pd.DataFrame()  # DataFrame of selected projects

        self.crude_production   = pd.DataFrame()  # DataFrame of crude production
        self.natgas_production  = pd.DataFrame()  # DataFrame of natural gas production

        #variables for internal calculations
        self.zero_year          = 0    # Zero year for submodule (equal to AEO year - 1)
        self.evaluation_years   = 0    # number of years to evaluate projects

        self.ngpl_production        = 0.0 # NGPL production
        self.ethane_production      = 0.0 # ethane production
        self.propane_production     = 0.0 # propane production
        self.butane_production      = 0.0 # butane production
        self.isobutane_production   = 0.0 # isobutane production
        self.proplus_production     = 0.0 # propane plus production

        #CashFlow object
        self.cash_flow = cf.CashFlow()


    def setup(self, setup_filename):
        """Setup Alaska submodule for HSM.

        Parameters
        ----------
        setup_filename : str
            Path to offshore setup file.

        Returns
        -------
        None
        """
        super().setup(setup_filename)

        ###Variables
        self.zero_year          = int(  self.setup_table.at[nam.zero_year               , nam.filename]) #Model start year
        self.evaluation_years   = int(  self.setup_table.at[nam.ak_evaluation_years     , nam.filename]) #Model run years
        self.prod_eval_year     = int(self.setup_table.at[nam.prod_evaluation_year      , nam.filename]) #Year from which past production is measured
        self.tech_rate          = float(self.setup_table.at[nam.tech_rate               , nam.filename]) #Technology Rate
        self.averaging_years    = int(self.setup_table.at[nam.averaging_years           , nam.filename]) #Number of price averaging years
        self.royalty_rate       = float(self.setup_table.at[nam.royalty_rate            , nam.filename]) #Royalty rate for DCF
        self.sev_rate_crude     = float(self.setup_table.at[nam.sev_rate_crude          , nam.filename]) #Crude severance tax rate for DCF
        self.sev_rate_natgas    = float(self.setup_table.at[nam.sev_rate_natgas         , nam.filename]) #Natgas severance tax rate for DCF
        self.exp_tang_frac      = float(self.setup_table.at[nam.exp_tang_frac           , nam.filename]) #Exploration tangible cost fraction for DCF
        self.dev_tang_frac      = float(self.setup_table.at[nam.dev_tang_frac           , nam.filename]) #Developing projects tangible cost fraction for DCF
        self.kap_tang_frac      = float(self.setup_table.at[nam.kap_tang_frac           , nam.filename]) #Capital costs tangible cost fraction for DCF
        self.intang_amor_frac   = float(self.setup_table.at[nam.intang_amor_frac        , nam.filename]) #Intangible cost amortization fraction for DCF
        self.amor_schedule      = str(self.setup_table.at[nam.amor_schedule             , nam.filename]) #Amortization schedule for DCF
        self.deprec_schedule    = str(self.setup_table.at[nam.deprec_schedule           , nam.filename]) #Depreciation schedule for DCF

        #Apply Side Case Tech Rate Adjustment
        self.tech_rate = self.tech_rate * self.parent.side_case_adj

        ###Tables
        self.field_size_classes     = super()._load_dataframe(self.alaska_input_path, nam.ak_field_size_classes)
        self.mapping                = super()._load_dataframe(self.alaska_input_path, nam.ak_mapping, index_col = nam.region_number)
        self.exp_drill_schedule     = super()._load_dataframe(self.alaska_input_path, nam.ak_exp_drill_schedule, index_col = nam.region_number)
        self.dev_drill_schedule     = super()._load_dataframe(self.alaska_input_path, nam.ak_dev_drill_schedule, index_col = nam.region_number)
        self.facility_costs         = super()._load_dataframe(self.alaska_input_path, nam.ak_facility_costs, index_col = nam.oil_resources)
        self.opex                   = super()._load_dataframe(self.alaska_input_path, nam.ak_opex, index_col = nam.index)



        ###Load Projects
        if (self.rest_curcalyr == self.zero_year) | (self.parent.integrated_switch == False):
            self.projects_known         = super()._load_dataframe(self.alaska_input_path, nam.ak_projects_known)
            self.projects_undiscovered  = super()._load_dataframe(self.alaska_input_path, nam.ak_projects_undiscovered)

            #Remove delayed/cancelled/uneconomical projects from input file
            mask = self.projects_known[nam.oil_resources] >= 0
            self.projects_known = self.projects_known[mask]


            ###Field size matching and production
            self.projects_known[nam.mean_resources_boe] = self.projects_known[nam.oil_resources] + self.projects_known[nam.gas_resources]
            self.projects_undiscovered[nam.mean_resources_boe] = self.projects_undiscovered[nam.oil_resources] + self.projects_undiscovered[nam.gas_resources]

            self.projects_known         = self.projects_known.sort_values(nam.mean_resources_boe)
            self.projects_undiscovered  = self.projects_undiscovered.sort_values(nam.mean_resources_boe)

            self.projects_known = pd.merge_asof(self.projects_known,
                                                self.field_size_classes,
                                                left_on=nam.mean_resources_boe,
                                                right_on=nam.min_resources_boe,
                                                suffixes=[None, '_table'])
            self.projects_undiscovered = pd.merge_asof(self.projects_undiscovered,
                                                       self.field_size_classes,
                                                       left_on=nam.mean_resources_boe,
                                                       right_on=nam.min_resources_boe)

            #Add resids to undiscovered fields and sort be resid
            self.projects_known         = self.projects_known.sort_values(nam.resid)
            self.projects_undiscovered[nam.resid]  = self.projects_undiscovered.index.copy() + self.projects_known[nam.resid].max() + 1
            self.projects_undiscovered  = self.projects_undiscovered.sort_values(nam.resid)

            #Convert max daily production (MB/D) to annual volume (MMB)
            self.projects_known[nam.initial_oil_prod_rate] = self.projects_known[nam.initial_oil_prod_rate].mul(0.365)
            self.projects_known[nam.max_oil_production_rate] = self.projects_known[nam.max_oil_production_rate].mul(0.365)
            #Get calculated max and initial production
            self.projects_known[nam.max_oil_production_rate_calc]  = (self.projects_known[nam.oil_resources] - self.projects_known[list(range(self.prod_eval_year, self.zero_year))].sum(axis=1)) * self.projects_known[nam.max_oil_production_rate_factor]
            self.projects_known[nam.initial_oil_prod_rate_calc]    = self.projects_known[nam.oil_resources] * self.projects_known[nam.initial_oil_prod_rate_factor]

            ###Set max production as min between calc and fixed max rate
            max_mask = self.projects_known[nam.max_oil_production_rate_calc] < self.projects_known[nam.max_oil_production_rate]
            temp_df = self.projects_known[max_mask].copy()
            temp_df[nam.max_oil_production_rate] = temp_df[nam.max_oil_production_rate_calc]
            self.projects_known.update(temp_df)

            ###Set initial production as min between calc and fixed max rate
            max_mask = self.projects_known[nam.initial_oil_prod_rate_calc] < self.projects_known[nam.initial_oil_prod_rate]
            temp_df = self.projects_known[max_mask].copy()
            temp_df[nam.initial_oil_prod_rate] = temp_df[nam.initial_oil_prod_rate_calc]
            self.projects_known.update(temp_df)

            #Drop calc columns
            self.projects_known = self.projects_known.drop([nam.initial_oil_prod_rate_calc, nam.max_oil_production_rate_calc], axis = 1)
            
            # Side case Adjustment
            temp = self.projects_known.loc[self.projects_known[nam.year_production_start] > self.zero_year].copy()
            temp[nam.oil_resources] = temp[nam.oil_resources] * self.parent.side_case_adj
            self.projects_known.update(temp)

        ###Load Intermediate Tables:
        if (self.rest_curcalyr > self.zero_year) & (self.parent.integrated_switch == True):
            self.projects           = self.parent.hsm_vars.ak_projects.copy()
            self.projects_known     = self.parent.hsm_vars.ak_projects_known.copy()
            self.crude_production   = self.parent.hsm_vars.ak_crude_production.copy()

        pass


    def run(self):
        """Run Alaska Submodule for HSM.

        Returns
        -------
        None
        """
        super().run()

        #Run projects setup
        if self.rest_curcalyr == self.zero_year:
            self.setup_production()

        #Run annual functions
        if self.rest_curcalyr >= self.zero_year:
            #Load Projects
            self.logger.info('Load Alaska Projects')
            self.load_projects()

            if len(self.projects):
                #Load Prices
                self.logger.info('Load Alaska Crude/Natural Gas Prices')
                self.load_prices()

                #Caclulate Drilling
                self.logger.info('Calculate Alaska Drilling and Expected Production')
                self.calculate_drilling()
                self.calculate_production()

                #Calculate Alaska Well Costs
                self.logger.info('Calculate Alaska Well Costs')
                self.calculate_kap_cost()
                self.calculate_operating_cost()

                #Load and Run Alaska Cash Flow
                self.logger.info('Run Alaska DCF')
                self.load_cash_flow()
                self.run_cash_flow()

                #Select Projects
                self.logger.info('Select Alaska Projects')
                self.select_projects()

                #Calculate NGPLS
                self.logger.info('Calculate Alaska NGPLS')
                self.calculate_ngpls()

            if self.parent.debug_switch:
                self.logger.info('Debug Alaska Submodule')
                self.debug_alaska()

            if ((self.parent.integrated_switch == True) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == True) & (self.parent.param_ncrl == 1)):
                self.write_intermediate_variables()


    ###START OF FUNCTIONS ONLY RUN IN FIRST MODEL YEAR###


    def setup_production(self):
        """Setup production for Alaska Submodule.

        Returns
        -------
        self.projects_known : df
            Table of announced Alaska projects

        self.crude_production
            DataFrame of alaska crude oil production
        """

        #Get average of last 3 years of production
        self.projects_known[nam.avg_3_prod] = self.projects_known[[self.zero_year - 2, self.zero_year - 1, self.zero_year]].mean(axis = 1)

        mask = self.projects_known[nam.year_production_start] < self.zero_year

        # historical production since evaluation year
        self.projects_known[nam.past_production] = self.projects_known[list(range(self.prod_eval_year, self.zero_year))].sum(axis=1)
        self.projects_known.loc[mask, nam.max_oil_production_rate] = self.projects_known[nam.max_prod]
        self.projects_known[nam.production_delay] = 0

        #Legacy Projects
        legacy_mask = (self.projects_known[nam.year_production_start] < self.zero_year - 4) & \
                      (self.projects_known[self.zero_year] > 0)

        #Oil
        temp_crude_production = self.projects_known[legacy_mask].apply(drill_eq.ak_decline_profile,
                                                                evaluation_years=self.parent.final_aeo_year - self.zero_year,
                                                                axis=1)

        #Recent Projects (Only run if there are producing projects within 4 years of zero year) - Oil
        recent_prod_mask = (self.projects_known[nam.year_production_start] > self.zero_year - 4) & \
                           (self.projects_known[nam.year_production_start] <= self.zero_year) & \
                           (self.projects_known[self.zero_year] > 0)

        if recent_prod_mask.any():
            temp_crude_production_recent = self.projects_known[recent_prod_mask].apply(drill_eq.ak_production_profile,
                                                                evaluation_years=self.parent.final_aeo_year - self.zero_year,
                                                                oil_or_gas=nam.oil,
                                                                axis=1)

            temp_crude_production = pd.concat([temp_crude_production, temp_crude_production_recent], ignore_index=False)


        #Shift from 0 indexed to zero_year indexed (e.g. 2021)
        temp_crude_production.columns = temp_crude_production.columns + self.zero_year + 1

        temp_crude_production.loc[mask, list(range(self.parent.param_baseyr, self.zero_year + 1))] = self.projects_known[list(range(1990, self.zero_year + 1))]

        temp_crude_production.loc[mask, nam.resid] = self.projects_known[nam.resid]
        temp_crude_production.loc[mask, nam.region_number] = self.projects_known[nam.region_number]
        temp_crude_production.loc[mask, nam.ak_region_number] = self.projects_known[nam.ak_region_number]
        self.crude_production = temp_crude_production.copy()

        pass


    ###START OF FUNCTIONS RUN EVERY YEAR###


    def load_projects(self):
        """Load Alaska projects based on production start date.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        year = self.rest_curcalyr
        mask = self.projects_known[nam.year_production_start] == year
        #If there are no existing project, need to join outer to get all the columns assigned correctly, otherwise join inner
        # announced fields
        if len(self.projects):
            self.projects = pd.concat([self.projects, self.projects_known[mask]], join='inner', ignore_index=True)
        else:
            self.projects = pd.concat([self.projects, self.projects_known[mask]], join='outer', ignore_index=True)
        pass


    def load_prices(self):
        """Calculate average crude oil and natural gas prices for Alaksa wells.

            * Oil prices are based on regional wellhead prices from LFMM
            * Natural Gas Prices are based on regional wellhead prices from NGMM


        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        avg_years = list(range(self.rest_curcalyr - self.averaging_years , self.rest_curcalyr + 1))

        #Load crude prices
        temp_price = pd.merge(self.projects[nam.region_number],
                              self.parent.reg_crude_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.crude_price] = temp_price[avg_years].mean(axis=1)
        self.projects[nam.crude_price] = temp_price[nam.crude_price].copy()

        #Load natural gas prices
        temp_price = pd.merge(self.projects[nam.region_number],
                              self.parent.reg_natgas_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.natgas_price] = temp_price[avg_years].mean(axis=1)
        self.projects[nam.natgas_price] = temp_price[nam.natgas_price].copy()


    def calculate_drilling(self):
        """Calculate Alaska drilling.

            * Calculates exploration and development drilling based on static drill schedule
            * Assigns drilling costs to cash flow
            * Applies technology improvement rate

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.cash_flow.exp_drill_cost : df
            Table of exploration well drill costs

        self.cash_flow.exp_dry_cost : df
            Table of exploration well dryhole costs

        self.cash_flow.dev_drill_cost : df
            Table of development well drill costs

        self.cash_flow.dev_dry_cost : df
            Table of development well dryhole costs
        """
        #Map porojects to drilling schedules and costs
        self.projects = self.projects.drop([nam.exp_drill_cost_rate, nam.dev_drill_cost_rate], axis=1, errors='ignore')
        self.projects = pd.merge(self.projects,
                                 self.mapping[[nam.exp_drill_cost_rate, nam.dev_drill_cost_rate]],
                                 left_on=nam.region_number,
                                 right_index=True,
                                 how='left')

        temp_exp_drilling = pd.merge(self.projects[[nam.region_number]],
                                     self.exp_drill_schedule,
                                     left_on=nam.region_number,
                                     right_index=True,
                                     how='left')
        temp_exp_drilling = temp_exp_drilling.drop(nam.region_number, axis=1)

        temp_dev_drilling = pd.merge(self.projects[[nam.region_number]],
                                     self.dev_drill_schedule,
                                     left_on=nam.region_number,
                                     right_index=True,
                                     how='left')
        temp_dev_drilling = temp_dev_drilling.drop(nam.region_number, axis=1)

        #Multiply schedule by costs
        self.cash_flow.exp_drill_cost  = temp_exp_drilling.mul(self.projects[nam.exp_drill_cost_rate], axis=0)
        self.cash_flow.exp_dry_cost    = temp_exp_drilling.mul(self.projects[nam.exp_drill_cost_rate], axis=0)
        self.cash_flow.dev_drill_cost  = temp_dev_drilling.mul(self.projects[nam.dev_drill_cost_rate], axis=0)
        self.cash_flow.dev_dry_cost    = temp_dev_drilling.mul(self.projects[nam.dev_drill_cost_rate], axis=0)

        #Apply tech rate
        for col in temp_dev_drilling.columns:
            self.cash_flow.exp_drill_cost[col]   = self.cash_flow.exp_drill_cost[col].mul((1-self.tech_rate) ** (self.rest_curcalyr - self.zero_year))
            self.cash_flow.exp_dry_cost[col]     = self.cash_flow.exp_dry_cost[col].mul((1-self.tech_rate) ** (self.rest_curcalyr - self.zero_year))
            self.cash_flow.dev_drill_cost[col]   = self.cash_flow.dev_drill_cost[col].mul((1-self.tech_rate) ** (self.rest_curcalyr - self.zero_year))
            self.cash_flow.dev_dry_cost[col]     = self.cash_flow.dev_dry_cost[col].mul((1-self.tech_rate) ** (self.rest_curcalyr - self.zero_year))

        pass


    def calculate_production(self):
        """Calculates Alaska production based on production profile equations in drilling_equations.py.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.cash_flow.crude_production : df
            DataFrame of alaska crude oil production

        self.cash_flow.natgas_production : df
            DataFrame of alaska natural gas production
        """

        def first_production(series):
            for i, val in series.items():
                if series[i] > 0:
                    return i
            return np.NaN
        self.projects[nam.production_delay] = self.cash_flow.dev_drill_cost.apply(first_production, axis=1)
        self.projects[nam.production_delay] = 0

        self.cash_flow.crude_production = self.projects.apply(drill_eq.ak_production_profile,
                                                              evaluation_years=self.evaluation_years,
                                                              oil_or_gas=nam.oil,
                                                              axis=1)
        self.cash_flow.natgas_production = self.projects.apply(drill_eq.ak_production_profile,
                                                               evaluation_years=self.evaluation_years,
                                                               oil_or_gas=nam.gas,
                                                               axis=1)

        pass


    def calculate_kap_cost(self):
        """Calculates capital costs and abandon rate based on a static cost table of capital cost rates by project oil resources.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.cash_flow.kap_cost : df
            Table of Alaska project capital costs
        """

        #Calculate capital costs
        self.projects = self.projects.sort_values(nam.oil_resources)
        self.projects[nam.oil_resources] = self.projects[nam.oil_resources].astype('int64')
        self.projects = self.projects.drop(nam.kap_cost_rate, axis=1, errors='ignore')
        self.projects = pd.merge_asof(self.projects,
                                      self.facility_costs,
                                      left_on=nam.oil_resources,
                                      right_index=True,
                                      direction = 'backward')
        self.projects = self.projects.sort_index()

        #Distribute capital costs over first four years
        self.cash_flow.kap_cost = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.kap_cost[[0, 1, 2, 3]] = 0.25
        self.cash_flow.kap_cost = self.cash_flow.kap_cost.mul(self.projects[nam.kap_cost_rate] * self.projects[nam.oil_resources], axis=0)

        #Abandonment rate calculation
        self.projects[nam.abandon_rate] = self.projects[nam.kap_cost_rate] * self.projects[nam.oil_resources] * 0.1

        pass


    def calculate_operating_cost(self):
        """Calculates project operating costs based on project crude production.

        Returns
        -------
        self.cash_flow.operating_cost : df
            Table of Alaska project operating costs
        """

        def fun(series):
            series_out = series.copy()
            for i,val in series.items():

                #Only apply costs when there's production
                if series[i] > 0:
                    series_out[i] = 1
                else:
                    series_out[i] = 0

                #Calculate opex by volume
                if series.max() / 100000 < 50:
                    temp_b = self.opex['b'][0]
                    temp_m = self.opex['m'][0]
                    series_out[i] = series_out[i] * (temp_b + temp_m * series[i])
                elif series.max() / 100000 < 160:
                    temp_b = self.opex['b'][1]
                    temp_m = self.opex['m'][1]
                    series_out[i] = series_out[i] * (temp_b + temp_m * series[i])
                elif series.max() / 100000 < 300:
                    temp_b = self.opex['b'][2]
                    temp_m = self.opex['m'][2]
                    series_out[i] = series_out[i] * (temp_b + temp_m * series[i])
                else:
                    temp_b = self.opex['b'][3]
                    temp_m = self.opex['m'][3]
                    series_out[i] = series_out[i] * (temp_b + temp_m * series[i])


            return series_out

        self.cash_flow.operating_cost = self.cash_flow.crude_production.apply(fun, axis=1)

        #Apply tech rate
        self.cash_flow.operating_cost = self.cash_flow.operating_cost.apply(lambda x: x.mul((1-self.tech_rate) ** (self.rest_curcalyr - self.zero_year)))

        pass


    def load_cash_flow(self):
        """Loads Alaska submodule cash flow.

        Returns
        -------
        self.cash_flow.properties : df
            DataFrame containing properties used in the cash flow (costs, tangible/intangible cost ratios, etc.)
        """

        #Initiate properties
        self.cash_flow.properties = self.projects[[nam.field_size_class,
                                                   nam.region_number,
                                                   nam.resid,
                                                   nam.crude_price,
                                                   nam.natgas_price,
                                                   nam.abandon_rate]].copy()

        #Apply tariff/transit costs
        self.cash_flow.properties = pd.merge(self.cash_flow.properties,
                                             self.mapping[[nam.crude_tariff_price, nam.natgas_tariff_price]],
                                             left_on=nam.region_number,
                                             right_index=True,
                                             how='left')

        self.cash_flow.properties[[nam.crude_trans_price, nam.natgas_trans_price]] = 0.0

        #Set royalty rate
        self.cash_flow.properties[nam.royalty_rate] = self.royalty_rate

        #Set severance tax rate
        self.cash_flow.properties[nam.sev_rate_crude_prod] = self.sev_rate_crude
        self.cash_flow.properties[nam.sev_rate_natgas_prod] = self.sev_rate_natgas

        #Set state abbreviation
        self.cash_flow.properties[nam.state] = 'AK'

        #Set tangible cost fractions
        self.cash_flow.properties[nam.exp_tang_frac]    = self.exp_tang_frac
        self.cash_flow.properties[nam.dev_tang_frac]    = self.dev_tang_frac
        self.cash_flow.properties[nam.kap_tang_frac]    = self.kap_tang_frac
        self.cash_flow.properties[nam.intang_amor_frac] = self.intang_amor_frac
        self.cash_flow.properties[nam.amor_schedule]    = self.amor_schedule
        self.cash_flow.properties[nam.deprec_schedule]  = self.deprec_schedule

        #Set federal tax rate
        self.cash_flow.properties[nam.fed_tax_rate] = self.parent.fed_tax_rate

        #Set discount rate
        self.cash_flow.properties[nam.discount_rate] = self.parent.discount_rate + 0.05 # 5% premium over l48 projects

        #Set Relevant properties equal to 0
        self.cash_flow.properties[nam.crude_trans_price, nam.natgas_price] = 0

        #Set unused DataFrames to 0
        self.cash_flow.equip_cost           = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.tangible_adjustment  = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.geo_geo_and_lease_aq = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.general_admin_cost   = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.depletion            = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.eor_tax_credit       = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.fed_credit           = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.invest_credit        = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.gg_la_cost           = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.ch4_emission_cost    = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.ch4_emissions        = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)

        pass


    def run_cash_flow(self):
        """Run Alaska Submodule Cash Flow.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """

        # run CashFlow
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
        self.cash_flow.calculate_profitability()

        self.projects[nam.net_present_value] = self.cash_flow.properties[nam.net_present_value]
        self.projects[nam.profitability] = self.cash_flow.properties[nam.profitability]

        pass


    def select_projects(self):
        """Select Alaska projects based on project NPV.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """

        #Force announced projects the year after they are eligible
        ann_mask = self.projects[nam.year_production_start] <= self.rest_curcalyr
        resid_mask = self.projects[nam.resid] <= 50
        ann_mask = resid_mask & ann_mask
        temp_df = self.projects[ann_mask].copy()
        temp_df[nam.profitability] = 1
        self.projects.update(temp_df)

        #Set profitability selection mask
        selected_mask = self.projects[nam.profitability] > 0

        #Limit ANWR production to one project every 2 years
        anwr_mask = self.projects[nam.resid] >= 51
        temp_df = self.projects[anwr_mask].copy()
        if len(temp_df.index) <= 1:
            pass
        else:
            #Update Projects so only one project is selected
            temp_df = temp_df.loc[temp_df[nam.resid] != temp_df[nam.resid].min()] #Drop smallest resid value which will always be largest prod
            temp_df[nam.year_production_start] = self.rest_curcalyr + 2
            self.projects.update(temp_df)

            #Update Projects known so that all other projects are pushed back 2 years
            projects_known_mask = (self.projects_known[nam.resid] >= 51) & (self.projects_known[nam.year_production_start] >= self.rest_curcalyr)
            temp_df = self.projects_known[projects_known_mask].copy()
            temp_df[nam.year_production_start] = temp_df[nam.year_production_start] + 2
            temp_df.loc[temp_df[nam.year_production_start] < (self.rest_curcalyr + 2), nam.year_production_start] = (self.rest_curcalyr + 2)
            self.projects_known.update(temp_df)

        #Create year mask
        year_mask = self.projects[nam.year_production_start] <= self.rest_curcalyr

        #Combine masks
        selected_mask = selected_mask & year_mask

        #Get production from cash_flow
        #Crude production
        temp = self.cash_flow.crude_production.loc[selected_mask].copy()
        temp.columns = temp.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.resid] = self.projects.loc[selected_mask, nam.resid]
        temp[nam.region_number] = self.projects.loc[selected_mask, nam.region_number]
        temp[nam.ak_region_number] = self.projects.loc[selected_mask, nam.ak_region_number]
        self.crude_production = pd.concat([self.crude_production, temp], ignore_index=True, join='outer')
        self.crude_production = self.crude_production.fillna(0)

        self.projects_selected = pd.concat([self.projects[selected_mask].copy(), self.projects_selected], ignore_index=True)
        self.projects = self.projects.loc[~selected_mask]

        self.projects_selected = pd.concat([self.projects[selected_mask].copy(), self.projects_selected], ignore_index=True)
        self.projects = self.projects.loc[~selected_mask]

        pass


    def calculate_ngpls(self):
        """Calculate NGPL Production based on a ratio relative to crude oil production.

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
        prod_years = list(range(self.zero_year, self.parent.final_aeo_year + 1))

        #Calculate total NGPL production
        temp_ngpl = self.crude_production[prod_years].copy()
        temp_ngpl[prod_years] = temp_ngpl[prod_years].mul(0.1, axis='index').mul(1000, axis='index').div(365,axis='index')

        #Get NGPL Production for restart variable
        self.ngpl_production        = temp_ngpl[self.rest_curcalyr].sum() / 1000000000
        self.ethane_production      = self.ngpl_production * 0
        self.propane_production     = self.ngpl_production * 0.1967
        self.butane_production      = self.ngpl_production * 0.2739
        self.isobutane_production   = self.ngpl_production * 0.0703
        self.proplus_production     = self.ngpl_production * 0.4591

        pass


    def debug_alaska(self):
        """Produce debug outputs for Alaska Submodule.

        Returns
        -------
        None

        """
        #Debug Files
        self.projects.to_csv(self.output_path + 'projects_debug//' + 'hsm_ak_projects' + '_' + str(self.rest_curcalyr) + '_' + str(self.parent.current_iteration) + '.csv')
        self.projects_known.to_csv(self.output_path + 'projects_debug//'  + 'hsm_ak_projects_known' + '_' + str(self.rest_curcalyr) + '_' + str(self.parent.current_iteration) + '.csv')
        self.crude_production.to_csv(self.output_path + 'module_results_debug//'  + 'hsm_ak_crude_production.csv')
        self.cash_flow.to_csv(self.output_path + 'cashflow_debug//', 'ak_cash_flow', self.rest_curcalyr, self.parent.current_iteration)

        pass


    def write_intermediate_variables(self):
        """Write local variables to restart file to be read back in each iteration.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.projects_known : df
            Table of announced Alaska projects

        self.crude_production
            DataFrame of alaska crude oil production
        """
        #Debug
        if self.parent.hsm_var_debug_switch == True:
            self.projects.to_csv(self.parent.hsm_var_output_path + 'hsm_ak_projects.csv')
            self.projects_known.to_csv(self.parent.hsm_var_output_path + 'hsm_ak_projects_known.csv')
            self.crude_production.to_csv(self.parent.hsm_var_output_path + 'hsm_ak_crude_production.csv')

        self.parent.hsm_vars.ak_projects         = self.projects.copy()
        self.parent.hsm_vars.ak_projects_known   = self.projects_known.copy()
        self.parent.hsm_vars.ak_crude_production = self.crude_production.copy()

        pass


    def report_results_unf(self):
        """Report results to restart variables.

        Returns
        -------
        Returns
        -------
        self.restart.pmmout_rfqtdcrd : df
            Total crude production by HSM region

        self.restart.pmmout_rfqdcrd : df
            Total crude oil production by HSM region (not including EOR)

        self.restart.ogsmout_ogqcrrep : df
            Crude oil production by oil category

        self.restart.ogsmout_ogoilprd : df
            Crude oil production by oil type and HSM district

        self.restart.ogsmout_ogcrdprd : df
            Crude oil production by HSM region and crude type

        self.restart.ogsmout_ogcruderef : df
            Crude oil production by LFMM crude oil type and region

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
        """
        #Crude Oil
        temp = self.crude_production.copy()
        temp = temp.reindex([nam.resid] + list(range(1990, self.parent.final_aeo_year + 1)), axis=1)
        temp = temp.sort_values(nam.resid)

        if self.rest_curcalyr >= self.parent.steo_years[0]:

            temp = self.crude_production.copy()[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqcrrep.at[(4,int(self.rest_curcalyr)), 'value'] = temp / 1000000

            #Update Crude by HSM region and LFMM fuel type
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby([nam.region_number]).sum()
            #Write to restart
            self.restart.ogsmout_ogcrdprd.at[(11,3, int(self.rest_curcalyr)), nam.value] = temp.at[11, int(self.rest_curcalyr)] / 1000000  #AK medium medium sou
            self.restart.ogsmout_ogcrdprd.at[(12,3, int(self.rest_curcalyr)), nam.value] = temp.at[12, int(self.rest_curcalyr)] / 1000000 #AK medium medium sou
            self.restart.ogsmout_ogcrdprd.at[(13,3, int(self.rest_curcalyr)), nam.value] = temp.at[13, int(self.rest_curcalyr)] / 1000000 #AK medium medium sou


            #Crude Production by Alaska Region
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.ak_region_number]]
            temp = temp.groupby([nam.ak_region_number]).sum()
            self.restart.ogsmout_ogprcoak['value'].update(temp[[int(self.rest_curcalyr)]].stack() / 1000000)


            ###Domestic Crude Oil Production by region for LFMM (including EOR)
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby(nam.region_number).sum()
            self.restart.pmmout_rfqtdcrd.at[(11, int(self.rest_curcalyr)), 'value'] = temp.at[(11, int(self.rest_curcalyr))] / 1000000 / 365
            self.restart.pmmout_rfqtdcrd.at[(12, int(self.rest_curcalyr)), 'value'] = temp.at[(12, int(self.rest_curcalyr))] / 1000000 / 365
            self.restart.pmmout_rfqtdcrd.at[(13, int(self.rest_curcalyr)), 'value'] = temp.at[(13, int(self.rest_curcalyr))] / 1000000 / 365


            ###Domestic Crude Oil Production by region for LFMM (not including EOR)
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby(nam.region_number).sum()
            self.restart.pmmout_rfqdcrd.at[(11, int(self.rest_curcalyr)), 'value'] = temp.at[(11, int(self.rest_curcalyr))] / 1000000 / 365
            self.restart.pmmout_rfqdcrd.at[(12, int(self.rest_curcalyr)), 'value'] = temp.at[(12, int(self.rest_curcalyr))] / 1000000 / 365
            self.restart.pmmout_rfqdcrd.at[(13, int(self.rest_curcalyr)), 'value'] = temp.at[(13, int(self.rest_curcalyr))] / 1000000 / 365

            ###Domestic Crude Production by LFMM Region
            self.restart.ogsmout_ogcruderef.at[(8, 3, int(self.rest_curcalyr)), 'value'] = ((self.restart.pmmout_rfqtdcrd.at[(11, int(self.rest_curcalyr)), 'value'] * 365) \
                                                                                    + (self.restart.pmmout_rfqtdcrd.at[(12, int(self.rest_curcalyr)), 'value'] * 365) \
                                                                                    + (self.restart.pmmout_rfqtdcrd.at[(13, int(self.rest_curcalyr)), 'value'] * 365))


            ###Set ogoilprd
            temp = self.restart.pmmout_rfqdcrd.at[(12, int(self.rest_curcalyr)), 'value'].copy()
            self.restart.ogsmout_ogoilprd.at[(3,1,int(self.rest_curcalyr)), 'value'] = temp

            
            ### Alaska Offshore Oil Production
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            
            # State
            temp_state = temp.loc[temp[nam.region_number] == 13].copy()
            temp_state = temp_state[self.rest_curcalyr].sum() / 1000000
            self.restart.ogsmout_ogprdoff.at[(5,1,int(self.rest_curcalyr)),nam.value] = float(temp_state)
            
            # Fed
            temp_fed = temp.loc[temp[nam.region_number] == 11].copy()
            temp_fed = temp_fed[self.rest_curcalyr].sum() / 1000000
            self.restart.ogsmout_ogprdoff.at[(6,1,int(self.rest_curcalyr)),nam.value] = float(temp_fed)


            ### Alaska Oil Production Federal/Non-Federal
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby([nam.region_number]).sum()
            # Write to restart
            self.restart.ogsmout_ogcoprd_fed.at[(11, int(self.rest_curcalyr)), nam.value] = temp.at[11, int(self.rest_curcalyr)] / 1000000 / 365 # Federal Offshore
            self.restart.ogsmout_ogcoprd_fed.at[(12, int(self.rest_curcalyr)), nam.value] = temp.at[12, int(self.rest_curcalyr)] / 1000000 / 365  # Federal? Onshore
            self.restart.ogsmout_ogcoprd_nonfed.at[(13, int(self.rest_curcalyr)), nam.value] = temp.at[13, int(self.rest_curcalyr)] / 1000000 / 365 # State Offshore



            #Adjust Alaska Natural Gas production for natural gas production by type
            temp = self.restart.ngtdmrep_ogprdng.copy()
            ak_off_fed = temp.at[(11,int(self.rest_curcalyr)), 'value']
            ak_on = temp.at[(12,int(self.rest_curcalyr)), 'value']
            ak_off_state = temp.at[(13,int(self.rest_curcalyr)), 'value']
            ak_ng_prod = ak_off_fed + ak_on + ak_off_state
            self.restart.ogsmout_ogqngrep.at[(8, int(self.rest_curcalyr)), 'value'] = ak_ng_prod
            self.restart.ogsmout_ogdngprd.at[(3, 1, int(self.rest_curcalyr)), 'value'] = ak_ng_prod
            self.restart.ogsmout_ogdngprd.at[(75, 1, int(self.rest_curcalyr)), 'value'] = ak_off_state # Alaska state offshore
            self.restart.ogsmout_ogdngprd.at[(84, 1, int(self.rest_curcalyr)), 'value'] = ak_off_fed # Alaska federal offshore

            # Alaska NG Offshore Prod
            self.restart.ogsmout_ogprdoff.at[(5,2,int(self.rest_curcalyr)),nam.value] = 0.0
            self.restart.ogsmout_ogprdoff.at[(6,2,int(self.rest_curcalyr)),nam.value] = 0.0


            ### Alaska Natural Gas Production Federal/Non-Federal
            self.restart.ogsmout_ogngprd_fed.at[(11, int(self.rest_curcalyr)), nam.value] = ak_off_fed / 1000000000  # Federal Offshore
            self.restart.ogsmout_ogngprd_fed.at[(12, int(self.rest_curcalyr)), nam.value] = ak_on / 1000000000  # Federal? Onshore
            self.restart.ogsmout_ogngprd_nonfed.at[(13, int(self.rest_curcalyr)), nam.value] = ak_off_state / 1000000000  # State Offshore


            #NGPLS
            self.restart.ogsmout_ognglak.at[int(self.rest_curcalyr), nam.value] = self.ngpl_production * 1000
            self.restart.ogsmout_ogngplprd.at[(3, int(self.rest_curcalyr)), nam.value] = self.ngpl_production
            self.restart.ogsmout_ogngplet.at[(3, int(self.rest_curcalyr)), nam.value] = self.ethane_production
            self.restart.ogsmout_ogngplpr.at[(3, int(self.rest_curcalyr)), nam.value] = self.propane_production
            self.restart.ogsmout_ogngplbu.at[(3, int(self.rest_curcalyr)), nam.value] = self.butane_production
            self.restart.ogsmout_ogngplis.at[(3, int(self.rest_curcalyr)), nam.value] = self.isobutane_production
            self.restart.ogsmout_ogngplpp.at[(3, int(self.rest_curcalyr)), nam.value] = self.proplus_production

        pass
