"""Submodule for calculating the discounted cash flow.

Summary
-------
The cashflow class is a universal cashflow calculation used by the **Onshore**, **Offshore** and **Alaska** submodule to
calculate project economics. The class operates as follows:

    1. The module is initialized in the onshore class *__init__* method, with class dataframes and variables being declared here.

    2. The *setup* function is called, which loads in base input file assumptions (i.e. tax rates), all other setup is done
       in the submodules (i.e. **Onshore.py**).

    3. Project revenue from crude oil and natural gas, is calculated in *calculate revenue*.

    4. Calculate project royalties and severance taxes for crude oil and natural gas production

    5. Calculate project drill cost (combination of dryhole and drilling costs)

    6. For **Onshore** submodule developing projects run *calculate_ngpl_operating_cost* and *calculate_operating_cost* to aggregate
       multiple cost streams. *calculate_ngpl_operating_cost* also includes NGPL revenues, which are allocated as a negative cost.
       In **Alaska** and **Offshore** submodules ngpl operating costs aren't calculated and aggregated operating costs
       are loaded in directly from the submodule

    7. Calculate project transportation costs.

    8. Split costs into intangible and tangible costs based on HSM input assumptions in *calculate_intangible_tangible*.

    9. Run *calculate_depreciation* to calculate amortization and depreciation based on intangible/tangible breakdown.
       For **Onshore** submodule only run *calculate_gg_la_depletion* to apply depletion for geological and lease costs for
       undiscovered conventional projects.

    10. Run *calculate_econ_limit* to determine the project operation year at which primary project operations are no longer profitable.

    11. Run *calculate_abandonment* to apply abandonment costs and zero out production for projects beyond their economic limit.

    12. Apply taxes and credits in *apply_co2_eor_tax_credit*, *calculate_state_tax* and *calculate_fed_tax*.

    13. Calculate project NPV in *calculate_cash_flow* and calculate project profitability in *calculate_profitability*


Input Files
___________
None


Model Functions and Class Methods
_________________________________
    * __init__ - Initializes CashFlow object
    * setup - Initializes CashFlow input variables
    * calculate_revenue - Calculates revenue from crude and natural gas production
    * calculate_royalty - Calculates crude and natural gas royalty
    * calculate_severance - Calculates crude and natural gas severance tax
    * calculate_drill_cost - Calculates drilling cost
    * calculate_ngpl_operating_cost - Calculates NGPL operating cost. Usually a negative value to be applied against opex
    * calculate_operating_cost - Calculates operating expenses for **Onshore** submodule (**Offshore** and **Alaska**
      operating costs are loaded in directly)
    * calculate_trans_cost - Calculates crude and natural gas transportation costs
    * calculate_intangible_tangible - Calculates all tangible and intangible cost
    * calculate_depreciation - Calculates all tangible and intangible cost
    * calculate_gg_la_depletion - Calculates depletion from geological, geophysical and least acquisition expenses
    * calculate_econ_limit - Calculates project economic limit (Net Operating Income) to determine well lifetime
    * calculate_state_tax - Calculates the state tax base and state tax
    * calculate_fed_tax - Calculates the federal tax base and federal tax
    * calculate_cash_flow - Calculates the discounted cash flow (NPV)
    * calculate_profitability - Calculates project profitability
    * to_csv - Prints cash flow data and properties to csv


Output Debug Files
__________________
    * cashflow_props.csv - Output of cashflow "properties" table
    * cashflow_costs.csv - Output of cashflow costs


Output Restart Variables
________________________
None


Example
-------
Time-dependent DataFrames should have the following format,
    index   0       1       2       ...
    0       val00   val01   val02   ...
    1       val10   val11   val12   ...
    2       val20   val21   val22   ...
    ...     ...     ...     ...     ...

    * Where the row/index values represent well/project/field id numbers, and columns are zero-indexed projection years.
    * Columns can be set up using, list(range(year_range))
    * Where year_range is the last year of the evaluation period.

One DataFrame, the self.properties DataFrame, should have the following form
    index   nam.crude_price nam.natgas_price nam.crude_tariff_price ...
    0       val00           val01            val02                  ...
    1       val10           val11            val12                  ...
    2       val20           val21            val22                  ...
    ...     ...             ...              ...                    ...
Where the row/index values represent well/project/field id numbers, and columns are non-time-dependent variables for
each project.

The recommended usage is,
    import cash_flow as cf
    cash_flow = cf.CashFlow()
    cash_flow.calculation_revenue()
    ...


Notes
-----
    * Calculations are currently unit-agnostic. Rates, e.g. state tax, are assumed to be fractional rather than percentile.

Cashflow Class Methods
______________________
"""
import pandas as pd
import names as nam
import numpy as np
import common as com
from pathlib import Path
from hsm import logging

# Get logging
logger = logging.getLogger('common.py')


class CashFlow:
    """DataFrame-based, discounted cash flow calculator.
    """

    # DataFrames used throughout all instances of CashFlow objects
    state_tax_filename              = ''            #: filename of state_tax_lookup
    state_tax_lookup                = pd.DataFrame  #: state tax rate lookup table
    depreciation_schedule_filename  = ''            #: filename of depreciation_schedule
    depreciation_schedule           = pd.DataFrame  #: table of depreciation schedules
    class_init_bool                 = False         #: check if CashFlow class is init'ed

    def __init__(self):

        """Initializes CashFlow object.

        Returns
        -------
        CashFlow

        """
        # DataFrame of non-time-depended properties
        self.properties = pd.DataFrame()

        # DataFrames for production and revenue calculations
        self.crude_production       = pd.DataFrame()  # crude production
        self.natgas_production      = pd.DataFrame()  # natural gas production
        self.ngpl_production        = pd.DataFrame()  # ngpl production
        self.ch4_emissions          = pd.DataFrame()  # dataframe of ch4 emissions
        self.co2_use                = pd.DataFrame()  # co2 use
        self.crude_revenue          = pd.DataFrame()  # after-tariff crude revenue
        self.natgas_revenue         = pd.DataFrame()  # after-tariff natural gas revenue
        self.ngpl_revenue           = pd.DataFrame()  # after-tariff ngpl revenue
        self.vent_natgas_revenue    = pd.DataFrame()  # Revenue from natural gas that is captured
        self.revenue                = pd.DataFrame()  # after-tariff revenue

        # DataFrames for transportation calculations
        self.crude_trans    = pd.DataFrame()  # crude transportation cost
        self.natgas_trans   = pd.DataFrame()  # natural gas transportation cost
        self.trans_cost = pd.DataFrame()      # total transportation cost

        # DataFrames for royalty calculations
        self.crude_royalty          = pd.DataFrame() # crude royalty payments
        self.natgas_royalty         = pd.DataFrame() # natural gas royalty payments
        self.vent_natgas_royalty    = pd.DataFrame() # natural gas vented/flared royalties
        self.royalty                = pd.DataFrame() # total royalty payments

        # DataFrames for severance calculations
        self.severance_tax  = pd.DataFrame()  # total severance tax payments

        # DataFrames for drill cost calculations
        self.drill_cost         = pd.DataFrame()  # drilling cost
        self.exp_drill_cost     = pd.DataFrame()  # total exploratory drilling cost
        self.dev_drill_cost     = pd.DataFrame()  # total development drilling cost
        self.exp_dry_cost       = pd.DataFrame()  # total exploratory dry drilling cost
        self.dev_dry_cost       = pd.DataFrame()  # total development dry drilling cost
        self.ch4_emission_cost  = pd.DataFrame() # total ch4 emissions costs

        # DataFrames for depletion, amortization, and depletion
        self.intang_cost            = pd.DataFrame()  # intangible costs
        self.tang_cost              = pd.DataFrame()  # tangible costs
        self.tangible_adjustment    = pd.DataFrame()  # tangible adjustment
        self.general_admin_cost     = pd.DataFrame()  # general and administrative (g&a) cost (for amortization)
        self.expn_intang_cost       = pd.DataFrame()  # expensed intangible cost
        self.amor_intang_cost       = pd.DataFrame()  # amortized intangible cost
        self.deprec_tang_cost       = pd.DataFrame()  # depreciated tangible cost

        # DataFrames for economic limit calculations
        self.fed_credit             = pd.DataFrame()  # federal tax credit payments
        self.operating_cost         = pd.DataFrame()  # operating cost
        self.co2_operating_cost     = pd.DataFrame()  # co2 operating cost
        self.crude_operating_cost   = pd.DataFrame()  # crude operating cost
        self.natgas_operating_cost  = pd.DataFrame()  # natgas operating cost
        self.ngpl_operating_cost    = pd.DataFrame()  # ngpl operating cost
        self.equip_cost             = pd.DataFrame()  # equipment cost
        self.econ_limit             = pd.DataFrame()  # economic limit

        # DataFrames for abandonment calculation
        self.abandon_cost   = pd.DataFrame()  # abandonment cost

        # DataFrames for state tax calculations
        self.depletion      = pd.DataFrame()  # depletion cost
        self.dry_hole_cost  = pd.DataFrame()  # dry hole cost
        self.tax_base_state = pd.DataFrame()  # state tax base
        self.state_tax      = pd.DataFrame()  # state tax

        # DataFrames for federal tax calculations
        self.eor_tax_credit = pd.DataFrame()  # EOR tax credit
        self.invest_credit  = pd.DataFrame()  # investment tax credit
        self.tax_base_fed   = pd.DataFrame()  # federal tax base
        self.fed_tax        = pd.DataFrame()  # federal tax

        # DataFrames for cash_flow calculations
        self.kap_cost                   = pd.DataFrame()  # other capital costs
        self.gg_la_cost                 = pd.DataFrame()  # geological, geophysical, and lease acquisition
        self.gg_la_cost_dep             = pd.DataFrame()  # geological, geophysical, and lease acquisition after depletion
        self.cash_flow                  = pd.DataFrame()  # un-discounted cash flow


    @classmethod
    def setup(cls):
        """Initializes CashFlow input variables.

        Will only load input data once. Must load in all input filenames into the relevant variables
        (e.g. state_tax_filename)

        Returns
        -------
        cls.state_tax_lookup : df
            DataFrame of state tax rates

        cls.depreciation_schedule : df
            DataFrame of depreciation schedule assumptions

        cls.class_init_bool : boolean
            Flag for whether cashflow class has been initialized

        """
        if not CashFlow.class_init_bool:
            ###Import lookup table for state taxes
            cls.state_tax_lookup = com.read_dataframe(cls.state_tax_filename)
            cls.state_tax_lookup[nam.state_tax_rate]       = \
                cls.state_tax_lookup[nam.state_tax_rate] / 100.
            cls.state_tax_lookup[nam.sev_rate_crude_rev]   = \
                cls.state_tax_lookup[nam.sev_rate_crude_rev] / 100.0
            cls.state_tax_lookup[nam.sev_rate_natgas_rev]  = \
                cls.state_tax_lookup[nam.sev_rate_natgas_rev] / 100.0


            ###Import depreciation Schedule
            cls.depreciation_schedule = com.read_dataframe(cls.depreciation_schedule_filename)
            cls.depreciation_schedule = cls.depreciation_schedule.set_index(nam.depreciation_method)
            cls.depreciation_schedule = cls.depreciation_schedule.div(100.0, fill_value=0.0)
            cls.depreciation_schedule = cls.depreciation_schedule.reset_index()

            cls.class_init_bool = True


    def calculate_revenue(self):
        """Calculates revenue from crude and natural gas production.

        Returns
        -------
        self.crude_revenue : df
            DataFrame of project revenue from crude oil production

        self.natgas_revenue : df
            DataFrame of project revenue from natural gas production

        self.revenue : df
            DataFrame of project revenue from crude oil and natural gas production

        """
        logger.info('Cashflow Calculate Revenue')

        try: #For New Production
            self.crude_revenue  = self.crude_production.mul(self.properties[nam.crude_price] -
                                                           self.properties[nam.crude_tariff_price], axis=0)
            self.natgas_revenue = self.natgas_production.mul(self.properties[nam.natgas_price] -
                                                             self.properties[nam.natgas_tariff_price], axis=0)
            self.vent_natgas_revenue = self.ch4_emissions.mul(self.properties[nam.natgas_price] -
                                                             self.properties[nam.natgas_tariff_price], axis=0)
            self.revenue = self.crude_revenue + self.natgas_revenue + self.vent_natgas_revenue

        except: #For Legacy production
            #Crude Revenue
            crude_production = self.crude_production.to_numpy()
            crude_price = self.crude_price.to_numpy()
            self.crude_revenue = pd.DataFrame(crude_production * crude_price)

            #Natgas Revenue
            natgas_production = self.natgas_production.to_numpy()
            natgas_price = self.natgas_price.to_numpy()
            self.natgas_revenue = pd.DataFrame(natgas_production * natgas_price)
            
            self.vent_natgas_revenue = pd.DataFrame(index = self.natgas_revenue.index, columns = self.natgas_revenue.columns, data = 0.0)

            #Total
            self.revenue = self.crude_revenue + self.natgas_revenue

        pass


    def calculate_royalty(self):
        """Calculates crude and natural gas royalty.

        Note
        ----
        Currently crude and natural gas have the same, time-dependent royalty rate. Royalties are applied to all projects
        because we don't have the means to differentiate between federal and non-federal land.

        Returns
        -------
        self.crude_royalty : df
            Royalty paid on project crude oil production

        self.natgas_royalty : df
            Royalty paid on project natural gas production

        self.royalty : df
            Royalty paid on project crude oil and natural gas production
        """
        logger.info('Cashflow Calculate Royalty')

        self.crude_royalty = self.crude_revenue.mul(self.properties[nam.royalty_rate], axis=0)
        self.natgas_royalty = self.natgas_revenue.mul(self.properties[nam.royalty_rate], axis=0)
        self.vent_natgas_royalty = self.vent_natgas_revenue.mul(self.properties[nam.royalty_rate], axis=0)
        self.royalty = self.crude_royalty + self.natgas_royalty + self.vent_natgas_royalty

        pass


    def calculate_severance(self):
        """Calculates crude and natural gas severance tax.

        Returns
        -------
        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)

        self.severance_tax : df
            DataFrame of project severance taxes
        """
        logger.info('Cashflow Calculate Severance')

        # Drop existing state tax rates and lookup rate based on state abbreviation
        self.properties = self.properties.drop(nam.sev_rate_crude_rev   , axis=1, errors='ignore')
        self.properties = self.properties.drop(nam.sev_rate_natgas_rev  , axis=1, errors='ignore')
        self.properties = self.properties.drop(nam.sev_rate_crude_prod  , axis=1, errors='ignore')
        self.properties = self.properties.drop(nam.sev_rate_natgas_prod , axis=1, errors='ignore')
        self.properties = self.properties.reset_index().merge(CashFlow.state_tax_lookup[[nam.state,
                                                              nam.sev_rate_crude_rev,
                                                              nam.sev_rate_natgas_rev,
                                                              nam.sev_rate_crude_prod,
                                                              nam.sev_rate_natgas_prod]],
                                   on=nam.state,
                                   how='left',
                                   suffixes=('_del', None)).set_index('index')

        self.severance_tax = ((self.crude_revenue  - self.crude_royalty ).mul(self.properties[nam.sev_rate_crude_rev], axis=0) +
                              (self.natgas_revenue - self.natgas_royalty).mul(self.properties[nam.sev_rate_natgas_rev], axis=0) +
                              self.crude_production.mul(self.properties[nam.sev_rate_crude_prod], axis=0) +
                              self.natgas_production.mul(self.properties[nam.sev_rate_natgas_prod], axis=0))

        pass


    def calculate_drill_cost(self):
        """Calculates drilling cost.

        Returns
        -------
        self.drill_cost : df
            DataFrame of overall project drilling costs

        self.dry_hole_cost : df
            DataFrame of just project dryhole costs
        """
        logger.info('Cashflow Calculate Drill Cost')

        self.drill_cost     = self.exp_drill_cost + self.exp_dry_cost + self.dev_drill_cost + self.dev_dry_cost
        self.dry_hole_cost  = self.exp_dry_cost + self.dev_dry_cost

        pass


    def calculate_ngpl_operating_cost(self):
        """Calculates NGPL operating cost. Usually a negative value to be applied against opex.

        Returns
        -------
        self.ngpl_operating_cost : df
            DataFrame of NGPL operating costs (or revenues when negative)

        """
        logger.info('Cashflow Calculate NGPL Operating Cost')

        ngpl_revenue = self.properties[nam.ngpl_price] * self.properties[nam.ngpl_volume] * 42.0 / 1000.0
        ngpl_rate = self.properties[nam.ngpl_cost] - ngpl_revenue
        self.ngpl_operating_cost = self.natgas_production.mul(ngpl_rate, axis=0)

        pass


    def calculate_operating_cost(self):
        """Calculates operating expenses for **Onshore** submodule (**Offshore** and **Alaska** operating costs are loaded in directly).

        Returns
        -------
        self.crude_operating_cost : df
            DataFrame of crude oil production operating cost

        self.natgas_operating_cost : df
            DataFrame of natural gas production operating cost

        self.co2_operating_cost : df
            DataFrame of CO2 EOR operating costs

        self.general_admin_cost : df
            DataFrame of general well admin costs

        self.operating_cost : df
            DataFrame of total operating costs
        """
        logger.info('Cashflow Calculate Operating Cost')
        self.crude_operating_cost = self.crude_production.mul(self.properties[nam.production_opex_brl], axis=0)
        self.natgas_operating_cost = self.natgas_production.div(5.62).mul(self.properties[nam.production_opex_brl], axis=0)
        self.co2_operating_cost = self.co2_use.mul(self.properties[nam.co2_cost], axis=0)
        self.general_admin_cost = self.general_admin_cost.mul(self.properties[nam.sga_opex_well], axis=0)
        self.operating_cost = self.crude_operating_cost + self.natgas_operating_cost + self.co2_operating_cost + self.general_admin_cost + self.ngpl_operating_cost

        pass


    def calculate_trans_cost(self):
        """Calculates crude and natural gas transportation costs.

        Returns
        -------
        self.crude_trans : df
            DataFrame of project crude oil transportation costs

        self.natgas_trans : df
            DataFrame of project natural gas transportation costs

        self.trans_cost : df
            DataFrame of total project transportation costs
        """
        logger.info('Cashflow Calculate Transportation')

        self.crude_trans    = self.crude_production.mul(self.properties[nam.crude_trans_price], axis=0)
        self.natgas_trans   = self.natgas_production.div(5.62).mul(self.properties[nam.natgas_trans_price], axis=0)
        self.trans_cost     = self.crude_trans + self.natgas_trans

        pass


    def calculate_intangible_tangible(self):
        """Calculates all tangible and intangible cost.

        Returns
        -------
        self.tang_cost : df
            DataFrame of project tangible costs

        self.intang_cost : df
            DataFrame of project intangible costs
        """
        logger.info('Cashflow Calculate Intangible/Tangible Costs')

        self.tang_cost = self.exp_drill_cost.mul(self.properties[nam.exp_tang_frac], axis=0) + \
                         self.dev_drill_cost.mul(self.properties[nam.dev_tang_frac], axis=0) + \
                         self.kap_cost.mul(self.properties[nam.kap_tang_frac], axis=0)
        self.intang_cost = self.exp_drill_cost.mul(1.0 - self.properties[nam.exp_tang_frac], axis=0) + \
                           self.dev_drill_cost.mul(1.0 - self.properties[nam.dev_tang_frac], axis=0) + \
                           self.kap_cost.mul(1.0 - self.properties[nam.kap_tang_frac], axis=0) + \
                           self.equip_cost

        pass


    def calculate_depreciation(self):
        """Calculates all tangible and intangible cost.

        Returns
        -------
        self.expn_intang_cost : df
            DataFrame of expensed intangible costs

        self.amor_intang_cost : df
            DataFrame of amortized intangible costs

        self.deprec_tang_cost : df
            DataFrame of depreciated tangible costs
        """
        logger.info('Cashflow Calculate Depreciation')

        self.expn_intang_cost = self.intang_cost.mul(1.0 - self.properties[nam.intang_amor_frac], axis=0)
        amor_intang_base = self.intang_cost.mul(self.properties[nam.intang_amor_frac], axis=0)

        # amortization
        temp_schedule = amor_intang_base.copy() * 0.0
        temp_schedule.update(pd.merge(self.properties,
                                      CashFlow.depreciation_schedule,
                                      left_on=nam.amor_schedule,
                                      right_on=nam.depreciation_method))

        self.amor_intang_cost = amor_intang_base.copy() * 0.0
        for col in amor_intang_base.columns:
            shifted = temp_schedule.shift(col, axis=1, fill_value=0)
            self.amor_intang_cost += shifted.mul(amor_intang_base[col], axis=0)

        # tangible depreciation
        temp_schedule = self.tang_cost.copy() * 0.0
        temp_schedule.update(pd.merge(self.properties,
                                      CashFlow.depreciation_schedule,
                                      left_on=nam.deprec_schedule,
                                      right_on=nam.depreciation_method))

        self.deprec_tang_cost = self.tang_cost.copy() * 0.0
        for col in self.tang_cost.columns:
            shifted = temp_schedule.shift(col, axis=1, fill_value=0)
            self.deprec_tang_cost += shifted.mul(self.tang_cost[col], axis=0)

        pass


    def calculate_gg_la_depletion(self):
        """Calculates depletion from geological, geophysical and least acquisition expenses.

        Returns
        -------
        self.depletion : df
            DataFrame of depletion from project geological, geophysical and lease costs

        self.gg_la_cost_dep : df
            DataFrame of depreciated project geological, geophysical and lease costs
        """
        logger.info('Cashflow Calculate GGLA Depletion')

        #Get total production for depletion
        total_prod = self.crude_production.add(self.natgas_production.div(5.62))
        remaining_resources_factor = total_prod.add(self.properties[nam.remaining_oil_resources],axis=0).add(self.properties[nam.remaining_oil_resources]/5.62,axis=0)

        #Get depletion and total geo, geo, lease acquisition cost after depletion (currently assumes 100% depletion/0% expense ratio)
        self.depletion = self.gg_la_cost.mul(total_prod).div(remaining_resources_factor).fillna(0.0)
        self.gg_la_cost_dep = self.gg_la_cost.sub(self.depletion)

        pass


    def calculate_econ_limit(self):
        """Calculates project economic limit (Net Operating Income) to determine well lifetime.

        Returns
        -------
        self.econ_limit : df
            DataFrame of project economic limits
        """
        logger.info('Cashflow Calculate Econ Limit')

        self.econ_limit = self.revenue - self.royalty - self.severance_tax - self.operating_cost - self.trans_cost \
                          + self.fed_credit

        pass


    def calculate_abandonment(self):
        """Calculates abandonment and zeros out properties beyond the economic limit.

        Note
        ----
        Assumes no less than 6 years before abandonment to match AOGSS

        Returns
        -------
        self.abandon_cost : df
            DataFrame of project abandonment costs

        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)

        self.revenue : df
            DataFrame of project revenue from crude oil and natural gas production

        self.royalty : df
            Royalty paid on project crude oil and natural gas production

        self.severance_tax : df
            DataFrame of project severance taxes

        self.operating_cost : df
            DataFrame of total operating costs

        self.expn_intang_cost : df
            DataFrame of expensed intangible costs

        self.amor_intang_cost : df
            DataFrame of amortized intangible costs

        self.deprec_tang_cost : df
            DataFrame of depreciated tangible costs

        self.dry_hole_cost : df
            DataFrame of just project dryhole costs

        self.drill_cost : df
            DataFrame of overall project drilling costs

        self.crude_production : df
            DataFrame of project crude oil production

        self.natgas_production : df
            DataFrame of project natural gas production

        self.crude_revenue : df
            DataFrame of project revenue from crude oil production

        self.natgas_revenue : df
            DataFrame of project revenue from natural gas production

        self.crude_trans : df
            DataFrame of crude oil transportation costs

        self.natgas_trans : df
            DataFrame of natural gas transportation costs

        self.trans_cost : df
            DataFrame of crude oil and natural gas transportation costs

        self.crude_royalty : df
            DataFrame of project crude oil royalties

        self.natgas_royalty : df
            DataFrame of project natural gas royalties
        """
        logger.info('Cashflow Calculate Abandonment')

        min_year = 6

        # make a mask for when the economic limit is less than 0 and the year is greater than the min year
        mask = self.econ_limit.lt(0.0)
        mask[list(range(min_year))] = 0.0
        mask = mask.cumsum(axis=1)
        # need a special mask with a 1/True in the year of abandonment
        abandon_mask = mask.copy().cumsum(axis=1).eq(1.0)
        mask = mask.lt(1.0)

        #Apply Abandonment Cost
        self.abandon_cost = abandon_mask.copy().mul(self.properties[nam.abandon_rate], axis=0)

        #Get economic life of projects
        self.properties[nam.econ_life] = 0
        for year in abandon_mask.columns:
            econ_limit_index = abandon_mask.index[abandon_mask[year] == True].tolist()
            self.properties.loc[econ_limit_index, nam.econ_life] = year

        #Set projects with econ life that exceeds the projection period end of the projection period
        self.properties.loc[self.properties[nam.econ_life] == 0, nam.econ_life] = 30

        #Set following properties to zero following abandonment
        self.revenue            = self.revenue*mask
        self.royalty            = self.royalty*mask
        self.severance_tax      = self.severance_tax*mask
        self.operating_cost     = self.operating_cost*mask
        self.ch4_emission_cost  = self.ch4_emission_cost*mask
        self.expn_intang_cost   = self.expn_intang_cost*mask
        self.amor_intang_cost   = self.amor_intang_cost*mask
        self.deprec_tang_cost   = self.deprec_tang_cost*mask
        self.dry_hole_cost      = self.dry_hole_cost*mask
        self.drill_cost         = self.drill_cost*mask

        self.crude_production   = self.crude_production*mask
        self.natgas_production  = self.natgas_production*mask
        self.crude_revenue      = self.crude_revenue*mask
        self.natgas_revenue     = self.natgas_revenue*mask
        self.crude_trans        = self.crude_trans*mask
        self.natgas_trans       = self.natgas_trans*mask
        self.trans_cost         = self.trans_cost*mask
        self.crude_royalty      = self.crude_royalty*mask
        self.natgas_royalty     = self.natgas_royalty*mask

        pass


    def calculate_co2_eor_tax_credit(self):
        """Calculates CO2 EOR tax Credit.

        Returns
        -------
        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)

        self.eor_tax_credit : df
            DataFrame of project CO2 EOR tax credits

        """
        logger.info('Cashflow Calculate CO2 EOR Tax Credit')

        #Set tax credit to 0 if oil price is greater than maximum price allowed in U.S. Code § 43
        mask = self.properties[nam.crude_price] > self.properties[nam.eor_tc_phaseout]
        temp_df = self.properties[mask].copy()
        temp_df[nam.eor_tc_rate] = 0.0
        self.properties.update(temp_df)

        ##Apply to Amortized intangible costs and Depreciated Tangible Costs
        amor_intang_eor_tax_credit = self.amor_intang_cost.mul(self.properties[nam.eor_tc_rate],axis =0)
        deprec_tang_eor_tax_credit = self.deprec_tang_cost.mul(self.properties[nam.eor_tc_rate],axis =0)

        #Add tax credits to EOR tax credit table
        self.eor_tax_credit = self.eor_tax_credit + amor_intang_eor_tax_credit
        self.eor_tax_credit = self.eor_tax_credit + deprec_tang_eor_tax_credit

        pass


    def calculate_ch4_emission_penalties(self):
        """Calculates CH4 emission penalties.

        * Converts Emissions from mcf to Metric tons
        * Calculates Emission Penalties

        Returns
        -------
        self.ch4_emissions
            DataFrame of CH4 emissions/project in metric tons

        self.ch4_emission_cost : df
            DataFrame of CH4 emission costs/project
        """
        logger.info('Cashflow Calculate CH4 Emissions Penalties')

        #Convert CH4 emissions from mcf to metric tons
        self.ch4_emissions = self.ch4_emissions.div(51.921)

        #Calculate Methane Venting & Flaring Cost Penalties
        self.ch4_emission_cost = self.ch4_emissions.mul(self.ch4_emission_cost, axis = 0)

        pass


    def calculate_state_tax(self):
        """Calculates the state tax base and state tax.

        Returns
        -------
        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)

        self.tax_base_state : df
            DataFrame of taxable project revenues for state taxes

        self.state_tax
            DataFrame of project state taxes
        """
        logger.info('Cashflow Calculate State Tax')

        #Drop existing state tax rate and lookup rate based on state abbreviation
        self.properties = self.properties.drop(nam.state_tax_rate, axis=1, errors='ignore')
        self.properties = self.properties.reset_index().merge(CashFlow.state_tax_lookup[[nam.state, nam.state_tax_rate]],
                                   on=nam.state,
                                   how='left',
                                   suffixes=('_del', None)).set_index('index')

        self.tax_base_state = self.revenue - self.royalty - self.severance_tax - self.operating_cost - \
                              self.abandon_cost - self.expn_intang_cost - self.amor_intang_cost - self.depletion - \
                              self.deprec_tang_cost - self.dry_hole_cost - self.trans_cost
        self.state_tax = self.tax_base_state.mul(self.properties[nam.state_tax_rate], axis=0)

        pass


    def calculate_fed_tax(self):
        """Calculates the federal tax base and federal tax.

        Returns
        -------
        self.tax_base_fed : df
            DataFrame of taxable project revenues for federal taxes

        self.fed_tax : df
            DataFrame of project federal taxes
        """
        logger.info('Cashflow Calculate Fed Tax')

        self.tax_base_fed = self.tax_base_state - self.state_tax - self.eor_tax_credit
        self.fed_tax = self.tax_base_fed.mul(self.properties[nam.fed_tax_rate], axis=0) - \
                       self.fed_credit - self.invest_credit

        pass


    def calculate_cash_flow(self):
        """Calculates the discounted cash flow (NPV).

        Returns
        -------
        self.cash_flow : df
            DataFrame of project cashflows

        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)
        """
        logger.info('Cashflow Calculate Discounted Cash Flow')

        self.cash_flow = self.revenue - self.royalty - self.severance_tax \
                         - self.drill_cost - self.equip_cost - self.kap_cost - self.gg_la_cost - self.operating_cost \
                         - self.abandon_cost - self.state_tax - self.fed_tax - self.trans_cost - self.ch4_emission_cost

        value = self.cash_flow.to_numpy()
        rate = self.properties[nam.discount_rate].to_numpy()
        npv = (value / (1 + rate[:, None]) ** np.arange(0, np.shape(value)[1])).sum(axis=1)
        self.properties[nam.net_present_value] = npv

        #Replace infinite values with nans
        self.properties[nam.net_present_value] = self.properties[nam.net_present_value].replace([np.inf, -np.inf], np.nan)

        pass


    def calculate_profitability(self):
        """Calculates project profitability.

        Returns
        -------
        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)
        """
        logger.info('Cashflow Calculate Profitability')

        value = self.drill_cost.to_numpy()
        rate = self.properties[nam.discount_rate].to_numpy()
        capital_cost = (value / (1 + rate[:, None]) ** np.arange(0, np.shape(value)[1])).sum(axis=1)
        self.properties[nam.capital_cost] = capital_cost
        self.properties[nam.profitability] = self.properties[nam.net_present_value]/capital_cost

        #Replace infinite values with nans
        self.properties[nam.profitability] = self.properties[nam.profitability].replace([np.inf, -np.inf], np.nan)

        pass


    def to_csv(self, output_path, filename, year, iteration, cycle = 'nan', mode='a', header=True):
        """Prints cash flow data and properties to csv.

        Returns
        -------
        None

        """
        pass
        logger.info('Cashflow Debug')

        temp = self.properties.copy()
        temp.insert(0, nam.year, year)
        temp.insert(0, nam.iteration, iteration)
        temp.insert(0, 'cycle', cycle)
        if Path(output_path + filename + '_props.csv').is_file():
            temp.to_csv(output_path + filename + '_props.csv', mode=mode, header=False)
        else:
            temp.to_csv(output_path + filename + '_props.csv', mode=mode, header=header)

        temp = self.crude_production.copy()
        temp.insert(0, nam.year, year)
        temp.insert(0, nam.iteration, iteration)
        temp.insert(0, 'cycle', cycle)
        temp.insert(0, nam.array, nam.crude_production)
        temp = temp.sort_index()


        costs = [nam.natgas_production, nam.crude_revenue, nam.natgas_revenue, nam.revenue, nam.crude_trans,
                 nam.natgas_trans, nam.crude_royalty, nam.natgas_royalty,
                 nam.royalty, nam.severance_tax, nam.drill_cost, nam.exp_drill_cost,
                 nam.dev_drill_cost, nam.exp_dry_cost, nam.dev_dry_cost, nam.intang_cost, nam.tang_cost,
                 nam.tangible_adjustment, nam.general_admin_cost, nam.expn_intang_cost, nam.amor_intang_cost,
                 nam.deprec_tang_cost, nam.fed_credit, nam.operating_cost, nam.equip_cost, nam.econ_limit,
                 nam.abandon_cost, nam.depletion, nam.dry_hole_cost, nam.tax_base_state, nam.state_tax,
                 nam.eor_tax_credit, nam.invest_credit, nam.tax_base_fed, nam.fed_tax, nam.kap_cost,
                 nam.gg_la_cost, nam.cash_flow]

        for cost in costs:
            temp = getattr(self, cost).copy()
            temp.insert(0, nam.year, year)
            temp.insert(0, nam.iteration, iteration)
            temp.insert(0, 'cycle', cycle)
            temp.insert(0, nam.array, cost)
            temp = temp.sort_index()
            if Path(output_path + filename + '_costs.csv').is_file():
                temp.to_csv(output_path + filename + '_costs.csv', mode=mode, header=False)
            else:
                temp.to_csv(output_path + filename + '_costs.csv', mode=mode, header=header)
        pass
