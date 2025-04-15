"""Submodule for calculating the natural gas processing facility discounted cash flow.

Summary
-------
The ngp cashflow class is a cashflow calculation specific to natural gas processing facility economics. The class operates as follows:

    1. The module is initialized in the ngp class *__init__* method, with class dataframes and variables being declared here.

    2. The *setup* function is called, which loads in base input file assumptions (i.e. tax rates), all other setup is done
       in ngp.py.

    3. Revenue from captured co2 is calculated in *calculate revenue*.

    4. Calculate energy costs.

    5. Calculate total project capital costs and operating costs.

    6. Split costs into intangible and tangible costs based on HSM input assumptions in *calculate_intangible_tangible*.

    7. Run *calculate_depreciation* to calculate amortization and depreciation based on intangible/tangible breakdown.

    8. Run *calculate_econ_limit* to determine the project operation year at which primary project operations are no longer profitable.

    9. Apply taxes.

    10. Calculate project NPV in *calculate_cash_flow* and calculate project profitability in *calculate_profitability*


Input Files
___________
None


Model Functions and Class Methods
_________________________________



Output Debug Files
__________________



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


class NGP_CashFlow:
    """DataFrame-based, discounted cash flow calculator.
    """

    # DataFrames used throughout all the instances of CashFlow objects
    depreciation_schedule_filename  = ''            #: filename of depreciation_schedule
    depreciation_schedule           = pd.DataFrame  #: table of depreciation schedules
    class_init_bool                 = False         #: check if CashFlow class is init'ed

    def __init__(self):
        """Initializes NGP CashFlow object.

        Returns
        -------
        CashFlow

        """
        # DataFrame of non-time-depended properties
        self.properties = pd.DataFrame()

        # DataFrames for production and revenue calculations
        self.co2_flow               = pd.DataFrame()  # DataFrame of CO2 emissions
        self.co2_cost               = pd.DataFrame()  # Revenue from carbon capture and storage
        self.electricity_demand     = pd.DataFrame()  # Total electricity demand

        # DataFrames for economic limit calculations
        self.electricity_cost        = pd.DataFrame()  # electricity cost
        self.operating_cost         = pd.DataFrame()  # operating cost
        self.capital_cost           = pd.DataFrame()  # capital cost
        self.econ_limit             = pd.DataFrame()  # economic limit

        # DataFrames for depletion, amortization, and depletion
        self.intang_cost            = pd.DataFrame()  # intangible costs
        self.tang_cost              = pd.DataFrame()  # tangible costs
        self.tangible_adjustment    = pd.DataFrame()  # tangible adjustment
        self.expn_intang_cost       = pd.DataFrame()  # expensed intangible cost
        self.amor_intang_cost       = pd.DataFrame()  # amortized intangible cost
        self.deprec_tang_cost       = pd.DataFrame()  # depreciated tangible cost


        # DataFrames for federal tax calculations
        self.tax_base_fed   = pd.DataFrame()  # federal tax base
        self.fed_tax        = pd.DataFrame()  # federal tax

        # DataFrames for cash_flow calculations
        self.cash_flow                  = pd.DataFrame()  # un-discounted cash flow


    @classmethod
    def setup(cls):
        """Initializes CashFlow input variables.

        Will only load input data once. Must load in all input filenames into the relevent variables
        (e.g. deprec_schedule filename)

        Returns
        -------
        cls.depreciation_schedule : df
            DataFrame of depreciation schedule assumptions

        cls.class_init_bool : boolean
            Flag for whether cashflow class has been initialized

        """
        if not NGP_CashFlow.class_init_bool:

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
        self.co2_cost : df
            DataFrame of facility cost from CO2 flows

        """
        logger.info('Cashflow Calculate Revenue')
        self.co2_cost  = self.co2_flow.mul(self.properties[nam.co2_price], axis=0)

        pass


    def calculate_electricity_cost(self):
        """Calculates electricity cost associated with CO2 capture at NGP facilities.

        Returns
        -------
        """
        logger.info('Cashflow Calculate Electricity Costs')
        self.electricity_cost  = (self.electricity_demand.mul((self.properties[nam.electricity_cost] * 
                                                              self.properties['hr_to_yr'] * 
                                                              self.properties['electric_retrofit_cap']), axis=0))

        pass


    def calculate_total_operating_costs(self):
        """Calculates total costs
        """
        logger.info('Cashflow Calculate Total Project Costs')


        # transport and storage costs are included in the "price" from CCATS
        self.operating_cost = self.electricity_cost + self.operating_cost

        pass


    def calculate_intangible_tangible(self):
        """Calculates all tangible and intangible cost.  Tangible and intangible drilling costs?
        Matt:    Tangible drilling costs (TDC) are expenses associated with the physical aspects of drilling for oil and NG.
            Intangible drilling costs (IDC) are expenses associated with everything except the drilling equipment.
        Returns
        -------
        self.tang_cost : df
            DataFrame of project tangible costs

        self.intang_cost : df
            DataFrame of project intangible costs
        """
        logger.info('Cashflow Calculate Intangible/Tangible Costs')

        self.tang_cost = self.capital_cost.mul(self.properties[nam.exp_tang_frac], axis=0)
        self.intang_cost = self.capital_cost.mul(1.0 - self.properties[nam.exp_tang_frac], axis=0)

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
                                      NGP_CashFlow.depreciation_schedule,
                                      left_on=nam.amor_schedule,
                                      right_on=nam.depreciation_method))

        self.amor_intang_cost = amor_intang_base.copy() * 0.0
        for col in amor_intang_base.columns:
            shifted = temp_schedule.shift(col, axis=1, fill_value=0)
            self.amor_intang_cost += shifted.mul(amor_intang_base[col], axis=0)

        # tangible depreciation
        temp_schedule = self.tang_cost.copy() * 0.0
        temp_schedule.update(pd.merge(self.properties,
                                      NGP_CashFlow.depreciation_schedule,
                                      left_on=nam.deprec_schedule,
                                      right_on=nam.depreciation_method))

        self.deprec_tang_cost = self.tang_cost.copy() * 0.0
        for col in self.tang_cost.columns:
            shifted = temp_schedule.shift(col, axis=1, fill_value=0)
            self.deprec_tang_cost += shifted.mul(self.tang_cost[col], axis=0)


        pass


    def calculate_econ_limit(self):
        """Calculates project economic limit (Net Operating Income) to determine facility lifetime.

        Returns
        -------
        self.econ_limit : df
            DataFrame of project economic limits
        """
        logger.info('Cashflow Calculate Econ Limit')

        self.econ_limit = -self.co2_cost - self.operating_cost

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

        self.tax_base_fed = -self.co2_cost - self.operating_cost - self.expn_intang_cost - self.amor_intang_cost - self.deprec_tang_cost
        self.fed_tax = self.tax_base_fed.mul(self.properties[nam.fed_tax_rate], axis=0)

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

        self.cash_flow = -self.co2_cost - self.capital_cost - self.operating_cost - self.fed_tax

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

        value = self.capital_cost.to_numpy()
        rate = self.properties[nam.discount_rate].to_numpy()
        capital_cost = (value / (1 + rate[:, None]) ** np.arange(0, np.shape(value)[1])).sum(axis=1) + 1
        self.properties[nam.capital_cost] = capital_cost  # Some sites have 0 capital costs
        self.properties[nam.profitability] = self.properties[nam.net_present_value]/capital_cost

        #Replace infinite values with nans
        self.properties[nam.profitability] = self.properties[nam.profitability].replace([np.inf, -np.inf], np.nan)

        pass


    def calculate_cost_tonne(self):
        """Calculates facility costs in $_tonne

        Returns
        -------
        self.properties : df
            DataFrame of key project properties for cashflow (i.e. project cost bases)
        """
        logger.info('Cashflow Calculate Cost per Tonne of CO2')
    
        # Total cost $/tonne CO2
        annual_incremental_costs = self.capital_cost[0] * (1 / len(self.capital_cost.columns)) + self.operating_cost[0]
        self.properties['total_cost_tonne_co2'] = annual_incremental_costs / self.co2_flow[0]
        
        # Investment cost $/tonne CO2
        annual_incremental_costs = self.capital_cost[0] * (1 / len(self.capital_cost.columns))
        self.properties['inv_cost_tonne_co2'] = annual_incremental_costs / self.co2_flow[0]
        
        # Operating cost $/tonne CO2
        self.properties['om_cost_tonne_co2'] = self.operating_cost[0] / self.co2_flow[0]
        
        pass


    def to_csv(self, output_path, filename, year, iteration, mode='a', header=True):
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
        if Path(output_path + filename + '_props.csv').is_file():
            temp.to_csv(output_path + filename + '_props.csv', mode=mode, header=False)
        else:
            temp.to_csv(output_path + filename + '_props.csv', mode=mode, header=header)

        temp = self.co2_flow.copy()
        temp.insert(0, nam.year, year)
        temp.insert(0, nam.iteration, iteration)
        temp.insert(0, nam.array, 'co2_flow')
        temp = temp.sort_index()


        costs = [nam.co2_flow, nam.co2_cost, nam.electricity_cost, nam.operating_cost, nam.capital_cost,
                 nam.intang_cost, nam.tang_cost, nam.tangible_adjustment, nam.expn_intang_cost, nam.amor_intang_cost,
                 nam.deprec_tang_cost, nam.econ_limit, nam.tax_base_fed, nam.fed_tax, nam.cash_flow]

        for cost in costs:
            temp = getattr(self, cost).copy()
            temp.insert(0, nam.year, year)
            temp.insert(0, nam.iteration, iteration)
            temp.insert(0, nam.array, cost)
            temp = temp.sort_index()
            if Path(output_path + filename + '_ngp_costs.csv').is_file():
                temp.to_csv(output_path + filename + '_ngp_costs.csv', mode=mode, header=False)
            else:
                temp.to_csv(output_path + filename + '_ngp_costs.csv', mode=mode, header=header)
        pass
