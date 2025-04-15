"""Class for declaring CCATS Financial Assumptions.

CCATS Financial: Summary
________________________
**ccats_financial** is the preprocessor for model financial assumptions and a utility for :ref:`preprocessor` to
calculate the present value of model costs by block.

The module operates as follows:

    1. Reads in financial assummption inputs in :meth:`~ccats_financial.CCATS_Finance.setup`.

    2. Calculates the CCATS model discount rate in :meth:`~ccats_financial.CCATS_Finance.calculate_discount_rate`.


:meth:`~ccats_financial.CCATS_Finance.calculate_discount_investment` and :meth:`~ccats_financial.CCATS_Finance.calculate_discount_variable_policy`
are called from :ref:`preprocessor`.

CCATS Financial: Functions and Class Methods
____________________________________________
    * :meth:`~ccats_financial.CCATS_Finance.__init__` - Constructor to initialize Class (instantiated by :meth:`module.Module.setup` in :ref:`module`).
    * :meth:`~ccats_financial.CCATS_Finance.setup` - CCATS_Financial Setup method (called by :meth:`module.Module.setup`).
    * :meth:`~ccats_financial.CCATS_Finance.run` - Runs main CCATS_Financial Processes (called by :meth:`module.Module.setup`).
    * :meth:`~ccats_financial.CCATS_Finance.calculate_discount_rate` - Calculate discount rate for Carbon Capture and Storage Projects (called by :meth:`~ccats_financial.CCATS_Finance.run`).
    * :meth:`~ccats_financial.CCATS_Finance.calculate_discount_investment` - Compute the present value of an investment (called by :meth:`preprocessor.Preprocessor.instantiate_pyomo_series`).
    * :meth:`~ccats_financial.CCATS_Finance.calculate_discount_variable_policy` - Compute the present value of a variable or policy cost (called by :meth:`preprocessor.Preprocessor.instantiate_pyomo_series`).


CCATS Financial: Input Files
____________________________
    * fin_setup.csv - setup file with model financial inputs (debt rate, block durations, etc.).


CCATS Financial: Output Debug Files
___________________________________
None


CCATS Financial: Output Restart Variables
_________________________________________
None


CCATS Financial: Code
_____________________
"""

import pandas as pd
import numpy as np
from ccats_common import common as com


class CCATS_Finance:
    """Class for declaring CCATS Financial Assumptions.

    """

    def __init__(self, parent):
        """
        Initializes attributes for CCATS_Finance.

        Parameters
        ----------
        parent : module.Module
            Pointer to head module 

        Returns
        -------
        None
        """
        self.parent = parent #: module.Module head module

        ### Input Variables
        self.debt_ratio                     = 0.0 # Ratio of debt financing
        self.return_over_capital_cost       = 0.0 # Assumed required return over capital cost for investment
        self.fed_tax_rate                   = 0.0 # Assumed federal tax rate
        self.duration_b0                    = 0   # Duration of optimization model block 1
        self.duration_b1                    = 0   # Duration of optimization model block 2
        self.duration_b2                    = 0   # Duration of optimization model block 3
        self.duration_45q                   = 0   # Duration of 45Q tax credit policy for a single facility
        self.financing_risk_premia          = 0.0 # Assumed risk premia required by investors over risk-free market rate
        self.financing_years_transport      = 0   # Assumed financing years for pipeline transport investments
        self.financing_years_storage        = 0   # Assumed financing years for saline formation storage investments
        self.fixed_om_fraction_transport    = 0.0 # Fixed O&M costs as a percentage of capital costs for pipeline transport
        self.fixed_om_fraction_storage      = 0.0 # Fixed O&M costs as a percentage of capital costs for saline formation storage
        self.site_dev_years_storage         = 0   # Assumed number of years for storage site development
        self.construction_years_storage     = 0   # Assumed number of years for storage site construction

        ### Process Variables
        self.expected_inflation_rate    = 0.0 # Expected inflation rate
        self.rate_discount              = 0.0 # Model discount rate
        self.rate_borrowing             = 0.0 # Model borrowing rate

        pass
    
    def setup(self, setup_filename):
        '''Setup function for CCATS financial assumptions.

        Parameters
        ----------
        setup_filename : str
            **ccats_financial** setup file name.

        Returns
        -------
        self.fin_table : DataFrame
            DataFrame of CCATS financial inputs (debt rate, block durations, etc.).

        self.debt_ratio : float
            Ratio of debt financing.

        self.return_over_capital_cost : float
            Assumed required return over capital cost for investment.

        self.fed_tax_rate : float
            Assumed federal tax rate.

        self.duration_b0 : int
            Duration of optimization model block 0.

        self.duration_b1 : int
            Duration of optimization model block 1.

        self.duration_b2 : int
            Duration of optimization model block 2.

        self.duration_45q : int
            Duration of 45Q tax credit policy for a single facility.

        self.financing_risk_premia : float
            Assumed risk premia required by investors over risk-free market rate.

        self.financing_years_transport : int
            Assumed financing years for pipeline transport investments.

        self.financing_years_storage : int
            Assumed financing years for saline formation storage investments.

        self.fixed_om_fraction_transport : float
            Fixed O&M costs as a percentage of capital costs for pipeline transport.

        self.fixed_om_fraction_storage : float
            Fixed O&M costs as a percentage of capital costs for saline formation storage.

        self.site_dev_years_storage : int
            Assumed number of years for storage site development.

        self.construction_years_storage : int
            Assumed number of years for storage site construction.

        self.rate_real : NumPy array
            Array of 10-year treasury rate by year

        self.rate_inflation : NumPy array
            Array of inflation rate by year.
        '''
        # Load in financial inputs
        self.fin_table                      = com.read_dataframe(self.parent.input_path + setup_filename, index_col=0)
        self.debt_ratio                     = float(self.fin_table.at['debt_ratio'                , 'value'])
        self.return_over_capital_cost       = float(self.fin_table.at['return_over_capital_cost'  , 'value'])
        self.fed_tax_rate                   = float(self.fin_table.at['fed_tax_rate'                   , 'value'])
        self.duration_b0                    = int(self.fin_table.at['duration_block0', 'value'])
        self.duration_b1                    = int(self.fin_table.at['duration_block1', 'value'])
        self.duration_b2                    = int(self.fin_table.at['duration_block2', 'value'])
        self.duration_45q                   = int(self.fin_table.at['duration_45q', 'value'])
        self.financing_risk_premia          = float(self.fin_table.at['financing_risk_premia', 'value'])
        self.financing_years_transport      = int(self.fin_table.at['financing_years_transport', 'value'])
        self.financing_years_storage        = int(self.fin_table.at['financing_years_storage', 'value'])
        self.fixed_om_fraction_transport    = float(self.fin_table.at['fixed_om_fraction_transport', 'value'])
        self.fixed_om_fraction_storage      = float(self.fin_table.at['fixed_om_fraction_storage', 'value'])
        self.site_dev_years_storage         = float(self.fin_table.at['site_dev_years_storage', 'value'])
        self.construction_years_storage     = float(self.fin_table.at['construction_years_storage', 'value'])

        # Ten year treasury and inflation rates
        self.rate_real = np.array(self.parent.rest_mc_rmtcm10y['value'].values)
        self.rate_inflation = np.array(self.parent.rest_mc_jpgdp['value'].values)

        pass


    def run(self):
        '''Run CCATS financial assumptions.

        Parameters
        ----------
        None

        Returns
        -------
        None
        '''
        # Declare logger locally
        self.logger = self.parent.logger

        ### Run financials
        # Calculate Discount Rate
        self.logger.info('Calculate Model Discount Rate')
        self.calculate_discount_rate()


    def calculate_discount_rate(self):
        '''Calculate discount rate for Carbon Capture and Storage Projects

        Parameters
        ----------
        None

        Returns
        -------
        self.expected_inflation_rate : NumPy array of floats
            Inflation rates by year (fraction). If length < n_years, then the last entry is used for missing years.

        self.rate_discount : NumPy array of floats
            Discount rates by year (fraction). If length < n_years, then the last entry is used for missing years.

        self.rate_borrowing : NumPy array of floats
            Borrowing rates by year (fraction). If length < n_years, then the last entry is used for missing years.
        '''

        #Assign bond and treasury rates (convert from % to fraction)
        self.corp_bond_rate = self.parent.rest_mc_rmcorpbaa.loc[self.parent.year_start:self.parent.year_final]/100.0#.value.to_numpy()/100.0
        self.ten_year_treas_note_yield = self.parent.rest_mc_rmtcm10y.loc[self.parent.year_start:self.parent.year_final]/100.0#.value.to_numpy()/100.0
        self.corp_bond_rate.index = self.corp_bond_rate.reset_index().loc[:,'1']
        self.ten_year_treas_note_yield.index = self.ten_year_treas_note_yield.reset_index().loc[:,'1']

        # Calculate inflation rate (fraction)
        inflation_multiplier_t         = self.parent.rest_mc_jpgdp.loc[self.parent.year_start:self.parent.year_final]
        inflation_multiplier_t_minus_1 = self.parent.rest_mc_jpgdp.loc[self.parent.year_start - 1:self.parent.year_final - 1]
        inflation_multiplier_t_minus_1.index = inflation_multiplier_t_minus_1.index + 1
        self.expected_inflation_rate = inflation_multiplier_t / inflation_multiplier_t_minus_1 - 1.0

        #Calculate weighted average cost of capital
        wacc = self.debt_ratio * self.corp_bond_rate * (1.0 - self.fed_tax_rate) + (1.0 - self.debt_ratio) * self.ten_year_treas_note_yield
        wacc_real = (1. + wacc) / (1.0 + self.expected_inflation_rate) - 1.
        self.rate_discount = wacc_real + self.return_over_capital_cost

        # calculate borrowing rate from discount rate
        self.rate_borrowing = self.rate_discount + self.financing_risk_premia

        pass


    def calculate_discount_investment(self, year_invest, n_financing, fom):
        """Compute the present value of an investment.

            * Called from :ref:`preprocessor` to discount storage and transport investment costs.

        Parameters
        ----------
        year_invest : int
            Year that investment decision occurs.
        n_financing : int
            Number of years that financing is paid over (and duration that Fixed O&M is paid).
        fom : float
            Fraction of CAPEX paid each year as Fixed O&M.

        Returns
        -------
        discount : float
            Relates a lump sum capex investment to its present value (n_years earlier).
        """
        # get borrowing_rate
        if year_invest in self.rate_borrowing.index:
            borrowing_rate = self.rate_borrowing.loc[year_invest,'value']
        else:
            borrowing_rate = self.rate_borrowing.loc[self.parent.year_final,'value']

        # calculate amortization
        amortization = (borrowing_rate * (1.0 + borrowing_rate) ** n_financing) / (
                (1.0 + borrowing_rate) ** n_financing - 1.0)

        # initialize discount and inflation factors
        discount_factor = 1.0
        inflation_factor = 1.0

        # update discount_factor to relate back to year 0 regardless of investment block
        for i in range(self.parent.year_current,year_invest+1):
            # update rates
            if i in self.rate_discount.index:
                discount_rate = self.rate_discount.loc[i,'value']
            else:
                discount_rate = self.rate_discount.loc[self.parent.year_final,'value']

            discount_factor = discount_factor / (1.0 + discount_rate)

        # initialize discount multipliers for capex and fom
        discount_capex = 0.0
        discount_fom = 0.0

        # iterate through each year of financing
        for i in range(year_invest + 1, year_invest + 1 + n_financing):  # start paying off investment one year later

            # sums for capex and fom
            discount_capex = discount_capex + discount_factor
            discount_fom = discount_fom + inflation_factor * discount_factor

            # update rates
            if i in self.rate_discount.index:
                discount_rate = self.rate_discount.loc[i,'value']
            else:
                discount_rate = self.rate_discount.loc[self.parent.year_final,'value']

            if i in self.expected_inflation_rate.index:
                inflation_rate = self.expected_inflation_rate.loc[i,'value']
            else:
                inflation_rate = self.expected_inflation_rate.loc[self.parent.year_final,'value']

            # update factors
            discount_factor = discount_factor / (1.0 + discount_rate)
            inflation_factor = inflation_factor * (1.0 + inflation_rate)

        # Apply amortization and FOM fraction, then combine
        discount_capex = amortization * discount_capex
        discount_fom = fom * discount_fom
        discount = discount_capex + discount_fom

        # remove FOM from the original cost (only applicable if provided as part of capex)
        discount = discount/(1.0+n_financing*fom)

        return discount


    def calculate_discount_variable_policy(self, block_start_year, n_years, include_inflation):
        """Compute the present value of a variable or policy cost.

            * Called from :ref:`preprocessor` to discount policy, CO\ :sub:`2` EOR net cost offers and opex.

        Parameters
        ----------
        year_start : int
            Start year as an index in discount_rates and inflation_rates.
        n_years : int
            Number of years in the block.
        include_inflation : bool
            True to include inflation in calculations, False to only include discount rate.

        Returns
        -------
        discount : float
            Discounts an annual variable or policy cost payment to its present value taking into account block duration.
        """
        # initialize discount and inflation factors
        discount_factor = 1.0
        inflation_factor = 1.0

        # update factor_discount to relate back to year 0 regardless of investment block
        for i in range(self.parent.year_current,block_start_year):
            # update rates
            if i in self.rate_discount.index:
                discount_rate = self.rate_discount.loc[i,'value']
            else:
                discount_rate = self.rate_discount.loc[self.parent.year_final,'value']

            discount_factor = discount_factor / (1.0 + discount_rate)

        # initialize discount multiplier
        discount = 0.0

        # iterate through each year of financing
        for i in range(block_start_year, block_start_year + n_years):

            # sum
            if include_inflation:
                discount = discount + inflation_factor * discount_factor
            else:  # 'policy'
                discount = discount + discount_factor

            # update rates
            if i in self.rate_discount.index:
                discount_rate = self.rate_discount.loc[i,'value']
            else:
                discount_rate = self.rate_discount.loc[self.parent.year_final,'value']

            if i in self.expected_inflation_rate.index:
                inflation_rate = self.expected_inflation_rate.loc[i,'value']
            else:
                inflation_rate = self.expected_inflation_rate.loc[self.parent.year_final,'value']

            # update factors
            discount_factor = discount_factor / (1.0 + discount_rate)
            inflation_factor = inflation_factor * (1.0 + inflation_rate)

        return discount
