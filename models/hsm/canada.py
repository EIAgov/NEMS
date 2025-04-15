"""Submodule for calculating Canadian well production.

SUMMARY
-------
This submodule takes baseline production values, decline curve estimations and well/price sensitivity estimations
derived from the Canada Energy Regulator's "Canada Energy Future" report, and uses these datasets and equations to
project Canadian production available for export to the United States. The Module operates as follows:

    1. The module is initialized in the canada class *__init__* method, with class dataframes and variables being declared here.

    2. The *setup* method is called, which reads in the necessary input files via .csv or .pkl (see **intermediate_var_pickle.py**) format.

    3. The *run* method is called, which kicks off the main model functions.

    4. Year 1 operations are called from the *run* method, these operations are listed below. All other operations are run every model
       year unless otherwise indicated:

        a. *canada_base_prod_setup* - Splits AD and NA gas fixed production into separate tables, and assigns production to regions (East/West).

        b. *calibrate_ad_gas* - Calibrates AD natural gas fixed production based on brent price.

        c. *wells_setup* - Reset Canada well counts to 0 for all model years.

        d.  *prod_profile_setup* - Solves for NA production profiles and creates net-new production dataframe.

    5. Run *calculate_wells*, where wells are calculated by region and fuel type based on Henry Hub price.

    6. Calculate regional na_prod based on a function of pre-calculated production profiles, and number of wells in *calculate_na_prod*.

    7. Aggregate data into outputs files in *sum_and_merge_production*

    8. Run *write_intermediate_variables* to store iterative calculation tables for the next model run year. These tables are written out to .pkl files in
        **intermediate_var_pickle.py**.

    9. *report_results_unf* is called from **module_unf.py** to report results to debug files and the restart file.


Input Files
-----------
    * can_natgas_baseline_prod.csv - Canada legacy production
    * can_natgas_dc_vars.csv - Decline Curve Variables
    * can_benchmark_prices.csv - Canada Energy Regulator Prices
    * can_natgas_no_export_prod.csv - Canadian Production for Domestic Use
    * can_wells.csv - Canadian well counts
    * can_natgas_poly_eqs.csv - Equations for calculating Canadian well drilling based on HH price
    * can_elasticity_ad_gas.csv - Static assumptions regarding AD natural gas production elasticities


Model Functions and Class Methods
---------------------------------
    * setup - Loads in relevant variables and processes.
    * canada_base_prod_setup - Splits AD and NA gas fixed production into separate tables, and assigns production to regions (East/West).
    * calibrate_ad_gas - Calibrates AD natural gas fixed production based on brent price.
    * wells_setup - Reset Canada well counts to 0 for all model years.
    * prod_profile_setup - Solves for NA production profiles and creates net-new production dataframe.
    * calculate_wells - Solves for number of producing wells by region, gas type, and year.
    * calculate_na_prod - Determines production from new drilling based on production profiles calculated in model year 1.
    * sum_and_merge_production - Aggregate production data.
    * write_intermediate_variables - Write local variables to restart file to be read back in each iteration.
    * report_results_unf - Report results to restart variables


Debug Files
-----------
    * hsm_can_na_prod.csv - Expected NA Gas Production
    * hsm_can_ad_prod.csv - Associated Dissolved Gas Production


***OUTPUT RESTART VARIABLES***
    * ogsmout.cnenagprd - Expected NA gas production in Canada  (1=east, 2=west)
    * ogsmout.cnrnagprd - Realized NA gas production in Canada (1=east, 2=west)
    * ogsmout.cnadagprd - AD gas production in Canada (1=east, 2=west)
"""

###Imports
import pandas as pd
import numpy as np
import names as nam
import submodule as sub
import common as com
import warnings


class Canada(sub.Submodule):
    """Canada submodule for HSM.

    Parameters
    ----------
    parent : str
        Module_unf.Module (Pointer to parent module)
    """

    def __init__(self, parent):
        super().__init__(parent, submodule_name='canada')

        #Input tables
        self.benchmark_prices           = pd.DataFrame()  #: DataFrame of benchmark NG and crude prices
        self.natgas_production          = pd.DataFrame()  #: DataFrame of natural gas production
        self.natgas_baseline_prod       = pd.DataFrame()  #: DataFrame of total baseline production
        self.natgas_dc_vars             = pd.DataFrame()  #: DataFrame of decline curve variable input
        self.natgas_poly_eqs            = pd.DataFrame()  #: DataFrame of WCSB natural gas polynomial drilling equations
        self.elasticity_ad_gas          = pd.DataFrame()  #: DataFrame of elasticity for AD gas
        self.can_wells                  = pd.DataFrame()  #: Dataframe with each region and gas_types' decline curve outputs from pre-processor
        self.natgas_no_export_prod      = pd.DataFrame()  #: Dataframe containing NA gas produced by Canada not available for export

        #Benchmark years setup
        self.zero_year                  = 0  #: Zero year for offshore submodule (equal to AEO year)
        self.input_years                = [*range(2005,2051)] #: Index years of Canadian Energy Regulator Reporting
        self.base_year                  = self.input_years[0]  #: Base model year
        self.output_start_year          = 1990
        self.model_start_year           = 2005
        self.output_end_year            = 2051

        ###Model Variables Setup

        #Variables
        self.bench_brent        = 0.0 #: Benchmark Brent Price
        self.bench_ng_prod      = 0.0 #: Benchmark natural gas production by area & fuel category (bcf/yr)
        self.b1                 = 0.0 #: Drilling equation parameter
        self.b0                 = 0.0 #: Drilling equation parameter
        self.bench_hen_hub      = 0.0 #: Benchmark Henry Hub Price
        self.can_well_v_HH      = [0, 0.85] #:  ***Canadian wellhead to Henry Hub price differential

        #Dataframes
        self.fixed_vols         = pd.DataFrame() #: DataFrame of Canadian gas fixed volumes
        self.ad_prod            = pd.DataFrame() #: DataFrame of total AD gas production
        self.na_prod            = pd.DataFrame() #: DataFrame of total NA gas production
        self.ad_prod            = pd.DataFrame() #: DataFrame of total AD production
        self.na_prod_new        = pd.DataFrame() #: DataFrame of new Canadian NA Production
        self.na_prod_sum        = pd.DataFrame() #: DataFrame of cumulative Canadian NA Production
        self.can_wells          = pd.DataFrame() #: DataFrame of Canadian Model Outputs
        self.cnenagprd          = pd.DataFrame() #: AIMMS output of expected NA gas production in Canada  (1=east, 2=west)
        self.cnadgprd           = pd.DataFrame() #: AIMMS output of AD gas production in Canada (1=east, 2=west)
        self.cnrnagprd          = pd.DataFrame() #: AIMMS output of historical NA gas production in Canada (1=east, 2=west)


    def setup(self, setup_filename):
        """Setup for Canada Submodule.

        Parameters
        ----------
        setup_filename : str
            Path to offshore setup file.

        Returns
        -------
        None

        """
        super().setup(setup_filename)

        #Load in variables
        self.zero_year = int(self.setup_table.at[nam.zero_year, nam.filename])

        #Warnings
        if self.parent.aeo_year - 2 != self.zero_year:
            warnings.warn('self.aeo_year - 2 != self.history_year', UserWarning)

        ##Load input files into tables
        self.benchmark_prices           = super()._load_dataframe(self.canada_input_path, nam.can_benchmark_prices, skiprows = 0, index_col = nam.year)

        self.natgas_dc_vars             = super()._load_dataframe(self.canada_input_path, nam.can_natgas_dc_vars, skiprows = 0, index_col = [nam.region, nam.gas_type])

        self.elasticity_ad_gas          = super()._load_dataframe(self.canada_input_path, nam.can_elasticity_ad_gas,skiprows = 0, index_col = [nam.year, nam.region])

        self.natgas_baseline_prod       = super()._load_dataframe(self.canada_input_path, nam.can_natgas_baseline_prod, skiprows = 0)

        self.can_wells                  = super()._load_dataframe(self.canada_input_path, nam.can_wells, skiprows = 0)

        self.natgas_poly_eqs            = super()._load_dataframe(self.canada_input_path, nam.can_natgas_poly_eqs, skiprows = 0,  index_col= [nam.region, nam.gas_type])

        self.natgas_no_export_prod      = super()._load_dataframe(self.canada_input_path, nam.can_natgas_no_export_prod, skiprows = 0,  index_col= [nam.year])


        #Common indices
        self.index_std = [nam.year, nam.region, nam.gas_type] #: Standard output DataFrame index

        self.na_index = [('Alberta', 'Non Associated'),
                    ('Alberta', 'Tight'),
                    ('Alberta', 'Shale'),
                    ('Alberta', 'Coalbed Methane'),
                    ('British Columbia', 'Tight'),
                    ('British Columbia', 'Shale'),
                    ('British Columbia', 'Non Associated'),
                    ('Saskatchewan', 'Tight'),
                    ('Saskatchewan', 'Non Associated')]


        ###Load Intermediate Tables:
        if (self.rest_curcalyr > self.zero_year) & (self.parent.integrated_switch == True):
            self.na_prod        = self.parent.hsm_vars.can_na_prod.copy()
            self.na_prod_new    = self.parent.hsm_vars.can_na_prod_new.copy()
            self.na_prod_sum    = self.parent.hsm_vars.can_na_prod_sum.copy()
            self.ad_prod        = self.parent.hsm_vars.can_ad_prod.copy()
            self.ad_prod_sum    = self.parent.hsm_vars.can_ad_prod_sum.copy()
            self.can_wells      = self.parent.hsm_vars.can_can_wells.copy()
            self.prod_profile   = self.parent.hsm_vars.can_prod_profile.copy()


    def run(self):
        """Run Canada Submodule for HSM.

        Returns
        -------
        None
        """
        com.print_out('running canada natural gas submodule')
        super().run()

        self.rest_curcalyr = int(self.rest_curcalyr)
        if self.rest_curcalyr == self.zero_year:
            self.logger.info('Run Canada Setup Functions')
            self.canada_base_prod_setup()
            self.calibrate_ad_gas()
            self.wells_setup()
            self.prod_profile_setup()

        if self.rest_curcalyr >= self.zero_year:
            self.logger.info('Run Canada Model')
            self.calculate_wells()
            self.calculate_na_prod()
            self.sum_and_merge_production()

        if ((self.parent.integrated_switch == True) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == True) & (self.parent.param_ncrl == 1)):
            self.logger.info('Run Canada Intermediate Variables')
            self.write_intermediate_variables()


    ###START OF YEAR 1 FUNCTIONS###


    def canada_base_prod_setup(self):
        """Splits AD and NA gas fixed production into separate tables, and assigns production to regions (East/West).

        Returns
        -------
        self.na_prod : df
            DataFrame of non-associated natural gas production by region and type.

        self.ad_prod : df
            DataFrame of associated dissolved natural gas production by region.
        """
        #Assign total historical production to relevant regions
        regions_dict = {'Alberta': 'Alberta',
                    'British Columbia': 'British Columbia',
                    'Saskatchewan': 'Saskatchewan',
                    'Nova Scotia': 'Other',
                    'New Brunswick': 'Other',
                    'Ontario': 'Other',
                    'Yukon': 'Other',
                    'Northwest Territories': 'Other',
                    'Quebec': 'Other',
                    'Newfoundland and Labrador': 'Other'}


        ###Create summed non associated gas dataframe and group to determine fixed NA prod
        #This df consists of Alberta, BC and Saskatchewan non-associated gas
        na_base = self.natgas_baseline_prod.copy()

        #Mask for regions and gas_type
        na_base = na_base[na_base[nam.gas_type].isin(['Tight', 'Shale', 'Coalbed Methane', 'Non Associated'])]
        na_base = na_base[~na_base[nam.region].isin(['Canada', 'WCSB'])]

        #Format Columns
        na_base[nam.region] = na_base[nam.region].map(regions_dict) #Map to HSM district names
        na_base = na_base.rename(columns = {nam.baseline_prod : nam.fixed_prod})

        #Merge to get NA Production
        na_base = na_base.groupby([nam.region, nam.gas_type, nam.year], axis = 0).sum()
        self.na_prod = na_base.copy()


        ###Create summed associated dissolved dataframe and group to determine total AD prod
        ad_base_wcsb = self.natgas_baseline_prod.copy()
        ad_base_wcsb = ad_base_wcsb[ad_base_wcsb[nam.gas_type].isin(['Solution'])]

        ad_base_small = self.natgas_baseline_prod.copy()
        ad_base_small = ad_base_small[ad_base_small[nam.region].isin(['Nova Scotia',
                                                                    'New Brunswick',
                                                                    'Quebec',
                                                                    'Ontario',
                                                                    'Yukon',
                                                                    'Northwest Territories'])]

        ad_base = pd.concat([ad_base_wcsb,ad_base_small], ignore_index=False)
        ad_base[nam.gas_type] = 'Associated Dissolved'
        ad_base = ad_base.groupby([nam.region, nam.gas_type, nam.year], axis=0).sum()
        ad_base = ad_base.rename(columns={nam.baseline_prod: nam.total_prod})
        self.ad_prod = ad_base.copy()

        pass


    def calibrate_ad_gas(self):
        """Calibrates AD natural gas fixed production based on brent price.

        Returns
        -------
        self.ad_prod : df
            DataFrame of associated dissolved natural gas production by region.
        """
        ###Calibrate oil prices for associated dissolved gas
        #Depending on whether NEMS Brent price or Canada price is better, assign a different elasticity factor to calibration equation
        brent_df = pd.DataFrame()
        brent_df[nam.brent_int] = self.parent.rest_brent_price[nam.value].copy()
        brent_df[nam.brent_can] = self.benchmark_prices[nam.brent_1987].copy()

        #Merge AD Prod to elasticity factors and brent prices
        ad_prod_elas_df = self.ad_prod.copy()
        ad_prod_elas_df = ad_prod_elas_df.reset_index(drop = False)
        ad_prod_elas_df = ad_prod_elas_df.merge(self.elasticity_ad_gas, how = 'left', on = [nam.year, nam.region]).fillna(0.0)
        ad_prod_elas_df = ad_prod_elas_df.merge(brent_df, how = 'left', left_on = nam.year, right_index = True)

        #Mask for whether CER crude price path is higher/lower than EIA
        high_price_mask = ad_prod_elas_df[nam.brent_int] > ad_prod_elas_df[nam.brent_can]

        #Apply elasticity factor based on mask
        #High Price Scenario
        high_price_df = ad_prod_elas_df[high_price_mask].copy()
        high_price_df[nam.total_prod] = high_price_df[nam.total_prod] * \
                                        ((high_price_df[nam.brent_int]/high_price_df[nam.brent_can]) ** high_price_df['high'])
        ad_prod_elas_df.update(high_price_df)

        #Low Price Scenario
        low_price_df = ad_prod_elas_df[~high_price_mask].copy()
        low_price_df[nam.total_prod] = low_price_df[nam.total_prod] * \
                                        ((low_price_df[nam.brent_int]/low_price_df[nam.brent_can]) ** low_price_df['low'])
        ad_prod_elas_df.update(low_price_df)

        #Update dataframes
        ad_prod_elas_df = ad_prod_elas_df.set_index([nam.region, nam.gas_type, nam.year])
        self.ad_prod.update(ad_prod_elas_df)

        pass


    def wells_setup(self):
        """Reset Canada well counts to 0 for all model years.

        Returns
        -------
        self.can_wells : df
            Canada natural gas well counts by region and type.
        """
        #Setup well dataframe and clear non-history well counts
        temp_wells = self.can_wells.copy()
        temp_wells = temp_wells.loc[temp_wells[nam.year] > self.parent.history_year]
        temp_wells[nam.wells] = 0
        self.can_wells.update(temp_wells, overwrite=True, errors='ignore')
        self.can_wells = self.can_wells.set_index([nam.region, nam.gas_type, nam.year])

        pass


    def prod_profile_setup(self):
        """Solves for NA production profiles and creates net-new production dataframe.

        Returns
        -------
        self.prod_profile : df
            DataFrame of static production profiles for Canadian Natural Gas well production by region and type.
        """
        ###Setup production profile table
        self.prod_profile = pd.DataFrame(columns = [nam.region, nam.gas_type, nam.decline_curve]) #Decline curve profile of each region/gas_type

        #Create new production Dataframe for summing iterative annual production of NA decline curves
        self.na_prod_new = pd.DataFrame(index = self.na_prod.index)


        ###Determine well production profile
        for index in self.na_index:
            decline_curves = pd.DataFrame() #reset decline curves df from pre-processing
            decline_curves.index.names = ['index_year']

            #Base variables pulled from pre-processing
            dr = self.natgas_dc_vars.at[index, 'dr']  # Decline Rate
            b = self.natgas_dc_vars.at[index, 'b']  # Parameter
            ipr = self.natgas_dc_vars.at[index, 'ipr']  # Initial Production Rate

            #Decline curve profile
            if b > 0:
                decline_curves[nam.decline_curve] = ipr * 365 / (1 + dr * b * (pd.Series(self.input_years).values - (self.input_years[0]) + 1)) ** (1 / b)
            else:
                decline_curves[nam.decline_curve] = ipr * 365 / (1 + dr * b * (pd.Series(self.input_years).values - (self.input_years[0]) + 1)) ** (1 / (b + 0.01))

            #Adjust year one (divide by 2 to account for months)
            decline_curves.at[0, nam.decline_curve] = decline_curves.at[0, nam.decline_curve] * 0.5

            #Add key and append
            decline_curves[nam.region] = index[0]
            decline_curves[nam.gas_type] = index[1]
            self.prod_profile = pd.concat([self.prod_profile, decline_curves], ignore_index = False)

        #Set Prod Profile Index
        self.prod_profile = self.prod_profile.set_index([nam.region,nam.gas_type])

        pass


    ###START OF FUNCTIONS RUN EVERY YEAR###


    def calculate_wells(self):
        """Solves for number of producing wells by region, gas type, and year.

            * Calibrates Henry Hub Price
            * Determines the number of wells drilled by region based on polynomial drilling equations calculated in pre-processors

        Returns
        -------
        self.can_wells : df
            Canada natural gas well counts by region and type.
        """

        #Calibrate to henry hub prices
        self.benchmark_prices[nam.hh_cal] = self.benchmark_prices[nam.henry_hub_1987] + self.can_well_v_HH[0]

        ###Determine Additional Drilling using polynomial regression model
        #Apply Polynomial Regression Fit
        if self.parent.current_year > self.parent.history_year:
            brent_int   = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value]  # International Model Ouptut Brent Price
            brent_can   = self.benchmark_prices.at[int(self.parent.current_year), nam.brent_1987]  # Canadian Energy Regulator Benchmark Brent Price
            hh_int     = self.benchmark_prices.at[self.rest_curcalyr, 'Henry Hub - US$/MMBTU (calibrated)'] # Henry Hub price #

            #Get Canada Price Bench Value
            can_ng_price_bench = self.restart.ogsmout_ogcnpprd.copy()
            can_ng_price_bench = can_ng_price_bench.xs(2, level = 0, drop_level = True).copy()
            can_ng_price_bench = can_ng_price_bench + (0.96 / self.parent.rest_mc_jpgdp.at[2016,nam.value]) #Bench for price diff

            for index in self.na_index:

                #Set coefficients
                b3 = self.natgas_poly_eqs.at[index, 'b3']
                b2 = self.natgas_poly_eqs.at[index, 'b2']
                b1 = self.natgas_poly_eqs.at[index, 'b1']
                b0 = self.natgas_poly_eqs.at[index, 'b0']

                #Predict new wells (regression coef + regression constant calibrated to hh - Canada price differential)
                #Calibration variable is set to 0 as regression fit does not currently require calibration
                if brent_int > brent_can:
                    calibration_var = self.benchmark_prices.at[int(self.parent.current_year), nam.hh_cal] * (brent_int / brent_can) ** self.natgas_poly_eqs.at[index, 'oilprc_high']
                    self.can_wells.at[(index[0], index[1], int(self.parent.current_year)), nam.wells] = \
                        ((b3 * hh_int**3) + (b2 * hh_int**2) + (b1 * hh_int) + b0) * can_ng_price_bench.at[self.rest_curcalyr,nam.value] * (calibration_var **0.75)

                else:
                    calibration_var = self.benchmark_prices.at[int(self.parent.current_year), nam.hh_cal] * (brent_int / brent_can) ** self.natgas_poly_eqs.at[index, 'oilprc_low']
                    self.can_wells.at[(index[0], index[1], int(self.parent.current_year)), nam.wells] = \
                        ((b3 * hh_int**3) + (b2 * hh_int**2) + (b1 * hh_int) + b0) * can_ng_price_bench.at[self.rest_curcalyr,nam.value] * (calibration_var **0.75)


                if self.can_wells.at[(index[0], index[1], int(self.parent.current_year)), nam.wells] < 0:
                    self.can_wells.at[(index[0], index[1], int(self.parent.current_year)), nam.wells] = 0
                else:
                    pass

                #Round Wells
                self.can_wells[nam.wells] = self.can_wells[nam.wells].round()

                #Set min wells to 0
                self.can_wells.loc[self.can_wells[nam.wells] < 0 , nam.wells] = 0

                #Output Canada wells
                if self.parent.debug_switch == 1:
                    self.can_wells.to_csv(self.output_path + 'module_results_debug//' + 'hsm_can_wells_debug.csv')

            pass


    def calculate_na_prod(self):
        """Determines production from net new drilling based on production profiles calculated in model year 1.

        Returns
        -------
        self.na_prod_new : df
            DataFrame of net new non-associated natural gas production.
        """
        ###Determine NA production from new drilling
        #Zero out baseline production
        self.na_prod_new['new_drill_prod'] = 0
        self.na_prod_new[self.rest_curcalyr] = 0

        #NA production from new drilling
        for index in self.na_index:
            #Set base vars
            tech = self.natgas_dc_vars.at[index, nam.tech]

            #Get well count by region and type
            well_count = self.can_wells.at[(index[0], index[1], self.rest_curcalyr), nam.wells]

            #Pull in decline curve and mask for year
            #Each year an additional 0 is added to the front of the decline curve mask, while an assigned value is spliced off the end
            curve_mask = self.prod_profile.index == (index[0], index[1])
            curve_temp = self.prod_profile[curve_mask].copy()
            curve_temp = curve_temp[nam.decline_curve].tolist()
            zero_list = [0] * (self.rest_curcalyr - self.base_year)
            curve_temp = zero_list + curve_temp
            splice = self.base_year - self.rest_curcalyr
            curve_temp = curve_temp[:splice]

            #Perform decline curve calculations
            temp_df = pd.DataFrame(index=self.input_years)
            temp_df.index.names = [nam.year]
            temp_df[self.rest_curcalyr] = curve_temp
            temp_df[self.rest_curcalyr] = temp_df[self.rest_curcalyr].apply(lambda well_prod: well_prod * well_count) * ((1 + tech)**(self.parent.current_year-self.base_year)) #Tech improvement rate already baked in

            #Create Keys and reorder index levels to match convention
            temp_df[nam.region] = index[0]
            temp_df[nam.gas_type] = index[1]
            temp_df = temp_df.set_index([nam.region,nam.gas_type], append=True)
            temp_df = temp_df.reorder_levels([1, 2, 0])

            #Merge to master Dataframe
            self.na_prod_new.update(temp_df, overwrite=True, errors='ignore')

        pass


    def sum_and_merge_production(self):
        """Aggregate production data.

            * Sum and merge production into NA and AD gas by region, type, and year
            * Assign production to NEMS regions by year and set to restart file format

        Returns
        -------
        self.na_prod : df
            DataFrame of non-associated natural gas production by region and type

        self.na_prod_new : df
            DataFrame of net new non-associated natural gas production

        self.ad_prod : df
            DataFrame of associated dissolved natural gas production by region

        self.na_prod_sum
            DataFrame of total non-associated natural gas production

        self.ad_prod_sum
            DataFrame of total associated dissolved natural gas production
        """
        #Merge NA new drill prod into NA prod table
        #Each year is only new production as fixed production is constant
        self.na_prod[self.rest_curcalyr] = self.na_prod_new[self.rest_curcalyr]
        self.na_prod[self.rest_curcalyr] = self.na_prod[self.rest_curcalyr].fillna(0)
        # Output Drilling Debug
        if self.parent.debug_switch == 1:
            self.na_prod.to_csv(self.output_path + 'module_results_debug//' + 'hsm_can_drill_debug.csv')


        ###Assign production to summed variables and convert to restart file format
        #West Canada NA
        temp_west_na_df = self.na_prod.copy()
        temp_west_na_df = temp_west_na_df[temp_west_na_df.index.get_level_values(0).isin(['Alberta', 'British Columbia', 'Saskatchewan'])]
        temp_west_na_df = temp_west_na_df.copy().groupby(nam.year).sum()

        #Convert to Restart File format
        temp_west_na_df = temp_west_na_df.rename({nam.total_prod: nam.value}, axis = 1)
        temp_west_na_df['NUMCAN'] = 2
        temp_west_na_df = temp_west_na_df.set_index(['NUMCAN'], append = True)
        temp_west_na_df = temp_west_na_df.reorder_levels([1, 0])

        #Set restart variable equal to NA production
        if self.na_prod_sum.empty:
            self.na_prod_sum = temp_west_na_df.copy()
        else:
            self.na_prod_sum[self.rest_curcalyr] = temp_west_na_df[self.rest_curcalyr].copy()


        #East Canada NA
        temp_east_na_df = self.na_prod.copy()
        temp_east_na_df = temp_east_na_df[~temp_east_na_df.index.get_level_values(0).isin(['Alberta', 'British Columbia', 'Saskatchewan'])]
        if temp_east_na_df.empty:
            temp_east_na_df = pd.DataFrame(index = temp_west_na_df.index.get_level_values(nam.year)) # get year
            temp_east_na_df['NUMCAN'] = 1
            temp_east_na_df[nam.fixed_prod] = 0
            temp_east_na_df[self.rest_curcalyr] = 0
            temp_east_na_df = temp_east_na_df.set_index(['NUMCAN'], append = True)
            temp_east_na_df = temp_east_na_df.reorder_levels([1, 0])
            self.na_prod_sum = pd.concat([self.na_prod_sum, temp_east_na_df], ignore_index=False)
            self.na_prod_sum = self.na_prod_sum.groupby(level=[0, 1]).sum()
        else:
            temp_east_na_df = temp_east_na_df.reset_index(drop = False)
            temp_east_na_df['NUMCAN'] = 1
            temp_east_na_df = temp_east_na_df.set_index(['NUMCAN', nam.year])
            self.na_prod_sum = pd.concat([self.na_prod_sum,temp_east_na_df], ignore_index = False)
            self.na_prod_sum = self.na_prod_sum.groupby(level = [0,1]).sum()


        #West Canada AD
        temp_west_ad_df = self.ad_prod.copy()
        temp_west_ad_df = temp_west_ad_df[temp_west_ad_df.index.get_level_values(nam.region).isin(['Alberta', 'British Columbia', 'Saskatchewan'])]
        temp_west_ad_df = temp_west_ad_df[[nam.total_prod]].copy().groupby(nam.year).sum()

        #Convert to Restart File Format
        temp_west_ad_df = temp_west_ad_df.rename({nam.total_prod: nam.value}, axis = 1)
        temp_west_ad_df['NUMCAN'] = 2
        temp_west_ad_df = temp_west_ad_df.set_index(['NUMCAN'], append = True)
        temp_west_ad_df = temp_west_ad_df.reorder_levels([1, 0])
        self.ad_prod_sum = temp_west_ad_df.copy()

        #East Canada AD
        temp_east_ad_df = self.ad_prod.copy()
        temp_east_ad_df = temp_east_ad_df[~temp_east_ad_df.index.get_level_values(nam.region).isin(['Alberta', 'British Columbia', 'Saskatchewan'])]
        temp_east_ad_df = temp_east_ad_df[[nam.total_prod]].copy().groupby(nam.year).sum()

        #Convert to Restart File Format
        temp_east_ad_df = temp_east_ad_df.rename({nam.total_prod: nam.value}, axis = 1)
        temp_east_ad_df['NUMCAN'] = 1
        temp_east_ad_df = temp_east_ad_df.set_index(['NUMCAN'], append = True)
        temp_east_ad_df = temp_east_ad_df.reorder_levels([1, 0])
        self.ad_prod_sum = pd.concat([self.ad_prod_sum, temp_east_ad_df], ignore_index=False)

        pass

    def write_intermediate_variables(self):
        """Write local variables to restart file to be read back in each iteration.

        Returns
        -------
        self.na_prod : df
            DataFrame of non-associated natural gas production by region and type

        self.na_prod_new : df
            DataFrame of net new non-associated natural gas production

        self.na_prod_sum
            DataFrame of total non-associated natural gas production

        self.ad_prod : df
            DataFrame of associated dissolved natural gas production by region

        self.ad_prod_sum
            DataFrame of total associated dissolved natural gas production

        self.can_wells : df
            Canada natural gas well counts by region and type.

        self.prod_profile : df
            DataFrame of static production profiles for Canadian Natural Gas well production by region and type.
        """

        self.parent.hsm_vars.can_na_prod        = self.na_prod.copy()
        self.parent.hsm_vars.can_na_prod_new    = self.na_prod_new.copy()
        self.parent.hsm_vars.can_na_prod_sum    = self.na_prod_sum.copy()
        self.parent.hsm_vars.can_ad_prod        = self.ad_prod.copy()
        self.parent.hsm_vars.can_ad_prod_sum    = self.ad_prod_sum.copy()
        self.parent.hsm_vars.can_can_wells      = self.can_wells.copy()
        self.parent.hsm_vars.can_prod_profile   = self.prod_profile.copy()


        #Debug
        if self.parent.hsm_var_debug_switch == True:
            self.na_prod.to_csv(self.parent.hsm_var_output_path + 'hsm_can_na_prod.csv')
            self.na_prod_new.to_csv(self.parent.hsm_var_output_path + 'hsm_can_na_prod_new.csv')
            self.na_prod_sum.to_csv(self.parent.hsm_var_output_path + 'hsm_can_na_prod_sum.csv')
            self.ad_prod.to_csv(self.parent.hsm_var_output_path + 'hsm_can_ad_prod.csv')
            self.ad_prod_sum.to_csv(self.parent.hsm_var_output_path + 'hsm_can_ad_prod_sum.csv')
            self.can_wells.to_csv(self.parent.hsm_var_output_path + 'hsm_can_wells.csv')
            self.prod_profile.to_csv(self.parent.hsm_var_output_path + 'hsm_can_prod_profile.csv')

        pass


    def report_results_unf(self):
        """Report results to restart variables.

        Returns
        -------
        self.restart.ogsmout.cnenagprd : df
            Expected NA gas production in Canada  (1=east, 2=west)

        self.restart.ogsmout.cnrnagprd : df
            Realized NA gas production in Canada (1=east, 2=west)

        self.restart.ogsmout.cnadagprd : df
            AD gas production in Canada (1=east, 2=west)
        """
        ### Export dataframes to csv
        if self.parent.debug_switch == 1:
            self.na_prod_sum.to_csv(self.output_path + 'module_results_debug//' + 'hsm_can_na_prod.csv')
            self.ad_prod.to_csv(self.output_path + 'module_results_debug//' + 'hsm_can_ad_prod.csv')

        if self.rest_curcalyr >= self.parent.steo_years[0]:


            ### Update Output Restart
            #Format NA prod and AD prod for Restart file
            temp_na = self.na_prod_sum.copy()
            temp_na['value'] = temp_na.sum(axis = 1)
            temp_na = temp_na[['value']]

            temp_ad = self.ad_prod_sum.copy()
            temp_ad = temp_ad[['value']]

            #Remove Production not available for Export
            temp_no_export = temp_na.loc[[2]]
            temp_no_export.loc[:,'no_export_prod'] = self.natgas_no_export_prod['prod'].values.copy()
            temp_no_export[nam.value] = temp_no_export[nam.value] - (temp_no_export['no_export_prod'] * 365)
            temp_no_export = temp_no_export.drop('no_export_prod', axis = 1)
            temp_no_export.loc[temp_no_export[nam.value] < 0, nam.value] = 0
            temp_na.update(temp_no_export)

            #Write to restart file
            self.restart.ogsmout_cnenagprd.at[(1,self.rest_curcalyr), 'value'] = temp_na.at[(1,self.rest_curcalyr), 'value']
            self.restart.ogsmout_cnenagprd.at[(2,self.rest_curcalyr), 'value'] = temp_na.at[(2,self.rest_curcalyr), 'value']
            self.restart.ogsmout_cnadgprd.at[(1,self.rest_curcalyr), 'value'] = temp_ad.at[(1,self.rest_curcalyr), 'value']
            self.restart.ogsmout_cnadgprd.at[(2,self.rest_curcalyr), 'value'] = temp_ad.at[(2,self.rest_curcalyr), 'value']
            self.restart.ogsmout_cnrnagprd = self.restart.ogsmout_cnenagprd.copy()


        pass

