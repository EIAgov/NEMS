"""Submodule for calculating natural gas processing facility carbon capture.

Summary
-------
This submodule produces captured CO2 volumes and reports them to CCATS in response to the CCATS CO2 price path. The
module operates as follows:

    1.  The module is initialized in the ngp class *__init__* method, with class dataframes and variables being declared
        here.

    2.  The *setup* method is called, which reads in the necessary input files via .csv or .pkl
        (see **intermediate_var_pickle.py**) format.

        a.  *load_input_tables* - Load NGP submodule input tables.

        b.  *adjust_inflation* - Adjust cost and price variables for inflation.

        c.  *setup_input_tables* - Setup NGP submodule input tables.

        d.  *setup_process_tables* - Setup NGP submodule process tables.

        e.  *assign_operating_cc_plants* - Assign operational cc plants to active_cc_facilities_df.

        f.  *load_intermediate_tables* - Load pickled tables.

    3. The *run* method is called, which kicks off the main model functions.

        a.  *calculate_flow_volumes* - Determine difference between natural gas processing facility database volumes and
            restart file natural gas flow.

        b.  *assign_legacy_plant_volumes* - Assign excess CO2 volume through STEO years to representative "legacy" facilities,
            and add these to the NGP facility database.

        c.  *assign_representative_plant_volumes* - Assign CO2 volumes after STEO years to representative "legacy" facilities,
            and add these to the NGP facility database.

        d.  *retire_legacy_plants* - Retire legacy plants that are not CO2 retrofit eligible.

        e.  *retrofit_eligibility* - Determine plant retrofit eligibility.

        f.  *assign_electricity_costs* - Load electricity costs from the restart file and calculate electricity
            opex/tonne of CO2.

    6.  Load and run cash flow in *load_cashflow* and *run_cashflow*. Rank projects by project net present value.

    7.  Setup to send CO2 capture volumes to CCATS:

        a.  *calculate_co2_capture_volumes* - Calculates CO2 capture volumes.  Put into two carbon capture tables:
            one for 45Q eligible CO2 (retrofits 12 years old or less), and one for 45Q ineligible CO2.

        b.  *calculate_co2_capture_costs* - Calculate average CO2 capital and opex (including electricity) capture costs,
            by Census division, and put in relevant table.

        c.  *calculate_electricity_demand* - Calculate total electricity demand from carbon capture.

        d.  *assign_active_status* - Assign profitable (NPV > 0) retrofits to active status.

    9.  Run *write_intermediate_variables* to store iterative calculation tables for the next model run year. These tables are written out to .pkl files in
        **intermediate_var_pickle.py**.
    10. *report_results_unf* is called from **module_unf.py** to report results to debug files and the restart file.

Input Files
___________
    - ngpp_input_eia_throughput???
    - ngpp_input_netl_throughput???
    - ngp_facility_input
    - netl_purchased_power_cost
    - netl_natural_gas_calculations
    - mapping

Model Functions and Class Methods
_________________________________

class NGP(sub.Submodule) - NGP submodule for HSM

Output Debug Files
__________________


Output Restart Variables
________________________

NGP Submodule Class Methods
___________________________
"""

import pandas as pd
import numpy as np
import names as nam
import submodule as sub
import common as com
import ngp_cash_flow as ngp_cf

class NGP(sub.Submodule):
    """Natural Gas Processing Submodule for HSM.

    Parameters
    ----------
    parent : str
        Module_unf.Module (Pointer to parent module)
    """

    def __init__(self, parent):
        super().__init__(parent, submodule_name='ngp')
        # NGP Variables
        self.zero_year              = 0.0  # First model year
        self.final_year             = 0.0  # Final model year
        self.evaluation_years       = 0.0  # Number of evaluation years for retrofit decision
        self.amor_schedule          = 0.0  # Amortization schedule
        self.deprec_schedule        = 0.0  # Depreciation Schedule


        # input tables
        self.facilities_base_df     = pd.DataFrame()  # DataFrame of natural gas processing facilities
        self.ccats_revenue          = pd.DataFrame()  # DataFrame of CCATS 45Q tax credit revenue
        self.netl_costs             = pd.DataFrame()  # DataFrame of read-in NETL costs
        self.mapping                = pd.DataFrame()  # Dataframe of mapping between HSM districts and other designations
        
        # process tables
        self.legacy_facilities_df       = pd.DataFrame() # DataFrame of legacy natural gas processing facilities
        self.facilities_df              = pd.DataFrame() # DataFrame of operational natural gas processing facilities
        self.active_cc_facilities_df    = pd.DataFrame() # DataFrame of facilities which have been selected for retrofit
        self.cf_facilities_df           = pd.DataFrame() # DataFrame of facilities which have not yet been retrofit for cashflow
        self.co2_capture_volumes_45q    = pd.DataFrame() # DataFrame of CO2 capture volumes from NGPPs that are 45Q eligible
        self.co2_capture_volumes_ntc    = pd.DataFrame() # DataFrame of CO2 capture volumes from NGPPs that are not 45Q eligible
        self.co2_capture_costs_inv      = pd.DataFrame() # DataFrame of capital costs for CO2 capture from NGPPs
        self.co2_capture_costs_om       = pd.DataFrame() # DataFrame of O&M costs for CO2 capture from NGPPs
        self.cc_electricity_demand      = pd.DataFrame() # Dataframe of electricity demand from NGPPs

        #Instantiate Cash Flow
        self.cash_flow = ngp_cf.NGP_CashFlow()

        pass


    def setup(self, setup_filename):
        """Setup Natural Gas Processing Submodule for HSM.

        Parameters
        ----------
        setup_filename : str
            Path to ngp setup file.

        Returns
        -------
        None
        """
        super().setup(setup_filename)
        
        # Load input variables
        self.zero_year                      = int(self.setup_table.at[nam.ngp_zero_year            , nam.filename]) # first ngpp year
        self.final_year                     = int(self.setup_table.at[nam.ngp_final_year           , nam.filename]) # final ngpp year
        self.final_45q_eligibility_year     = int(self.setup_table.at[nam.ngp_final_45q_elig_year  , nam.filename])  # Final 45Q eligibility year
        self.amor_schedule                  = str(self.setup_table.at[nam.ngp_amor_schedule        , nam.filename]) # amortization schedule from HSM
        self.deprec_schedule                = str(self.setup_table.at[nam.ngp_deprec_schedule      , nam.filename]) # depreciation schedule from HSM
        self.ngp_cost_tech_rate             = float(self.setup_table.at[nam.ngp_cost_tech_rate      , nam.filename]) # ngp technology improvement rate
        self.ngp_exp_tang_frac              = float(self.setup_table.at[nam.ngp_exp_tang_frac       , nam.filename]) # fraction of tangible exploration costs
        self.ngp_dev_tang_frac              = float(self.setup_table.at[nam.ngp_dev_tang_frac       , nam.filename]) # fraction of tangible development costs
        self.ngp_kap_tang_frac              = float(self.setup_table.at[nam.ngp_kap_tang_frac       , nam.filename]) # fraction of tangible capital costs
        self.ngp_intang_amor_frac           = float(self.setup_table.at[nam.ngp_intang_amor_frac    , nam.filename]) # intangible amortization fraction
        self.census_divisions               = int(self.setup_table.at[nam.census_divisions          , nam.filename]) # number of indices in "census region" restart file parameter
        self.evaluation_years               = int(self.setup_table.at[nam.ngp_eval_years            , nam.filename]) # cashflow evaluation years
        self.electric_retrofit_cap          = float(self.setup_table.at[nam.ngp_post_retrofit_cf    , nam.filename]) # electricity post-retrofit capacity factor = 63% in NETL
        self.ng_thruput_capacity_factor     = float(self.setup_table.at[nam.ngp_thruput_cf          , nam.filename]) # ng throughput capacity factor = 61% in NETL
        self.ngp_new_plant_capacity_factor  = float(self.setup_table.at[nam.ngp_new_plant_cf        , nam.filename]) # new plant capacity factor = 70%, per discussion with Warren since between 60 and 80%
        self.hr_to_yr                       = float(self.setup_table.at[nam.hr_to_yr                , nam.filename]) # hours to years
        self.ngpp_retire_rate               = float(self.setup_table.at[nam.ngpp_retire_rate        , nam.filename]) # natural gas processing plant retirement rate = 2%
        self.tech_rate_inv                  = float(self.setup_table.at[nam.tech_rate_inv           , nam.filename]) # natural gas processing plant investment cost tech imporvement rate
        self.tech_rate_om                   = float(self.setup_table.at[nam.tech_rate_om            , nam.filename]) # natural gas processing plant O&M cost tech imporvement rate
        self.max_growth                     = float(self.setup_table.at[nam.max_growth              , nam.filename]) # Maximum growth rate allowed by census division

        #Run Setup Functions
        self.logger.info('Run Natural Gas Processing Setup Functions')

        #Setup input tables and retrofit status tracking
        if (int(self.rest_curcalyr) == self.zero_year) | (self.parent.integrated_switch == 0):
            self.logger.info('Load input tables')
            self.load_input_tables()
            self.logger.info('Adjust for inflation')
            self.adjust_inflation()
            self.logger.info('Setup input tables')
            self.setup_input_tables()
            self.logger.info('Setup  process tables')
            self.setup_process_tables()
            self.logger.info('Assign Operating Carbon Capture Plants')
            self.assign_operating_cc_plants()
            
        #Load pickled tables
        elif (int(self.rest_curcalyr) > self.zero_year) & (self.parent.integrated_switch == 1):
            self.logger.info('Load Pickled Variables')
            self.load_intermediate_tables()

        pass


    def load_input_tables(self):
        """Load NGP submodule input tables.

        Returns
        -------
        None

        """
        # Only load projects in model year one, after we can just load projects lists from intermediate tables
        if (int(self.rest_curcalyr) == self.zero_year) | (self.parent.integrated_switch == 0):
            self.facilities_base_df = super()._load_dataframe(self.ngp_input_path, nam.ngp_facility_input)
        else:
            pass

        # Load netl_retrofit_costs
        self.netl_costs = super()._load_dataframe(self.ngp_input_path, nam.netl_natural_gas_calculations)
        
        pass


    def adjust_inflation(self):
        '''Adjust cost and price variables for inflation.

        Returns
        -------

        '''
        # NETL Retrofit costs
        self.netl_costs['total_cap_costs'] = self.netl_costs['total_cap_mm2018$'] * com.calculate_inflation(self.parent.rest_mc_jpgdp, 2018)
        self.netl_costs['total_opr_costs'] = self.netl_costs['total_om_cost_mm2018$'] * com.calculate_inflation(self.parent.rest_mc_jpgdp, 2018)

        pass


    def setup_input_tables(self):
        """Setup NGP submodule input tables

        Returns
        -------
        """
        # Drop non-eligible, non-overflow facilities
        eligible_mask = self.facilities_base_df['ccs_retrofit_eligible'] == True
        overflow_mask = self.facilities_base_df['existing_ntc'] == True
        existing_mask = self.facilities_base_df['retrofit_status'] == True
        self.facilities_base_df = self.facilities_base_df[eligible_mask | overflow_mask | existing_mask]

        # Merge netl retrofit costs to facilities
        self.netl_costs = self.netl_costs.drop(['facility_name','state','co2_capture_tons','ng_thruput_tons','ccs_retrofit_applicable'], axis = 1)
        self.facilities_base_df = self.facilities_base_df.merge(self.netl_costs,
                                                                        how='left',
                                                                        on = ['facility_id'])

        # Convert costs to $millions
        self.facilities_base_df['total_cap_costs']  *= 1000000
        self.facilities_base_df['total_opr_costs']  *= 1000000

        # Set throughput capacity factor as 61%
        self.facilities_base_df['ng_thruput_mmcf_cap'] = self.facilities_base_df['ng_thruput_mmcf'] * self.ng_thruput_capacity_factor

        # Remove capital costs for facilities which have already retrofit
        self.facilities_base_df.loc[self.facilities_base_df['retrofit_status'] == True, 'total_cap_costs'] = 0

        pass


    def setup_process_tables(self):
        """Setup NGP submodule process tables
        Returns
        -------
        """
        ### NGP Facilities
        # Get representative facilities and split from ngp_facililites
        self.facilities_df = self.facilities_base_df.loc[self.facilities_base_df['rep_facility'] == False].copy()
        self.rep_facilities_df = self.facilities_base_df.loc[self.facilities_base_df['rep_facility'] == True].copy()

        # Create base legacy plants file and assign unique ids
        self.legacy_facilities_df = self.rep_facilities_df.copy()
        self.legacy_facilities_df['facility_id'] = self.legacy_facilities_df['facility_id'] + '_leg'
        self.legacy_facilities_df['facility_name'] = self.legacy_facilities_df['facility_name'] + '_LEG'


        ### Results tables
        self.co2_capture_volumes_45q    = pd.DataFrame(0.0, index = list(range(1, self.census_divisions + 1)), columns = list(range(self.zero_year,self.final_year + 1)))
        self.co2_capture_volumes_45q.index.names = ['census_division']

        self.co2_capture_volumes_ntc    = pd.DataFrame(0.0, index = list(range(1, self.census_divisions + 1)), columns = list(range(self.zero_year,self.final_year + 1)))
        self.co2_capture_volumes_ntc.index.names = ['census_division']
        
        self.co2_capture_costs_inv      = pd.DataFrame(999, index = list(range(1, self.census_divisions + 1)), columns = list(range(self.zero_year,self.final_year + 1)))
        self.co2_capture_costs_inv.index.names = ['census_division']

        self.co2_capture_costs_om      = pd.DataFrame(999, index = list(range(1, self.census_divisions + 1)), columns = list(range(self.zero_year,self.final_year + 1)))
        self.co2_capture_costs_om.index.names = ['census_division']
        
        self.cc_electricity_demand  = pd.DataFrame(0.0, index = list(range(1, self.census_divisions + 1)), columns = list(range(self.zero_year,self.final_year + 1)))
        self.cc_electricity_demand.index.names = ['census_division']

        pass


    def assign_operating_cc_plants(self):
        """Assign operational cc plants to active_cc_facilities_df.

        Returns
        _______
        """
        self.active_cc_facilities_df = pd.concat([self.active_cc_facilities_df, self.facilities_df.loc[
            self.facilities_df['retrofit_status'] == True]])

        pass
    
    
    def load_intermediate_tables(self):
        """Load pickled tables.
        
        Returns
        -------
        None
        """
        self.facilities_base_df         = self.parent.hsm_vars.ngp_facilities_base_df.copy()
        self.rep_facilities_df          = self.parent.hsm_vars.ngp_rep_facilities_df.copy()
        self.legacy_facilities_df       = self.parent.hsm_vars.ngp_legacy_facilities_df.copy()
        self.facilities_df              = self.parent.hsm_vars.ngp_facilities_df.copy()
        self.active_cc_facilities_df    = self.parent.hsm_vars.ngp_active_cc_facilities_df.copy()
        self.co2_capture_volumes_45q    = self.parent.hsm_vars.ngp_co2_capture_volumes_45q.copy()
        self.co2_capture_volumes_ntc    = self.parent.hsm_vars.ngp_co2_capture_volumes_ntc.copy()
        self.co2_capture_costs_inv      = self.parent.hsm_vars.ngp_co2_capture_costs_inv.copy()
        self.co2_capture_costs_om       = self.parent.hsm_vars.ngp_co2_capture_costs_om.copy()
        self.cc_electricity_demand      = self.parent.hsm_vars.ngp_cc_electricity_demand.copy()



    def run(self):
        """Run Natural Gas Processing Submodule for HSM.

        Returns
        -------
        self.ngpp_cash_flow : df
                   DataFrame of ngp project cashflows

        self.ngpp_properties : df
                   DataFrame of key ngp project properties for cashflow (i.e. project cost bases)
        """
        super().run()

        #Run Main Model Functions
        self.logger.info('Run Natural Gas Processing Runtime Functions')

        #Calculate difference between natural gas processing facility database volumes and restart file natural gas flow
        self.logger.info('Calculate difference between NGP facility volumes and realized natural gas volumes')
        self.calculate_flow_volumes()

        # Apply technology Improvement Rate
        self.logger.info('Calculate Electricity Costs')
        self.tech_improvement()

        #Assign flow volumes to legacy and representative facilites
        if int(self.rest_curcalyr) <= self.parent.steo_years[-1]:
            self.logger.info('Assign CO2 flow to legacy facilities')
            self.assign_legacy_plant_volumes()
        else:
            # Assign CO2 flow to representative facilities
            self.logger.info('Assign CO2 flow to Representative Facilities')
            self.assign_representative_plant_volumes()

            # Retire legacy plants
            self.logger.info('Retire legacy plants')
            self.retire_legacy_plants()

            # Assign facility 45Q eligibility
            self.logger.info('Assign Facility 45Q Eligibility')
            self.retrofit_eligibility()

            # Assign CO2 Prices
            self.logger.info('Assign CO2 Prices from CCATS')
            self.assign_co2_prices()

            #Calculate Electricty Costs
            self.logger.info('Calculate Electricity Costs')
            self.assign_electricity_costs()
    
            #Load and run cashflow
            self.logger.info('Run Cashflow')
            self.load_cashflow()
            self.run_cashflow()
    
            #Assign facilities which have been retrofit to "active" status
            self.logger.info('Assign Profitiable Retrofits to active Status')
            self.assign_active_status()
    
        #Calculate CO2 capture volumes
        self.logger.info('Calculate CO2 Capture Volumes')
        self.calculate_co2_capture_volumes()
    
        #Calculate CO2 capture volumes
        self.logger.info('Calculate CO2 Capture Costs')
        self.calculate_co2_capture_costs()
        
        #Calculate electricity demand
        self.logger.info('Calculate Electricity Demand')
        self.calculate_electricity_demand()
    
        #Write Pickle local variables
        if ((self.parent.integrated_switch == 1) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == 1) & (self.parent.param_ncrl == 1)):
            self.logger.info('Write Pickle Variables')
            self.write_intermediate_variables()
    
        # Report Results
        self.logger.info('Report Results to Restart File')
        self.report_results()

        pass


    def calculate_flow_volumes(self):
        """Determine difference between natural gas processing facility database volumes and restart file natural gas flow (ogrnagprd).

        Returns
        -------
        """
        ### Get temp dfs for realized NA prod and realized AD prod
        # Realized NA Prod
        self.na_prod = self.parent.restart.ogsmout_ogrnagprd.copy()
        self.na_prod = self.na_prod[self.na_prod.index.isin([5], level=1)]

        # Realized AD Prod
        self.ad_prod = self.parent.restart.ogsmout_ogadgprd.copy()
        self.ad_prod = self.ad_prod[self.ad_prod.index.isin([5], level=1)]
        
        
        ### Merge AD and NA volumes to a single df
        self.real_ng_prod = self.na_prod.merge(self.ad_prod, 
                                               how = 'left',
                                               left_index = True,
                                               right_index = True,
                                               suffixes = ['_na','_ad'])
        self.real_ng_prod[nam.natgas_prod] =  self.real_ng_prod['value_na'] + self.real_ng_prod['value_ad']
        self.real_ng_prod = self.real_ng_prod.drop(['value_na','value_ad'], axis = 1)
        
        # Merge production to mapping
        self.real_ng_prod = self.real_ng_prod.reset_index()
        self.real_ng_prod.columns = [nam.district_number,nam.fuel_type_num,nam.year, nam.natgas_prod]
        self.real_ng_prod = self.real_ng_prod.drop([nam.fuel_type_num], axis = 1)
        self.real_ng_prod = self.real_ng_prod.merge(self.parent.mapping[[nam.district_number,nam.census_division]],
                                                    how = 'left',
                                                    left_on = nam.district_number,
                                                    right_on = nam.district_number)
        
        ### Drop offshore and Alaksa
        self.real_ng_prod = self.real_ng_prod.loc[self.real_ng_prod[nam.census_division] != 0]
        self.real_ng_prod = self.real_ng_prod.loc[self.real_ng_prod[nam.district_number] != 3]
        

        ### Get natural gas production volumes and facility capacities by census division 
        # Total NG production
        temp_ng_prod = self.real_ng_prod.loc[self.real_ng_prod[nam.year] == int(self.rest_curcalyr)].copy()
        temp_ng_prod = temp_ng_prod[['natgas_prod','census_division']]
        temp_ng_prod = temp_ng_prod.groupby('census_division').sum()
        temp_ng_prod['natgas_prod'] = temp_ng_prod['natgas_prod'] * 1000 # convert bcf to mmcf
        
        # Get total operational facility volumes
        temp_opr_facilities = pd.concat([self.facilities_df, self.legacy_facilities_df])
        temp_opr_facilities = temp_opr_facilities[['ng_thruput_mmcf_cap','census_division']].groupby('census_division').sum()
        
        
        ### Get difference between total natural gas production and retrofit facilities
        self.ng_prod_compare_df = temp_ng_prod.merge(temp_opr_facilities,
                                                     how = 'left',
                                                     left_index = True,
                                                     right_index = True).fillna(0.0)
        self.ng_prod_compare_df['excess_ng_prod'] = self.ng_prod_compare_df['natgas_prod'] - self.ng_prod_compare_df['ng_thruput_mmcf_cap'] # real difference
        self.ng_prod_compare_df['excess_ng_prod_cap'] = self.ng_prod_compare_df['natgas_prod'] - self.ng_prod_compare_df['ng_thruput_mmcf_cap'] * self.ngp_new_plant_capacity_factor # Difference at 90% capacity factor
        
        pass


    def tech_improvement(self):
        """Apply technology improvement rate to INV and O&M costs.

        Returns
        _______
        """
        self.facilities_df['total_cap_costs'] = self.facilities_df['total_cap_costs'] * (1 - self.tech_rate_inv)
        self.facilities_df['total_opr_costs'] = self.facilities_df['total_opr_costs'] * (1 - self.tech_rate_om)

        pass


    def assign_legacy_plant_volumes(self):
        """Assign excess CO2 volume through STEO years to representative "legacy" facilities, and add these to the NGP facility database.


        Returns
        _______
        """
        # get census divisions where production exceeds facility capacity
        temp_ng_prod_compare = self.ng_prod_compare_df.loc[self.ng_prod_compare_df['excess_ng_prod'] > 0]

        # Merge excees ng prod to legacy plants
        self.legacy_facilities_df = self.legacy_facilities_df.merge(temp_ng_prod_compare[['excess_ng_prod']],
                                                      how = 'left',
                                                      left_on = 'census_division',
                                                      right_index = True).fillna(0.0)
        self.legacy_facilities_df['ng_thruput_mmcf'] = self.legacy_facilities_df['ng_thruput_mmcf'] + self.legacy_facilities_df['excess_ng_prod']
        self.legacy_facilities_df['ng_thruput_mmcf_cap'] = self.legacy_facilities_df['ng_thruput_mmcf_cap'] + self.legacy_facilities_df['excess_ng_prod'] / self.ng_thruput_capacity_factor
        self.legacy_facilities_df = self.legacy_facilities_df.drop(['excess_ng_prod'], axis = 1)

        pass


    def assign_representative_plant_volumes(self):
        """Assign CO2 volumes after STEO years to representative "legacy" facilities, and add these to the NGP facility database.


        Returns
        _______
        """       
        # get census divisions where production exceeds facility capacity
        temp_ng_prod_compare = self.ng_prod_compare_df.loc[self.ng_prod_compare_df['excess_ng_prod_cap'] > 0]
        
        # Merge excess ng prod to legacy plants
        self.new_plants_df = self.rep_facilities_df.copy()
        self.new_plants_df = self.new_plants_df.merge(temp_ng_prod_compare[['excess_ng_prod_cap']],
                                                      how = 'left',
                                                      left_on = 'census_division',
                                                      right_index = True).fillna(0.0)
        self.new_plants_df['ng_thruput_mmcf'] = self.new_plants_df['excess_ng_prod_cap']
        self.new_plants_df['ng_thruput_mmcf_cap'] = self.new_plants_df['excess_ng_prod_cap']
        self.new_plants_df = self.new_plants_df.drop(['excess_ng_prod_cap'], axis = 1)

        # Assign unique plant id and name based on year
        self.new_plants_df['facility_id'] = self.new_plants_df['facility_id'] + '_' + str(int(self.rest_curcalyr))
        self.new_plants_df['facility_name'] = self.new_plants_df['facility_name'] + '_' + str(int(self.rest_curcalyr))
        
        # Concat new plants to facility list
        self.new_plants_df = self.new_plants_df.loc[self.new_plants_df['ng_thruput_mmcf'] > 0]
        self.facilities_df = pd.concat([self.facilities_df, self.new_plants_df], ignore_index= True)
        
        pass


    def retire_legacy_plants(self):
        """Retire legacy plants that are not CO2 retrofit eligible

        Returns
        _______
        """
        # Retire legacy plants at a rate of 2% year
        self.legacy_facilities_df['ng_thruput_mmcf_cap'] = self.legacy_facilities_df['ng_thruput_mmcf_cap'] * (1 - self.ngpp_retire_rate)
        self.legacy_facilities_df['ng_thruput_mmcf'] = self.legacy_facilities_df['ng_thruput_mmcf'] * (1 - self.ngpp_retire_rate)

        # Retire facilities that are older than 60
        self.facilities_df = self.facilities_df.loc[self.facilities_df['years_operating']<=60]
        self.active_cc_facilities_df = self.active_cc_facilities_df.loc[self.active_cc_facilities_df['years_operating']<=60]

        pass


    def retrofit_eligibility(self):
        """Determine plant retrofit eligibility
        
        Returns
        -------
        None
        
        """
        if int(self.rest_curcalyr) <= self.final_45q_eligibility_year:
            # Set 45Q eligibility for facilities which have been operating for more than 12 years to false and remove from active list
            self.facilities_df.loc[self.facilities_df['years_operating'] > self.evaluation_years, '45q_eligible'] = False
            self.active_cc_facilities_df = self.active_cc_facilities_df.loc[self.active_cc_facilities_df['years_operating'] <= (self.evaluation_years - 1)]
        else:
            self.active_cc_facilities_df = self.active_cc_facilities_df.loc[self.active_cc_facilities_df['years_operating'] <= (self.evaluation_years - 1)]
            self.facilities_df['45q_eligible'] = False
        
        pass


    def assign_co2_prices(self):
        '''Assign CO2 Price from CCATS.

        Returns
        _______
        '''
        # Reset CO2 Price
        self.facilities_df['co2_price'] = 0

        ### Get CO2 prices from CCATS
        #45Q
        temp_co2_price_45q_df = self.parent.rest_co2_price_45q.copy()
        temp_co2_price_45q_df = temp_co2_price_45q_df.loc[temp_co2_price_45q_df.index.get_level_values(1) == int(self.rest_curcalyr)]
        temp_co2_price_45q_df = temp_co2_price_45q_df.droplevel(1)

        #NTC
        temp_co2_price_ntc_df = self.parent.rest_co2_price_ntc.copy()
        temp_co2_price_ntc_df = temp_co2_price_ntc_df.loc[temp_co2_price_ntc_df.index.get_level_values(1) == int(self.rest_curcalyr)]
        temp_co2_price_ntc_df = temp_co2_price_ntc_df.droplevel(1)


        ### Merge CO2 prices to facilities
        temp_45q_df = self.facilities_df[['census_division','45q_eligible']].copy()
        temp_45q_df = temp_45q_df.loc[temp_45q_df['45q_eligible'] == True]
        temp_45q_df = temp_45q_df.merge(temp_co2_price_45q_df,
                                        how = 'left',
                                        left_on = 'census_division',
                                        right_index = True)
        temp_45q_df.columns = ['census_division','45q_eligible','co2_price']

        temp_ntc_df = self.facilities_df[['census_division','45q_eligible']].copy()
        temp_ntc_df = temp_ntc_df.loc[temp_ntc_df['45q_eligible'] == False]
        temp_ntc_df = temp_ntc_df.merge(temp_co2_price_ntc_df,
                                        how = 'left',
                                        left_on = 'census_division',
                                        right_index = True)
        temp_ntc_df.columns = ['census_division','45q_eligible','co2_price']
        
        
        ### Update facilities
        self.facilities_df.update(temp_45q_df)
        self.facilities_df.update(temp_ntc_df)

        pass
    

    def assign_electricity_costs(self):
        """Load electricity costs from the restart file and calculate electricity opex/tonne of CO2.


        Returns
        _______
        """
        # Drop last year's electricity cost
        if 'electricity_cost' in self.facilities_df.columns:
            self.facilities_df = self.facilities_df.drop(['electricity_cost'], axis = 1)
        else:
            pass

        # Assign electricity_cost
        electricity_costs = self.parent.rest_pelin.copy()
        electricity_costs = electricity_costs.loc[electricity_costs.index.isin([int(self.rest_curcalyr)], level = 1)]
        electricity_costs = electricity_costs.droplevel(1)
        electricity_costs.columns = ['electricity_cost']
        self.facilities_df = self.facilities_df.merge(electricity_costs,
                                                              how='left',
                                                              left_on = 'census_division',
                                                              right_index = True)
        
        pass


    def load_cashflow(self):
        """load Cashflow


        Returns
        _______
        """
        # Reinstantiate economic cashflow outputs
        self.facilities_df[nam.net_present_value]   = -1
        self.facilities_df[nam.profitability]       = -1
        self.facilities_df['total_cost_tonne_co2']  = 0
        self.facilities_df['inv_cost_tonne_co2']    = 0
        self.facilities_df['om_cost_tonne_co2']     = 0
        
        # Only run projects that haven't been retrofit through the cashflow or those which have run through 45Q eligibility
        retrofit_status_mask = self.facilities_df['retrofit_status'] == False
        operating_years_mask = self.facilities_df['years_operating'] >= self.evaluation_years
        mask = retrofit_status_mask | operating_years_mask
        self.cf_facilities_df = self.facilities_df[mask].copy()
        self.cash_flow.properties = pd.DataFrame()

        # Load CO2 flow
        self.cash_flow.co2_flow = pd.DataFrame(index=self.cf_facilities_df.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        for year in list(range(self.evaluation_years)):
            self.cash_flow.co2_flow[year] = self.cf_facilities_df['co2_capture_tonnes'].copy()

        # Load Properties
        self.cash_flow.properties['facility_id'] = self.cf_facilities_df['facility_id'].copy()
        self.cash_flow.properties[nam.co2_price] = self.cf_facilities_df[nam.co2_price].copy()
        self.cash_flow.properties[nam.electricity_cost] = self.cf_facilities_df[nam.electricity_cost].copy()
        self.cash_flow.properties[nam.discount_rate] = self.parent.discount_rate

        # Load Electricity Demand and attributes
        self.cash_flow.electricity_demand = pd.DataFrame(index=self.cf_facilities_df.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        for year in list(range(self.evaluation_years)):
            self.cash_flow.electricity_demand[year] = self.cf_facilities_df['electricity_cost'].copy()
        self.cash_flow.properties['hr_to_yr'] = self.hr_to_yr
        self.cash_flow.properties['electric_retrofit_cap'] = self.electric_retrofit_cap

        # Load capex and opex
        self.cash_flow.capital_cost = pd.DataFrame(index=self.cf_facilities_df.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.capital_cost[0] = self.cf_facilities_df['total_cap_costs'].copy().fillna(0.0)

        self.cash_flow.operating_cost = pd.DataFrame(index=self.cf_facilities_df.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        for year in list(range(self.evaluation_years)):
            self.cash_flow.operating_cost[year] = self.cf_facilities_df['total_opr_costs'].copy()

        # Load in fractions for tangible costs
        self.cash_flow.properties[nam.exp_tang_frac] = self.ngp_exp_tang_frac
        self.cash_flow.properties[nam.dev_tang_frac] = self.ngp_dev_tang_frac

        # Load in fraction for tangible capital costs
        self.cash_flow.properties[nam.kap_tang_frac] = self.ngp_kap_tang_frac

        # Load in amortization and depreciation schedules
        self.cash_flow.properties[nam.amor_schedule] = self.amor_schedule
        self.cash_flow.properties[nam.deprec_schedule] = self.deprec_schedule

        # Load in Fed Tax Rate
        self.cash_flow.properties[nam.fed_tax_rate] = self.parent.fed_tax_rate

        # Load in intangible amortization fraction
        self.cash_flow.properties[nam.intang_amor_frac] = self.ngp_intang_amor_frac

        # Reorder Properties for DCF
        self.cash_flow.properties = self.cash_flow.properties.sort_index()

        pass


    def run_cashflow(self):
        """Run Cash Flow.

        Returns
        -------
        """
        self.cash_flow.calculate_revenue()
        self.cash_flow.calculate_electricity_cost()
        self.cash_flow.calculate_total_operating_costs()
        self.cash_flow.calculate_intangible_tangible()
        self.cash_flow.calculate_depreciation()
        self.cash_flow.calculate_econ_limit()
        self.cash_flow.calculate_fed_tax()
        self.cash_flow.calculate_cash_flow()
        self.cash_flow.calculate_profitability()
        self.cash_flow.calculate_cost_tonne()

        # Return profitability indicators
        self.cf_facilities_df[nam.net_present_value] = self.cash_flow.properties[nam.net_present_value].copy()
        self.cf_facilities_df[nam.profitability] = self.cash_flow.properties[nam.profitability].copy()
        self.cf_facilities_df['total_cost_tonne_co2'] = self.cash_flow.properties['total_cost_tonne_co2'].copy()
        self.cf_facilities_df['inv_cost_tonne_co2'] = self.cash_flow.properties['inv_cost_tonne_co2'].copy()
        self.cf_facilities_df['om_cost_tonne_co2'] = self.cash_flow.properties['om_cost_tonne_co2'].copy()

        # Update main df facilities_df
        self.facilities_df.update(self.cf_facilities_df)
        self.facilities_df = self.facilities_df.sort_values('profitability', ascending= False)

        pass


    def assign_active_status(self):
        """Assign profitable retrofits to active status


        Returns
        -------
        """
        # Assign retrofit status to True
        self.facilities_df.loc[self.facilities_df[nam.net_present_value] > 0,'retrofit_status'] = True

        # Set capital costs to 0
        self.facilities_df.loc[self.facilities_df[nam.net_present_value] > 0,'total_cap_costs'] = 0

        # Assign "year operational" and concatenate selected facilities to active_cc_facilities_df
        temp_retrofit_df = self.facilities_df.loc[self.facilities_df[nam.net_present_value] > 0].copy()
        temp_retrofit_df['year_operational'] = int(self.rest_curcalyr)
        self.active_cc_facilities_df = pd.concat([self.active_cc_facilities_df, temp_retrofit_df])


        ### Limit ng processing growth to max_growth value (2x min(1,000,000 or last year's capture)
        # Get max growth values by Census Division
        max_growth_df = self.parent.restart.ccatsdat_sup_ngp_45q.copy()
        max_growth_df['value'] = max_growth_df['value'] + self.parent.restart.ccatsdat_sup_ngp_ntc['value']
        max_growth_df = max_growth_df.loc[max_growth_df.index.get_level_values(1) == (self.rest_curcalyr - 1)]
        max_growth_df.loc[max_growth_df['value'] < 1000000,'value'] = 1000000
        max_growth_df['value'] = max_growth_df['value'] * self.max_growth
        max_growth_df.index = max_growth_df.index.droplevel(1)
        max_growth_df.columns = ['max_growth']

        # Get active facilities cumsum by Census Division
        self.active_cc_facilities_df['cum_capture'] = self.active_cc_facilities_df.groupby(['census_division'])['co2_capture_tonnes'].cumsum()
        
        # Merge max growth to active cc facilities and cut facilities that fall below the max growth number
        self.active_cc_facilities_df = self.active_cc_facilities_df.reset_index().merge(max_growth_df,
                                                                          how = 'left',
                                                                          left_on = 'census_division',
                                                                          right_index = True).set_index('index')
        self.active_cc_facilities_df = self.active_cc_facilities_df.loc[self.active_cc_facilities_df['cum_capture'] <= self.active_cc_facilities_df['max_growth']]
        self.active_cc_facilities_df = self.active_cc_facilities_df.drop(['max_growth','cum_capture'], axis = 1)

        ### Add one year to retrofit facility df status
        years_operating_mask = self.facilities_df['years_operating'] > 0
        active_cc_facilities_mask = self.facilities_df['facility_id'].isin(self.active_cc_facilities_df['facility_id'])
        mask = years_operating_mask | active_cc_facilities_mask
        temp = self.facilities_df[mask].copy()
        temp['years_operating'] += 1
        self.facilities_df.update(temp)
        
        if self.rest_curcalyr >= self.parent.steo_years[0]:
            self.active_cc_facilities_df['years_operating'] += 1

        pass


    def calculate_co2_capture_volumes(self):
        """Calculate capture volumes


        Returns
        -------
        """
        # Mask for 45Q eligibility
        evaluation_year_mask = self.active_cc_facilities_df['years_operating'] <= self.evaluation_years
        existing_mask = self.active_cc_facilities_df['existing_45q'] == True
        eligibility_mask = self.active_cc_facilities_df['45q_eligible'] == True
        mask = (evaluation_year_mask & existing_mask) | (evaluation_year_mask & eligibility_mask)

        # 45Q eligible CO2
        temp_capture_45q_df = self.active_cc_facilities_df[mask].copy()
        temp_capture_45q_df = temp_capture_45q_df[['co2_capture_tonnes','census_division']]
        temp_capture_45q_df = temp_capture_45q_df.groupby(['census_division']).sum()
        temp_capture_45q_df = temp_capture_45q_df.reindex(list(range(1, 10)), fill_value=0.0)
        temp_capture_45q_df.columns = [int(self.rest_curcalyr)]
        self.co2_capture_volumes_45q.update(temp_capture_45q_df)

        # CO2 flows not eligible for 45Q tax credits
        temp_capture_ntc_df = self.active_cc_facilities_df[~mask].copy()
        temp_capture_ntc_df = temp_capture_ntc_df[['co2_capture_tonnes','census_division']]
        temp_capture_ntc_df = temp_capture_ntc_df.groupby(['census_division']).sum()
        temp_capture_ntc_df = temp_capture_ntc_df.reindex(list(range(1, 10)), fill_value=0.0)
        temp_capture_ntc_df.columns = [int(self.rest_curcalyr)]
        self.co2_capture_volumes_ntc.update(temp_capture_ntc_df)

        pass
        

    def calculate_co2_capture_costs(self):
        """Calculate average cost of capture


        Returns
        -------
        """
        if int(self.rest_curcalyr) <= self.parent.steo_years[-1]:
            pass
        else:
            # Investment costs
            temp_inv_capture_costs_df = self.active_cc_facilities_df.loc[self.active_cc_facilities_df['year_operational'] == int(self.rest_curcalyr)].copy()
            temp_inv_capture_costs_df = temp_inv_capture_costs_df[['inv_cost_tonne_co2','census_division']]
            temp_inv_capture_costs_df = temp_inv_capture_costs_df.groupby(['census_division']).mean()
            temp_inv_capture_costs_df.columns = [int(self.rest_curcalyr)]
            self.co2_capture_costs_inv.update(temp_inv_capture_costs_df)

            # O&M costs
            temp_om_capture_costs_df = self.active_cc_facilities_df.copy()
            temp_om_capture_costs_df = temp_om_capture_costs_df[['om_cost_tonne_co2','census_division']]
            temp_om_capture_costs_df = temp_om_capture_costs_df.groupby(['census_division']).mean()
            temp_om_capture_costs_df.columns = [int(self.rest_curcalyr)]
            self.co2_capture_costs_om.update(temp_om_capture_costs_df)

            pass

        pass


    def calculate_electricity_demand(self):
        """Calculate total electricity demand from carbon capture.

        Returns
        -------
        """
        temp_electric_demand_df = self.active_cc_facilities_df.copy()
        temp_electric_demand_df = temp_electric_demand_df[['tot_parasitic_retfitloadMW','census_division']]
        temp_electric_demand_df['tot_parasitic_retfitloadMW'] = temp_electric_demand_df['tot_parasitic_retfitloadMW'] * self.hr_to_yr * self.electric_retrofit_cap # Convert units to MWHs/Year
        temp_electric_demand_df = temp_electric_demand_df.groupby(['census_division']).sum()
        temp_electric_demand_df.columns = [int(self.rest_curcalyr)]
        self.cc_electricity_demand.update(temp_electric_demand_df)
        
        # Updating electricity consumed by natural gas plant during carbon capture
        temp_electric_demand_df['year'] = self.rest_curcalyr
        temp_electric_demand_df = temp_electric_demand_df.set_index(['year'],append=True)
        temp_electric_demand_df.index.names = self.restart.qmore_qngpin.index.names
        temp_electric_demand_df.columns = [nam.value]
        temp_electric_demand_df = temp_electric_demand_df.mul(.000003412141)  # Convert from MWh to Tbtu
        self.restart.qmore_qngpin.update(temp_electric_demand_df)

        pass


    def write_intermediate_variables(self):
        """Pickle local variables.

        Returns
        -------
        None
        """
        self.parent.hsm_vars.ngp_facilities_base_df        = self.facilities_base_df.copy() 
        self.parent.hsm_vars.ngp_legacy_facilities_df      = self.legacy_facilities_df.copy()
        self.parent.hsm_vars.ngp_facilities_df             = self.facilities_df.copy() 
        self.parent.hsm_vars.ngp_active_cc_facilities_df   = self.active_cc_facilities_df.copy() 
        self.parent.hsm_vars.ngp_co2_capture_volumes_45q   = self.co2_capture_volumes_45q.copy() 
        self.parent.hsm_vars.ngp_co2_capture_volumes_ntc   = self.co2_capture_volumes_ntc.copy() 
        self.parent.hsm_vars.ngp_co2_capture_costs_inv     = self.co2_capture_costs_inv.copy() 
        self.parent.hsm_vars.ngp_co2_capture_costs_om      = self.co2_capture_costs_om.copy() 
        self.parent.hsm_vars.ngp_cc_electricity_demand     = self.cc_electricity_demand.copy()
        self.parent.hsm_vars.ngp_rep_facilities_df         = self.rep_facilities_df.copy()

        # Debug
        if self.parent.hsm_var_debug_switch == True:
            self.facilities_base_df.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_facilities_base_df.csv')
            self.legacy_facilities_df.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_legacy_facilities_df.csv')
            self.facilities_df.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_facilities_df.csv')
            self.active_cc_facilities_df.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_active_cc_facilities_df.csv')
            self.co2_capture_volumes_45q.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_co2_capture_volumes_45q.csv')
            self.co2_capture_volumes_ntc.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_co2_capture_volumes_ntc.csv')
            self.co2_capture_costs_inv.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_co2_capture_costs_inv.csv')
            self.co2_capture_costs_om.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_co2_capture_costs_om.csv')
            self.cc_electricity_demand.to_csv(self.parent.hsm_var_output_path + 'hsm_ngp_cc_electricity_demand.csv')
        
        pass


    def report_results(self):
        """Report results to relevant restart variables

        * Report CO2 supply by 45Q eligibility and opex/capex costs respectively

        CCATSDATA CST_NGP_INV  - Investment cost for CO2 capture from natural gas processing facilities
        CCATSDATA CST_NGP_OM   - O&M cost for CO2 capture from natural gas processing facilities
        CCATSDATA SUP_NGP_45Q  - CO2 volumes from natural gas processing facilities that are 45Q eligible
        CCATSDATA SUP_NGP_NTC  - CO2 volumes from natural gas processing facilities, no tax credit

        """
       
        # 45Q Supply
        temp_sup_45q = self.co2_capture_volumes_45q.copy()
        temp_sup_45q = temp_sup_45q[[int(self.rest_curcalyr)]]
        temp_sup_45q['year'] = int(self.rest_curcalyr)
        temp_sup_45q = temp_sup_45q.set_index(['year'], append = True)
        temp_sup_45q.index.names = self.restart.ccatsdat_sup_ngp_45q.index.names
        temp_sup_45q.columns = [nam.value]
        self.restart.ccatsdat_sup_ngp_45q.update(temp_sup_45q)
        
        # NTC Supply
        temp_sup_ntc = self.co2_capture_volumes_ntc.copy()
        temp_sup_ntc = temp_sup_ntc[[int(self.rest_curcalyr)]]
        temp_sup_ntc['year'] = int(self.rest_curcalyr)
        temp_sup_ntc = temp_sup_ntc.set_index(['year'], append = True)
        temp_sup_ntc.index.names = self.restart.ccatsdat_sup_ngp_ntc.index.names
        temp_sup_ntc.columns = [nam.value]
        self.restart.ccatsdat_sup_ngp_ntc.update(temp_sup_ntc)

        # Cost Investment
        temp_cst_inv = self.co2_capture_costs_inv.copy()
        temp_cst_inv = temp_cst_inv[[int(self.rest_curcalyr)]]
        temp_cst_inv['year'] = int(self.rest_curcalyr)
        temp_cst_inv = temp_cst_inv.set_index(['year'], append = True)
        temp_cst_inv.index.names = self.restart.ccatsdat_cst_ngp_inv.index.names
        temp_cst_inv.columns = [nam.value]
        self.restart.ccatsdat_cst_ngp_inv.update(temp_cst_inv)
        
        # Cost Opex
        temp_cst_om = self.co2_capture_costs_om.copy()
        temp_cst_om = temp_cst_om[[int(self.rest_curcalyr)]]
        temp_cst_om['year'] = int(self.rest_curcalyr)
        temp_cst_om = temp_cst_om.set_index(['year'], append = True)
        temp_cst_om.index.names = self.restart.ccatsdat_cst_ngp_om.index.names
        temp_cst_om.columns = [nam.value]
        self.restart.ccatsdat_cst_ngp_om.update(temp_cst_om)

        # CO2 Emissions from Natural Gas Plants, returns CO2 capture from NGPPs where capture is unprofitable
        # Prepare legacy emissions
        temp_ngp_co2_em_legacy = self.legacy_facilities_df.copy()[['facility_id', 'co2_capture_tonnes']]
        temp_ngp_co2_em_legacy['census_division'] = temp_ngp_co2_em_legacy['facility_id'].str.extract(r'(\d+)')
        temp_ngp_co2_em_legacy['census_division'] = pd.to_numeric(temp_ngp_co2_em_legacy['census_division'])
        temp_ngp_co2_em_legacy['year'] = self.rest_curcalyr
        temp_ngp_co2_em_legacy = temp_ngp_co2_em_legacy.groupby(['census_division', 'year'])['co2_capture_tonnes'].sum().to_frame()
        temp_ngp_co2_em_legacy.index.names = self.restart.ogsmout_ngpco2em.index.names
        temp_ngp_co2_em_legacy.columns = [nam.value]

        # Prepare emissions from retrofitted facilities
        if int(self.rest_curcalyr) <= self.parent.steo_years[-1]: # including and before steo years
            temp_ngp_co2_em_retrofit = self.facilities_df.copy()[['retrofit_status', 'census_division', 'co2_capture_tonnes']].query("retrofit_status == False")
        else: # after steo years
            temp_ngp_co2_em_retrofit = self.cf_facilities_df.copy()[['facility_id','census_division', nam.profitability, 'co2_capture_tonnes']]
            temp_ngp_co2_em_retrofit = temp_ngp_co2_em_retrofit.loc[~temp_ngp_co2_em_retrofit['facility_id'].isin(self.active_cc_facilities_df['facility_id'])]
            
        temp_ngp_co2_em_retrofit['year'] = self.rest_curcalyr
        temp_ngp_co2_em_retrofit = temp_ngp_co2_em_retrofit.groupby(['census_division', 'year'])[
            'co2_capture_tonnes'].sum().to_frame()
        temp_ngp_co2_em_retrofit.index.names = self.restart.ogsmout_ngpco2em.index.names
        temp_ngp_co2_em_retrofit.columns = [nam.value]

        # Add legacy and retrofit emissions
        temp_ngp_co2_em_total = temp_ngp_co2_em_legacy.add(temp_ngp_co2_em_retrofit,fill_value=0)
        self.restart.ogsmout_ngpco2em.update(temp_ngp_co2_em_total)

        pass
    pass