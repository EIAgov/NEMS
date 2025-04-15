"""Submodule for localizing and concatenating restart variables.

Localize Restart: Summary
-------------------------
CCATS receives CO\ :sub:`2` supply and demand from several NEMS modules. Each CO\ :sub:`2` supply or demand source has its own restart file variable.
To rationalize and simplify this data, supply and demand data restart variables are re-formatted and concatenated into larger files.
The :class:`Localize` submodule accomplishes this as follows:

    1. EOR demand and net cost offers are received as DataFrames via the restart file in a base-1 ordered index. In :meth:`~localize_restart.Localize.setup_eor`
       these DataFrames are remapped to the HSM play numbers and census division numbers used to identify CO\ :sub:`2` EOR plays.

    2. CO\ :sub:`2` supply volumes are aggregated and concatenated into DataFrames *industrial_co2_supply_45q* and *industrial_co2_supply_ntc* in
       :meth:`~localize_restart.Localize.concat_restart_variables_supply`

    3. CO\ :sub:`2` supply volumes are aggregated and concatenated into DataFrames *industrial_co2_cost_inv* and *industrial_co2_cost_om* in
       :meth:`~localize_restart.Localize.concat_restart_variables_cost`.

    
Localize Restart: Model Functions and Class Methods
___________________________________________________
* :meth:`~localize_restart.Localize.__init__` - Constructor to initialize class (instantiated by :meth:`module.Module.setup` in :ref:`module`)
* :meth:`~localize_restart.Localize.setup_eor` - Format EOR demand input files for CCATS preprocessor (called by :meth:`module.Module.setup`).
* :meth:`~localize_restart.Localize.concat_restart_variables_supply` - Concatenate CO\ :sub:`2` capture type supply restart variables into 45Q and NTC supply tables (called by :meth:`module.Module.setup`).
* :meth:`~localize_restart.Localize.concat_restart_variables_cost` - Concatenate CO\ :sub:`2` capture type cost restart variables into 45Q and NTC capture cost tables (called by :meth:`module.Module.setup`).


Localize Restart: Output Debug Files
____________________________________
None

Localize Restart: Output Restart Variables
__________________________________________
None


Localize Restart: Code
______________________
"""

import pandas as pd

class Localize:
    """Localize submodule for CCATS.

    """
    def __init__(self, parent):
        """Initializes Localize object.

        Parameters
        ----------
        parent : str
            Module.Module (Pointer to parent module)

        Returns
        -------
        None

        """
        self.parent = parent  #: module.Module head module

        # Declare Process dfs
        self.rest_dem_eor               = pd.DataFrame() # CO2 demand (metric tonnes) from CO2 EOR
        self.rest_cst_eor               = pd.DataFrame() # CO2 price offers (1987$/tonne) from CO2 EOR
        self.industrial_co2_supply_45q  = pd.DataFrame() # Industrial co2 supply (metric tonnes) eligible for 45Q tax credits
        self.industrial_co2_supply_ntc  = pd.DataFrame() # Industrial co2 supply (metric tonnes) not eligible for 45Q tax credits
        self.industrial_co2_cost_inv    = pd.DataFrame() # Carbon capture investment costs (1987$) by NEMS module and region
        self.industrial_co2_cost_om     = pd.DataFrame() # Carbon capture O&M costs (1987$) by NEMS module and region

        pass
    
    
    def setup_eor(self):
        '''Map EOR demand input files to HSM play numbers and census divisions for CCATS preprocessor.

        Returns
        -------
        self.rest_dem_eor : DataFrame
            DataFrame of CO\ :sub:`2` demand (metric tonnes) from CO\ :sub:`2` EOR.

        self.rest_cst_eor : DataFrame
            DataFrame of CO\ :sub:`2` price offers (1987$/tonne) from CO\ :sub:`2` EOR.
        '''
        ### EOR demand
        # Reformat EOR demand
        temp_eor_inj = self.parent.rest_dem_eor.copy()
        temp_eor_years = temp_eor_inj.index.get_level_values(1).unique()
        temp_eor_inj = temp_eor_inj.unstack(1)
        temp_eor_inj.columns = temp_eor_years
        
        # Map EOR plays to play_map
        temp_eor_inj = temp_eor_inj.merge(self.parent.rest_play_map, 
                                  how = 'left',
                                  left_index = True,
                                  right_index = True)
        temp_eor_inj = temp_eor_inj.rename(columns = {'value':'play_id'})
        
        # Map play_map to census divisions
        temp_eor_inj = temp_eor_inj.merge(self.parent.co2_eor_mapping[['play_id','census_division','census_region']],
                                  how = 'left',
                                  on = 'play_id')
        temp_eor_inj = temp_eor_inj.drop_duplicates(keep = 'last', subset = ['play_id'])
        self.rest_dem_eor = temp_eor_inj.copy()
        
        
        ### EOR price offer
        temp_eor_cost = self.parent.rest_cst_eor.copy()
        temp_eor_years = temp_eor_cost.index.get_level_values(1).unique()
        temp_eor_cost = temp_eor_cost.unstack(1)
        temp_eor_cost.columns = temp_eor_years

        # Map EOR plays to play_map
        temp_eor_cost = temp_eor_cost.merge(self.parent.rest_play_map,
                                          how='left',
                                          left_index=True,
                                          right_index=True)
        temp_eor_cost = temp_eor_cost.rename(columns={'value': 'play_id'})

        # Map play_map to census divisions
        temp_eor_cost = temp_eor_cost.merge(self.parent.co2_eor_mapping[['play_id', 'census_division','census_region']],
                                          how='left',
                                          on='play_id')
        temp_eor_cost = temp_eor_cost.drop_duplicates(keep = 'last', subset = ['play_id'])
        self.rest_cst_eor = temp_eor_cost.copy()

        pass


    def concat_restart_variables_supply(self):
        '''Concatenate CO\ :sub:`2` capture type supply restart variables into 45Q and NTC supply tables.
        
        Returns
        -------
        self.industrial_co2_supply_45q : DataFrame
            DataFrame of industrial CO\ :sub:`2` supply (metric tonnes) eligible for 45Q tax credits.

        self.industrial_co2_supply_ntc : DataFrame
            DataFrame of industrial CO\ :sub:`2` supply (metric tonnes) not eligible for 45Q tax credits.

        '''
        ### CO2 supply 45Q eligible
        # Coal powerplants - New
        temp_coal_supply_45q_new_df = self.parent.rest_sup_emm_45q.copy()
        temp_coal_supply_45q_new_df = temp_coal_supply_45q_new_df.loc[temp_coal_supply_45q_new_df.index.get_level_values(0).isin([1])] # 1=coal new, 2=coal retrofit
        temp_coal_supply_45q_new_df = temp_coal_supply_45q_new_df.droplevel(0)
        temp_coal_supply_45q_new_df = temp_coal_supply_45q_new_df.groupby(temp_coal_supply_45q_new_df.index).sum()
        temp_coal_supply_45q_new_df['facility_type'] = 'pp_coal'
        temp_coal_supply_45q_new_df['new_vs_retrofit'] = 'new'

        # Coal powerplants - Retrofit
        temp_coal_supply_45q_retro_df = self.parent.rest_sup_emm_45q.copy()
        temp_coal_supply_45q_retro_df = temp_coal_supply_45q_retro_df.loc[temp_coal_supply_45q_retro_df.index.get_level_values(0).isin([2])] # 1=coal new, 2=coal retrofit
        temp_coal_supply_45q_retro_df = temp_coal_supply_45q_retro_df.droplevel(0)
        temp_coal_supply_45q_retro_df = temp_coal_supply_45q_retro_df.groupby(temp_coal_supply_45q_retro_df.index).sum()
        temp_coal_supply_45q_retro_df['facility_type'] = 'pp_coal'
        temp_coal_supply_45q_retro_df['new_vs_retrofit'] = 'retrofit'

        # Natural gas powerplants - New
        temp_natgas_supply_45q_new_df = self.parent.rest_sup_emm_45q.copy()
        temp_natgas_supply_45q_new_df = temp_natgas_supply_45q_new_df.loc[temp_natgas_supply_45q_new_df.index.get_level_values(0).isin([3])] # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_supply_45q_new_df = temp_natgas_supply_45q_new_df.droplevel(0)
        temp_natgas_supply_45q_new_df = temp_natgas_supply_45q_new_df.groupby(temp_natgas_supply_45q_new_df.index).sum()
        temp_natgas_supply_45q_new_df['facility_type'] = 'pp_natgas'
        temp_natgas_supply_45q_new_df['new_vs_retrofit'] = 'new'

        # Natural gas powerplants - Retrofit
        temp_natgas_supply_45q_retro_df = self.parent.rest_sup_emm_45q.copy()
        temp_natgas_supply_45q_retro_df = temp_natgas_supply_45q_retro_df.loc[temp_natgas_supply_45q_retro_df.index.get_level_values(0).isin([4])] # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_supply_45q_retro_df = temp_natgas_supply_45q_retro_df.droplevel(0)
        temp_natgas_supply_45q_retro_df = temp_natgas_supply_45q_retro_df.groupby(temp_natgas_supply_45q_retro_df.index).sum()
        temp_natgas_supply_45q_retro_df['facility_type'] = 'pp_natgas'
        temp_natgas_supply_45q_retro_df['new_vs_retrofit'] = 'retrofit'
        
        # BECCS
        temp_beccs_supply_45q_df = self.parent.rest_sup_emm_45q.copy()
        temp_beccs_supply_45q_df = temp_beccs_supply_45q_df.loc[temp_beccs_supply_45q_df.index.get_level_values(0).isin([5])] # 5=BECCS
        temp_beccs_supply_45q_df = temp_beccs_supply_45q_df.droplevel(0)
        temp_beccs_supply_45q_df['facility_type'] = 'beccs'
        temp_beccs_supply_45q_df['new_vs_retrofit'] = 'new'

        # Natgas Processing
        temp_ngp_supply_45q_df = self.parent.rest_sup_ngp_45q.copy()
        temp_ngp_supply_45q_df['facility_type'] = 'ng_processing'
        temp_ngp_supply_45q_df['new_vs_retrofit'] = 'both'

        # Cement 
        temp_cmt_supply_45q_df = self.parent.rest_sup_cmt_45q.copy()
        temp_cmt_supply_45q_df['facility_type'] = 'cement'
        temp_cmt_supply_45q_df = temp_cmt_supply_45q_df.reset_index()
        temp_cmt_supply_45q_df = temp_cmt_supply_45q_df.merge(self.parent.idm_mapping, # map to IDM census regions
                                                      how = 'right',
                                                      left_on = '1',
                                                      right_on = 'census_region')
        temp_cmt_supply_45q_df['value'] = temp_cmt_supply_45q_df['value'] * temp_cmt_supply_45q_df['distribution_ratio'] # distribute CO2 supplies from cenesus divisions to census regions
        temp_cmt_supply_45q_df = temp_cmt_supply_45q_df.set_index(['census_division','2'])
        temp_cmt_supply_45q_df.index.names = ['1','2']
        temp_cmt_supply_45q_df = temp_cmt_supply_45q_df[['value','facility_type']]
        temp_cmt_supply_45q_df['new_vs_retrofit'] = 'both'

        # Ethanol
        temp_eth_supply_45q_df = self.parent.rest_sup_eth_45q.copy()
        temp_eth_supply_45q_df['facility_type'] = 'ethanol'
        temp_eth_supply_45q_df['new_vs_retrofit'] = 'both'
        
        # Hydrodgen
        temp_h2_supply_45q_df = self.parent.rest_sup_h2_45q.copy()
        temp_h2_supply_45q_df['facility_type'] = 'ammonia'
        temp_h2_supply_45q_df['new_vs_retrofit'] = 'both'

        # Concatenate supply dfs
        self.industrial_co2_supply_45q = pd.concat([temp_ngp_supply_45q_df,temp_coal_supply_45q_new_df,temp_coal_supply_45q_retro_df,
                                                    temp_natgas_supply_45q_new_df,temp_natgas_supply_45q_retro_df,
                                                    temp_beccs_supply_45q_df,temp_cmt_supply_45q_df,temp_eth_supply_45q_df,temp_h2_supply_45q_df], ignore_index = False)

        # Clean dataset and remove summary census divisions
        self.industrial_co2_supply_45q = self.industrial_co2_supply_45q.reset_index()
        self.industrial_co2_supply_45q.columns = ['census_division', 'year', 'capture_volume_tonnes','facility_type','new_vs_retrofit']
        self.industrial_co2_supply_45q = self.industrial_co2_supply_45q.loc[self.industrial_co2_supply_45q['census_division'] <= 9]
        self.industrial_co2_supply_45q = self.industrial_co2_supply_45q.loc[self.industrial_co2_supply_45q['year'] >= self.parent.year_start]


        ### CO2 supply not eligible for tax credits
        # Coal powerplants - New
        temp_coal_supply_ntc_new_df = self.parent.rest_sup_emm_ntc.copy()
        temp_coal_supply_ntc_new_df = temp_coal_supply_ntc_new_df.loc[temp_coal_supply_ntc_new_df.index.get_level_values(0).isin([1])] # 1=coal new, 2=coal retrofit
        temp_coal_supply_ntc_new_df = temp_coal_supply_ntc_new_df.droplevel(0)
        temp_coal_supply_ntc_new_df = temp_coal_supply_ntc_new_df.groupby(temp_coal_supply_ntc_new_df.index).sum()
        temp_coal_supply_ntc_new_df['facility_type'] = 'pp_coal'
        temp_coal_supply_ntc_new_df['new_vs_retrofit'] = 'new'

        # Coal powerplants - Retrofit
        temp_coal_supply_ntc_retro_df = self.parent.rest_sup_emm_ntc.copy()
        temp_coal_supply_ntc_retro_df = temp_coal_supply_ntc_retro_df.loc[temp_coal_supply_ntc_retro_df.index.get_level_values(0).isin([2])] # 1=coal new, 2=coal retrofit
        temp_coal_supply_ntc_retro_df = temp_coal_supply_ntc_retro_df.droplevel(0)
        temp_coal_supply_ntc_retro_df = temp_coal_supply_ntc_retro_df.groupby(temp_coal_supply_ntc_retro_df.index).sum()
        temp_coal_supply_ntc_retro_df['facility_type'] = 'pp_coal'
        temp_coal_supply_ntc_retro_df['new_vs_retrofit'] = 'retrofit'

        # Natural gas powerplants - New
        temp_natgas_supply_ntc_new_df = self.parent.rest_sup_emm_ntc.copy()
        temp_natgas_supply_ntc_new_df = temp_natgas_supply_ntc_new_df.loc[temp_natgas_supply_ntc_new_df.index.get_level_values(0).isin([3])] # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_supply_ntc_new_df = temp_natgas_supply_ntc_new_df.droplevel(0)
        temp_natgas_supply_ntc_new_df = temp_natgas_supply_ntc_new_df.groupby(temp_natgas_supply_ntc_new_df.index).sum()
        temp_natgas_supply_ntc_new_df['facility_type'] = 'pp_natgas'
        temp_natgas_supply_ntc_new_df['new_vs_retrofit'] = 'new'

        # Natural gas powerplants - Retrofit
        temp_natgas_supply_ntc_retro_df = self.parent.rest_sup_emm_ntc.copy()
        temp_natgas_supply_ntc_retro_df = temp_natgas_supply_ntc_retro_df.loc[temp_natgas_supply_ntc_retro_df.index.get_level_values(0).isin([4])] # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_supply_ntc_retro_df = temp_natgas_supply_ntc_retro_df.droplevel(0)
        temp_natgas_supply_ntc_retro_df = temp_natgas_supply_ntc_retro_df.groupby(temp_natgas_supply_ntc_retro_df.index).sum()
        temp_natgas_supply_ntc_retro_df['facility_type'] = 'pp_natgas'
        temp_natgas_supply_ntc_retro_df['new_vs_retrofit'] = 'retrofit'

        # BECCS
        temp_beccs_supply_ntc_df = self.parent.rest_sup_emm_ntc.copy()
        temp_beccs_supply_ntc_df = temp_beccs_supply_ntc_df.loc[temp_beccs_supply_ntc_df.index.get_level_values(0).isin([5])] # 5=BECCS
        temp_beccs_supply_ntc_df = temp_beccs_supply_ntc_df.droplevel(0)
        temp_beccs_supply_ntc_df['facility_type'] = 'beccs'
        temp_beccs_supply_ntc_df['new_vs_retrofit'] = 'both'

        # Natgas Processing
        temp_ngp_supply_ntc_df = self.parent.rest_sup_ngp_ntc.copy()
        temp_ngp_supply_ntc_df['facility_type'] = 'ng_processing'
        temp_ngp_supply_ntc_df['new_vs_retrofit'] = 'both'

        # Cement
        temp_cmt_supply_ntc_df = self.parent.rest_sup_cmt_ntc.copy()
        temp_cmt_supply_ntc_df['facility_type'] = 'cement'
        temp_cmt_supply_ntc_df = temp_cmt_supply_ntc_df.reset_index()
        temp_cmt_supply_ntc_df = temp_cmt_supply_ntc_df.merge(self.parent.idm_mapping, # map to IDM census regions
                                                      how = 'right',
                                                      left_on = '1',
                                                      right_on = 'census_region')
        temp_cmt_supply_ntc_df['value'] = temp_cmt_supply_ntc_df['value'] * temp_cmt_supply_ntc_df['distribution_ratio'] # distribute CO2 supplies from cenesus divisions to census regions
        temp_cmt_supply_ntc_df = temp_cmt_supply_ntc_df.set_index(['census_division','2'])
        temp_cmt_supply_ntc_df.index.names = ['1','2']
        temp_cmt_supply_ntc_df = temp_cmt_supply_ntc_df[['value','facility_type']]
        temp_cmt_supply_ntc_df['new_vs_retrofit'] = 'both'

        # Ethanol
        temp_eth_supply_ntc_df = self.parent.rest_sup_eth_ntc.copy()
        temp_eth_supply_ntc_df['facility_type'] = 'ethanol'
        temp_eth_supply_ntc_df['new_vs_retrofit'] = 'both'

        # Hydrodgen
        temp_h2_supply_ntc_df = self.parent.rest_sup_h2_ntc.copy()
        temp_h2_supply_ntc_df['facility_type'] = 'ammonia'
        temp_h2_supply_ntc_df['new_vs_retrofit'] = 'both'

        # Concatenate supply dfs
        self.industrial_co2_supply_ntc = pd.concat([temp_ngp_supply_ntc_df,temp_coal_supply_ntc_new_df,temp_coal_supply_ntc_retro_df,
                                                    temp_natgas_supply_ntc_new_df,temp_natgas_supply_ntc_retro_df,
                                                    temp_beccs_supply_ntc_df,temp_cmt_supply_ntc_df,temp_eth_supply_ntc_df,temp_h2_supply_ntc_df], ignore_index = False)

        # Clean dataset and remove summary census divisions
        self.industrial_co2_supply_ntc = self.industrial_co2_supply_ntc.reset_index()
        self.industrial_co2_supply_ntc.columns = ['census_division', 'year', 'capture_volume_tonnes','facility_type','new_vs_retrofit']
        self.industrial_co2_supply_ntc = self.industrial_co2_supply_ntc.loc[self.industrial_co2_supply_ntc['census_division'] <= 9]
        self.industrial_co2_supply_ntc = self.industrial_co2_supply_ntc.loc[self.industrial_co2_supply_ntc['year'] >= self.parent.year_start]

        pass
    
    
    def concat_restart_variables_cost(self):
        '''Concatenate CO\ :sub:`2` capture type cost restart variables into 45Q and NTC capture cost tables.

        Returns
        -------
        self.industrial_co2_cost_inv : DataFrame
            Carbon capture investment costs (1987$) by NEMS module and region.

        self.industrial_co2_cost_om : DataFrame
            Carbon capture O&M costs (1987$) by NEMS module and region.
        
        '''
        ### CO2 capture cost investment
        # Coal Powerplants - New
        temp_coal_cost_inv_new_df = self.parent.rest_cst_emm_inv.copy()
        temp_coal_cost_inv_new_df = temp_coal_cost_inv_new_df.loc[temp_coal_cost_inv_new_df.index.get_level_values(0).isin([1])]  # 1=coal new, 2=coal retrofit
        temp_coal_cost_inv_new_df = temp_coal_cost_inv_new_df.droplevel(0)
        temp_coal_cost_inv_new_df = temp_coal_cost_inv_new_df.groupby(temp_coal_cost_inv_new_df.index).sum()
        temp_coal_cost_inv_new_df['facility_type'] = 'pp_coal'
        temp_coal_cost_inv_new_df['new_vs_retrofit'] = 'new'

        # Coal Powerplants - Retrofit
        temp_coal_cost_inv_retro_df = self.parent.rest_cst_emm_inv.copy()
        temp_coal_cost_inv_retro_df = temp_coal_cost_inv_retro_df.loc[temp_coal_cost_inv_retro_df.index.get_level_values(0).isin([2])]  # 1=coal new, 2=coal retrofit
        temp_coal_cost_inv_retro_df = temp_coal_cost_inv_retro_df.droplevel(0)
        temp_coal_cost_inv_retro_df = temp_coal_cost_inv_retro_df.groupby(temp_coal_cost_inv_retro_df.index).sum()
        temp_coal_cost_inv_retro_df['facility_type'] = 'pp_coal'
        temp_coal_cost_inv_retro_df['new_vs_retrofit'] = 'retrofit'

        # Natural Gas Powerplants - New
        temp_natgas_cost_inv_new_df = self.parent.rest_cst_emm_inv.copy()
        temp_natgas_cost_inv_new_df = temp_natgas_cost_inv_new_df.loc[temp_natgas_cost_inv_new_df.index.get_level_values(0).isin([3])]  # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_cost_inv_new_df = temp_natgas_cost_inv_new_df.droplevel(0)
        temp_natgas_cost_inv_new_df = temp_natgas_cost_inv_new_df.groupby(temp_natgas_cost_inv_new_df.index).sum()
        temp_natgas_cost_inv_new_df['facility_type'] = 'pp_natgas'
        temp_natgas_cost_inv_new_df['new_vs_retrofit'] = 'new'

        # Natural Gas Powerplants - Retrofit
        temp_natgas_cost_inv_retro_df = self.parent.rest_cst_emm_inv.copy()
        temp_natgas_cost_inv_retro_df = temp_natgas_cost_inv_retro_df.loc[temp_natgas_cost_inv_retro_df.index.get_level_values(0).isin([4])]  # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_cost_inv_retro_df = temp_natgas_cost_inv_retro_df.droplevel(0)
        temp_natgas_cost_inv_retro_df = temp_natgas_cost_inv_retro_df.groupby(temp_natgas_cost_inv_retro_df.index).sum()
        temp_natgas_cost_inv_retro_df['facility_type'] = 'pp_natgas'
        temp_natgas_cost_inv_retro_df['new_vs_retrofit'] = 'retrofit'

        # BECCS
        temp_beccs_cost_inv_df = self.parent.rest_cst_emm_inv.copy()
        temp_beccs_cost_inv_df = temp_beccs_cost_inv_df.loc[temp_beccs_cost_inv_df.index.get_level_values(0).isin([5])]  # 5=BECCS
        temp_beccs_cost_inv_df = temp_beccs_cost_inv_df.droplevel(0)
        temp_beccs_cost_inv_df['facility_type'] = 'beccs'
        temp_beccs_cost_inv_df['new_vs_retrofit'] = 'both'

        # Natural gas processing
        temp_ngp_cost_inv_df = self.parent.rest_cst_ngp_inv.copy()
        temp_ngp_cost_inv_df['facility_type'] = 'ng_processing'
        temp_ngp_cost_inv_df['new_vs_retrofit'] = 'both'

        # Cement 
        temp_cmt_cost_inv_df = self.parent.rest_cst_cmt_inv.copy()
        temp_cmt_cost_inv_df['facility_type'] = 'cement'
        temp_cmt_cost_inv_df = temp_cmt_cost_inv_df.reset_index()
        temp_cmt_cost_inv_df = temp_cmt_cost_inv_df.merge(self.parent.idm_mapping, # Map to IDM census region
                                                      how = 'right',
                                                      left_on = '1',
                                                      right_on = 'census_region')
        temp_cmt_cost_inv_df['value'] = temp_cmt_cost_inv_df['value'] * temp_cmt_cost_inv_df['distribution_ratio'] # distribute CO2 supplies from cenesus divisions to census regions
        temp_cmt_cost_inv_df = temp_cmt_cost_inv_df.set_index(['census_division','2'])
        temp_cmt_cost_inv_df.index.names = ['1','2']
        temp_cmt_cost_inv_df = temp_cmt_cost_inv_df[['value','facility_type']]
        temp_cmt_cost_inv_df['new_vs_retrofit'] = 'both'

        # Ethanol
        temp_eth_cost_inv_df = self.parent.rest_cst_eth_inv.copy()
        temp_eth_cost_inv_df['facility_type'] = 'ethanol'
        temp_eth_cost_inv_df['new_vs_retrofit'] = 'both'

        # Hydrodgen
        temp_h2_cost_inv_df = self.parent.rest_cst_h2_inv.copy()
        temp_h2_cost_inv_df['facility_type'] = 'ammonia'
        temp_h2_cost_inv_df['new_vs_retrofit'] = 'both'

        # Concatenate supply dfs
        self.industrial_co2_cost_inv = pd.concat([temp_ngp_cost_inv_df, temp_coal_cost_inv_new_df, temp_coal_cost_inv_retro_df,
                                                  temp_natgas_cost_inv_new_df, temp_natgas_cost_inv_retro_df,
                                                    temp_beccs_cost_inv_df, temp_cmt_cost_inv_df, temp_eth_cost_inv_df,
                                                    temp_h2_cost_inv_df], ignore_index=False)

        # Clean dataset and remove summary census divisions
        self.industrial_co2_cost_inv = self.industrial_co2_cost_inv.reset_index()
        self.industrial_co2_cost_inv.columns = ['census_division', 'year', 'nems_cost_1987$','facility_type','new_vs_retrofit']
        self.industrial_co2_cost_inv = self.industrial_co2_cost_inv.loc[self.industrial_co2_cost_inv['census_division'] <= 9]
        self.industrial_co2_cost_inv = self.industrial_co2_cost_inv.loc[self.industrial_co2_cost_inv['year'] >= self.parent.year_start]


        ### CO2 capture cost O&M
        # Coal Powerplants - New
        temp_coal_cost_om_new_df = self.parent.rest_cst_emm_om.copy()
        temp_coal_cost_om_new_df = temp_coal_cost_om_new_df.loc[temp_coal_cost_om_new_df.index.get_level_values(0).isin([1])]  # 1=coal new, 2=coal retrofit
        temp_coal_cost_om_new_df = temp_coal_cost_om_new_df.droplevel(0)
        temp_coal_cost_om_new_df = temp_coal_cost_om_new_df.groupby(temp_coal_cost_om_new_df.index).sum()
        temp_coal_cost_om_new_df['facility_type'] = 'pp_coal'
        temp_coal_cost_om_new_df['new_vs_retrofit'] = 'new'

        # Coal Powerplants - Retrofit
        temp_coal_cost_om_retro_df = self.parent.rest_cst_emm_om.copy()
        temp_coal_cost_om_retro_df = temp_coal_cost_om_retro_df.loc[temp_coal_cost_om_retro_df.index.get_level_values(0).isin([2])]  # 1=coal new, 2=coal retrofit
        temp_coal_cost_om_retro_df = temp_coal_cost_om_retro_df.droplevel(0)
        temp_coal_cost_om_retro_df = temp_coal_cost_om_retro_df.groupby(temp_coal_cost_om_retro_df.index).sum()
        temp_coal_cost_om_retro_df['facility_type'] = 'pp_coal'
        temp_coal_cost_om_retro_df['new_vs_retrofit'] = 'retrofit'

        # Natural Gas Powerplants - New
        temp_natgas_cost_om_new_df = self.parent.rest_cst_emm_om.copy()
        temp_natgas_cost_om_new_df = temp_natgas_cost_om_new_df.loc[temp_natgas_cost_om_new_df.index.get_level_values(0).isin([3])]  # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_cost_om_new_df = temp_natgas_cost_om_new_df.droplevel(0)
        temp_natgas_cost_om_new_df = temp_natgas_cost_om_new_df.groupby(temp_natgas_cost_om_new_df.index).sum()
        temp_natgas_cost_om_new_df['facility_type'] = 'pp_natgas'
        temp_natgas_cost_om_new_df['new_vs_retrofit'] = 'new'

        # Natural Gas Powerplants - Retrofit
        temp_natgas_cost_om_retro_df = self.parent.rest_cst_emm_om.copy()
        temp_natgas_cost_om_retro_df = temp_natgas_cost_om_retro_df.loc[temp_natgas_cost_om_retro_df.index.get_level_values(0).isin([4])]  # 3=NGCC new, 4=NGCC retrofit
        temp_natgas_cost_om_retro_df = temp_natgas_cost_om_retro_df.droplevel(0)
        temp_natgas_cost_om_retro_df = temp_natgas_cost_om_retro_df.groupby(temp_natgas_cost_om_retro_df.index).sum()
        temp_natgas_cost_om_retro_df['facility_type'] = 'pp_natgas'
        temp_natgas_cost_om_retro_df['new_vs_retrofit'] = 'retrofit'

        # BECCS
        temp_beccs_cost_om_df = self.parent.rest_cst_emm_om.copy()
        temp_beccs_cost_om_df = temp_beccs_cost_om_df.loc[temp_beccs_cost_om_df.index.get_level_values(0).isin([3, 4])]  # 5=BECCS
        temp_beccs_cost_om_df = temp_beccs_cost_om_df.droplevel(0)
        temp_beccs_cost_om_df['facility_type'] = 'beccs'
        temp_beccs_cost_om_df['new_vs_retrofit'] = 'both'

        # Natural Gas Processing
        temp_ngp_cost_om_df = self.parent.rest_cst_ngp_om.copy()
        temp_ngp_cost_om_df['facility_type'] = 'ng_processing'
        temp_ngp_cost_om_df['new_vs_retrofit'] = 'both'

        # Cement
        temp_cmt_cost_om_df = self.parent.rest_cst_cmt_om.copy()
        temp_cmt_cost_om_df['facility_type'] = 'cement'
        temp_cmt_cost_om_df = temp_cmt_cost_om_df.reset_index()
        temp_cmt_cost_om_df = temp_cmt_cost_om_df.merge(self.parent.idm_mapping, # map to IDM census regions
                                                      how = 'right',
                                                      left_on = '1',
                                                      right_on = 'census_region')
        temp_cmt_cost_om_df['value'] = temp_cmt_cost_om_df['value'] * temp_cmt_cost_om_df['distribution_ratio'] # distribute CO2 supplies from cenesus divisions to census regions
        temp_cmt_cost_om_df = temp_cmt_cost_om_df.set_index(['census_division','2'])
        temp_cmt_cost_om_df.index.names = ['1','2']
        temp_cmt_cost_om_df = temp_cmt_cost_om_df[['value','facility_type']]
        temp_cmt_cost_om_df['new_vs_retrofit'] = 'both'

        # Ethanol
        temp_eth_cost_om_df = self.parent.rest_cst_eth_om.copy()
        temp_eth_cost_om_df['facility_type'] = 'ethanol'
        temp_eth_cost_om_df['new_vs_retrofit'] = 'both'

        # Hydrodgen
        temp_h2_cost_om_df = self.parent.rest_cst_h2_om.copy()
        temp_h2_cost_om_df['facility_type'] = 'ammonia'
        temp_h2_cost_om_df['new_vs_retrofit'] = 'both'

        # Concatenate supply dfs
        self.industrial_co2_cost_om = pd.concat([temp_ngp_cost_om_df, temp_coal_cost_om_new_df, temp_coal_cost_om_retro_df,
                                                    temp_natgas_cost_om_new_df, temp_natgas_cost_om_retro_df,
                                                    temp_beccs_cost_om_df, temp_cmt_cost_om_df, temp_eth_cost_om_df,
                                                    temp_h2_cost_om_df], ignore_index=False)

        # Clean dataset and remove summary census divisions
        self.industrial_co2_cost_om = self.industrial_co2_cost_om.reset_index()
        self.industrial_co2_cost_om.columns = ['census_division', 'year', 'nems_cost_1987$','facility_type','new_vs_retrofit']
        self.industrial_co2_cost_om = self.industrial_co2_cost_om.loc[self.industrial_co2_cost_om['census_division'] <= 9]
        self.industrial_co2_cost_om = self.industrial_co2_cost_om.loc[self.industrial_co2_cost_om['year'] >= self.parent.year_start]

        pass
