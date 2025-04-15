"""Class for handling intermediate variables in HSM.

    Notes
    -----
        * In OGSM we kept cumulative variables in Fortran working memory (i.e. projects each year)
        * In HSM this isn't possible, so instead these tables are written to Pickle (PKL) files which are stored in the hsm directory between model years
        * In year 1 the PKLs are created, and then in subsequent years they are written to and read
        * The PKL files are only written out when fcrl = 1 (final regular iteration) or ncrl = 1 (report iteration) to avoid duplication during other NEMS iterations
        * There are therefore two sets of .pkl files, one "fcrl" set for regular iterations, and one "ncrl" set for reporting iterations
        * All .pkl filenames and corresponding df names match the relevant corresponding submodule variable names
          (i.e. on_projects_xxxx.pkl = self.projects in the onshore submodule).


    The Pickle methods operate as follows:

        1. The class is initialized in the intermediate_vars *__init__* method, with class being assigned as a child to **module_unf** here.

        2. *read_pkl_vars* is called from **module_unf.py** in all model years excluding year 1. In this method all required .pkl
           local, iterative variable files are read into HSM as class dataframes accessible by all other modules in HSM.

        3. *write_pkl_vars* is called from **module_unf.py** in all model years. In this method all required local, iterative dataframes
           are written to .pkl files.

    Pickle library documentation: https://docs.python.org/3/library/pickle.html
"""

import pandas as pd

class intermediate_vars:
    def __init__(self, parent):
        self.parent = parent #: module.Module head module


    def read_pkl_vars(self, itr_type, current_year, temp_filepath):
        '''Read in Pickle Intermediate Tables.

        Returns
        -------
        PKL Dataframes

        '''
        ###Onshore
        self.on_projects                    = pd.read_pickle(temp_filepath + 'on_projects_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_projects_undiscovered       = pd.read_pickle(temp_filepath + 'on_projects_undiscovered_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_projects_discovered         = pd.read_pickle(temp_filepath + 'on_projects_discovered_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_crude_production            = pd.read_pickle(temp_filepath + 'on_crude_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_natgas_production           = pd.read_pickle(temp_filepath + 'on_natgas_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_ngpl_production             = pd.read_pickle(temp_filepath + 'on_ngpl_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_ngpl_ethane_production      = pd.read_pickle(temp_filepath + 'on_ngpl_ethane_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_ngpl_propane_production     = pd.read_pickle(temp_filepath + 'on_ngpl_propane_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_ngpl_butane_production      = pd.read_pickle(temp_filepath + 'on_ngpl_butane_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_ngpl_isobutane_production   = pd.read_pickle(temp_filepath + 'on_ngpl_isobutane_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_ngpl_proplus_production     = pd.read_pickle(temp_filepath + 'on_ngpl_proplus_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_wells                       = pd.read_pickle(temp_filepath + 'on_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_producing_wells             = pd.read_pickle(temp_filepath + 'on_producing_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_producing_footage           = pd.read_pickle(temp_filepath + 'on_producing_footage_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_dryholes                    = pd.read_pickle(temp_filepath + 'on_dryholes_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_exploratory_wells           = pd.read_pickle(temp_filepath + 'on_exploratory_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_co2_injected                = pd.read_pickle(temp_filepath + 'on_co2_injected_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_co2_recycled                = pd.read_pickle(temp_filepath + 'on_co2_recycled_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_co2_eor_wells               = pd.read_pickle(temp_filepath + 'on_co2_eor_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.on_co2_net_cost                = pd.read_pickle(temp_filepath + 'on_co2_net_cost_' + itr_type + '_' + str(current_year - 1) + '.pkl')



        ###Offshore
        self.off_reg_crude_price            = pd.read_pickle(temp_filepath + 'off_reg_crude_price' + '.pkl')
        self.off_reg_natgas_price           = pd.read_pickle(temp_filepath + 'off_reg_natgas_price' + '.pkl')
        self.off_announced_fields           = pd.read_pickle(temp_filepath + 'off_announced_fields_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_discovered_fields          = pd.read_pickle(temp_filepath + 'off_discovered_fields_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_discovered_field_dist      = pd.read_pickle(temp_filepath + 'off_discovered_field_dist_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_undiscovered_fields        = pd.read_pickle(temp_filepath + 'off_undiscovered_fields_' + itr_type + '_' + str(current_year - 1) +  '.pkl')
        self.off_fields                     = pd.read_pickle(temp_filepath + 'off_fields_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_fields_selected            = pd.read_pickle(temp_filepath + 'off_fields_selected_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_total_fields               = pd.read_pickle(temp_filepath + 'off_total_fields_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_cumulative_nfws            = pd.read_pickle(temp_filepath + 'off_cumulative_nfws_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_crude_production           = pd.read_pickle(temp_filepath + 'off_crude_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_natgas_production          = pd.read_pickle(temp_filepath + 'off_natgas_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_nagas_production           = pd.read_pickle(temp_filepath + 'off_nagas_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_adgas_production           = pd.read_pickle(temp_filepath + 'off_adgas_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_wells                      = pd.read_pickle(temp_filepath + 'off_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_exploratory_wells          = pd.read_pickle(temp_filepath + 'off_exploratory_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_drill_rig_constraint       = pd.read_pickle(temp_filepath + 'off_drill_rig_constraint_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_field_crude_production     = pd.read_pickle(temp_filepath + 'off_field_crude_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.off_field_natgas_production    = pd.read_pickle(temp_filepath + 'off_field_natgas_production_' + itr_type + '_' + str(current_year - 1) + '.pkl')

        ###Alaska
        self.ak_projects            = pd.read_pickle(temp_filepath + 'ak_projects_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ak_projects_known      = pd.read_pickle(temp_filepath + 'ak_projects_known_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ak_crude_production    = pd.read_pickle(temp_filepath + 'ak_crude_production_' + itr_type + '_' + str(current_year - 1) +'.pkl')


        ###Canada
        self.can_na_prod        = pd.read_pickle(temp_filepath + 'can_na_prod_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.can_na_prod_new    = pd.read_pickle(temp_filepath + 'can_na_prod_new_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.can_na_prod_sum    = pd.read_pickle(temp_filepath + 'can_na_prod_sum_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.can_ad_prod        = pd.read_pickle(temp_filepath + 'can_ad_prod_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.can_ad_prod_sum    = pd.read_pickle(temp_filepath + 'can_ad_prod_sum_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.can_can_wells      = pd.read_pickle(temp_filepath + 'can_can_wells_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.can_prod_profile   = pd.read_pickle(temp_filepath + 'can_prod_profile_' + itr_type + '_' + str(current_year - 1) + '.pkl')

        ###NGPP
        self.ngp_facilities_base_df        = pd.read_pickle(temp_filepath + 'ngp_facilities_base_df_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_rep_facilities_df         = pd.read_pickle(temp_filepath + 'ngp_rep_facilities_df_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_legacy_facilities_df      = pd.read_pickle(temp_filepath + 'ngp_legacy_facilities_df_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_facilities_df             = pd.read_pickle(temp_filepath + 'ngp_facilities_df_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_active_cc_facilities_df   = pd.read_pickle(temp_filepath + 'ngp_active_cc_facilities_df_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_co2_capture_volumes_45q   = pd.read_pickle(temp_filepath + 'ngp_co2_capture_volumes_45q_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_co2_capture_volumes_ntc   = pd.read_pickle(temp_filepath + 'ngp_co2_capture_volumes_ntc_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_co2_capture_costs_inv     = pd.read_pickle(temp_filepath + 'ngp_co2_capture_costs_inv_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_co2_capture_costs_om      = pd.read_pickle(temp_filepath + 'ngp_co2_capture_costs_om_' + itr_type + '_' + str(current_year - 1) + '.pkl')
        self.ngp_cc_electricity_demand     = pd.read_pickle(temp_filepath + 'ngp_cc_electricity_demand_' + itr_type + '_' + str(current_year - 1) + '.pkl')

    def write_pkl_variables(self, itr_type,  current_year, temp_filepath):
        '''Write out Pickle Intermediate Tables.

        Returns
        -------
        PKL Files
        '''

        ###Onshore
        self.on_projects.to_pickle(temp_filepath + 'on_projects_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_projects_undiscovered.to_pickle(temp_filepath + 'on_projects_undiscovered_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_projects_discovered.to_pickle(temp_filepath + 'on_projects_discovered_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_crude_production.to_pickle(temp_filepath + 'on_crude_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_natgas_production.to_pickle(temp_filepath + 'on_natgas_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_ngpl_production.to_pickle(temp_filepath + 'on_ngpl_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_ngpl_ethane_production.to_pickle(temp_filepath + 'on_ngpl_ethane_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_ngpl_propane_production.to_pickle(temp_filepath + 'on_ngpl_propane_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_ngpl_butane_production.to_pickle(temp_filepath + 'on_ngpl_butane_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_ngpl_isobutane_production.to_pickle(temp_filepath + 'on_ngpl_isobutane_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_ngpl_proplus_production.to_pickle(temp_filepath + 'on_ngpl_proplus_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_wells.to_pickle(temp_filepath + 'on_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_producing_wells.to_pickle(temp_filepath + 'on_producing_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_producing_footage.to_pickle(temp_filepath + 'on_producing_footage_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_dryholes.to_pickle(temp_filepath + 'on_dryholes_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_exploratory_wells.to_pickle(temp_filepath + 'on_exploratory_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_co2_injected.to_pickle(temp_filepath + 'on_co2_injected_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_co2_recycled.to_pickle(temp_filepath + 'on_co2_recycled_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_co2_eor_wells.to_pickle(temp_filepath + 'on_co2_eor_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.on_co2_net_cost.to_pickle(temp_filepath + 'on_co2_net_cost_' + itr_type + '_' + str(current_year) + '.pkl')

        ###Offshore
        self.off_reg_crude_price.to_pickle(temp_filepath + 'off_reg_crude_price' + '.pkl')
        self.off_reg_natgas_price.to_pickle(temp_filepath + 'off_reg_natgas_price' + '.pkl')
        self.off_announced_fields.to_pickle(temp_filepath + 'off_announced_fields_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_discovered_fields.to_pickle(temp_filepath + 'off_discovered_fields_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_discovered_field_dist.to_pickle(temp_filepath + 'off_discovered_field_dist_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_undiscovered_fields.to_pickle(temp_filepath + 'off_undiscovered_fields_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_fields.to_pickle(temp_filepath + 'off_fields_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_fields_selected.to_pickle(temp_filepath + 'off_fields_selected_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_total_fields.to_pickle(temp_filepath + 'off_total_fields_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_cumulative_nfws.to_pickle(temp_filepath + 'off_cumulative_nfws_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_crude_production.to_pickle(temp_filepath + 'off_crude_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_natgas_production.to_pickle(temp_filepath + 'off_natgas_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_nagas_production.to_pickle(temp_filepath + 'off_nagas_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_adgas_production.to_pickle(temp_filepath + 'off_adgas_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_wells.to_pickle(temp_filepath + 'off_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_exploratory_wells.to_pickle(temp_filepath + 'off_exploratory_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_drill_rig_constraint.to_pickle(temp_filepath + 'off_drill_rig_constraint_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_field_crude_production.to_pickle(temp_filepath + 'off_field_crude_production_' + itr_type + '_' + str(current_year) + '.pkl')
        self.off_field_natgas_production.to_pickle(temp_filepath + 'off_field_natgas_production_' + itr_type + '_' + str(current_year) + '.pkl')

        ###Alaska
        self.ak_projects.to_pickle(temp_filepath + 'ak_projects_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ak_projects_known.to_pickle(temp_filepath + 'ak_projects_known_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ak_crude_production.to_pickle(temp_filepath + 'ak_crude_production_' + itr_type + '_' + str(current_year) + '.pkl')


        ###Canada
        self.can_na_prod.to_pickle(temp_filepath + 'can_na_prod_' + itr_type + '_' + str(current_year) + '.pkl')
        self.can_na_prod_new.to_pickle(temp_filepath + 'can_na_prod_new_' + itr_type + '_' + str(current_year) + '.pkl')
        self.can_na_prod_sum.to_pickle(temp_filepath + 'can_na_prod_sum_' + itr_type + '_' + str(current_year) + '.pkl')
        self.can_ad_prod.to_pickle(temp_filepath + 'can_ad_prod_' + itr_type + '_' + str(current_year) + '.pkl')
        self.can_ad_prod_sum.to_pickle(temp_filepath + 'can_ad_prod_sum_' + itr_type + '_' + str(current_year) + '.pkl')
        self.can_can_wells.to_pickle(temp_filepath + 'can_can_wells_' + itr_type + '_' + str(current_year) + '.pkl')
        self.can_prod_profile.to_pickle(temp_filepath + 'can_prod_profile_' + itr_type + '_' + str(current_year) + '.pkl')

        ###NGPP
        self.ngp_facilities_base_df.to_pickle(temp_filepath + 'ngp_facilities_base_df_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_rep_facilities_df.to_pickle(temp_filepath + 'ngp_rep_facilities_df_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_legacy_facilities_df.to_pickle(temp_filepath + 'ngp_legacy_facilities_df_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_facilities_df.to_pickle(temp_filepath + 'ngp_facilities_df_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_active_cc_facilities_df.to_pickle(temp_filepath + 'ngp_active_cc_facilities_df_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_co2_capture_volumes_45q.to_pickle(temp_filepath + 'ngp_co2_capture_volumes_45q_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_co2_capture_volumes_ntc.to_pickle(temp_filepath + 'ngp_co2_capture_volumes_ntc_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_co2_capture_costs_inv.to_pickle(temp_filepath + 'ngp_co2_capture_costs_inv_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_co2_capture_costs_om.to_pickle(temp_filepath + 'ngp_co2_capture_costs_om_' + itr_type + '_' + str(current_year) + '.pkl')
        self.ngp_cc_electricity_demand.to_pickle(temp_filepath + 'ngp_cc_electricity_demand_' + itr_type + '_' + str(current_year) + '.pkl')


        pass