"""Class for handling intermediate variables in CCATS.

CCATS Pickle: Summary
_____________________
* In year 1 the .pkl files are created, and then in subsequent years they are written to and read.
* The .pkl files are only written out when fcrl = 1 (final regular iteration) or ncrl = 1 (report iteration) to avoid duplication during other NEMS iterations.
* There are therefore two sets of .pkl files, one "fcrl" set for regular iterations, and one "ncrl" set for reporting iterations.
* All *.pkl* filenames and corresponding df names match the relevant corresponding submodule variable names
  (i.e. preproc_i_storage_xxxx.pkl = self.i_storage_df in :ref:`preprocessor`).


The Pickle methods operate as follows:

    1. The class is initialized in the :meth:`~ccats_pickle.CCATS_Pickle.__init__` method, with class being assigned as a child to :ref:`module` here.

    2. :meth:`~ccats_pickle.CCATS_Pickle.read_pkl_vars` is called from :ref:`module` in all model years excluding year 1. In this method all required .pkl
       local, iterative variable files are read into HSM as class dataframes accessible by all other modules in HSM.

    3. :meth:`~ccats_pickle.CCATS_Pickle.write_pkl_variables` is called from :ref:`module` in all model years. In this method all required local, iterative dataframes
       are written to .pkl files.

Pickle library documentation: https://docs.python.org/3/library/pickle.html


CCATS Pickle: Functions and Class Methods
_________________________________________
    * :meth:`~ccats_pickle.CCATS_Pickle.__init__` - Constructor to initialize Class (instantiated by :meth:`module.Module.setup` in :ref:`module`).
    * :meth:`~ccats_pickle.CCATS_Pickle.read_pkl_vars` - Read in Pickle local tables (called by :meth:`module.Module.setup`).
    * :meth:`~ccats_pickle.CCATS_Pickle.write_pkl_variables` - Write out Pickle local tables (called by :meth:`output.Output.write_pkl`).


CCATS Pickle: Input Files
_________________________
None


CCATS Pickle: Output Debug Files
________________________________
None


CCATS Pickle: Output Restart Variables
______________________________________
None


CCATS Pickle: Code
__________________
"""

import pandas as pd


class CCATS_Pickle:
    """Class for handling intermediate variables in CCATS

    """

    def __init__(self, parent):
        """Initializes CCATS_Pickle object.

        Parameters
        ----------
        parent : module.Module
            Pointer to head module 

        Returns
        -------
        None

        """
        self.parent = parent #: module.Module head module


    def read_pkl_vars(self, itr_type, year_current, temp_filepath):
        '''Read in Pickle local tables.

        Parameters
        ----------
        itr_type : str
            FCRL or NCRL iteration.

        year_current : int
            Current year being modeled.
        
        temp_filepath : str
            Location of .pkl files.

        Returns
        -------
        self.preproc_i_storage_df : DataFrame
            DataFrame of storage formations - input data.
        self.preproc_i_co2_supply_facility_df : DataFrame
            DataFrame of Co\ :sub:`2` cost curve from NETL - input data.
        self.preproc_i_pipeline_lookup_df : DataFrame
            DataFrame of Co\ :sub:`2` pipeline lookup table - input data.
        self.preproc_i_eor_demand_df : DataFrame
            DataFrame of Co\ :sub:`2` EOR site CO2 demanded - input data.
        self.preproc_i_eor_cost_net_df : DataFrame
            DataFrame of Co\ :sub:`2` EOR net cost for Co\ :sub:`2` - input data.
        self.preproc_i_ts_multiplier_df : DataFrame
            DataFrame of multipliers for ts-ts node arcs - input data.
        self.preproc_pipes_existing_df : DataFrame
             DataFrame of existing Co\ :sub:`2` pipeline infrastructure in a given model year.
        self.preproc_storage_existing_df : DataFrame
            DataFrame of existing Co\ :sub:`2` storage infrastructure in a given model year.
        self.preproc_co2_facility_eligibility_df : DataFrame
            DataFrame of Co\ :sub:`2` facility 45Q eligibility.
        self.mod_new_built_pipes_df : DataFrame
            DataFrame of new pipelines built in previous model year Block 1.
        self.mod_new_aors_df : DataFrame
            DataFrame of new AORS, carried over from the previous model year.
        self.mod_store_prev_b0_df : DataFrame
            DataFrame of previous model year Co\ :sub:`2` stored in Block 0.
        '''
        ### Preprocessor
        self.preproc_i_storage_df                   = pd.read_pickle(temp_filepath + 'preproc_i_storage_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_i_co2_supply_facility_df       = pd.read_pickle(temp_filepath + 'preproc_i_co2_supply_facility_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_i_pipeline_lookup_df           = pd.read_pickle(temp_filepath + 'preproc_i_pipeline_lookup_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_i_eor_demand_df                = pd.read_pickle(temp_filepath + 'preproc_i_eor_demand_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_i_eor_cost_net_df              = pd.read_pickle(temp_filepath + 'preproc_i_eor_cost_net_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_i_ts_multiplier_df             = pd.read_pickle(temp_filepath + 'preproc_i_ts_multiplier_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')

        self.preproc_pipes_existing_df              = pd.read_pickle(temp_filepath + 'preproc_pipes_existing_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_storage_existing_df            = pd.read_pickle(temp_filepath + 'preproc_storage_existing_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.preproc_co2_facility_eligibility_df    = pd.read_pickle(temp_filepath + 'preproc_co2_facility_eligibility_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')

        ### Postprocessor/Module
        self.mod_new_built_pipes_df     = pd.read_pickle(temp_filepath + 'mod_new_built_pipes_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.mod_new_aors_df            = pd.read_pickle(temp_filepath + 'mod_new_aors_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')
        self.mod_store_prev_b0_df       = pd.read_pickle(temp_filepath + 'mod_store_prev_b0_df_' + itr_type + '_' + str(year_current - 1) + '.pkl')

        pass


    def write_pkl_variables(self, itr_type, year_current, temp_filepath):
        '''Write out Pickle local tables.

        Parameters
        ----------
        itr_type : str
            FCRL or NCRL iteration.

        year_current : int
            Current year being modeled.
        
        temp_filepath : str
            Location of .pkl files.

        Returns
        -------
        None


        '''
        ### Preprocessor
        self.preproc_i_storage_df.to_pickle(temp_filepath + 'preproc_i_storage_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_i_co2_supply_facility_df.to_pickle(temp_filepath + 'preproc_i_co2_supply_facility_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_i_pipeline_lookup_df.to_pickle(temp_filepath + 'preproc_i_pipeline_lookup_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_i_eor_demand_df.to_pickle(temp_filepath + 'preproc_i_eor_demand_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_i_eor_cost_net_df.to_pickle(temp_filepath + 'preproc_i_eor_cost_net_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_i_ts_multiplier_df.to_pickle(temp_filepath + 'preproc_i_ts_multiplier_df_' + itr_type + '_' + str(year_current) + '.pkl')

        self.preproc_pipes_existing_df.to_pickle(temp_filepath + 'preproc_pipes_existing_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_storage_existing_df.to_pickle(temp_filepath + 'preproc_storage_existing_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.preproc_co2_facility_eligibility_df.to_pickle(temp_filepath + 'preproc_co2_facility_eligibility_df_' + itr_type + '_' + str(year_current) + '.pkl')

        ### Postprocessor/Module
        self.mod_new_built_pipes_df.to_pickle(temp_filepath + 'mod_new_built_pipes_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.mod_new_aors_df.to_pickle(temp_filepath + 'mod_new_aors_df_' + itr_type + '_' + str(year_current) + '.pkl')
        self.mod_store_prev_b0_df.to_pickle(temp_filepath + 'mod_store_prev_b0_df_' + itr_type + '_' + str(year_current) + '.pkl')

        pass
