"""Class for declaring CCATS Historical Data.

CCATS History: Summary
______________________
**ccats_history** is the preprocessor for model historical data, overwriting history with up-to-date inputs.

The module operates as follows:

    1. Reads in historical data :meth:`~ccats_history.CCATS_History.setup`.

    2. Sets history of relevant restart variables to zero in :meth:`~ccats_history.CCATS_History.zero_out_history`
       then overwrites history with input data in :meth:`~ccats_history.CCATS_History.update_history`.


CCATS History: Functions and Class Methods
__________________________________________
    * :meth:`~ccats_history.CCATS_History.__init__` - Constructor to initialize Class (instantiated by :meth:`module.Module.setup` in :ref:`module`).
    * :meth:`~ccats_history.CCATS_History.setup` - CCATS_History Setup method (called by :meth:`module.Module.setup`).
    * :meth:`~ccats_history.CCATS_History.run` - Runs main CCATS_History Processes (called by :meth:`module.Module.setup`).
    * :meth:`~ccats_history.CCATS_History.zero_out_history` - Set history year data of relevant restart variables to 0 (called by :meth:`~ccats_history.CCATS_History.run`).
    * :meth:`~ccats_history.CCATS_History.update_history` - Overwrite history in relevant restart files with up-to-date inputs (called by :meth:`~ccats_history.CCATS_History.run`).


CCATS History: Input Files
__________________________
    * hist_setup.csv - setup file with model financial inputs (debt rate, block durations, etc.).
    * co2_supply_history_div.csv - CO\ :sub:`2` supply history by census division.
    * co2_supply_history_reg.csv - CO\ :sub:`2` supply history by census region.
    * co2_eor_history_div.csv - CO\ :sub:`2` EOR demand history by census division.
    * co2_eor_history_reg.csv - CO\ :sub:`2` EOR demand history by census region.
    * co2_seq_history_div.csv - CO\ :sub:`2` saline formation storage history by census division.
    * co2_seq_history_reg.csv - CO\ :sub:`2` saline formation storage history by census region.


CCATS History: Output Debug Files
_________________________________
None


CCATS History: Output Restart Variables
_______________________________________
    * *CCATSDAT_CO2_sup_out* - CO\ :sub:`2` supply output after optimization, by census division.
    * *CCATSDAT_CO2_sup_out_r* - CO\ :sub:`2` supply output after optimization, by census region.
    * *CCATSDAT_CO2_seq_out* - CO\ :sub:`2` sequestration output after optimization, by census division.
    * *CCATSDAT_CO2_seq_out_r* - CO\ :sub:`2` sequestration output after optimization, by census region.


CCATS History: Code
___________________
"""

import pandas as pd
import numpy as np
from ccats_common import common as com


class CCATS_History:
    """Class for declaring CCATS Historical Data.

    """
    
    def __init__(self, parent):
        """Initializes CCATS_History object.

        Parameters
        ----------
        parent : module.Module
            Pointer to head module 

        Returns
        -------
        None

        """
        self.parent = parent #: module.Module head module

        # Input tables
        self.co2_supply_hist_div    = pd.DataFrame() # DataFrame of CO2 supply history by census division
        self.co2_supply_hist_reg    = pd.DataFrame() # DataFrame of CO2 supply history by census region
        self.co2_eor_hist_div       = pd.DataFrame() # DataFrame of CO2 EOR demand history by census division
        self.co2_eor_hist_reg       = pd.DataFrame() # DataFrame of CO2 EOR demand history by census region
        self.co2_seq_hist_div       = pd.DataFrame() # DataFrame of CO2 saline formation storage history by census division
        self.co2_seq_hist_reg       = pd.DataFrame() # DataFrame of CO2 saline formation storage history by census region


    def setup(self, setup_filename):
        '''Setup function for CCATS financial assumptions.

        Parameters
        ----------
        setup_filename : str
            **ccats_history** setup file name.

        Returns
        -------
        self.co2_supply_hist_div : DataFrame
            DataFrame of CO\ :sub:`2` supply history by census division.

        self.co2_supply_hist_reg : DataFrame
            DataFrame of CO\ :sub:`2` supply history by census region.

        self.co2_eor_hist_div : DataFrame
            DataFrame of CO\ :sub:`2` EOR demand history by census division.

        self.co2_eor_hist_reg : DataFrame
            DataFrame of CO\ :sub:`2` EOR demand history by census region.

        self.co2_seq_hist_div : DataFrame
            DataFrame of CO\ :sub:`2` saline formation storage history by census division.

        self.co2_seq_hist_reg : DataFrame
            DataFrame of CO\ :sub:`2` saline formation storage history by census region.

        '''
        # Load in input file
        self.setup_table  = com.read_dataframe(self.parent.input_path + setup_filename, index_col=0)
        
        # Load in history input tables
        input_path = self.parent.input_path + 'history_steo//'
        self.co2_supply_hist_div = com.read_dataframe(input_path + self.setup_table.at['co2_supply_history_div','value'])
        self.co2_supply_hist_reg = com.read_dataframe(input_path + self.setup_table.at['co2_supply_history_reg','value'])
        self.co2_eor_hist_div = com.read_dataframe(input_path + self.setup_table.at['co2_eor_history_div','value'])
        self.co2_eor_hist_reg = com.read_dataframe(input_path + self.setup_table.at['co2_eor_history_reg','value'])
        self.co2_seq_hist_div = com.read_dataframe(input_path + self.setup_table.at['co2_seq_history_div','value'])
        self.co2_seq_hist_reg = com.read_dataframe(input_path + self.setup_table.at['co2_seq_history_reg','value'])

        pass


    def run(self):
        '''Run CCATS history preprocessor.

        Parameters
        ----------
        None

        Returns
        -------
        self.co2_supply_hist_div : DataFrame
            DataFrame of CO\ :sub:`2` supply history by census division.

        self.co2_supply_hist_reg : DataFrame
            DataFrame of CO\ :sub:`2` supply history by census region.

        '''
        ### Localize parent variables
        self.logger = self.parent.logger

        # Map CO2 facility types to the restart file facility index
        self.co2_supply_hist_div['facility_type'] = self.co2_supply_hist_div['facility_type'].map(self.parent.co2_supply_index)
        self.co2_supply_hist_reg['facility_type'] = self.co2_supply_hist_reg['facility_type'].map(self.parent.co2_supply_index)
        
        # Zero out history
        self.logger.info('Zero out History')
        self.zero_out_history()

        #Update history
        self.logger.info('Update History')
        self.update_history()
        
        pass
    

    def zero_out_history(self):
        '''Set data for history years equal to 0 in relevant restart variables.

            * This is done to ensure that historical dataset only contains volumes from the most up-to-date input files.

        Parameters
        ----------
        None

        Returns
        -------
        self.parent.rest_co2_sup_out : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SUP_OUT*.

        self.parent.rest_co2_sup_out_r : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SUP_OUT_R*.

        self.parent.rest_co2_seq_out : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SEQ_OUT*.

        self.parent.rest_co2_seq_out_r : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SEQ_OUT_R*.
        '''
        ### Set historical volumes for relevant restart variables to 0
        temp_hist = self.parent.rest_co2_sup_out.loc[self.parent.rest_co2_sup_out.index.get_level_values(2) < self.parent.year_start].copy()
        temp_hist['value'] = 0
        self.parent.rest_co2_sup_out.update(temp_hist)
        
        temp_hist = self.parent.rest_co2_sup_out_r.loc[self.parent.rest_co2_sup_out_r.index.get_level_values(2) < self.parent.year_start].copy()
        temp_hist['value'] = 0
        self.parent.rest_co2_sup_out_r.update(temp_hist)
                
        temp_hist = self.parent.rest_co2_seq_out.loc[self.parent.rest_co2_seq_out.index.get_level_values(2) < self.parent.year_start].copy()
        temp_hist['value'] = 0
        self.parent.rest_co2_seq_out.update(temp_hist)
        
        temp_hist = self.parent.rest_co2_seq_out_r.loc[self.parent.rest_co2_seq_out_r.index.get_level_values(2) < self.parent.year_start].copy()
        temp_hist['value'] = 0
        self.parent.rest_co2_seq_out_r.update(temp_hist)

        pass


    def update_history(self):
        '''Update relevant restart files with historical data from input files.

        Parameters
        ----------
        None

        Returns
        -------
        self.parent.rest_co2_sup_out : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SUP_OUT*.

        self.parent.rest_co2_sup_out_r : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SUP_OUT_R*.

        self.parent.rest_co2_seq_out : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SEQ_OUT*.

        self.parent.rest_co2_seq_out_r : DataFrame
            Localized copy of restart variable *CCATSDAT_CO2_SEQ_OUT_R*.
        '''
        # Write history to local reported restart variables
        # CO2 Supply
        temp_supply_div = self.co2_supply_hist_div.copy()
        temp_supply_div = temp_supply_div.set_index(['census_division','facility_type','year'])
        temp_supply_div.columns = ['value']
        temp_supply_div.index.names = self.parent.rest_co2_sup_out.index.names
        self.parent.rest_co2_sup_out.update(temp_supply_div)

        temp_supply_reg = self.co2_supply_hist_reg.copy()
        temp_supply_reg = temp_supply_reg.set_index(['census_region','facility_type','year'])
        temp_supply_reg.columns = ['value']
        temp_supply_reg.index.names = self.parent.rest_co2_sup_out.index.names
        self.parent.rest_co2_sup_out_r.update(temp_supply_reg)

        #CO2 EOR
        self.co2_eor_hist_div['co2_seq_type'] = 1
        self.co2_eor_hist_reg['co2_seq_type'] = 1

        temp_co2_eor_div = self.co2_eor_hist_div.copy()
        temp_co2_eor_div = temp_co2_eor_div.set_index(['census_division','co2_seq_type','year'])
        temp_co2_eor_div.columns = ['value']
        temp_co2_eor_div.index.names = self.parent.rest_co2_seq_out.index.names
        self.parent.rest_co2_seq_out.update(temp_co2_eor_div)

        temp_co2_eor_reg = self.co2_eor_hist_reg.copy()
        temp_co2_eor_reg = temp_co2_eor_reg.set_index(['census_region','co2_seq_type','year'])
        temp_co2_eor_reg.columns = ['value']
        temp_co2_eor_reg.index.names = self.parent.rest_co2_seq_out_r.index.names
        self.parent.rest_co2_seq_out_r.update(temp_co2_eor_reg)

        # CO2 Sequestered
        self.co2_seq_hist_div['co2_seq_type'] = 2
        self.co2_seq_hist_reg['co2_seq_type'] = 2

        temp_co2_seq_div = self.co2_seq_hist_div.copy()
        temp_co2_seq_div = temp_co2_seq_div.set_index(['census_division', 'co2_seq_type', 'year'])
        temp_co2_seq_div.columns = ['value']
        temp_co2_seq_div.index.names = self.parent.rest_co2_seq_out.index.names
        self.parent.rest_co2_seq_out.update(temp_co2_seq_div)

        temp_co2_seq_reg = self.co2_seq_hist_reg.copy()
        temp_co2_seq_reg = temp_co2_seq_reg.set_index(['census_region', 'co2_seq_type', 'year'])
        temp_co2_seq_reg.columns = ['value']
        temp_co2_seq_reg.index.names = self.parent.rest_co2_seq_out_r.index.names
        self.parent.rest_co2_seq_out_r.update(temp_co2_seq_reg)

        pass
