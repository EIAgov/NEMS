"""Submodule for outputting CCATS results.

Output: Summary
_______________
This submodule outputs CCATS results:

    1. :meth:`~output.Output.setup` and :meth:`~output.Output.run` are called from :ref:`module`.

    2. Pickle results.
    
    3. Reset prices if "self.parent.price_reset_switch" === True.
    
    4. Prepare variables to be written to restart file.


Output: Input Files
___________________

None


Output: Model Functions and Class Methods
_________________________________________

* :meth:`~output.Output.__init__` - Initializes variables to be populated by Output (called by :meth:`module.Module.setup`)
* :meth:`~output.Output.setup` - Set-up the submodule (called by :meth:`module.Module.setup`)
* :meth:`~output.Output.run` - Calls remaining Output functions (called by :meth:`module.Module.run`)
* :meth:`~output.Output.write_pkl` - Write results using pickle (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~output.Output.reset_prices` - Reset prices (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~output.Output.write_local_restart_vars` - Move local restart variables to global variables (called by :meth:`~postprocessor.Postprocessor.run`)
    

Output: Output Debug Files
__________________________

None


Output: Output Restart Variables
________________________________

* **tcs45q_ccs_eor_45q** - CO\ :sub:`2` sequestered in EOR.
* **tcs45q_ccs_saline_45q** - CO\ :sub:`2` sequestered in saline storage.
* **tcs45q_i_45q_duration** - Tax code section 45Q subsidy duration
* **tcs45q_i_45q_syr** - Start year of tax code section 45Q subsidy
* **tcs45q_i_45q_lyr_ret** - End year of tax code section 45Q subsidy for retrofits
* **tcs45q_i_45q_lyr_new** - End year of tax code section 45Q subsidy for new builds
* **ccatsdat_co2_elec** - Electricity consumed by CO\ :sub:`2` transport.
* **ccatsdat_co2_prc_dis_45q** - CO\ :sub:`2` for 45Q eligible flow at census district level.
* **ccatsdat_co2_prc_dis_ntc** - CO\ :sub:`2` for 45Q ineligible flow at census district level.
* **ccatsdat_co2_prc_reg_45q** - CO\ :sub:`2` for 45Q eligible flow at census region level.
* **ccatsdat_co2_prc_reg_ntc** - CO\ :sub:`2` for 45Q ineligible flow at census region level.
* **ccatsdat_co2_sup_out** - Total CO\ :sub:`2` supplied to CCATS, by census division.
* **ccatsdat_co2_seq_out** - Total CO\ :sub:`2` sequestered by CCATS, by census division.
* **ccatsdat_co2_sup_out_r** - Total CO\ :sub:`2` supplied to CCATS, by census region.
* **ccatsdat_co2_seq_out_r** - Total CO\ :sub:`2` sequestered by CCATS, by census region.

"""

import pandas as pd
import numpy as np
import submodule_ccats as subccats


class Output(subccats.Submodule):
    """Output Submodule for CCATS.

    """


    def __init__(self, parent):
        """Initializes Output object.

        Parameters
        ----------

        parent : str
            Module.Module (Pointer to parent module)

            
        Returns
        -------
        
        None

        """
        
        ### I/O Setup
        self.parent = parent  #: module.Module head module

    def setup(self):
        """Setup Output Submodule for CCATS.

        Parameters
        ----------
        
        None

        
        Returns
        -------

        None
        """
        ### Localize parent variables
        self.logger = self.parent.logger

        pass


    def run(self):
        """Run Output Submodule for CCATS.

        Parameters
        ----------

        None


        Returns
        -------
        None
        """
        if self.parent.integrated_switch == 1:
            self.logger.info('Write Local Variables to Pickle')
            self.write_pkl()

        if ((self.parent.integrated_switch == 1) & (self.parent.price_reset_switch == True) & (self.parent.cycle_current == 1)) | \
            ((self.parent.integrated_switch == 0) & (self.parent.price_reset_switch == True)):
            self.logger.info('Reset CO2 Prices in NEMS')
            self.reset_prices()

        self.logger.info('Write Local Restart Variables to Global Restart Variables')
        self.write_local_restart_vars()

        pass


    ### Write PKL Variables
    def write_pkl(self):
        '''Write local variables to pickle

        * Because CCATS is in Python and Main is in Fortran, Python must read/write local variables between model years
        * This is done using Pickle
        * Different pickle outputs are written depending on whether fcrl = 0, fcrl = 1, or ncrl=1 (is this a reporting loop)
        * If fcrl=0, do not write pickle outputs
        * If fcrl=1 and ncrl=0, write fcrl pickle outputs
        * If ncrl=1, write ncrl pickle outputs

        Parameters
        ----------

        None


        Returns
        -------
        
        None

        '''
        if (self.parent.param_fcrl == 1) & (self.parent.param_ncrl != 1):  # Last regular integration loop
            print('Start Writing CCATS Intermediate Variables to FCRL PKL')
            self.parent.pkl.write_pkl_variables('fcrl', self.parent.year_current, self.parent.ccats_var_output_path)
            print('End Writing CCATS Intermediate Variables to FCRL PKL')
        elif self.parent.param_ncrl == 1:  # Reporting Loop
            print('Start Writing CCATS Local Variables to NCRL PKL')
            self.parent.pkl.write_pkl_variables('ncrl', self.parent.year_current, self.parent.ccats_var_output_path)
            print('End Writing CCATS Local Variables to NCRL PKL')
        else:
            print('No PKL produced in first iterations of history year')
            pass

        pass


    def reset_prices(self):
        '''Resets prices 

        Parameters
        ----------
        
        None

        
        Returns
        -------
        
        self.parent.ccatsdat_co2_prc_dis_45q : DataFrame
            CO\ :sub:`2` for 45Q eligible flow at census district level.

        self.parent.ccatsdat_co2_prc_reg_45q : DataFrame
            CO\ :sub:`2` for 45Q eligible flow at census region level.

        self.parent.ccatsdat_co2_prc_dis_ntc : DataFrame
            CO\ :sub:`2` for 45Q ineligible flow at census district level.

        self.parent.ccatsdat_co2_prc_reg_ntc : DataFrame
            CO\ :sub:`2` for 45Q ineligible flow at census region level.

        '''
        self.parent.rest_co2_prc_dis_45q['value'] = self.parent.price_reset_value_45q
        self.parent.rest_co2_prc_reg_45q['value'] = self.parent.price_reset_value_45q

        self.parent.rest_co2_prc_dis_ntc['value'] = self.parent.price_reset_value_ntc
        self.parent.rest_co2_prc_reg_ntc['value'] = self.parent.price_reset_value_ntc

        pass


    def write_local_restart_vars(self):
        '''Write local restart variables to global restart variables.

        Parameters
        ----------

        None


        Returns
        -------
        
        self.parent.restart.tcs45q_ccs_eor_45q : DataFrame 
            CO\ :sub:`2` sequestered in EOR from legacy common block.

        self.parent.restart.tcs45q_ccs_saline_45q : DataFrame 
            CO\ :sub:`2` sequestered in saline storage from legacy common block.

        self.parent.restart.tcs45q_i_45q_duration : int
            Tax code section 45Q subsidy duration

        self.parent.restart.tcs45q_i_45q_syr : int
            Start year of tax code section 45Q subsidy

        self.parent.restart.tcs45q_i_45q_lyr_ret : int
            End year of tax code section 45Q subsidy for retrofits

        self.parent.restart.tcs45q_i_45q_lyr_new : int
            End year of tax code section 45Q subsidy for new builds

        self.parent.restart.ccatsdat_co2_elec : DataFrame
            Electricity consumed by CO\ :sub:`2` transport.

        self.parent.restart.ccatsdat_co2_prc_dis_45q : DataFrame
            CO\ :sub:`2` for 45Q eligible flow at census district level.

        self.parent.restart.ccatsdat_co2_prc_dis_ntc : DataFrame
            CO\ :sub:`2` for 45Q ineligible flow at census district level.

        self.parent.restart.ccatsdat_co2_prc_reg_45q : DataFrame
            CO\ :sub:`2` for 45Q eligible flow at census region level.

        self.parent.restart.ccatsdat_co2_prc_reg_ntc : DataFrame
            CO\ :sub:`2` for 45Q ineligible flow at census region level.

        self.parent.restart.ccatsdat_co2_sup_out : DataFrame
            Total CO\ :sub:`2` supplied to CCATS, by census division.

        self.parent.restart.ccatsdat_co2_seq_out : DataFrame
            Total CO\ :sub:`2` sequestered by CCATS, by census division.

        self.parent.restart.ccatsdat_co2_sup_out_r : DataFrame
            Total CO\ :sub:`2` supplied to CCATS, by census region.

        self.parent.restart.ccatsdat_co2_seq_out_r : DataFrame
            Total CO\ :sub:`2` sequestered by CCATS, by census region.
        '''
        # 45Q values
        temp_eor_45q = self.parent.rest_ccs_eor_45q.copy()
        temp_eor_45q.index = self.parent.restart.tcs45q_ccs_eor_45q.index
        self.parent.restart.tcs45q_ccs_eor_45q.update(temp_eor_45q)

        temp_saline_45q = self.parent.rest_ccs_saline_45q.copy()
        temp_saline_45q.index = self.parent.restart.tcs45q_ccs_saline_45q.index
        self.parent.restart.tcs45q_ccs_saline_45q.update(temp_eor_45q)

        # 45Q accounting
        self.parent.restart.tcs45q_i_45q_duration = self.parent.rest_i_45q_duration
        self.parent.restart.tcs45q_i_45q_syr = self.parent.rest_i_45q_syr
        self.parent.restart.tcs45q_i_45q_lyr_ret = self.parent.rest_i_45q_lyr_ret
        self.parent.restart.tcs45q_i_45q_lyr_new = self.parent.rest_i_45q_lyr_new

        # Electricity
        self.parent.restart.ccatsdat_co2_elec.update(self.parent.rest_co2_elec)

        # Prices
        self.parent.restart.ccatsdat_co2_prc_dis_45q.update(self.parent.rest_co2_prc_dis_45q)
        self.parent.restart.ccatsdat_co2_prc_dis_ntc.update(self.parent.rest_co2_prc_dis_ntc)
        self.parent.restart.ccatsdat_co2_prc_reg_45q.update(self.parent.rest_co2_prc_reg_45q)
        self.parent.restart.ccatsdat_co2_prc_reg_ntc.update(self.parent.rest_co2_prc_reg_ntc)

        # Reporting
        self.parent.restart.ccatsdat_co2_sup_out.update(self.parent.rest_co2_sup_out)
        self.parent.restart.ccatsdat_co2_seq_out.update(self.parent.rest_co2_seq_out)
        self.parent.restart.ccatsdat_co2_sup_out_r.update(self.parent.rest_co2_sup_out_r)
        self.parent.restart.ccatsdat_co2_seq_out_r.update(self.parent.rest_co2_seq_out_r)

        pass
