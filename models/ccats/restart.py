"""Class for handling the Restart File in CCATS.

Restart: Summary
----------------
The :ref:`restart` module reads in restart variables from the restart file, stores these variables in a class dictionary, and writes these CCATS
output variables back to the restart file after CCATS processes have been run. The restart file is read from, and written to, using **self.parent.pyfiler1.pyd**,
which is maintained by the Integration team. The module operates as follows:

    1. :meth:`~restart.Restart.run` is called from the :ref:`module` parent class to read in the restart file.

    2. :meth:`~restart.Restart.run` calls the *read_filer* method from **pyfiler1**.
       This method loads all the NEMS restart variables into a class dictionary. Using the "output" argument of *read_filer*
       a second "output" dictionary is also instantiated, indicating which variables need to be output back to the restart file at the end of each CCATS iteration.

    3. CCATS restart variables are instantiated, read into appropriately indexed dataframes, and offset by
       calendar years in the relevant indices. Once this is done, CCATS operations move back to :ref:`module`.

    5. Restart variables are updated in the various CCATS modules.

    6. In the :func:`ccats.run_ccats` function of :ref:`ccats` the :meth:`~restart.Restart.write_results` function in :ref:`restart` is called.

    7. Each instantiated output variable is called from the :ref:`restart` class dictionary, re-sized to match restart file formatting,
       and then written to the appropriate restart variable in the restart file using the *write_filer* method of **pyfiler1**.


Restart: Model Functions and Class Methods
__________________________________________

* :meth:`~restart.Restart.__init__` - Constructor to initialize Restart submodule (instantiated by :meth:`module.Module.setup` in :ref:`module`).
* :meth:`~restart.Restart.run` - Calls the read_filer method from self.parent.pyfiler1 and loads the restart file into CCATS (called by :meth:`module.Module.setup`).
* :meth:`~restart.Restart.add_int` - Function for adding an integer from the restart file into CCATS as an integer (called by :meth:`~restart.Restart.__init__`).
* :meth:`~restart.Restart.add_df` - Function for adding a dataframe from the restart file into NEMS (called by :meth:`~restart.Restart.__init__`).
* :meth:`~restart.Restart.write_results` - Repacks local restart file variables into multi-dimensional arrays, and writes to restart file (called by :func:`ccats.run_ccats`).
* :meth:`~restart.Restart.dump_restart` - Dumps restart file to .xlsx debug file (called by :meth:`~restart.Restart.write_results`).


Restart: Output Debug Files
___________________________
**rest_all_<cycle>.xlsx** - Debug of CCATS output restart variables, where <cycle> is the current NEMS cycle.


Restart: Code
_____________
"""
import os
import pandas as pd
import numpy as np
import sys
sys.path.append(r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler")
os.add_dll_directory(r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler")

from ccats_common import common as com


class Restart:
    """Class for handling the Restart File in CCATS

    """
    def __init__(self, parent):
        """Initializes Restart object.        
        
        Parameters
        ----------
        parent : str
            Module.Module (Pointer to parent module)

        Returns
        -------
        None
        
        """
        ### I/O setup
        self.parent                 = parent  #: module.Module head module
        self.output_path            = ''  #: output path
        self.df_dict_in             = {}  #:
        self.df_dict_out            = {}  #:
        self.int_dict_in            = {}  #:
        self.int_dict_out           = {}  #:


        ### Parameters
        self.parametr_ncrl          = self.add_int('parametr_ncrl'         , self.parent.pyfiler1.utils.ncrl            , output=False)  # ! Parameter for NCRL loop (NCRL = 1 == NCRL loop)
        self.parametr_fcrl          = self.add_int('parametr_fcrl'         , self.parent.pyfiler1.utils.fcrl            , output=False)  # ! Parameter for FCRL loop (FCRL = 1 == FCRL loop)
        self.parametr_baseyr        = self.add_int('parametr_baseyr'       , self.parent.pyfiler1.other.baseyr          , output=False)  # ! Parameter for NEMS base year


        ### Input
        # General
        self.ncntrl_curcalyr        = self.add_int('ncntrl_curcalyr'       , self.parent.pyfiler1.ncntrl.curcalyr      , output=False) # CURRENT CALENDAR YEAR
        self.ncntrl_curitr          = self.add_int('ncntrl_curitr'         , self.parent.pyfiler1.ncntrl.curitr        , output=False) # CURRENT ITERATION
        self.ncntrl_curiyr          = self.add_int('ncntrl_curiyr'         , self.parent.pyfiler1.ncntrl.curiyr        , output=False) # CURRENT YEAR INDEX
        self.ncntrl_lastyr          = self.add_int('ncntrl_lastyr'         , self.parent.pyfiler1.ncntrl.lastyr        , output=False) # LAST FORECAST YEAR INDEX  (EG. 29)

        # Macro
        self.macout_mc_jpgdp        = self.add_df('macout_mc_jpgdp'        , self.parent.pyfiler1.macout.mc_jpgdp      , output=False, offset=[(0, self.parametr_baseyr - 3)]) # Chained price index-gross domestic product, 1987 not 1990
        self.macout_mc_rmcorpbaa    = self.add_df('macout_mc_rmcorpbaa'    , self.parent.pyfiler1.macout.mc_rmcorpbaa  , output=False, offset=[(0, self.parametr_baseyr)]) # Rate on industrial BAA bonds
        self.macout_mc_rmtcm10y     = self.add_df('macout_mc_rmtcm10y'     , self.parent.pyfiler1.macout.mc_rmtcm10y   , output=False, offset=[(0, self.parametr_baseyr)]) # 10 year Treasury note yield

        # Electricity_costs
        self.mpblk_pelin            = self.add_df('mpblk_pelin', self.parent.pyfiler1.mpblk.pelin, output = False, offset= [(0, 1), (1, self.parametr_baseyr)]) # Purchased Electricity Price - Industrial

        # Electric Powerplant Retrofits
        self.udatout_ucapisn        = self.add_df('udatout_ucapisn', self.parent.pyfiler1.udatout.ucapisn, output = False, offset= [(0, 1), (1, self.parametr_baseyr)]) # ELECTR NonUtil Advanced Coal Steam W/Seq Capacity by EMM region
        self.udatout_ucapsqn        = self.add_df('udatout_ucapsqn', self.parent.pyfiler1.udatout.ucapsqn, output=False, offset=[(0, 1), (1, self.parametr_baseyr)]) # ELECTR NonUtil Coal Steam Capacity (existing) w/Seq by EMM region
        self.udatout_ucappqn        = self.add_df('udatout_ucappqn', self.parent.pyfiler1.udatout.ucappqn, output=False, offset=[(0, 1), (1, self.parametr_baseyr)]) # ELECTR NonUtil IGCC w/PartSeq Capacity by NERC

        self.udatout_ucapasn        = self.add_df('udatout_ucapasn', self.parent.pyfiler1.udatout.ucapasn, output = False, offset= [(0, 1), (1, self.parametr_baseyr)]) # ELECTR NonUtil Advanced Combined Cycle W/Seq Capacity by EMM region
        self.udatout_ucapa2n        = self.add_df('udatout_ucapa2n', self.parent.pyfiler1.udatout.ucapa2n, output = False, offset= [(0, 1), (1, self.parametr_baseyr)]) # ELECTR NonUtil New Adv CC Capacity by NERC

        # OGSMOUT PLAYMAP
        self.ogsmout_play_map       = self.add_df('ogsmout_play_map'     , self.parent.pyfiler1.ogsmout.play_map   , output=False) # Map of Geological Formations (Plays) to CCATS

        # Supply
        self.ccatsdat_cst_ngp_inv  = self.add_df('ccatsdat_cst_ngp_inv'    , self.parent.pyfiler1.ccatsdat.cst_ngp_inv , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # Investment cost for CO2 capture from natural gas processing facilities
        self.ccatsdat_cst_ngp_om   = self.add_df('ccatsdat_cst_ngp_om'     , self.parent.pyfiler1.ccatsdat.cst_ngp_om  , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # O&M cost for CO2 capture from natural gas processing facilities
        self.ccatsdat_sup_ngp_45q  = self.add_df('ccatsdat_sup_ngp_45q'    , self.parent.pyfiler1.ccatsdat.sup_ngp_45q , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from natural gas processing facilities that are 45Q eligible
        self.ccatsdat_sup_ngp_ntc  = self.add_df('ccatsdat_sup_ngp_ntc'    , self.parent.pyfiler1.ccatsdat.sup_ngp_ntc , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from natural gas processing facilities, no tax credit
        self.ccatsdat_cst_emm_inv  = self.add_df('ccatsdat_cst_emm_inv'    , self.parent.pyfiler1.ccatsdat.cst_emm_inv , output = False, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # Investment costs for CO2 capture from power plants
        self.ccatsdat_cst_emm_om   = self.add_df('ccatsdat_cst_emm_om'     , self.parent.pyfiler1.ccatsdat.cst_emm_om  , output = False, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # O&M cost for CO2 capture from power plants
        self.ccatsdat_sup_emm_45q  = self.add_df('ccatsdat_sup_emm_45q'    , self.parent.pyfiler1.ccatsdat.sup_emm_45q , output = False, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # CO2 volumes from power plants that are 45Q eligible
        self.ccatsdat_sup_emm_ntc  = self.add_df('ccatsdat_sup_emm_ntc'    , self.parent.pyfiler1.ccatsdat.sup_emm_ntc , output = False, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # CO2 volumes from power plants, no tax credit
        self.ccatsdat_cst_cmt_inv  = self.add_df('ccatsdat_cst_cmt_inv'    , self.parent.pyfiler1.ccatsdat.cst_cmt_inv , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # Investment cost for CO2 capture from cement plants
        self.ccatsdat_cst_cmt_om   = self.add_df('ccatsdat_cst_cmt_om'     , self.parent.pyfiler1.ccatsdat.cst_cmt_om  , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # O&M cost for CO2 capture from cement plants
        self.ccatsdat_sup_cmt_45q  = self.add_df('ccatsdat_sup_cmt_45q'    , self.parent.pyfiler1.ccatsdat.sup_cmt_45q , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from cement plants that are 45Q eligible
        self.ccatsdat_sup_cmt_ntc  = self.add_df('ccatsdat_sup_cmt_ntc'    , self.parent.pyfiler1.ccatsdat.sup_cmt_ntc , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from cement plants, no tax credit
        self.ccatsdat_cst_eth_inv  = self.add_df('ccatsdat_cst_eth_inv'    , self.parent.pyfiler1.ccatsdat.cst_eth_inv , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # Investment cost for carbon capture from ethanol production
        self.ccatsdat_cst_eth_om   = self.add_df('ccatsdat_cst_eth_om'     , self.parent.pyfiler1.ccatsdat.cst_eth_om  , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # O&M cost for carbon capture from ethanol production
        self.ccatsdat_sup_eth_45q  = self.add_df('ccatsdat_sup_eth_45q'    , self.parent.pyfiler1.ccatsdat.sup_eth_45q , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from ethanol production that are 45Q eligible
        self.ccatsdat_sup_eth_ntc  = self.add_df('ccatsdat_sup_eth_ntc'    , self.parent.pyfiler1.ccatsdat.sup_eth_ntc , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from ethanol production, no tax credit
        self.ccatsdat_cst_h2_inv   = self.add_df('ccatsdat_cst_h2_inv'     , self.parent.pyfiler1.ccatsdat.cst_h2_inv  , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # Investment cost for carbon capture from hydrogen production
        self.ccatsdat_cst_h2_om    = self.add_df('ccatsdat_cst_h2_om'      , self.parent.pyfiler1.ccatsdat.cst_h2_om   , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # O&M cost for carbon capture from hydrogen production
        self.ccatsdat_sup_h2_45q   = self.add_df('ccatsdat_sup_h2_45q'     , self.parent.pyfiler1.ccatsdat.sup_h2_45q  , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from hydrogen production that are 45Q eligible
        self.ccatsdat_sup_h2_ntc   = self.add_df('ccatsdat_sup_h2_ntc'     , self.parent.pyfiler1.ccatsdat.sup_h2_ntc  , output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 volumes from hydrogen production, no tax credit

        # Demand
        self.ccatsdat_dem_eor  = self.add_df('ccatsdat_dem_eor', self.parent.pyfiler1.ccatsdat.dem_eor, output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 demand from EOR volumes
        self.ccatsdat_cst_eor  = self.add_df('ccatsdat_cst_eor', self.parent.pyfiler1.ccatsdat.cst_eor, output = False, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 price CO2 EOR projects are willing to offer for CO2


        ### Output
        # 45Q tax credits
        self.tcs45q_ccs_eor_45q     = self.add_df('tcs45q_ccs_eor_45q'     , self.parent.pyfiler1.tcs45q.ccs_eor_45q   , output=True, offset=[(0, self.parametr_baseyr)]) # 45Q tax credit for enhanced oil recovery
        self.tcs45q_ccs_saline_45q  = self.add_df('tcs45q_ccs_saline_45q'  , self.parent.pyfiler1.tcs45q.ccs_saline_45q, output=True, offset=[(0, self.parametr_baseyr)]) # 45Q tax credit for saline injection
        self.tcs45q_i_45q_duration  = self.add_int('tcs45q_i_45q_duration' , self.parent.pyfiler1.tcs45q.i_45q_duration, output=True) # Start year of tax code section 45Q subsidy
        self.tcs45q_i_45q_syr       = self.add_int('tcs45q_i_45q_syr'      , self.parent.pyfiler1.tcs45q.i_45q_syr     , output=True) # Start year of tax code section 45Q subsidy
        self.tcs45q_i_45q_lyr_ret   = self.add_int('tcs45q_i_45q_lyr_ret'  , self.parent.pyfiler1.tcs45q.i_45q_lyr_ret , output=True) # End year of tax code section 45Q subsidy for retrofits
        self.tcs45q_i_45q_lyr_new   = self.add_int('tcs45q_i_45q_lyr_new'  , self.parent.pyfiler1.tcs45q.i_45q_lyr_new , output=True) # End year of tax code section 45Q subsidy for new builds

        # Prices
        self.ccatsdat_co2_prc_dis_45q  = self.add_df('ccatsdat_co2_prc_dis_45q'     , self.parent.pyfiler1.ccatsdat.co2_prc_dis_45q  , output = True, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 price output after optimization, 45Q eligible CO2, Census Division
        self.ccatsdat_co2_prc_dis_ntc  = self.add_df('ccatsdat_co2_prc_dis_ntc'     , self.parent.pyfiler1.ccatsdat.co2_prc_dis_ntc  , output = True, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 price output after optimization, no tax credit, Census Division
        self.ccatsdat_co2_prc_reg_45q  = self.add_df('ccatsdat_co2_prc_reg_45q'     , self.parent.pyfiler1.ccatsdat.co2_prc_reg_45q  , output = True, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 price output after optimization, 45Q eligible CO2, Census Region
        self.ccatsdat_co2_prc_reg_ntc  = self.add_df('ccatsdat_co2_prc_reg_ntc'     , self.parent.pyfiler1.ccatsdat.co2_prc_reg_ntc  , output = True, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 price output after optimization, no tax credit, Census Region

        # Electricity
        self.ccatsdat_co2_elec = self.add_df('ccatsdat_co2_elec'     , self.parent.pyfiler1.ccatsdat.co2_elec, output = True, offset = [(0, 1), (1, self.parametr_baseyr)]) # CO2 electricity consumption

        # Reporting
        self.ccatsdat_co2_sup_out      = self.add_df('ccatsdat_co2_sup_out'   , self.parent.pyfiler1.ccatsdat.co2_sup_out  , output = True, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # CO2 supply output after optimization by census division
        self.ccatsdat_co2_seq_out      = self.add_df('ccatsdat_co2_seq_out'   , self.parent.pyfiler1.ccatsdat.co2_seq_out  , output = True, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # CO2 demand output after optimization by census division
        self.ccatsdat_co2_sup_out_r    = self.add_df('ccatsdat_co2_sup_out_r' , self.parent.pyfiler1.ccatsdat.co2_sup_out_r, output = True, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # CO2 supply output after optimization by census region
        self.ccatsdat_co2_seq_out_r    = self.add_df('ccatsdat_co2_seq_out_r' , self.parent.pyfiler1.ccatsdat.co2_seq_out_r, output = True, offset = [(0, 1), (1, 1), (2, self.parametr_baseyr)]) # CO2 demand output after optimization by census region

        pass


    def run(self, temp_filename, temp_rest):
        '''Calls the read_filer method from **pyfiler1** and loads the restart file into CCATS.

            * Determines if CCATS is running integrated in NEMS or standalone.
                * If integrated, pass, since pyfiler1 is imported from **main.py** containing the complete restart file.
                * If standalone, read in the restart file.
            * Then instantiate all the restart variables and fill the :ref:`restart` class dictionary with the CCATS restart variable keys.

        Parameters
        ----------
        temp_filename : str
            Restart filename.

        Returns
        -------
        self.__dict__ : dictionary
            Class dictionary of CCATS restart variables read in from the restart file.
        
        '''

        if self.parent.integrated_switch == True:
            pass
        else:
            if os.path.exists(temp_rest):
                self.parent.pyfiler1.utils.read_filer(temp_filename)
            else:
                restartunf = open(temp_rest, "w")
                restartunf.write('')
                restartunf.close()
                self.parent.pyfiler1.utils.read_filer(temp_filename)

        self.parent.logger.info('running restart submodule')
        for key, value in self.int_dict_in.items():
            self.__dict__[key] = int(value)
        for key, value in self.df_dict_in.items():
            # Sorting and (effectively) reindexing DataFrame being updated
            df2 = self.__dict__[key].sort_index()
            df1 = com.array_to_df(value)
            temp_index = df2.index.copy()
            df2.index = df1.index
            df2.update(df1)
            df2.index = temp_index
            self.__dict__[key] = df2
        pass


    def add_int(self, dict_name, restart_ref, output):
        '''Function for reading an integer from the restart file into CCATS as an integer.

        Parameters
        ----------
        dict_name : str
            Name of restart variable or parameter in **pyfiler1**.

        restart_ref : int
            Integer data value from **pyfiler1**.

        output : bool
            Boolean of whether the restart variable is an output value of CCATS.

        Returns
        -------
        restart_ref : int
            Integer value from the restart file.
        
        '''
        self.int_dict_in[dict_name] = restart_ref
        if output:
            self.int_dict_out[dict_name] = restart_ref
        # Return com.array_to_df(restart_ref)
        return int(restart_ref)


    def add_df(self, dict_name, restart_ref, output, offset=None):
        '''Function for reading an array from the restart file into CCATS as a DataFrame.

        Parameters
        ----------
        dict_name : str
            Name of restart variable or parameter in **pyfiler1**.

        restart_ref : int
            Integer data value from **pyfiler1**.

        output : bool
            Boolean of whether the restart variable is an output value of CCATS.

        offset : list
            List indicating output DataFrame index value offset.

        Returns
        -------
        df : DataFrame
            Restart variable DataFrame.
        
        '''
        if offset is None:
            offset = []
        self.df_dict_in[dict_name] = restart_ref
        if output:
            self.df_dict_out[dict_name] = restart_ref
        df = com.array_to_df(restart_ref)
        for level, off_value in offset:
            df.index = df.index.set_levels(df.index.levels[level] +
                                           off_value,
                                           level=level,
                                           verify_integrity=False)
        return df


    def write_results(self, temp_filename):
        """Repacks local restart file variables into multi-dimensional arrays, and writes these to the restart file.

            * Loop through the class dictionary based on keys in *int_dict_out* and *df_dict_out*,
            * Split out each of the dictionary keys into different variables,
            * Use "getattr" to produce the output array index,
            * Set the output array index with "setattr", and
            * These arrays are returned to **main.py** via **pyfiler1**.

        Parameters
        ----------
        temp_filename : str
            Restart File name.

        Returns
        -------
        None

        """
        # Convert ints/dfs to arrays
        for key, value in self.int_dict_out.items():
            cblock = key.split('_')[0]
            var = key.split('_')[1]
            nested_method = getattr(self.parent.pyfiler1, cblock, None)
            setattr(nested_method, var, value)
        for key, value in self.df_dict_out.items():
            value[:] = com.df_to_array(self.__dict__[key])
            cblock = key.split('_')[0]
            var = key.split('_')[1]
            nested_method = getattr(self.parent.pyfiler1, cblock, None)
            setattr(nested_method, var, value)

        # Restart File Debug Output
        if self.parent.debug_restart_itr_switch == True:
            self.parent.logger.info('Debug Restart File Start')
            self.dump_restart()
            self.parent.logger.info('Debug Restart File End')
        elif (self.parent.year_current == self.parent.year_final):# & (self.parametr_ncrl == 1):
            self.parent.logger.info('Debug Restart File Start')
            self.dump_restart()
            self.parent.logger.info('Debug Restart File End')
        else:
            pass

        if self.parent.output_restart_unf_switch == True:
            self.parent.pyfiler1.utils.write_filer(temp_filename)
        else:
            pass


    def dump_restart(self):
        """Dumps restart file to .xlsx debug file.

        Returns
        -------
        None

        """
        self.output_path = self.parent.output_path
        writer = pd.ExcelWriter(self.output_path + 'restart_file_debug\\' + 'ccats_rest_all_' + str(self.parent.cycle_current) + '.xlsx')


        ### Outputs
        self.ccatsdat_cst_ngp_inv.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_ngp_inv')
        self.ccatsdat_cst_ngp_om.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_ngp_om')
        self.ccatsdat_sup_ngp_45q.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_ngp_45q')
        self.ccatsdat_sup_ngp_ntc.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_ngp_ntc')
        self.ccatsdat_cst_emm_inv.unstack(2).to_excel(  writer, sheet_name='ccatsdat_cst_emm_inv')
        self.ccatsdat_cst_emm_om.unstack(2).to_excel(  writer, sheet_name='ccatsdat_cst_emm_om')
        self.ccatsdat_sup_emm_45q.unstack(2).to_excel(  writer, sheet_name='ccatsdat_sup_emm_45q')
        self.ccatsdat_sup_emm_ntc.unstack(2).to_excel(  writer, sheet_name='ccatsdat_sup_emm_ntc')
        self.ccatsdat_cst_cmt_inv.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_cmt_inv')
        self.ccatsdat_cst_cmt_om.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_cmt_om')
        self.ccatsdat_sup_cmt_45q.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_cmt_45q')
        self.ccatsdat_sup_cmt_ntc.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_cmt_ntc')
        self.ccatsdat_cst_eth_inv.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_eth_inv')
        self.ccatsdat_cst_eth_om.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_eth_om')
        self.ccatsdat_sup_eth_45q.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_eth_45q')
        self.ccatsdat_sup_eth_ntc.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_eth_ntc')
        self.ccatsdat_cst_h2_inv.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_h2_inv')
        self.ccatsdat_cst_h2_om.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_h2_om')
        self.ccatsdat_sup_h2_45q.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_h2_45q')
        self.ccatsdat_sup_h2_ntc.unstack(1).to_excel(  writer, sheet_name='ccatsdat_sup_h2_ntc')
        self.ccatsdat_dem_eor.unstack(1).to_excel(  writer, sheet_name='ccatsdat_dem_eor')
        self.ccatsdat_cst_eor.unstack(1).to_excel(  writer, sheet_name='ccatsdat_cst_eor')

        self.ccatsdat_co2_prc_dis_45q.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_prc_dis_45q')
        self.ccatsdat_co2_prc_dis_ntc.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_prc_dis_ntc')
        self.ccatsdat_co2_prc_reg_45q.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_prc_reg_45q')
        self.ccatsdat_co2_prc_reg_ntc.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_prc_reg_ntc')

        self.ccatsdat_co2_sup_out.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_sup_out')
        self.ccatsdat_co2_seq_out.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_seq_out')
        self.ccatsdat_co2_sup_out_r.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_sup_out_r')
        self.ccatsdat_co2_seq_out_r.unstack(1).to_excel(  writer, sheet_name='ccatsdat_co2_seq_out_r')

        # Close the Pandas Excel writer and output the .xlsx file
        writer.close()

        pass
