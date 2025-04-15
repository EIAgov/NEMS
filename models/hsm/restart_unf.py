"""Class for handling the Restart File in HSM.

Summary
-------
The **Restart** module reads in the variables from the restart file, stores these variables in a dictionary, and writes these HSM
output variables back to the restart file after HSM processes have been run. These processes are performed using **self.parent.pyfiler1.pyd**,
which is maintained by the Integration team. The module operates as follows:

    1. **restart_unf** is called from the module_unf parent class to read in the restart file.

    2. The *setup* function is called, which in turn calls run function. The run function calls the read_filer method in self.parent.pyfiler1
       and loads all the variables into a class dictionary from restart_HSMIN.unf. From this class dictionary HSM is able to identify
       which variables need to be written back out to the restart file at the end of each HSM run.

    3. Relevant restart variables are instantiated, read into appropriately indexed dataframes, and offset to the
       current calendar years in the relevant indices. Once this is done, HSM operations move back to **module_unf**.

    5. Restart variables are updated in the various HSM modules.

    6. In the *prepare_results* method of **module_unf** the *write_results* **restart_unf** function is called.

    7. Each instantiated variable is called from the **restart_unf** class dictionary, re-sized to match restart file formatting,
       and then written to the appropriate restart variable.

    8. A new restart file, restart_HSMOUT.unf, is produced and written to the model's parent directory, which is picked up by NEMS.


Input Files
-----------
    * restart_HSMIN.unf - Restart file from NEMS in UNF format


Model Functions and Class Methods
_________________________________

class (sub.Submodule) - Restart File submodule for HSM

    * __init__ - Constructor to initialize Restart submodule
    * setup - Setup function for reading the restart file into HSM
    * run - Calls the read_filer method from self.parent.pyfiler1 and loads the restart file into HSM
    * add_int - Function for adding an integer from the restart file into HSM as an integer
    * add_df - Function for adding a dataframe from the restart file into NEMS
    * write_results - Repacks local restart file variables into multi-dimensional arrays, and writes to restart file
    * dump_restart - Dumps restart file to .xlsx debug file


Output Restart Variables
________________________

**Crude Oil Restart Variables**
    * pmmout_rfqtdcrd - Total crude production by HSM region
    * pmmout_rfqdcrd - Total crude oil production by HSM region (not including EOR)
    * ogsmout_ogqcrrep - Crude oil production by oil category
    * ogsmout_ogcoprd - Crude oil production by lower 48 region
    * ogsmout_ogqshloil - Crude oil production by select tight oil play
    * ogsmout_ogoilprd - Crude oil production by oil type and HSM district
    * ogsmout_ogcrdprd - Crude oil production by HSM region and crude type
    * ogsmout_ogcruderef - Crude oil production by LFMM crude oil type and region
    * ogsmout_ogcrdheat - Heat rate by type of crude oil
    * ogsmout_ogeorprd - CO2 EOR crude oil production
    * ogsmout_ogcoprdgom - Crude oil production Gulf of Mexico
    * ogsmout_ogprcoak - Crude oil production by Alaska region


**Natural Gas Restart Variables**
    * ogsmout_ogenagprd - Natural gas expected production by natural gas type and HSM district
    * ogsmout_ogrnagprd - Natural gas realized production by natural gas type and HSM district
    * ogsmout_ogadgprd - Natural gas associated dissolved production by oil type and HSM district
    * ogsmout_ogprdad - Natural gas associated dissolved production by HSM region
    * ogsmout_ogqshlgas - Natural gas production by select natural gas play
    * ogsmout_ogqngrep - Natural gas production by natural gas type
    * ogsmout_ogprdugr - Lower 48 unconventional natural gas production
    * ogsmout_ogdngprd - Dry natural gas production by state/district and gas type
    * ogsmout_ogngprd - Natural Gas production by natural gas category
    * ogsmout_ogngprdgom - Natural gas production Gulf of Mexico
    * ogsmout_ogprdadof - Offshore AD natural gas production (BCF)
    * ogsmout_ogprdoff - Lower 48 offshore production split into Federal/State categories


**Crude Oil and Natural Gas Production Restart Variable**
    * ogsmout_ogregprd - Total crude oil and natural gas production by production type


**Crude Oil Price Restart Variables**
    * ogsmout_ogcowhp - Crude oil wellhead price by HSM region
    * ogsmout_ogpcrwhp -  Crude oil HSM average wellhead price


**Natural Gas Price Restart Variable**
    * ogsmout_ogngwhp - Natural gas wellhead price by HSM region
    * ogsmout_ogpngwhp - Average national wellhead price


**Well Restart Variables**
    * ogsmout_ogogwells - Total wells
    * ogsmout_ognowell - Total completed wells
    * ogsmout_ogwellsl48 - Total lower 48 wells
    * ogsmout_ogsrl48 - Lower 48 drilling success rates


**NGPL Production Restart Variables**
    * ogsmout_ogngplprd - NGPL production by HSM district
    * ogsmout_ogngplet - Ethane production by HSM district
    * ogsmout_ogngplpr - Propane production by HSM district
    * ogsmout_ogngplbu - Butane production by HSM district
    * ogsmout_ogngplis - Isobutane production by HSM district
    * ogsmout_ogngplpp - Pentanes production by HSM district
    * ogsmout_ognglak - Alaska ngpl production


**EOR Restart Variables**
    * ogsmout_ogco2rec - CO2 recycled by HSM region and CO2 type
    * ogsmout_ogco2inj - CO2 injected by HSM region and CO2 type
    * ogsmout_ogco2pur - CO2 purchased by HSM region and CO2 type
    * ogsmout_ogco2avl - CO2 available by HSM region and CO2 type
    * ogsmout_ogco2prc - CO2 price by HSM region and CO2 type
    * ogsmout_ns_start -
    * ogsmout_ogco245q -


**Canada Restart Variables**
    * ogsmout.cnenagprd - Expected NA gas production in Canada  (1=east, 2=west)
    * ogsmout.cnrnagprd - Realized NA gas production in Canada (1=east, 2=west)
    * ogsmout.cnadagprd - AD gas production in Canada (1=east, 2=west)

**NGP Restart Variables
    * qmore.qngpin - Natural Gas Processing Plant Electricity Consumption during Carbon Capture
    * ogsmout.ngpco2em - CO2 emissions from Natural Gas Processing Plants


**Technology Improvement Rate Restart Variable**
    * ogsmout_ogtechon - HSM technology improvement rate


**Miscellanous Restart Variables**
    * ogsmout_oggrowfac - HSM growth factor (deprecated)
    * ogsmout_ogjobs - Oil and gas industry jobs
    * ogsmout_ogprcexp - HSM price expectation adj (deprecated)


Output Debug Files
__________________
rest_all.xlsx - Debug of HSM output restart variables

Restart Submodule Class Methods
_______________________________
"""
import os
import pandas as pd
import numpy as np
import common as com
#import self.parent.pyfiler1
from hsm import logging

logger = logging.getLogger('module.py')


class Restart:
    def __init__(self, parent):
        self.parent                 = parent  #: module.Module head module
        self.output_path            = ''  #: output path
        self.df_dict_in             = {}  #:
        self.df_dict_out            = {}  #:
        self.int_dict_in            = {}  #:
        self.int_dict_out           = {}  #:

        # parameters
        self.parametr_ncrl          = self.add_int('parametr_ncrl'         , self.parent.pyfiler1.utils.ncrl            , output=False)  # ! parameter, not restart variable
        self.parametr_fcrl          = self.add_int('parametr_fcrl'         , self.parent.pyfiler1.utils.fcrl            , output=False)  # ! parameter, not restart variable
        self.parametr_baseyr        = self.add_int('parametr_baseyr'       , self.parent.pyfiler1.other.baseyr          , output=False)  # ! parameter, not restart variable
        self.parametr_gastypes      = self.add_int('parametr_gastypes'     , self.parent.pyfiler1.other.gastypes        , output=False)  # ! parameter, not restart variable
        self.parametr_mnumor        = self.add_int('parametr_mnumor'       , self.parent.pyfiler1.other.mnumor          , output=False)  # ! parameter, not restart variable
        self.parameter_mnumcr       = self.add_int('parametr_mnumcr'       , self.parent.pyfiler1.utils.mnumcr          , output=False)  # ! parameter, not restart variable
        self.parameter_mnumyr       = self.add_int('parametr_mnumyr'       , self.parent.pyfiler1.utils.mnumyr          , output=False)  # ! parameter, not restart variable
        self.parameter_ogdist       = self.add_int('parametr_ogdist'       , self.parent.pyfiler1.utils.ogdist          , output=False)  # ! parameter, not restart variable
        self.parameter_oiltypes     = self.add_int('parametr_oiltypes'     , self.parent.pyfiler1.utils.oiltypes        , output=False)  # ! parameter, not restart variable
        self.parameter_gastypes     = self.add_int('parametr_gastypes'     , self.parent.pyfiler1.utils.gastypes        , output=False)  # ! parameter, not restart variable
        self.parameter_mnl48n       = self.add_int('parametr_mnl48n'       , self.parent.pyfiler1.utils.mnl48n          , output=False)  # ! parameter, not restart variable
        self.parameter_mnl48f       = self.add_int('parametr_mnl48f'       , self.parent.pyfiler1.utils.mnl48f          , output=False)  # ! parameter, not restart variable
        self.parameter_mnogcro      = self.add_int('parametr_mnogcro'      , self.parent.pyfiler1.utils.mnogcro         , output=False)  # ! parameter, not restart variable
        self.parameter_mncrud       = self.add_int('parametr_mncrud'       , self.parent.pyfiler1.utils.mncrud          , output=False)  # ! parameter, not restart variable
        self.parameter_mnumor       = self.add_int('parametr_mnumor'       , self.parent.pyfiler1.utils.mnumor          , output=False)  # ! parameter, not restart variable
        self.parameter_mnumpr       = self.add_int('parametr_mnumpr'       , self.parent.pyfiler1.utils.mnumpr          , output=False)  # ! parameter, not restart variable

        # input
        self.convfact_cflgq         = self.add_df('convfact_cflgq'         , self.parent.pyfiler1.convfact.cflgq       , output=False, offset=[(0, self.parametr_baseyr)])
        #self.emission_emetax        = self.add_df('emission_emetax'        , self.parent.pyfiler1.emission.emetax      , output=False)
        self.emission_extrarisk     = self.add_df('emission_extrarisk'     , self.parent.pyfiler1.emission.extrarisk   , output=False, offset=[(0, self.parametr_baseyr)])
        self.intout_brent_price     = self.add_df('intout_brent_price'     , self.parent.pyfiler1.intout.brent_price   , output=False, offset=[(0, self.parametr_baseyr)])
        #self.intout_it_wop          = self.add_df('intout_it_wop'          , self.parent.pyfiler1.intout.it_wop        , output=False)
        #self.intout_report          = self.add_df('intout_report'          , self.parent.pyfiler1.intout.report        , output=False)
        self.intout_start_price     = self.add_df('intout_start_price'     , self.parent.pyfiler1.intout.start_price   , output=False)
        #self.intout_wti_price       = self.add_df('intout_wti_price'       , self.parent.pyfiler1.intout.wti_price     , output=False)
        self.lfmmout_rfcrudewhp     = self.add_df('lfmmout_rfcrudewhp'     , self.parent.pyfiler1.lfmmout.rfcrudewhp   , output=False, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.macout_mc_jpgdp        = self.add_df('macout_mc_jpgdp'        , self.parent.pyfiler1.macout.mc_jpgdp      , output=False, offset=[(0, self.parametr_baseyr - 3)])  # 1987 not 1990
        self.macout_mc_rmcorpbaa    = self.add_df('macout_mc_rmcorpbaa'    , self.parent.pyfiler1.macout.mc_rmcorpbaa  , output=False, offset=[(0, self.parametr_baseyr)])
        self.macout_mc_rmtcm10y     = self.add_df('macout_mc_rmtcm10y'     , self.parent.pyfiler1.macout.mc_rmtcm10y   , output=False, offset=[(0, self.parametr_baseyr)])
        #self.mxpblk_xogwprng        = self.add_df('mxpblk_xogwprng'        , self.parent.pyfiler1.mxpblk.xogwprng      , output=False)
        self.ncntrl_curcalyr        = self.add_int('ncntrl_curcalyr'       , self.parent.pyfiler1.ncntrl.curcalyr      , output=False)
        self.ncntrl_curitr          = self.add_int('ncntrl_curitr'         , self.parent.pyfiler1.ncntrl.curitr        , output=False)
        self.ncntrl_curiyr          = self.add_int('ncntrl_curiyr'         , self.parent.pyfiler1.ncntrl.curiyr        , output=False)  # ! shouldn't be needed for hsm
        #self.ncntrl_firsyr          = self.add_int('ncntrl_firsyr'         , self.parent.pyfiler1.ncntrl.firsyr        , output=False)
        #self.ncntrl_i4scnt          = self.add_int('ncntrl_i4scnt'         , self.parent.pyfiler1.ncntrl.i4scnt        , output=False)
        #self.ncntrl_ijumpcalyr      = self.add_int('ncntrl_ijumpcalyr'     , self.parent.pyfiler1.ncntrl.ijumpcalyr    , output=False)
        #self.ncntrl_ijumpyr         = self.add_int('ncntrl_ijumpyr'        , self.parent.pyfiler1.ncntrl.ijumpyr       , output=False)
        self.ncntrl_lastyr          = self.add_int('ncntrl_lastyr'         , self.parent.pyfiler1.ncntrl.lastyr        , output=False)
        #self.ncntrl_ncrl            = self.add_int('ncntrl_ncrl'           , self.parent.pyfiler1.ncntrl.ncrl          , output=False)
        #self.ncntrl_prtdbgl         = self.add_int('ncntrl_prtdbgl'        , self.parent.pyfiler1.ncntrl.prtdbgl       , output=False)
        #self.ncntrl_wwop            = self.add_int('ncntrl_wwop'           , self.parent.pyfiler1.ncntrl.wwop          , output=False)
        #self.ngtdmrep_ngexpvol      = self.add_df('ngtdmrep_ngexpvol'      , self.parent.pyfiler1.ngtdmrep.ngexpvol    , output=False)
        self.ngtdmrep_oghhprng      = self.add_df('ngtdmrep_oghhprng'      , self.parent.pyfiler1.ngtdmrep.oghhprng    , output=False, offset=[(0, self.parametr_baseyr)])
        self.ngtdmrep_ogprcng       = self.add_df('ngtdmrep_ogprcng'       , self.parent.pyfiler1.ngtdmrep.ogprcng     , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ngtdmrep_ogprdng       = self.add_df('ngtdmrep_ogprdng'       , self.parent.pyfiler1.ngtdmrep.ogprdng     , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        #self.ngtdmrep_ogprsup       = self.add_df('ngtdmrep_ogprsup'       , self.parent.pyfiler1.ngtdmrep.ogprsup     , output=False)
        self.ngtdmrep_ogwprng       = self.add_df('ngtdmrep_ogwprng'       , self.parent.pyfiler1.ngtdmrep.ogwprng     , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogcnpprd       = self.add_df('ogsmout_ogcnpprd'       , self.parent.pyfiler1.ogsmout.ogcnpprd     , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.pmore_plginpf          = self.add_df('pmore_plginpf'          , self.parent.pyfiler1.pmore.plginpf        , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        #self.qblk_qngas             = self.add_df('qblk_qngas'             , self.parent.pyfiler1.qblk.qngas           , output=False)
        self.tcs45q_ccs_eor_45q     = self.add_df('tcs45q_ccs_eor_45q'     , self.parent.pyfiler1.tcs45q.ccs_eor_45q   , output=False)
        self.tcs45q_i_45q_duration  = self.add_int('tcs45q_i_45q_duration' , self.parent.pyfiler1.tcs45q.i_45q_duration, output=False)
        self.tcs45q_i_45q_lyr_ret   = self.add_int('tcs45q_i_45q_lyr_ret'  , self.parent.pyfiler1.tcs45q.i_45q_lyr_ret , output=False)
        self.tcs45q_i_45q_syr       = self.add_int('tcs45q_i_45q_syr'      , self.parent.pyfiler1.tcs45q.i_45q_syr     , output=False)
        #self.uefdout_co2_ccs       = self.add_df('uefdout_co2_ccs'        , self.parent.pyfiler1.uefdout.co2_ccs      , output=False)  # ! Huge table
        self.mpblk_pelin            = self.add_df('mpblk_pelin'            , self.parent.pyfiler1.mpblk.pelin        , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])

        # outputs
        #self.cogen_cgogscap         = self.add_df('cogen_cgogscap'         , self.parent.pyfiler1.cogen.cgogscap       , output=True)
        #self.cogen_cgogsgen         = self.add_df('cogen_cgogsgen'         , self.parent.pyfiler1.cogen.cgogsgen       , output=True)
        #self.cogen_cgogsq           = self.add_df('cogen_cgogsq'           , self.parent.pyfiler1.cogen.cgogsq         , output=True)
        #OGSMOUT
        self.ogsmout_cnadgprd       = self.add_df('ogsmout_cnadgprd'       , self.parent.pyfiler1.ogsmout.cnadgprd     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_cnenagprd      = self.add_df('ogsmout_cnenagprd'      , self.parent.pyfiler1.ogsmout.cnenagprd    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ns_start       = self.add_df('ogsmout_ns_start'       , self.parent.pyfiler1.ogsmout.ns_start     , output=True)
        self.ogsmout_ogadgprd       = self.add_df('ogsmout_ogadgprd'       , self.parent.pyfiler1.ogsmout.ogadgprd     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogco245q       = self.add_df('ogsmout_ogco245q'       , self.parent.pyfiler1.ogsmout.ogco245q     , output=True)
        self.ogsmout_ogco2inj       = self.add_df('ogsmout_ogco2inj'       , self.parent.pyfiler1.ogsmout.ogco2inj     , output=True)
        self.ogsmout_ogco2rec       = self.add_df('ogsmout_ogco2rec'       , self.parent.pyfiler1.ogsmout.ogco2rec     , output=True)
        self.ogsmout_ogcoprd        = self.add_df('ogsmout_ogcoprd'        , self.parent.pyfiler1.ogsmout.ogcoprd      , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogcoprdgom     = self.add_df('ogsmout_ogcoprdgom'     , self.parent.pyfiler1.ogsmout.ogcoprdgom   , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogcowhp        = self.add_df('ogsmout_ogcowhp'        , self.parent.pyfiler1.ogsmout.ogcowhp      , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogcrdheat      = self.add_df('ogsmout_ogcrdheat'      , self.parent.pyfiler1.ogsmout.ogcrdheat    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogcrdprd       = self.add_df('ogsmout_ogcrdprd'       , self.parent.pyfiler1.ogsmout.ogcrdprd     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogcruderef     = self.add_df('ogsmout_ogcruderef'     , self.parent.pyfiler1.ogsmout.ogcruderef   , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogdngprd       = self.add_df('ogsmout_ogdngprd'       , self.parent.pyfiler1.ogsmout.ogdngprd     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogenagprd      = self.add_df('ogsmout_ogenagprd'      , self.parent.pyfiler1.ogsmout.ogenagprd    , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogeorprd       = self.add_df('ogsmout_ogeorprd'       , self.parent.pyfiler1.ogsmout.ogeorprd     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_oggrowfac      = self.add_df('ogsmout_oggrowfac'      , self.parent.pyfiler1.ogsmout.oggrowfac    , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ogjobs         = self.add_df('ogsmout_ogjobs'         , self.parent.pyfiler1.ogsmout.ogjobs       , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ognglak        = self.add_df('ogsmout_ognglak'        , self.parent.pyfiler1.ogsmout.ognglak      , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ogngplbu       = self.add_df('ogsmout_ogngplbu'       , self.parent.pyfiler1.ogsmout.ogngplbu     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngplet       = self.add_df('ogsmout_ogngplet'       , self.parent.pyfiler1.ogsmout.ogngplet     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngplis       = self.add_df('ogsmout_ogngplis'       , self.parent.pyfiler1.ogsmout.ogngplis     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngplpp       = self.add_df('ogsmout_ogngplpp'       , self.parent.pyfiler1.ogsmout.ogngplpp     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngplpr       = self.add_df('ogsmout_ogngplpr'       , self.parent.pyfiler1.ogsmout.ogngplpr     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngplprd      = self.add_df('ogsmout_ogngplprd'      , self.parent.pyfiler1.ogsmout.ogngplprd    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngprd        = self.add_df('ogsmout_ogngprd'        , self.parent.pyfiler1.ogsmout.ogngprd      , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngprdgom     = self.add_df('ogsmout_ogngprdgom'     , self.parent.pyfiler1.ogsmout.ogngprdgom   , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngwhp        = self.add_df('ogsmout_ogngwhp'        , self.parent.pyfiler1.ogsmout.ogngwhp      , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ognowell       = self.add_df('ogsmout_ognowell'       , self.parent.pyfiler1.ogsmout.ognowell     , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ogogwells      = self.add_df('ogsmout_ogogwells'      , self.parent.pyfiler1.ogsmout.ogogwells    , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogoilprd       = self.add_df('ogsmout_ogoilprd'       , self.parent.pyfiler1.ogsmout.ogoilprd     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogpcrwhp       = self.add_df('ogsmout_ogpcrwhp'       , self.parent.pyfiler1.ogsmout.ogpcrwhp     , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ogpngwhp       = self.add_df('ogsmout_ogpngwhp'       , self.parent.pyfiler1.ogsmout.ogpngwhp     , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ogprcexp       = self.add_df('ogsmout_ogprcexp'       , self.parent.pyfiler1.ogsmout.ogprcexp     , output=True, offset=[(0, self.parametr_baseyr)])
        self.ogsmout_ogprcoak       = self.add_df('ogsmout_ogprcoak'       , self.parent.pyfiler1.ogsmout.ogprcoak     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogprdad        = self.add_df('ogsmout_ogprdad'        , self.parent.pyfiler1.ogsmout.ogprdad      , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogprdadof      = self.add_df('ogsmout_ogprdadof'      , self.parent.pyfiler1.ogsmout.ogprdadof    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogprdoff       = self.add_df('ogsmout_ogprdoff'       , self.parent.pyfiler1.ogsmout.ogprdoff     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogprdugr       = self.add_df('ogsmout_ogprdugr'       , self.parent.pyfiler1.ogsmout.ogprdugr     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogqcrrep       = self.add_df('ogsmout_ogqcrrep'       , self.parent.pyfiler1.ogsmout.ogqcrrep     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogqngrep       = self.add_df('ogsmout_ogqngrep'       , self.parent.pyfiler1.ogsmout.ogqngrep     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogqshlgas      = self.add_df('ogsmout_ogqshlgas'      , self.parent.pyfiler1.ogsmout.ogqshlgas    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogqshloil      = self.add_df('ogsmout_ogqshloil'      , self.parent.pyfiler1.ogsmout.ogqshloil    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogregprd       = self.add_df('ogsmout_ogregprd'       , self.parent.pyfiler1.ogsmout.ogregprd     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogsrl48        = self.add_df('ogsmout_ogsrl48'        , self.parent.pyfiler1.ogsmout.ogsrl48      , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogtechon       = self.add_df('ogsmout_ogtechon'       , self.parent.pyfiler1.ogsmout.ogtechon     , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogwellsl48     = self.add_df('ogsmout_ogwellsl48'     , self.parent.pyfiler1.ogsmout.ogwellsl48   , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])
        self.ogsmout_ogcoprd_fed    = self.add_df('ogsmout_ogcoprd_fed'    , self.parent.pyfiler1.ogsmout.ogcoprd_fed  , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogcoprd_nonfed = self.add_df('ogsmout_ogcoprd_nonfed' , self.parent.pyfiler1.ogsmout.ogcoprd_nonfed, output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngprd_fed    = self.add_df('ogsmout_ogngprd_fed'    , self.parent.pyfiler1.ogsmout.ogngprd_fed  , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_ogngprd_nonfed = self.add_df('ogsmout_ogngprd_nonfed' , self.parent.pyfiler1.ogsmout.ogngprd_nonfed, output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ogsmout_play_map       = self.add_df('ogsmout_play_map'       , self.parent.pyfiler1.ogsmout.play_map     , output=True, offset=[(0, 1)])
        self.ogsmout_ngpco2em       = self.add_df('ogsmout_ngpco2em'       , self.parent.pyfiler1.ogsmout.ngpco2em     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])

        #Variable shared with NGMM
        self.ogsmout_ogrnagprd      = self.add_df('ogsmout_ogrnagprd'      , self.parent.pyfiler1.ogsmout.ogrnagprd    , output=True, offset=[(0, 1), (1, 1), (2, self.parametr_baseyr)])  # ! Shared with NGMM
        self.ogsmout_cnrnagprd      = self.add_df('ogsmout_cnrnagprd'      , self.parent.pyfiler1.ogsmout.cnrnagprd    , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])  # ! Shared with NGMM

        #PMMOUT
        self.pmmout_dcrdwhp         = self.add_df('pmmout_dcrdwhp'         , self.parent.pyfiler1.pmmout.dcrdwhp       , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.pmmout_rfqdcrd         = self.add_df('pmmout_rfqdcrd'         , self.parent.pyfiler1.pmmout.rfqdcrd       , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.pmmout_rfqtdcrd        = self.add_df('pmmout_rfqtdcrd'        , self.parent.pyfiler1.pmmout.rfqtdcrd      , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])


        #CCATSDAT 
        self.ccatsdat_co2_prc_dis_45q   = self.add_df('ccatsdat_co2_prc_dis_45q' , self.parent.pyfiler1.ccatsdat.co2_prc_dis_45q   , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_co2_prc_dis_ntc   = self.add_df('ccatsdat_co2_prc_dis_ntc' , self.parent.pyfiler1.ccatsdat.co2_prc_dis_ntc   , output=False, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_cst_ngp_inv       = self.add_df('ccatsdat_cst_ngp_inv'     , self.parent.pyfiler1.ccatsdat.cst_ngp_inv       , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_cst_ngp_om        = self.add_df('ccatsdat_cst_ngp_om'      , self.parent.pyfiler1.ccatsdat.cst_ngp_om        , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_sup_ngp_45q       = self.add_df('ccatsdat_sup_ngp_45q'     , self.parent.pyfiler1.ccatsdat.sup_ngp_45q       , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_sup_ngp_ntc       = self.add_df('ccatsdat_sup_ngp_ntc'     , self.parent.pyfiler1.ccatsdat.sup_ngp_ntc       , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_dem_eor           = self.add_df('ccatsdat_dem_eor'         , self.parent.pyfiler1.ccatsdat.dem_eor           , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])
        self.ccatsdat_cst_eor           = self.add_df('ccatsdat_cst_eor'         , self.parent.pyfiler1.ccatsdat.cst_eor           , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])

        #QMORE
        self.qmore_qngpin               = self.add_df('qmore_qngpin'        , self.parent.pyfiler1.qmore.qngpin     , output=True, offset=[(0, 1), (1, self.parametr_baseyr)])

        pass


    def setup(self, temp_filename, temp_rest):
        '''Setup function for reading the restart file into HSM.

        Parameters
        ----------
        temp_filename : str
            Filename for abbreviated HSM input Restart file

        temp_rest : str
            Filename for main NEMS restart file

        Returns
        -------
        None
        '''
        self.output_path = self.parent.output_path
        self.run(temp_filename, temp_rest)


    def run(self,temp_filename, temp_rest):
        '''Calls the read_filer method from self.parent.pyfiler1 and loads the restart file into HSM.

            * If main NEMS restart file exist, read abbreviated restart file
            * If not, write abbreviated restart file as main restart file

            * Read in all restart variables and create dictionary storing all restart variable keys

        Parameters
        ----------
        temp_filename

        Returns
        -------
        Restart File
        self.__dict__
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
        

        com.print_out('running restart submodule')
        for key, value in self.int_dict_in.items():
            self.__dict__[key] = int(value)
        for key, value in self.df_dict_in.items():
            # sorting and (effectively) reindexing DataFrame being updated
            df2 = self.__dict__[key].sort_index()
            df1 = com.array_to_df(value)
            temp_index = df2.index.copy()
            df2.index = df1.index
            df2.update(df1)
            df2.index = temp_index
            self.__dict__[key] = df2
        pass


    def add_int(self, dict_name, restart_ref, output):
        '''Function for adding an integer from the restart file into HSM as an integer.

        Parameters
        ----------
        dict_name : str
            Name of restart variable or parameter in self.parent.pyfiler1

        restart_ref : int
            Integer data value from self.parent.pyfiler1

        output : boolean
            Boolean of whether the restart variable is an output value of HSM

        Returns
        -------
        restart_ref : int
            Integer value from the restart file
        '''
        self.int_dict_in[dict_name] = restart_ref
        if output:
            self.int_dict_out[dict_name] = restart_ref
        return int(restart_ref)


    def add_df(self, dict_name, restart_ref, output, offset=None):
        '''Function for adding a dataframe from the restart file into NEMS as a local DataFrame.

        Parameters
        ----------
        dict_name : str
            Name of restart variable or parameter in self.parent.pyfiler1

        restart_ref : int
            Integer data value from self.parent.pyfiler1

        output : boolean
            Boolean of whether the restart variable is an output value of HSM

        offset : list
            List indicating output df index value offset

        Returns
        -------
        df : df
            Restart variable df
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
        """Repacks local restart file variables into multi-dimensional arrays, and writes to restart file.

            * Loop through the variable dictionaries
            * Split out each of the method levels into different variables
            * Use getattr to link two method levels together
            * Link the third method level with setattr, and set to value

        Parameters
        ----------
        temp_filename : str
            Restart File name

        Returns
        -------
        None
        """
        #Spit out .csv for pmmout_rfqtdcrd
        for key, value in self.int_dict_out.items():
            value[:] = np.array(self.__dict__[key])
            cblock = key.split('_')[0]
            var = key.split('_',1)[1]
            nested_method = getattr(self.parent.pyfiler1, cblock, None)
            setattr(nested_method, var, value)
        for key, value in self.df_dict_out.items():
            value[:] = com.df_to_array(self.__dict__[key])
            cblock = key.split('_')[0]
            var = key.split('_', 1)[1]
            nested_method = getattr(self.parent.pyfiler1, cblock, None)
            setattr(nested_method, var, value)

        #Debugger
        if (self.parent.current_year == self.parent.final_aeo_year) & (self.parametr_ncrl == 1):
            logger.info('Debug Restart File Start')
            self.dump_restart()
            logger.info('Debug Restart File End')

        #self.parent.pyfiler1.utils.write_filer(temp_filename)

        pass


    def dump_restart(self):
        """Dumps restart file to .xlsx debug file.

        Returns
        -------
        None
        """

        writer = pd.ExcelWriter(self.output_path + 'restart_file_debug\\' + 'hsm_rest_all.xlsx')


        ###Outputs
        self.ogsmout_ogcnpprd.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogcnpprd')
        self.ogsmout_ogrnagprd.unstack(2).to_excel( writer, sheet_name='ogsmout_ogrnagprd')
        self.ogsmout_cnadgprd.unstack(1).to_excel(  writer, sheet_name='ogsmout_cnadgprd')
        self.ogsmout_cnenagprd.unstack(1).to_excel( writer, sheet_name='ogsmout_cnenagprd')
        self.ogsmout_ns_start.unstack(1).to_excel(  writer, sheet_name='ogsmout_ns_start')
        self.ogsmout_ogadgprd.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogadgprd')
        self.ogsmout_ogco245q.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogco245q')
        self.ogsmout_ogco2inj.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogco2inj')
        self.ogsmout_ogco2rec.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogco2rec')
        self.ogsmout_ogcoprd.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogcoprd')
        self.ogsmout_ogcoprdgom.unstack(1).to_excel(writer, sheet_name='ogsmout_ogcoprdgom')
        self.ogsmout_ogcowhp.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogcowhp')
        self.ogsmout_ogcrdheat.unstack(1).to_excel( writer, sheet_name='ogsmout_ogcrdheat')
        self.ogsmout_ogcrdprd.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogcrdprd')
        self.ogsmout_ogcruderef.unstack(2).to_excel(writer, sheet_name='ogsmout_ogcruderef')
        self.ogsmout_ogdngprd.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogdngprd')
        self.ogsmout_ogenagprd.unstack(2).to_excel( writer, sheet_name='ogsmout_ogenagprd')
        self.ogsmout_ogeorprd.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogeorprd')
        self.ogsmout_oggrowfac.T.to_excel(          writer, sheet_name='ogsmout_oggrowfac')
        self.ogsmout_ogjobs.T.to_excel(             writer, sheet_name='ogsmout_ogjobs')
        self.ogsmout_ognglak.T.to_excel(            writer, sheet_name='ogsmout_ognglak')
        self.ogsmout_ogngplbu.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogngplbu')
        self.ogsmout_ogngplet.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogngplet')
        self.ogsmout_ogngplis.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogngplis')
        self.ogsmout_ogngplpp.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogngplpp')
        self.ogsmout_ogngplpr.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogngplpr')
        self.ogsmout_ogngplprd.unstack(1).to_excel( writer, sheet_name='ogsmout_ogngplprd')
        self.ogsmout_ogngprd.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogngprd')
        self.ogsmout_ogngprdgom.unstack(1).to_excel(writer, sheet_name='ogsmout_ogngprdgom')
        self.ogsmout_ogngwhp.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogngwhp')
        self.ogsmout_ognowell.T.to_excel(           writer, sheet_name='ogsmout_ognowell')
        self.ogsmout_ogogwells.unstack(2).to_excel( writer, sheet_name='ogsmout_ogogwells')
        self.ogsmout_ogoilprd.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogoilprd')
        self.ogsmout_ogpcrwhp.T.to_excel(           writer, sheet_name='ogsmout_ogpcrwhp')
        self.ogsmout_ogpngwhp.T.to_excel(           writer, sheet_name='ogsmout_ogpngwhp')
        self.ogsmout_ogprcexp.T.to_excel(           writer, sheet_name='ogsmout_ogprcexp')
        self.ogsmout_ogprcoak.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogprcoak')
        self.ogsmout_ogprdad.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogprdad')
        self.ogsmout_ogprdadof.unstack(1).to_excel( writer, sheet_name='ogsmout_ogprdadof')
        self.ogsmout_ogprdoff.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogprdoff')
        self.ogsmout_ogprdugr.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogprdugr')
        self.ogsmout_ogqcrrep.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogqcrrep')
        self.ogsmout_ogqngrep.unstack(1).to_excel(  writer, sheet_name='ogsmout_ogqngrep')
        self.ogsmout_ogqshlgas.unstack(1).to_excel( writer, sheet_name='ogsmout_ogqshlgas')
        self.ogsmout_ogqshloil.unstack(1).to_excel( writer, sheet_name='ogsmout_ogqshloil')
        self.ogsmout_ogregprd.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogregprd')
        self.ogsmout_ogsrl48.unstack(2).to_excel(   writer, sheet_name='ogsmout_ogsrl48')
        self.ogsmout_ogtechon.unstack(2).to_excel(  writer, sheet_name='ogsmout_ogtechon')
        self.ogsmout_ogwellsl48.unstack(2).to_excel(writer, sheet_name='ogsmout_ogwellsl48')
        self.pmmout_dcrdwhp.unstack(1).to_excel(    writer, sheet_name='pmmout_dcrdwhp')
        self.pmmout_rfqdcrd.unstack(1).to_excel(    writer, sheet_name='pmmout_rfqdcrd')
        self.pmmout_rfqtdcrd.unstack(1).to_excel(   writer, sheet_name='pmmout_rfqtdcrd')
        self.ccatsdat_dem_eor.unstack(1).to_excel(   writer, sheet_name='ccatsdat_dem_eor')
        self.ccatsdat_cst_eor.unstack(1).to_excel(   writer, sheet_name='ccatsdat_cst_eor')
        self.ogsmout_play_map.to_excel(   writer, sheet_name='ogsmout_play_map')
        self.ogsmout_ogcoprd_fed.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogcoprd_fed')
        self.ogsmout_ogcoprd_nonfed.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogcoprd_nonfed')
        self.ogsmout_ogngprd_fed.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogngprd_fed')
        self.ogsmout_ogngprd_nonfed.unstack(1).to_excel(   writer, sheet_name='ogsmout_ogngprd_nonfed')
        self.qmore_qngpin.unstack(1).to_excel(   writer, sheet_name='qmore_qngpin')
        self.ogsmout_ngpco2em.unstack(1).to_excel(writer, sheet_name='ogsmout_ngpco2em')

        #Close the Pandas Excel writer and output the .xlsx file
        writer.close()
