"""Parent module of HSM where all main processes are called. Parent class for all HSM submodules.

Summary
-------
The module class performs all key model setup functions and runs all setup sub-classes (**restart.py**, **intermediate_var_pickle.py**, **history_steo.py**).
It then runs each of the HSM submodules (**onshore.py**, **offshore.py**, **alaska.py**, and **canada.py**), aggregates model results, and performs
reporting functions. The module operates as follows:

    1. The module is initialized in the *__init__* method after being called by **hsm.py**, with class dataframes, variables,
       flags, and filepaths being declared here.

    2. The *setup* method is called each model year, which in turn calls several additional setup functions. The *setup* method operates as follows:

        a. Model filepaths are initialized, and the main **setup.txt** table containing key model attributes
           (i.e. model switches, the HSM history year, etc.) is read in.

        b. Other global tables are read in (i.e. mapping and discount tables).

        c. *self.restart.setup* is called from child class in **Restart.py**, reading in the the NEMS Restart File and assigning
           all the restart variables to local HSM variables.

        d. *load_parameters* and *process_restart_variables* are called, loading in NEMS parameters (i.e. model year, iteration number, etc.)
           and creating an additiona layer of local variables for use by the HSM submodules to avoid data overwrites.

        e. The scedes "Scenario flag" is read in. The "Scenario flag" is used to determine if NEMS is running a side case, and if so,
           what model adjustments need to be made in HSM as part of this side case.

        f. Unlike OGSM, HSM is written in Python, meaning that its iterative, local variables cannot be stored in memory. Instead,
           these intermediate variables are stored in ".pkl" files each year and read in  each model year. As a first step, the
           intermediate variable folder is cleaned out. In model year 1 the .pkl output directory is cleaned out entirely, in other model years
           "model year - 2" .pkl files are deleted. Then the relevant .pkl intermediate variables are read in from **intermediate_var_pickle.py**.

        g. Load historical data and apply to relevant restart variables.

        f. Calculate NGPL ratios for STEO calculations.

        h. Load in STEO overwrite data and apply to relevant restart variables.

        i. Setup the **Cashflow** class to be used by all HSM submodules.

        g. Setup the **Onshore**, **Offshore**, **Alaska**, and **Canada** submodules by calling their respective *setup* class methods.

    3. The *run* method is called each model year, which operates as follows:

        a. Model year and iteration are declared.

        b. Model year discount rate is calculated in *calculate_discount_rate*.

        c. Model year prices are calculated in *calculate_prices* based on LFMM and NGMM regional wellhead prices.

        d. Run the **Onshore**, **Offshore**, **Alaska**, and **Canada** submodules by calling their respective *run* class methods.

    4. Once all of the submodules have run, the *prepare_results* method is called, which operates as follows:

        a. Run the **Onshore**, **Offshore**, **Alaska**, and **Canada** *report results* methods to write initial submodule results
           to the restart variables.

        b. Run the *aggregate_results_across_submodules* method to combine production volumes across submodules for cumulative
           variables.

        c. In the first STEO year (AEO year - 1), run *set_expected_production_to_steo*, which overwrites production volumes
           with STEO production values.

        d. Run *adjust_production* to respond to actual natural gas demand volumes provided by NGMM.

        f. Call *write_pkl_variables* from **intermediate_var_pickle.py** to write intermediate variables out for the next model year.


Input Files
-----------
    * setup.csv - setup file with key model inputs (model flags, model history year, etc.)
    * mapping.csv - setup file with HSM regional mapping (mapping HSM districts to regions to LFMM regions, etc.)
    * discount_tables.csv - setup file with key discount rate assumptions


Model Functions and Class Methods
_________________________________
    * __init__ - Constructor to initialize Module Class
    * setup - HSM Setup function
    * load_parameters - Loads in HSM Parameters
    * process_restart_variables - Creates global variables from restart file for use in the submodules
    * run - Method to run main HSM model functions
    * calculate_discount_rate - "Calculates discount rate to be used in discounted cash flows (**CashFlow.py**)
    * calculate_prices - Aggregates and shares out crude type and natural gas prices to regions and districts
    * prepare_results - Prepare results for writing to the restart file
    * aggregate_results_across_submodules - Calculate sum  values in restart variable tables that are aggregated across submodules
    * adjust_production - Adjust Restart variables so that they reflect final model year realized production


Output Debug Files
__________________
hsm_steo_nagas_adjustment_factor.csv - STEO adjustment factors for na natural gas production
hsm_steo_adgas_adjustment_factor.csv - STEO adjustment factors for ad natural gas production


Output Restart Variables
________________________
**Crude Oil Restart Variables**
    * pmmout_rfqtdcrd - Total crude production by HSM region
    * pmmout_rfqdcrd - Total crude oil production by HSM region (not including EOR)
    * ogsmout_ogqcrrep - Crude oil production by oil category
    * ogsmout_ogcrdprd - Crude oil production by HSM region and crude type
    * ogsmout_ogcruderef - Crude oil production by LFMM crude oil type and region


**Natural Gas Restart Variable**
    * ogsmout_ogqngrep - Natural gas production by natural gas type


**Well Restart Variables**
    * ogsmout_ogogwells - Total wells
    * ogsmout_ognowell - Total completed wells


**NGPL Production Restart Variables**
    * ogsmout_ogngplprd - NGPL production by HSM district
    * ogsmout_ogngplet - Ethane production by HSM district
    * ogsmout_ogngplpr - Propane production by HSM district
    * ogsmout_ogngplbu - Butane production by HSM district
    * ogsmout_ogngplis - Isobutane production by HSM district
    * ogsmout_ogngplpp - Pentanes production by HSM district


Module Class Methods
____________________
"""

import pandas as pd
import numpy as np
import names as nam
import common as com
import time



from hsm import logging

import warnings
import os
import shutil
import pyscedes


logger = logging.getLogger('module.py')


class Module:
    """
    Head module for HSM
    """

    def __init__(self):
        """Initializes Module object.

        Returns
        -------
        Module

        """

        

        ###Input and output paths

        #Input Paths
        self.main_directory         = ''  #Path to main directory
        self.input_path             = ''  #Path to directory containing main input files
        self.hist_input_path        = ''  #Path to directory containing history input files
        self.steo_input_path        = ''  #Path to directory containing steo input files
        self.onshore_input_path     = ''  #Path to directory containing onshore input files
        self.offshore_input_path    = ''  #Path to directory containing offshore input files
        self.alaska_input_path      = ''  #Path to directory containing alaska input files
        self.canada_input_path      = ''  #Path to directory containing canada input files
        self.ngp_input_path         = ''  # Path to directory containing NGP input files

        #Output Paths
        self.hsm_var_output_path    = ''  #Path to directory containing intermediate files that are loaded in each iteration
        self.output_path            = ''  #Path to directory containing debug files


        ###Tables
        self.setup_table        = pd.DataFrame()  #Setup table
        self.hist_setup_table   = pd.DataFrame()  #Historical data setup table
        self.discounting_table  = pd.DataFrame()  #Table of variables for discount rate



        ###General
        self.mapping            = pd.DataFrame()  #Dataframe of regional relationships
        self.scedes             = {} #Dictionary of scedes flags from keys.sed
        self.base_price_year    = 0  #Price year for all calculations in HSM (NEMS defaults to 1997)
        self.aeo_year           = 0  #Year of the current AEO (e.g. AEO2020, aeo_year = 2020)
        self.history_year       = 0  #Last year of history, not including STEO years
        self.technology_year    = 0  #First year of technological improvement
        self.final_aeo_year     = 0  #Last aeo year (MNUMYR)


        ###Switches
        self.integrated_switch          = False     #False means standalone run, True means integrated run
        self.side_case_steo_overwrite_switch      = False     #False means side case overwrites disabled, True means enable side case STEO overwrites from CSV files
        self.run_every_itr_switch       = False     #False means only run itrs 1, fcrl = 1 & ncrl = 1, True means run every iteration
        self.debug_switch               = False     #False means local debug files are not printed out, True means local debug files are printed out
        self.low_price_case             = False     #Flag for if run is a low price case, False means off, True means on
        self.onshore_switch             = False     #Onshore submodule switch for testing, False means off, True means on
        self.offshore_switch            = False     #Offshore submodule switch for testing, False means off, True means on
        self.alaska_switch              = False     #Alaska submodule switch for testing, False means off, True means on
        self.canada_switch              = False     #Canada submodule switch for testing, False means off, True means on
        self.ngp_switch                 = False     #NGP submodule switch for testing, False means off, True means on
        self.side_case_adj              = 0         #Adjustment factor for running low oil and gas supply case, 0.5 means low supply, 1.5 means high supply

        ## financial data
        self.corp_bond_rate            = 0.0  #Corporate bond rate                      (ogsm: BAA)
        self.debt_ratio                = 0.0  #Debt ratio                               (ogsm: debtratio)
        self.fed_tax_rate              = 0.0  #Federal tax rate                         (ogsm: fedtxr)
        self.ten_year_treas_note_yield = 0.0  #Averaged Yield on 10-year Treasury notes (ogsm: t10yr)
        self.market_risk_premium       = 0.0  #Market risk premium                      (ogsm: ogmrp)
        self.industry_beta             = 0.0  #Industry beta                            (ogsm: ogbeta)
        self.expected_inflation_rate   = 0.0  #Expected inflation rate                  (ogsm: gdpch)
        self.return_over_capital_cost  = 0.0  #Required return over cost of capital     (ogsm: 0.05)
        self.discount_rate             = 0.0  #Discount rate                            (ogsm: disc)


        ###Restart Variables
        #Model control variables
        self.current_year       = 0  #(NCNTRL, CURCALYR)
        self.current_iteration  = 0  #(NCNTRL, CURITR)
        self.current_cycle      = 0  #(NCNTRL, CURITR)



        ###HSM Prices
        #Crude prices
        self.lfmm_reg_crude_price       = pd.DataFrame  #Crude wellhead price by LFMM region
        self.lfmm_reg_crude_prod        = pd.DataFrame  #Crude wellhead production by LFMM region
        self.lfmm_reg_crude_type_price  = pd.DataFrame  #Crude wellhead price by LFMM by type and region
        self.lfmm_reg_crude_type_prod   = pd.DataFrame  #Crude wellhead production by LFMM by type and region
        self.dist_crude_price           = pd.DataFrame  #Crude wellhead price by HSM district
        self.dist_crude_type_price      = pd.DataFrame  #Crude wellhead price by HSM district and type
        self.reg_crude_price            = pd.DataFrame  #Crude wellhead price by HSM region


        #Natural gas prices
        self.dist_natgas_price      = pd.DataFrame()  #Natural gas wellhead price by HSM district
        self.dist_natgas_prod       = pd.DataFrame()  #Total natural gas production by HSM district
        self.reg_natgas_price       = pd.DataFrame()  #Natural gas wellhead price by  HSMregion
        self.reg_natgas_prod        = pd.DataFrame()  #Total natural gas production by HSM region
        self.dist_natgas_type_prod  = pd.DataFrame()  #Total natural gas production by NG type by HSM district

        ###Parameters
        self.param_baseyr   = 1990  #Base calendar year corresponding to curiyr=1
        self.param_fcrl     = 0     #Flag to indicate last regular model iteration
        self.param_ncrl     = 0     #Flag to indicate reporting model iteration


        ###Local restart variables
        self.rest_intout_start_price    = pd.DataFrame()  #intout:      REAL START_PRICE(MNUMYR)  ! world oil price as specified in memo
        self.rest_mc_rmcorpbaa          = pd.DataFrame()  #intout:      REAL INTOUT_MC_RMCORPBAA(MNUMYR) ! Corporate bond rate
        self.rest_mc_rmtcm10y           = pd.DataFrame()  #intout:      REAL INTOUT_MC_RMTCM10Y(MNUMYR) ! Ten year treasury rate
        self.rest_brent_price           = pd.DataFrame()  #intout:      REAL INTOUT_BRENT_PRICE(MNUMOR,MNUMYR) ! International brent price
        self.rest_ccs_eor_45q           = pd.DataFrame()  #tc45q:       REAL CCS_EOR_45Q(MNUMYR)     ! Tax credits for enhanced oil recovery in dollars per metric ton co2
        self.rest_45q_duration          = pd.DataFrame()  #tc45q:       INTEGER I_45Q_DURATION       ! Number of years tax credit apply to plant
        self.rest_45q_lyr_ret           = pd.DataFrame()  #tc45q:       INTEGER I_45Q_LYR_RET        ! Year tax credit expire for retrofits
        self.rest_cflgq                 = pd.DataFrame()  #convfact:    REAL CFLGQ(MNUMYR) !  Liquid Petroleum Gas
        self.rest_plginpf               = pd.DataFrame()  #pmore:       REAL PLGINPF(MNUMOR, MNUMYR) !NGPL Prices
        self.rest_ogwprng               = pd.DataFrame()  #ngtdmrep     REAL OGWRPNG(MNUMOR,MNUMYR) ! NG wellhead price ($87/MCF)
        self.rest_ogprcng               = pd.DataFrame()  #ngtdmrep     REAL OGPRCNG(OGDIST,MNUMYR) ! Natural gas wellhead-type price at district level (1987 dollars per thousand cubic feet)
        self.rest_rfcrudewhp            = pd.DataFrame()  #lfmmout      REAL RFCRUDEWHP     ! Crude oil wellhead price by refinery region and type
        self.rest_cnadgprd              = pd.DataFrame()  #ogsmout:     REAL CNADGPRD(NUMCAN,MNUMYR)      ! AD gas production in Canada (1=east, 2=west)
        self.rest_cnenagprd             = pd.DataFrame()  #ogsmout:     REAL CNENAGPRD(NUMCAN,MNUMYR)     ! Expected NA gas production in Canada  (1=east, 2=west)
        self.rest_cnrnagprd             = pd.DataFrame()  #ogsmout:     REAL CNRNAGPRD(NUMCAN,MNUMYR)     ! Realized NA gas production in Canada (1=east, 2=west)
        self.rest_ogadgprd              = pd.DataFrame()  #ogsmout:     REAL OGADGPRD(OGDIST,OILTYPES,MNUMYR) ! associated-dissolved natural gas production
        self.rest_ogeorprd              = pd.DataFrame()  #ogsmout      REAL OGEORPRD(MNLNP1,13,MNUMYR)   ! EOR production from CO2 projects (mbbl)
        self.rest_ogco2inj              = pd.DataFrame()  #ogsmout:     REAL OGCO2INJ(MNLNP1,13,MNUMYR)   ! CO2 injected (mmcf)
        self.rest_ogco2rec              = pd.DataFrame()  #ogsmout:     REAL OGCO2INJ(MNLNP1,13,MNUMYR)   ! CO2 recycled (mmcf)
        self.rest_ogcoprd               = pd.DataFrame()  #ogsmout:     REAL OGCOPRD(MNL48T,MNUMYR)       ! CRUDE PRODUCTION BY Lower 48 region
        self.rest_ogcoprdgom            = pd.DataFrame()  #ogsmout:     REAL OGCOPRDGOM(2,MNUMYR)         !  Crude oil offshore production where (1, is shallow and (2, is deep water
        self.rest_ogcowhp               = pd.DataFrame()  #ogsmout:     REAL OGCOWHP(MNL48T,MNUMYR)       ! CRUDE WELLHEAD PRICE BY Lower 48 region
        self.rest_ogcrdheat             = pd.DataFrame()  #ogsmout:     REAL OGCRDHEAT(MNCRUD,MNUMYR)     ! Heat rate by type of crude oil
        self.rest_ogcrdprd              = pd.DataFrame()  #ogsmout:     REAL OGCRDPRD(MNUMOR,MNCRUD,MNUMYR)   ! Crude oil production by HSM region and type of crude
        self.rest_ogcruderef            = pd.DataFrame()  #ogsmout:     REAL OGCRUDEREF(MNUMPR,MNCRUD,MNUMYR) ! Crude oil production by refinery region and crude type
        self.rest_ogdngprd              = pd.DataFrame()  #ogsmout:     REAL OGDNGPRD(OGDIST,GASTYPES,MNUMYR) ! Dry natural gas production by state/district and gas type
        self.rest_ogenagprd             = pd.DataFrame()  #ogsmout:     REAL OGENAGPRD(OGDIST,GASTYPES,MNUMYR)    ! expected non-associated natural gas production
        self.rest_oggrowfac             = pd.DataFrame()  #ogsmout:     REAL OGGROWFAC(MNUMYR)            ! factor reflects expected future cons growth
        self.rest_ogjobs                = pd.DataFrame()  #ogsmout:     REAL OGJOBS(MNUMYR)
        self.rest_ognglak               = pd.DataFrame()  #ogsmout:     REAL OGNGLAK(MNUMYR)              ! Natural Gas Liquids from Alaska (t BBL/day)
        self.rest_ogngplbu              = pd.DataFrame()  #ogsmout:     REAL OGNGPLBU(OGDIST,MNUMYR)      ! Butane production
        self.rest_ogngplet              = pd.DataFrame()  #ogsmout:     REAL OGNGPLET(OGDIST,MNUMYR)      ! Ethane production
        self.rest_ogngplis              = pd.DataFrame()  #ogsmout:     REAL OGNGPLIS(OGDIST,MNUMYR)      ! Isobutane production
        self.rest_ogngplpp              = pd.DataFrame()  #ogsmout:     REAL OGNGPLPP(OGDIST,MNUMYR)      ! Pentanes plus production
        self.rest_ogngplpr              = pd.DataFrame()  #ogsmout:     REAL OGNGPLPR(OGDIST,MNUMYR)      ! Propane production
        self.rest_ogngplprd             = pd.DataFrame()  #ogsmout:     REAL OGNGPLPRD(OGDIST,MNUMYR)     ! Natural gas plant output by NGPL region
        self.rest_ogngprd               = pd.DataFrame()  #ogsmout:     REAL OGNGPRD(MNL48T,MNUMYR)       ! NG PRODUCTION BY GAS CATEGORY
        self.rest_ogngprdgom            = pd.DataFrame()  #ogsmout:     REAL OGNGPRDGOM(2,MNUMYR)         !  Natural gas
        self.rest_ogngwhp               = pd.DataFrame()  #ogsmout:     REAL OGNGWHP(MNL48T,MNUMYR)       ! NG WELLHEAD  PRICE BY GAS CATEGORY
        self.rest_ognowell              = pd.DataFrame()  #ogsmout:     REAL OGNOWELL(MNUMYR)             ! WELLS COMPLETED
        self.rest_ogogwells             = pd.DataFrame()  #ogsmout:     REAL OGOGWELLS(OGDIST,WELLTYPES,MNUMYR)   ! oil and natural gas wells
        self.rest_ogoilprd              = pd.DataFrame()  #ogsmout:     REAL OGOILPRD(OGDIST,OILTYPES,MNUMYR)     ! crude oil production
        self.rest_ogpcrwhp              = pd.DataFrame()  #ogsmout:     REAL OGPCRWHP(MNUMYR)             ! CRUDE AVG WELLHEAD PRICE
        self.rest_ogpngwhp              = pd.DataFrame()  #ogsmout:     REAL OGPNGWHP(MNUMYR)             ! NG AVG WELLHEAD PRICE
        self.rest_ogprcexp              = pd.DataFrame()  #ogsmout:     REAL OGPRCEXP(MNUMYR)             ! Adjust last yrs price to reflect diff expectation
        self.rest_ogprcoak              = pd.DataFrame()  #ogsmout:     REAL OGPRCOAK(MNALAS,MNUMYR)      ! ALASKA PRODUCTION BY REGION  dim based on AKRGN
        self.rest_ogprdad               = pd.DataFrame()  #ogsmout:     REAL OGPRDAD(MNL48A,MNUMYR)       ! ASSOCIATED/DISSOLVED GAS PRODUCTION
        self.rest_ogprdadof             = pd.DataFrame()  #ogsmout:     REAL OGPRDADOF(MNL48F,MNUMYR)     ! OFFSHORE AD GAS PROD (BCF)
        self.rest_ogprdoff              = pd.DataFrame()  #ogsmout:     REAL OGPRDOFF(6,2,MNUMYR)         ! Lower 48 offshore production split into Federal/State categories
        self.rest_ogprdugr              = pd.DataFrame()  #ogsmout:     REAL OGPRDUGR(MNL48N,3,MNUMYR)    ! Lower 48 unconventional natural gas production (region;1=tight gas,2=shale gas,3=coalbed methane;year)
        self.rest_ogqcrrep              = pd.DataFrame()  #ogsmout:     REAL OGQCRREP(MNOGCRO,MNUMYR)     ! CRUDE PRODUCTION BY OIL CAT
        self.rest_ogqngrep              = pd.DataFrame()  #ogsmout:     REAL OGQNGREP(MNOGCAT,MNUMYR)     ! NG PRODUCTION BY GAS CAT
        self.rest_ogqshlgas             = pd.DataFrame()  #ogsmout:     REAL OGQSHLGAS(SOPLAY,MNUMYR)     ! Natural gas production from select shale gas plays
        self.rest_ogqshloil             = pd.DataFrame()  #ogsmout:     REAL OGQSHLOIL(SOPLAY,MNUMYR)     ! Crude oil production from select shale oil plays
        self.rest_ogregprd              = pd.DataFrame()  #ogsmout:     REAL OGREGPRD(MNL48N,7,MNUMYR)    ! Regional crude oil and natural gas production by type of production
        self.rest_ogrnagprd             = pd.DataFrame()  #ogsmout:     REAL OGRNAGPRD(MNUMYR)            ! realized non-associated natural gas production
        self.rest_ogsrl48               = pd.DataFrame()  #ogsmout:     REAL OGSRL48(MNL48T,7,MNUMYR)     ! Lower 48 drilling success rates
        self.rest_ogtechon              = pd.DataFrame()  #ogsmout:     REAL OGTECHON(3,6,MNUMYR)         ! TECH FACTORS BY COSTCAT/FUEL/YEAR
        self.rest_ogwellsl48            = pd.DataFrame()  #ogsmout:     REAL OGWELLSL48(MNL48T,7,MNUMYR)  ! Lower 48 successful and dry well counts
        self.rest_dcrdwhp               = pd.DataFrame()  #pmmout:      REAL DCRDWHP(MNUMOR,MNUMYR)       ! DOM CRUDE WELLHEAD PRICE (calculated from reg_crude_price, derived from rfcrudewhp)
        self.rest_rfqdcrd               = pd.DataFrame()  #pmmout:      REAL RFQDCRD(MNUMOR+2,MNUMYR)     ! DOMESTIC TOTAL CRUDE MMBBL/
        self.rest_rfqtdcrd              = pd.DataFrame()  #pmmout:      REAL RFQTDCRD(MNUMOR+2,MNUMYR)    ! DOM TOTAL CRUDE (INCL EOR)
        self.rest_ogcoprd_fed           = pd.DataFrame()  #ogsmout:     REAL OGCOPRD_FED(MNL48T, MNUMYR)  !  Crude oil federal
        self.rest_ogcoprd_nofed         = pd.DataFrame()  #ogsmout:     REAL OGCOPRD_NONFED(MNL48T, MNUMYR) !  Crude oil non-federal
        self.rest_ogngprd_fed           = pd.DataFrame()  #ogsmout:     REAL OGNGPRD_FED(MNL48T, MNUMYR)  !  Natural gas federal
        self.rest_ogngprd_nofed         = pd.DataFrame()  #ogsmout:     REAL OGNGPRD_NONFED(MNL48T, MNUMYR) !  Natural gas  non-federal
        self.rest_pelin                 = pd.DataFrame()  #mpblk:       REAL MPBLK_PELIN(MNUMCR,MNUMYR)! Purchased Electricity - Industrial

        ###Timing Variables
        self.onshore_runtime    = 0.0  #Runtime for Onshore submodule
        self.offshore_runtime   = 0.0  #Runtime for Offshore submodule
        self.alaska_runtime     = 0.0  #Runtime for Alaska submodule
        self.canada_runtime     = 0.0  #Runtime for Canada submodule
        self.prepare_results_runtime = 0.0  #Runtime for prepare_results method
        self.ngp_runtime        = 0.0  #Runtime for NGP submodule


    def setup(self, standalone_directory,
              integrated_directory,
              integrated_input_path,
              setup_filename,
              year,
              pyfiler1,
              cycle,
              scedes):
        """HSM Setup function.


            * Assigns input paths and global variables
            * Reads in setup tables
            * Calls the Restart File *setup* method
            * Loads in parameters
            * Reads in relevant scedes flags
            * Reads in iterative model intermediate variables
            * Loads in history and steo values, and writes these to restart file variables
            * Runs the cashflow and submodule setup methods

        Parameters
        ----------
        standalone_directory : str
            Filepath for standalone HSM run main directory

        integrated_directory : str
            Filepath for integrated HSM run main directory

        integrated_input_path : str
            Filepath for integrated HSM run input directory

        setup_filename : str
            setup.txt filename


        Returns
        -------
        self.main_directory : str
            Main directory filepath

        self.input_path : str
            Inputs directory filepath

        self.output_path : str
            Outputs directory filepath

        self.hsm_var_output_path : str
            Intermediate variable file directory path

        self.integrated_switch : boolean
            Flag indicating whether HSM is running in an integrated environment or as a standlone run

        self.hist_input_path : str
            Historical data input file directory filepath

        self.steo_input_path : str
            STEO input file directory filepath

        self.onshore_input_path : str
            Onshore submodule input file directory filepath

        self.offshore_input_path : str
            Offshore submodule input file directory filepath

        self.alaska_input_path : str
            Alaska submodule input file directory filepath

        self.canada_input_path : str
            Canada submodule input file directory filepath

        self.setup_table : df
            DataFrame of key model inputs (model flags, model history year, etc.)

        self.base_price_year : int
            Hardcoded base year for prices in NEMS (1987)

        self.aeo_year : int
            AEO publication year

        self.history_year : int
            Last year for which HSM contains historical data

        self.technology_year : int
            Base year for technology improvement in Submodules

        self.final_aeo_year : int
            Final AEO model year

        self.steo_years : list
            Current STEO forecast years

        self.run_every_itr_switch : boolean
            Switch indicating whether HSM should run every iteration, or just itrs 1, fcrl = 1 and ncrl = 1

        self.side_case_steo_overwrite_switch : boolean
            Switch indicating whether side case STEO overwrites from reference case CSV files should be enabled

        self.debug_switch : boolean
            Switch indicating whether HSM-specific debug files should be printed out (increases runtime)

        self.hsm_var_debug_switch : boolean
            Switch indicating whether .csv copies of hsm intermediate files should be printed out (increases runtime)

        self.discounting_table : df
            DataFrame with key discount rate assumptions

        self.debt_ratio : float
            Assumption of debt ratio used to calculated WACC

        self.fed_tax_rate : float
            Federal tax rate

        self.industry_beta : float
            Assumption of industry beta used to calculated WACC

        self.market_risk_premium : float
            Assumption of market risk premium used to calculated WACC

        self.return_over_capital_cost : float
            Assumption of return over capital cost used to calculated WACC

        self.scedes : dict
            Dictionary of key scedes values used by HSM

        self.side_case_adj : float
            Adjustment factor for side case (used for LOGS/HOGS side cases)

        self.low_price_case : boolean
            Flag indicating that a low oil price side case is being run
        """


        ### Instantiate Logger
        self.logger = logger

        ### Get cycle
        if cycle == 'no load on cycle':
            self.current_cycle = 'nan'
        else:
            self.current_cycle = cycle


        ###Assign directories for standalone vs. integrated run based on where setup.csv is located
        file_exists = os.path.exists(integrated_input_path + 'setup.csv')
        self.logger.info('parent input path: ' + integrated_directory)
        if file_exists:
            self.main_directory             = integrated_directory
            self.input_path                 = integrated_directory + 'hsm\\input\\'
            self.output_path                = integrated_directory + 'hsm\\debug\\'
            self.hsm_var_output_path        = integrated_directory + 'hsm\\intermediate_tables\\'
            self.integrated_switch          = True
            self.logger.info('INTEGRATED SWITCH ON')

        else:
            self.main_directory             = standalone_directory
            self.input_path                 = standalone_directory + 'input\\'
            self.output_path                = standalone_directory + 'debug\\'
            self.hsm_var_output_path        = standalone_directory + 'intermediate_tables\\'
            self.integrated_switch          = False
            self.logger.info('INTEGRATED SWITCH OFF')
            
            # Validate required files for standalone runs
            self._validate_standalone_files()


        #Assign other input paths
        self.hist_input_path            = self.input_path + 'history\\'
        self.steo_input_path            = self.input_path + 'steo\\'
        self.onshore_input_path         = self.input_path + 'onshore\\'
        self.offshore_input_path        = self.input_path + 'offshore\\'
        self.alaska_input_path          = self.input_path + 'alaska\\'
        self.canada_input_path          = self.input_path + 'canada\\'
        self.ngp_input_path             = self.input_path + 'ngp\\'
        self.variable_mapping_input_path = self.input_path + 'variable_mapping\\'

        ### Instantiate Pyfiler
        if self.integrated_switch == True:
            self.pyfiler1 = pyfiler1
        else:
            try:
                import pyfiler1
                self.pyfiler1 = pyfiler1
            except ModuleNotFoundError as e:
                import sys
                error_msg = (
                    f"Failed to import pyfiler1. This is likely a Python version mismatch.\n"
                    f"Current Python version: {sys.version}\n"
                    f"pyfiler1.cp312-win_amd64.pyd is compiled for Python 3.12.\n"
                    f"Please use Python 3.12 or obtain a pyfiler1 version compiled for Python {sys.version_info.major}.{sys.version_info.minor}.\n"
                    f"Current directory: {os.getcwd()}\n"
                    f"Python path includes: {[p for p in sys.path if 'hsm' in p]}"
                )
                raise ModuleNotFoundError(error_msg) from e


        #Assign base model parameters
        self.current_year               = int(year)
        self.setup_table                = com.read_dataframe(self.input_path + setup_filename, index_col=0)
        self.base_price_year            = int(self.setup_table.at[nam.base_price_year   , nam.filename])
        self.aeo_year                   = int(self.setup_table.at[nam.aeo_year          , nam.filename])
        self.history_year               = int(self.setup_table.at[nam.history_year      , nam.filename])
        self.technology_year            = int(self.setup_table.at[nam.technology_year   , nam.filename])
        self.final_aeo_year             = int(self.setup_table.at[nam.final_aeo_year    , nam.filename])
        self.steo_years                 = list(range(self.aeo_year - 1, self.aeo_year + 2))
        self.run_every_itr_switch       = str(self.setup_table.at[nam.run_every_itr_switch, nam.filename]).upper() == 'True'.upper()
        self.side_case_steo_overwrite_switch      = str(self.setup_table.at[nam.side_case_steo_overwrite_switch, nam.filename]).upper() == 'True'.upper()
        self.debug_switch               = str(self.setup_table.at[nam.debug_switch, nam.filename]).upper() == 'True'.upper()
        self.hsm_var_debug_switch       = str(self.setup_table.at[nam.hsm_var_debug_switch, nam.filename]).upper() == 'True'.upper()
        self.debug_restart_yearly_switch = str(self.setup_table.at[nam.debug_restart_yearly_switch, nam.filename]).upper() == 'True'.upper()
        self.suppress_warnings_switch = str(self.setup_table.at[nam.suppress_warnings_switch, nam.filename]).upper() == 'True'.upper()
        self.standalone_scedes_file = str(self.setup_table.at[nam.standalone_scedes_file, nam.filename])

        #Configure warnings filter if suppress_warnings_switch is enabled. This should be done prudently and only to help modeler see logging clearly
        if self.suppress_warnings_switch:
            warnings.filterwarnings('ignore')

        #Load in project finance variables
        temp_filename                   = self.setup_table.at[nam.discounting, nam.filename]
        self.discounting_table          = com.read_dataframe(self.input_path + temp_filename, index_col=0)
        self.debt_ratio                 = float(self.discounting_table.at[nam.debt_ratio                , nam.value])
        self.fed_tax_rate               = float(self.discounting_table.at[nam.fed_tax_rate              , nam.value])
        self.industry_beta              = float(self.discounting_table.at[nam.industry_beta             , nam.value])
        self.market_risk_premium        = float(self.discounting_table.at[nam.market_risk_premium       , nam.value])
        self.return_over_capital_cost   = float(self.discounting_table.at[nam.return_over_capital_cost  , nam.value])

        #Load in HSM regional mapping
        temp_filename                   = self.setup_table.at[nam.mapping, nam.filename]
        self.mapping                    = com.read_dataframe(self.input_path + temp_filename)

        #Import Submodules
        import restart_unf as rst
        import intermediate_var_pickle as ivpkl
        import history_steo as hist_steo
        import cash_flow as cf
        import offshore as off
        import canada as can
        import onshore as on
        import alaska as ak
        import ngp as ngp
        import ngp_cash_flow as ngpcf
        import prepare_results as pr

        ###Submodules and restart
        self.restart            = rst.Restart(self)             #Restart object
        self.hsm_vars           = ivpkl.intermediate_vars(self) #Intermediate variables for HSM object
        self.hist_steo          = hist_steo.hist_steo(self)     #History and STEO object
        self.onshore            = on.Onshore(self)              #Onshore submodule object
        self.offshore           = off.Offshore(self)            #Offshore submodule object
        self.alaska             = ak.Alaska(self)               #Alaska submodule object
        self.canada             = can.Canada(self)              #Canada submodule object
        self.ngp                = ngp.NGP(self)                 #Natural Gas Processing submodule object
        self.prep_results       = pr.Prep_Results(self)         #Prepare results object

        

        #Model warnings
        if self.aeo_year - 2 != self.history_year:
            com.print_out(self.aeo_year)
            com.print_out(self.history_year)
            warnings.warn('self.aeo_year - 2 != self.history_year', UserWarning)
        if self.aeo_year - 1 != self.technology_year:
            com.print_out(self.aeo_year)
            com.print_out(self.technology_year)
            warnings.warn('self.aeo_year - 1 != self.technology_year', UserWarning)


        ###Load restart file
        self.logger.info('Load Restart File')
        temp_rest       = self.main_directory + 'restart.npz'
        self.restart.setup(temp_rest)
        self.load_parameters()
        self.process_restart_variables()
        self.logger.info('End Load Restart File')


        ###Load Scedes scenario flag to determine if run is a side case
        self.logger.info('Load Scedes and Side Cases')
        ###Load Scedes
        # Standalone Run (Offline)
        if scedes is None:
            scedes_filepath = pyscedes.find_scedes(self.standalone_scedes_file)
            self.scedes     = pyscedes.parse_scedes(scedes_filepath)

            #Load side case flag from scedes for High Oil and Low Oil Supply Cases
            if (self.scedes.get('OGTECH') == '23'): #HOGS
                self.side_case_adj = 1.5
            elif (self.scedes.get('OGTECH') == '30'): #LOGS
                self.side_case_adj = 0.5
            else:
                self.side_case_adj = 1.0

            self.logger.info('Scenario Flag')
            scenario = self.standalone_scedes_file.split('.')[1]  # derive from the scedes filename which includes scenario between periods
            self.logger.info(f'Standalone scedes file: {self.standalone_scedes_file}')
            self.logger.info(f'Scenario: {scenario}')
            self.logger.info(f'O&G Tech Adjustment: {self.side_case_adj}')

            #Get Low Price flag for drilling eq sensitivity
            if '1' in self.scedes.get('WWOP'):
                self.low_price_case = True

            self.param_ncrl = 0 # avoid feedback loop with static natural gas data as NGMM cannot be run with HSM in offline standalone

        # Integrated Run
        else:
            self.scedes = scedes.copy()

            # Load side case flag from scedes for High Oil and Low Oil Supply Cases
            if (self.scedes.get('OGTECH') == '23'): #HOGS
                self.side_case_adj = 1.5
            elif (self.scedes.get('OGTECH') == '30'): #LOGS
                self.side_case_adj = 0.5
            else:
                self.side_case_adj = 1.0

            self.logger.info('Scenario Flag')
            self.logger.info(self.scedes['SCEN'])
            self.logger.info(self.side_case_adj)

            # Get Low Price flag for drilling eq sensitivity
            if self.scedes['WWOP'] == '1':
                self.low_price_case = True

        if (self.scedes.get('OGTECH') in ['23', '30'] or self.scedes.get('WWOP') in ['1', '3']):
            # For HOGS/LOGS, HP/LP side cases, only benchmark to first STEO year
            self.steo_years = list(range(self.aeo_year - 1, self.aeo_year))

        ###Intermediate variable pre-processing
        #Clear old intermediate variable files
        if (self.current_year == self.history_year) & (self.current_iteration == 1): #Delete Existing pickle files to prevent duplication
            for filename in os.listdir(self.hsm_var_output_path):
                if str('off_reg') in filename or filename == '.gitignore':
                    pass
                else:
                    file_path = os.path.join(self.hsm_var_output_path, filename)
                    try:
                        if os.path.isfile(file_path) or os.path.islink(file_path):
                            os.unlink(file_path)
                        elif os.path.isdir(file_path):
                            shutil.rmtree(file_path)
                    except Exception as e:
                        self.logger.info('Failed to delete %s. Reason: %s' % (file_path, e))
        else: #Delete files from model year - 2 to clear out model bloat
            for filename in os.listdir(self.hsm_var_output_path):
                if str(self.current_year - 2) in filename and filename != '.gitignore':
                    file_path = os.path.join(self.hsm_var_output_path, filename)
                    try:
                        if os.path.isfile(file_path) or os.path.islink(file_path):
                            os.unlink(file_path)
                        elif os.path.isdir(file_path):
                            shutil.rmtree(file_path)
                    except Exception as e:
                        self.logger.info('Failed to delete %s. Reason: %s' % (file_path, e))
                else:
                    pass


        ###Load HSM intermediate variables
        #Because HSM is in Python and Main is in Fortran, Python must read/write local variables between model years
        #This is done using Pickle
        #Different pickle outputs are read in to HSM depending on whether ncrl=1 (if we are running a reporting loop)
        #All non-reporting iterations use the previous model year fcrl=1 iteration variables
        #All reporting iterations use the previous model year ncrl=1 iteration variables
        self.logger.info('Start Load Intermediate Variables')
        if (self.param_ncrl != 1) & (self.integrated_switch == True) & (self.current_year > self.history_year): #Normal loop files
            self.logger.info('Loading FCRL Pickle Vars')
            self.hsm_vars.read_pkl_vars('fcrl', self.current_year, self.hsm_var_output_path)

        elif (self.param_ncrl == 1) & (self.integrated_switch == True) & (self.current_year > self.history_year): #Reporting loop files
            self.logger.info('Loading NCRL Pickle Vars')
            self.hsm_vars.read_pkl_vars('ncrl', self.current_year, self.hsm_var_output_path)
        else:
            self.logger.info('No Pickle Vars read')
            pass

        self.logger.info('End Load Intermediate Variables')


        ###Load History Data
        if (self.current_year == self.history_year) | (self.integrated_switch == False):
            self.logger.info('Load History')
            self.hist_steo.load_history()
            self.hist_steo.overwrite_restart_var_history()
        else:
            pass


        ###Get NGPL history production ratios
        self.hist_steo.calculate_ngpl_region_shares()


        ###Load STEO Data
        if (self.current_year <= (self.steo_years[-1])) | (self.integrated_switch == False):
            self.logger.info('Load STEO')
            self.hist_steo.load_steo_tables()
            self.hist_steo.overwrite_restart_var_steo()
            self.hist_steo.load_steo_adjustments()
            self.hist_steo.aggregate_pmm_output_totals()
        else:
            pass


        ###Setup CashFlow Class for Submodule discounted Cash Flows
        self.logger.info('Setup Cashflow Class')
        if (self.current_year >= self.history_year) | (self.integrated_switch == False):
            temp_filename = self.setup_table.at[nam.state_tax_table, nam.filename]
            cf.CashFlow.state_tax_filename = self.input_path + temp_filename
            temp_filename = self.setup_table.at[nam.depreciation_schedules, nam.filename]
            cf.CashFlow.depreciation_schedule_filename = self.input_path + temp_filename
            cf.CashFlow.setup()


        ###Setup NGP CashFlow Class for NGP  Submodule discounted Cash Flows
        self.logger.info('Setup NGP Cashflow Class')
        if (self.current_year >= self.history_year) | (self.integrated_switch == False):
            temp_filename = self.setup_table.at[nam.depreciation_schedules, nam.filename]
            ngpcf.NGP_CashFlow.depreciation_schedule_filename = self.input_path + temp_filename
            ngpcf.NGP_CashFlow.setup()


        ###Setup Submodules
        if (self.current_year >= self.history_year) | (self.integrated_switch == False):
            self.logger.info('Running Submodule Setup year ' + str(self.current_year))

            #Setup Onshore Submodule
            self.onshore_switch = str(self.setup_table.at[nam.onshore_switch, nam.filename]).upper() == 'True'.upper()
            if self.onshore_switch:
                self.logger.info('Setup Onshore')
                temp_filename = self.setup_table.at[nam.on_setup, nam.filename]
                self.onshore.setup(temp_filename)
            else:
                warnings.warn('Onshore Submodule Turned Off', UserWarning)

            #Setup Offshore Submodule
            self.offshore_switch = str(self.setup_table.at[nam.offshore_switch, nam.filename]).upper() == 'True'.upper()
            if self.offshore_switch:
                self.logger.info('Setup Offshore')
                temp_filename = self.setup_table.at[nam.off_setup, nam.filename]
                self.offshore.setup(temp_filename)
            else:
                warnings.warn('Offshore Submodule Turned Off', UserWarning)

            #Setup Alaska Submodule
            self.alaska_switch = str(self.setup_table.at[nam.alaska_switch, nam.filename]).upper() == 'True'.upper()
            if self.alaska_switch:
                self.logger.info('Setup Alaska')
                temp_filename = self.setup_table.at[nam.ak_setup, nam.filename]
                self.alaska.setup(temp_filename)
            else:
                warnings.warn('Alaska Submodule Turned Off', UserWarning)

            #Setup Canada Submodule
            self.canada_switch = str(self.setup_table.at[nam.canada_switch, nam.filename]).upper() == 'True'.upper()
            if self.canada_switch:
                self.logger.info('Setup Canada')
                temp_filename = self.setup_table.at[nam.can_setup, nam.filename]
                self.canada.setup(temp_filename)
            else:
                warnings.warn('Canada Submodule Turned Off', UserWarning)

            #Setup ngp Submodule
            self.ngp_switch = str(self.setup_table.at[nam.ngp_switch, nam.filename]).upper() == 'True'.upper()
            if self.ngp_switch:
                self.logger.info('Setup NGP Submodule')
                temp_filename = self.setup_table.at[nam.ngp_setup, nam.filename]
                self.ngp.setup(temp_filename)
            else:
                warnings.warn('NGP Submodule Turned Off', UserWarning)

        pass


    def _validate_standalone_files(self):
        """Validate that all required files are present for standalone runs.
        
        Checks for the following required files:
        - dict.txt and varlist.txt in input directory
        - restart.npz, pyfiler1.cp312-win_amd64.pyd, and FILELIST in main directory
        
        Raises
        ------
        FileNotFoundError
            If any required files are missing, with a detailed error message listing all missing files.
        """
        missing_files = []
        
        # Check files in input directory
        required_input_files = ['dict.txt', 'varlist.txt']
        for filename in required_input_files:
            filepath = os.path.join(self.input_path, filename)
            if not os.path.exists(filepath):
                missing_files.append(filepath)
        
        # Check files in main directory
        required_main_files = [
            'restart.npz',
            'pyfiler1.cp312-win_amd64.pyd',
            'FILELIST'
        ]
        for filename in required_main_files:
            filepath = os.path.join(self.main_directory, filename)
            if not os.path.exists(filepath):
                missing_files.append(filepath)
        
        if missing_files:
            error_msg = "STANDALONE RUN FILE VALIDATION FAILED\n"
            error_msg += "The following required files are missing:\n"
            for filepath in missing_files:
                error_msg += f"  - {filepath}\n"
            error_msg += "\nPlease copy the missing files to the appropriate locations before running in standalone mode."
            self.logger.error(error_msg)
            raise FileNotFoundError(error_msg)


    def load_parameters(self):
        """Loads in HSM Parameters.

        Returns
        -------
        self.param_baseyr : int
             Base AEO model year for history (hardcoded as 1990)

        self.param_fcrl : boolean
            Flag to indicate last regular model iteration

        self.param_ncrl : boolean
            Flag to indicate reporting model iteration

        self.param_num_ogsm_region : int
            Number of HSM regions

        self.param_num_yr : int
            Number of model years

        self.param_oiltypes : int
            Number of HSM oil types

        self.param_gastype : int
            Number of HSM gas types

        self.param_onshore_regions : int
            Number of HSM onshore regions

        self.param_offshore_regions : int
            Number of HSM offshore regions
        """
        self.param_baseyr               = self.restart.parametr_baseyr
        self.param_fcrl                 = self.restart.parametr_fcrl
        self.param_ncrl                 = self.restart.parametr_ncrl
        self.param_num_ogsm_regions     = self.restart.parametr_mnumor
        self.param_num_yr               = self.restart.parameter_mnumyr
        self.param_oiltypes             = self.restart.parameter_oiltypes
        self.param_gastypes             = self.restart.parameter_gastypes
        self.param_onshore_regions      = self.restart.parameter_mnl48n
        self.param_offshore_regions     = self.restart.parameter_mnl48f


        #Print key parameters to debug
        self.logger.info('param_fcrl_switch')
        self.logger.info(self.param_fcrl)
        self.logger.info('param_ncrl_switch')
        self.logger.info(self.param_ncrl)


    def process_restart_variables(self):
        """Processes Restart Variables for HSM.

        * Sets current model year variables
        * Sets relevant iterative restart variable values to 0
        * Creates local variables from restart file for use in the submodules

        Returns
        -------
        self.current_year : int
            Current model run year

        self.current_iteration : int
            Current model run iteration

        self.rest_mc_jpgdp : df
            DataFrame of inflation multiplier relative to base hardcoded 1987 prices

        self.rest_mc_rmcorpbaa : df
            Corporate bond rate

        self.rest_mc_rmtcm10y : int
            10-year treasury rate

        self.rest_ogprcng : df
            Natural gas wellhead-type price at district level (1987$/ thouand-cubic-feet)

        self.rest_ogrnagprd : df
            Realized non-associated natural gas production

        self.rest_ogwprng : df
            NG wellhead price (1987$)

        self.rest_rfcrudewhp : df
            Crude oil wellhead price by refinery region and type

        self.rest_ogcruderef : df
            Crude oil production by refinery region and crude type

        self.rest_dcrdwhp : df
            Domestic crude oil wellhead price by region (calculated from reg_crude_price, derived from rfcrudewhp)

        self.rest_rfqtdcrd : df
            Domestic crude oil production by region (including EOR)

        self.rest_start_price : df
            Model starting world oil price

        self.rest_ogpngwhp : df
            National natural gas average welhead price

        self.rest_plginpf : df
            NGPL prices from LFMM

        self.rest_cflgq : df
            NGPL conversion factor

        self.rest_45q_lyr_ret : df
            Year tax credit expire for retrofits

        self.rest_45q_duration : df
            Number of years tax credit apply to plant

        self.rest_ccs_eor_45q : df
            Tax credits for enhanced oil recovery in dollars per metric ton co2

        self.rest_brent_price : df
            Brent price path

        self.rest_hh_price : df
            Henry Hub price path

        """
        ###Years
        # ADS, here self.restart.ncntrl_curcalyr is zero. 
        self.current_year       = int(self.restart.ncntrl_curcalyr)
        self.current_iteration  = int(self.restart.ncntrl_curitr)

        ###Set Current year values to 0 since they are calculated cumulatively across models
        if self.current_year > self.steo_years[0]:
            self.restart.ogsmout_ognowell.at[self.current_year, nam.value] = 0
            self.restart.ogsmout_ogcruderef.loc[self.restart.ogsmout_ogcruderef.index.get_level_values(2).isin([self.current_year])] = 0

        if self.current_year >= self.history_year:
            # In standalone mode, zero all projection years for debugging
            # In integrated mode, only zero the current year being processed
            if self.integrated_switch == False:  # Standalone mode
                # Zero all projection years (history_year + 1 through final_aeo_year)
                projection_years = list(range(self.history_year + 1, self.final_aeo_year + 1))
                
                self.restart.ogsmout_ogenagprd.loc[(slice(None), slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogadgprd.loc[(slice(None), slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogdngprd.loc[(slice(None), slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogqngrep.loc[(slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogregprd.loc[(slice(None), slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogngprd.loc[(slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogngprd_fed.loc[(slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogngprd_nonfed.loc[(slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogcoprd_fed.loc[(slice(None), projection_years), 'value'] = 0
                self.restart.ogsmout_ogcoprd_nonfed.loc[(slice(None), projection_years), 'value'] = 0
                
                if self.current_iteration == 1:
                    self.restart.ogsmout_ogrnagprd.loc[(slice(None), slice(None), projection_years), 'value'] = 0
            else:  # Integrated mode - keep existing behavior
                self.restart.ogsmout_ogenagprd.loc[(slice(None), slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogadgprd.loc[(slice(None), slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogdngprd.loc[(slice(None), slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogqngrep.loc[(slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogregprd.loc[(slice(None), slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogngprd.loc[(slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogngprd_fed.loc[(slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogngprd_nonfed.loc[(slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogcoprd_fed.loc[(slice(None), self.current_year), 'value'] = 0
                self.restart.ogsmout_ogcoprd_nonfed.loc[(slice(None), self.current_year), 'value'] = 0

                if self.current_iteration == 1:
                    self.restart.ogsmout_ogrnagprd.loc[(slice(None), slice(None), self.current_year), 'value'] = 0

        self.logger.info('Reset Restart Variables to 0')

        ###NEMS Restart Variables without any format changes
        self.rest_mc_rmcorpbaa  = self.restart.macout_mc_rmcorpbaa.copy()
        self.rest_mc_rmtcm10y   = self.restart.macout_mc_rmtcm10y.copy()
        self.rest_ogprcng       = self.restart.ngtdmrep_ogprcng.copy()
        self.rest_ogrnagprd     = self.restart.ogsmout_ogrnagprd.copy()
        self.rest_ogwprng       = self.restart.ngtdmrep_ogwprng.copy()
        self.rest_rfcrudewhp    = self.restart.lfmmout_rfcrudewhp.copy()
        self.rest_ogcruderef    = self.restart.ogsmout_ogcruderef.copy()


        ###NEMS Restart variables reformatted
        #Set Domestic Crude Price
        #Note: rest_dcrdwhp is now calculated from reg_crude_price in calculate_prices()
        #Initialize as empty DataFrame - will be populated in calculate_prices()
        self.rest_dcrdwhp = pd.DataFrame()

        #Set Domestic Crude Production
        self.rest_rfqtdcrd = self.restart.pmmout_rfqtdcrd.copy().unstack()
        self.rest_rfqtdcrd.columns = list(range(self.param_baseyr,self.final_aeo_year + 1))

        #International Start Oil Price
        self.rest_start_price   = self.restart.intout_start_price.copy()
        self.rest_start_price.index = list(range(self.param_baseyr,self.final_aeo_year + 1))

        #Natural Gas Average Wellhead Price
        self.rest_ogpngwhp = self.restart.ogsmout_ogpngwhp.copy()
        self.rest_ogpngwhp.index = list(range(self.param_baseyr, self.final_aeo_year + 1))

        #Extra Risk
        self.rest_extrarisk     = self.restart.emission_extrarisk.copy()
        self.rest_extrarisk.index = list(range(self.param_baseyr, self.final_aeo_year + 1))

        #NGPL Prices
        self.rest_plginpf = self.restart.pmore_plginpf.copy()

        #NGPL Conversion Factor
        self.rest_cflgq = self.restart.convfact_cflgq.copy()
        self.rest_cflgq.index = list(range(self.param_baseyr, self.final_aeo_year + 1))

        #45Q Last Eligible Year
        self.rest_45q_lyr_ret = self.restart.tcs45q_i_45q_lyr_ret

        #45Q Duration
        self.rest_45q_duration =self.restart.tcs45q_i_45q_duration

        #45Q Rate
        self.rest_ccs_eor_45q = self.restart.tcs45q_ccs_eor_45q.copy()
        self.rest_ccs_eor_45q.index = list(range(self.param_baseyr, self.final_aeo_year + 1))

        # Play_Map
        self.rest_play_map = self.restart.ogsmout_play_map.reset_index(drop = True)
        self.rest_play_map.index = self.rest_play_map.index + 1


        ###CCATS variables
        self.rest_co2_price_45q = self.restart.ccatsdat_co2_prc_dis_45q.copy()
        self.rest_co2_price_ntc = self.restart.ccatsdat_co2_prc_dis_ntc.copy()


        ###Canada submodule vars
        #Set Brent Price
        self.rest_brent_price       = self.restart.intout_brent_price.copy()
        self.rest_brent_price.index = list(range(self.param_baseyr,self.final_aeo_year + 1))

        #Set Henry Hub Price
        self.rest_hh_price          = self.restart.ngtdmrep_oghhprng.copy()
        self.rest_hh_price.index    = list(range(self.param_baseyr,self.final_aeo_year + 1))

        #Set Inflation index
        self.rest_mc_jpgdp      = self.restart.macout_mc_jpgdp.copy()
        self.rest_mc_jpgdp.index  = list(range(self.base_price_year,self.final_aeo_year + 1))

        ###NGPP submodule vars
        # Electricity price
        self.rest_pelin = self.restart.mpblk_pelin.copy()

        pass


    def run(self, year=None, iteration=None):
        """Runs HSM Model.

            * Sets master model year and iteration variables
            * Performs global calculations for discount rate and prices
            * Runs Submodules

        Parameters
        ----------
        year : int
            Current model year

        iteration : int
            Current model iteration

        Returns
        -------
        self.current_year : df
            Current model year

        self.current_iteration : df
            Current model iteration
        """
        if year is not None:
            warnings.warn('Overriding ncntrl_curcalyr', UserWarning)
            self.current_year = year
        if iteration is not None:
            warnings.warn('Overriding ncntrl_curitr', UserWarning)
            self.current_iteration = iteration

        self.calculate_discount_rate()

        self.calculate_prices()

        if self.onshore_switch:
            self.logger.info('Running Onshore Submodule')
            start_time = time.time()
            self.onshore.run()
            elapsed = time.time() - start_time
            self.onshore_runtime += elapsed
            self.logger.info(f'Onshore Submodule completed in {self.format_time(elapsed)}')
        else:
            self.logger.info('Onshore Submodule Switch Off')

        if self.offshore_switch:
            self.logger.info('Running Offshore Submodule')
            start_time = time.time()
            self.offshore.run()
            elapsed = time.time() - start_time
            self.offshore_runtime += elapsed
            self.logger.info(f'Offshore Submodule completed in {self.format_time(elapsed)}')
        else:
            self.logger.info('Offshore Submodule Switch Off')

        if self.alaska_switch:
            self.logger.info('Running Alaska Submodule')
            start_time = time.time()
            self.alaska.run()
            elapsed = time.time() - start_time
            self.alaska_runtime += elapsed
            self.logger.info(f'Alaska Submodule completed in {self.format_time(elapsed)}')
        else:
            self.logger.info('Alaska Submodule Switch Off')

        if self.canada_switch:
            self.logger.info('Running Canada Submodule')
            start_time = time.time()
            self.canada.run()
            elapsed = time.time() - start_time
            self.canada_runtime += elapsed
            self.logger.info(f'Canada Submodule completed in {self.format_time(elapsed)}')
        else:
            self.logger.info('Canada Submodule Switch Off')

        pass


    def calculate_discount_rate(self):
        """Calculates discount rate to be used in discounted cash flows (**CashFlow.py**).

        Returns
        -------
        self.corp_bond_rate : float
            Corporate bond rate

        self.ten_year_treas_note_yield : float
            Ten-year Treasury note yield

        self.expected_inflation_rate : float
            Expected inflation rate

        self.discount_rate : float
            Discount rate

        """
        # Assign bond and treasury rates
        self.corp_bond_rate = self.rest_mc_rmcorpbaa.loc[self.technology_year:self.final_aeo_year].mean()
        if isinstance(self.corp_bond_rate, pd.Series):
            self.corp_bond_rate = float(self.corp_bond_rate.iloc[0])

        self.ten_year_treas_note_yield = self.rest_mc_rmtcm10y.loc[self.technology_year:self.final_aeo_year].mean()
        if isinstance(self.ten_year_treas_note_yield, pd.Series):
            self.ten_year_treas_note_yield = float(self.ten_year_treas_note_yield.iloc[0])

        #Calculate inflation multiplier
        temp_exp_inf = self.restart.macout_mc_jpgdp.loc[self.technology_year:self.final_aeo_year].to_numpy() / \
            self.restart.macout_mc_jpgdp.loc[self.technology_year - 1:self.final_aeo_year - 1].to_numpy()
        self.expected_inflation_rate = temp_exp_inf.mean()

        #Calculate weighted average cost of capital
        wacc = self.debt_ratio * self.corp_bond_rate * (1.0 - self.fed_tax_rate) + \
            (1.0 - self.debt_ratio) * (self.ten_year_treas_note_yield + self.industry_beta * self.market_risk_premium)
        wacc_real = (1. + wacc / 100.) / self.expected_inflation_rate - 1.
        self.discount_rate = wacc_real + self.rest_extrarisk.at[int(self.current_year), nam.value] + self.return_over_capital_cost

        pass

    def _safe_divide(self, numerator, denominator, fill_value=np.nan):
        """Safely divide two DataFrames/Series, handling zero denominators.
        
        Parameters
        ----------
        numerator : pd.DataFrame or pd.Series
            Numerator values
        denominator : pd.DataFrame or pd.Series
            Denominator values
        fill_value : float, default np.nan
            Value to use when denominator is zero or NaN
            
        Returns
        -------
        pd.DataFrame or pd.Series
            Result of division with zero/NaN protection
        """
        # Handle zero/NaN denominators with fill_value
        return numerator.div(denominator, fill_value=fill_value).replace([np.inf, -np.inf], fill_value)

    def _calculate_production_weighted_price(self, prices, production, groupby_level):
        """Calculate production-weighted average price with NaN/zero handling.
        
        This function computes a production-weighted average price by:
        1. Multiplying prices by production to get price-weighted production
        2. Summing by group
        3. Dividing by total production (with zero protection)
        
        Parameters
        ----------
        prices : pd.DataFrame
            Price data indexed by (region/type, ...)
        production : pd.DataFrame
            Production data with same index structure as prices
        groupby_level : str or list
            Level(s) to group by for aggregation
            
        Returns
        -------
        pd.Series or pd.DataFrame
            Production-weighted average prices
        """
        # Calculate production-weighted average price
        price_weighted_prod = prices * production
        price_weighted_sum = price_weighted_prod.groupby(groupby_level).sum()
        total_production = production.groupby(groupby_level).sum()
        # Zero production results in NaN
        weighted_price = self._safe_divide(price_weighted_sum, total_production, fill_value=np.nan)
        
        return weighted_price

    def _process_natural_gas_prices(self):
        """Process natural gas prices and production by district and region.
        
        This method:
        1. Formats natural gas production by type and district
        2. Aggregates production to regions
        3. Calculates production-weighted regional prices from district data
        4. Handles special cases (Alaska region mapping, regions without production)
        
        Sets the following instance attributes:
        - self.dist_natgas_type_prod: Natural gas production by district and type
        - self.dist_natgas_prod: Total natural gas production by district
        - self.reg_natgas_prod: Total natural gas production by region
        - self.dist_natgas_price: Natural gas price by district
        - self.reg_natgas_price: Natural gas price by region (updated with calculated values)
        """
        # Format natural gas production by type by district
        self.dist_natgas_type_prod = self.rest_ogrnagprd.unstack(2).copy()
        self.dist_natgas_type_prod.columns = self.dist_natgas_type_prod.columns.droplevel(0)
        self.dist_natgas_type_prod.index.set_names([nam.district_number, nam.gas_type_number], inplace=True)

        # Get total natural gas production by district (sum across param_gastypes)
        self.dist_natgas_prod = self.dist_natgas_type_prod.loc(axis=0)[:, self.param_gastypes].copy()
        self.dist_natgas_prod = self.dist_natgas_prod.droplevel(nam.gas_type_number)

        # Aggregate production to region using mapping table
        self.reg_natgas_prod = pd.merge(
            self.dist_natgas_prod,
            self.mapping[[nam.district_number, nam.region_number]],
            on=nam.district_number
        )
        self.reg_natgas_prod = self.reg_natgas_prod.groupby(nam.region_number).sum()
        self.reg_natgas_prod = self.reg_natgas_prod.drop(nam.district_number, axis=1)

        # Format natural gas prices by district
        self.dist_natgas_price = self.rest_ogprcng.unstack(1).copy()
        self.dist_natgas_price.columns = self.dist_natgas_price.columns.droplevel(0)
        self.dist_natgas_price.index.set_names(nam.district_number, inplace=True)

        # Format natural gas prices by region (from restart file)
        self.reg_natgas_price = self.rest_ogwprng.unstack(1).copy()
        self.reg_natgas_price.columns = self.reg_natgas_price.columns.droplevel(0)
        self.reg_natgas_price.index.set_names(nam.region_number, inplace=True)
        # Remove total region
        self.reg_natgas_price = self.reg_natgas_price.drop([self.param_num_ogsm_regions])

        # Calculate production-weighted regional prices from district data
        price_weighted_prod = self.dist_natgas_prod * self.dist_natgas_price
        price_weighted_prod = pd.merge(
            price_weighted_prod,
            self.mapping[[nam.district_number, nam.region_number]],
            on=nam.district_number
        )
        price_weighted_prod = price_weighted_prod.groupby(nam.region_number).sum()
        
        # Calculate weighted average price (zero production results in NaN)
        calculated_reg_price = self._safe_divide(price_weighted_prod, self.reg_natgas_prod, fill_value=np.nan)
        calculated_reg_price = calculated_reg_price.drop(nam.district_number, axis=1, errors='ignore')
        
        # Alaska region 11 uses same price as region 13
        calculated_reg_price.loc[11] = calculated_reg_price.loc[13]
        
        # Fill NaN regions with previous values from restart file (avoids .update())
        self.reg_natgas_price = calculated_reg_price.where(calculated_reg_price.notna(), self.reg_natgas_price)

    def _process_crude_prices(self):
        """Process crude oil prices and production by LFMM region, district, and HSM region.
        
        This method:
        1. Formats crude prices and production by type and LFMM region
        2. Handles NaN/zero values in rfcrudewhp and ogcruderef
        3. Maps prices from LFMM regions to districts and HSM regions
        4. Calculates production-weighted average prices
        5. Updates restart variable pmmout_dcrdwhp
        
        Sets the following instance attributes:
        - self.lfmm_reg_crude_type_price: Crude price by LFMM region and type
        - self.lfmm_reg_crude_type_prod: Crude production by LFMM region and type
        - self.dist_crude_type_price: Crude price by district and type
        - self.lfmm_reg_crude_price: Type-averaged crude price by LFMM region
        - self.dist_crude_price: Type-averaged crude price by district
        - self.reg_crude_price: Type-averaged crude price by HSM region
        - self.rest_dcrdwhp: Domestic crude wellhead price by region (for restart file)
        """
        # Format crude prices by type by LFMM region
        self.lfmm_reg_crude_type_price = self.rest_rfcrudewhp.unstack(2).copy()
        self.lfmm_reg_crude_type_price.columns = self.lfmm_reg_crude_type_price.columns.droplevel(0)
        self.lfmm_reg_crude_type_price.index.set_names([nam.lfmm_region_number, nam.crude_type_number], inplace=True)

        # Format crude production by type by LFMM region
        self.lfmm_reg_crude_type_prod = self.rest_ogcruderef.unstack(2).copy()
        self.lfmm_reg_crude_type_prod.columns = self.lfmm_reg_crude_type_prod.columns.droplevel(0)
        self.lfmm_reg_crude_type_prod.index.set_names([nam.lfmm_region_number, nam.crude_type_number], inplace=True)

        # Handle NaN/zero: replace zero production with NaN (avoids division issues)
        # Replace NaN prices with 0 (zero price is valid)
        self.lfmm_reg_crude_type_prod = self.lfmm_reg_crude_type_prod.replace(0, np.nan)
        self.lfmm_reg_crude_type_price = self.lfmm_reg_crude_type_price.fillna(0)
        
        # Use current_year - 1 production values for all years (fixes issue where rest_ogcruderef
        # gets zeroed out at beginning of each year in integrated mode)
        weight_year = self.current_year - 1
        if weight_year in self.lfmm_reg_crude_type_prod.columns:
            # Extract the previous year's production values
            prev_year_prod = self.lfmm_reg_crude_type_prod[weight_year]
            # Replace all columns with previous year's production values
            self.lfmm_reg_crude_type_prod = pd.DataFrame(
                {col: prev_year_prod for col in self.lfmm_reg_crude_type_prod.columns},
                index=self.lfmm_reg_crude_type_prod.index
            )

        # Map crude prices from LFMM regions to districts
        self.dist_crude_type_price = pd.merge(
            self.mapping[[nam.district_number, nam.lfmm_region_number]],
            self.lfmm_reg_crude_type_price.reset_index(),
            on=nam.lfmm_region_number
        )
        self.dist_crude_type_price = self.dist_crude_type_price.drop(nam.lfmm_region_number, axis=1)
        self.dist_crude_type_price = self.dist_crude_type_price.set_index([nam.district_number, nam.crude_type_number])

        # Calculate type-averaged crude prices by LFMM region (production-weighted)
        self.lfmm_reg_crude_price = self._calculate_production_weighted_price(
            self.lfmm_reg_crude_type_price,
            self.lfmm_reg_crude_type_prod,
            nam.lfmm_region_number
        )

        # Map type-averaged crude prices from LFMM regions to districts
        self.dist_crude_price = pd.DataFrame(
            self.mapping[[nam.district_number, nam.lfmm_region_number]]
        )
        self.dist_crude_price = pd.merge(
            self.dist_crude_price,
            self.lfmm_reg_crude_price,
            on=nam.lfmm_region_number
        )
        self.dist_crude_price = self.dist_crude_price.set_index(nam.district_number)
        self.dist_crude_price = self.dist_crude_price.sort_index()
        self.dist_crude_price = self.dist_crude_price.drop(nam.lfmm_region_number, axis=1)

        # Map type-averaged crude prices from LFMM regions to HSM regions
        self.reg_crude_price = pd.DataFrame(
            self.reg_natgas_price.reset_index()[nam.region_number]
        )
        self.reg_crude_price = pd.merge(
            self.reg_crude_price,
            self.mapping[[nam.region_number, nam.lfmm_region_number + '_old']],
            on=nam.region_number
        )
        self.reg_crude_price = pd.merge(
            self.reg_crude_price,
            self.lfmm_reg_crude_price,
            left_on=nam.lfmm_region_number + '_old',
            right_on=nam.lfmm_region_number
        )
        self.reg_crude_price = self.reg_crude_price.drop(nam.lfmm_region_number + '_old', axis=1)
        # Use mean() if multiple LFMM regions map to same HSM region
        self.reg_crude_price = self.reg_crude_price.groupby(nam.region_number).mean()

        # Populate rest_dcrdwhp from reg_crude_price (replaces old restart file value)
        self.rest_dcrdwhp = self.reg_crude_price.copy()

        # Calculate region 14 as production-weighted average of regions 1-13
        regions_1_13 = list(range(1, 14))
        temp_prod = self.rest_rfqtdcrd.loc[regions_1_13].copy()
        
        # Calculate production weights (handle zero total production)
        total_prod = temp_prod.sum(axis=0)
        temp_prod_weights = temp_prod.div(total_prod, axis=1)
        temp_prod_weights = temp_prod_weights.replace([np.inf, -np.inf, np.nan], 0)
        
        # Calculate weighted average price for region 14
        region_14_price = temp_prod_weights.mul(self.rest_dcrdwhp, axis=0).sum(axis=0)
        region_14_price.name = 14

        # Add region 14 to rest_dcrdwhp
        self.rest_dcrdwhp = pd.concat([self.rest_dcrdwhp, region_14_price.to_frame().T])

        # Update pmmout_dcrdwhp restart variable with calculated values
        temp_restart_update = self.rest_dcrdwhp.stack()
        temp_restart_update.index.names = self.restart.pmmout_dcrdwhp.index.names

        common_indices = temp_restart_update.index.intersection(self.restart.pmmout_dcrdwhp.index)
        if len(common_indices) > 0:
            self.restart.pmmout_dcrdwhp.loc[common_indices, nam.value] = temp_restart_update.loc[common_indices]

    def calculate_prices(self):
        """Aggregates and shares out crude type and natural gas prices to regions and districts.

        The calculation of regional prices should be aggregated up from the district, crude type level, currently
        crude production by type and district is not output by OGSM, so the LFMM regions are directly converted to
        OGSM/HSM regions.

        Orchestrates price calculations via helper methods with NaN/zero handling.

        Returns
        -------
        self.dist_natgas_type_prod
            Natural gas production by HSM district and type

        self.dist_natgas_prod
            Natural gas production by HSM district

        self.dist_natgas_price
            Natural gas price by HSM district

        self.reg_natgas_price
            Natural gas price by HSM region

        self.lfmm_reg_crude_type_price
            Crude oil price by type and LFMM region

        self.lfmm_reg_crude_type_prod
            Crude oil production by type and LFMM region

        self.dist_crude_type_price
            Crude oil price by type and HSM district

        self.reg_crude_price
            Crude oil price by HSM region

        """
        # Process natural gas prices (district-to-region aggregation)
        self._process_natural_gas_prices()

        # Process crude oil prices (LFMM→district→HSM region mapping, NaN/zero handling)
        self._process_crude_prices()


    def prepare_results(self):
        """Prepare results for writing to the restart file.

            * Prepares results by submodule
            * Then aggregates results across submodule
            * Then benchmarks to STEO if model year in STEO years
            * Then adjusts results to match realized natural gas production from NGMM if ncrl = 1
            * Then writes out intermediate pickles and write results to the restart file

        Returns
        -------
        self.restart.ogsmout_ogprcexp : df
            HSM price expectation adj (deprecated)

        self.restart.ogsmout_oggrowfac : df
            HSM growth factor (deprecated)
        """
        start_time = time.time()
        
        #Set Price Expectation Adjustment
        #self.restart.ogsmout_ogprcexp.at[(self.current_year,), nam.value] = 1.00

        #Set OG Growth Factor
        #self.restart.ogsmout_oggrowfac.at[self.current_year, nam.value] = 1.00
        self.restart.ogsmout_oggrowfac.iloc[self.current_year-self.param_baseyr, 0] = 1.00

        if self.onshore_switch:
            self.logger.info('Onshore Prepare Results')
            self.onshore.report_results_unf()

        if self.offshore_switch:
            self.logger.info('Offshore Prepare Results')
            self.offshore.report_results_unf()

        if self.alaska_switch:
            self.logger.info('Alaska Prepare Results')
            self.alaska.report_results_unf()

        if self.canada_switch:
            self.logger.info('Canada Prepare Results')
            self.canada.report_results_unf()

        #Calculate outputs that are aggregated across submodules
        if self.current_year >= self.history_year:
            self.logger.info('Aggregate Results Across Submodules')
            self.prep_results.aggregate_results_across_submodules()

        # Apply STEO fixed benchmarks for first two STEO years
        # Overwrites model-calculated production (crude oil, natural gas, NGPL) with STEO forecast values
        # Always runs for first two STEO years (steo_years[0:2]) - no flag required
        # Excludes the last STEO year (steo_years[2]) from benchmarking
        # Note: In side cases (HOGS/LOGS, HP/LP), steo_years is set to only the first STEO year, so this runs only for that year
        if self.current_year in self.steo_years[0:2]: # Don't benchmark to last STEO year (i.e. in AEO2025, that's 2026)
            self.logger.info('STEO Fixed Benchmarks (First Two STEO Years)')
            self.prep_results.apply_steo_fixed_benchmarks()

        #Apply Side Case STEO Overwrites
        # For variables not in apply_steo_fixed_benchmarks()
        # Only runs for side cases in first STEO year when side_case_steo_overwrite_switch == True
        is_side_case = (self.scedes.get('OGTECH') in ['23', '30'] or self.scedes.get('WWOP') in ['1', '3'])
        if (self.side_case_steo_overwrite_switch == True) & (self.current_year == self.steo_years[0]) & is_side_case:
            self.logger.info('Side Case STEO Overwrites (First STEO Year)')
            self.prep_results.apply_side_case_steo_overwrites()

        # Apply Realized Production Ratio Adjustment
        # Skip for side cases in first STEO year only to avoid interfering with side case STEO overwrites
        if not (is_side_case & (self.current_year == self.steo_years[0])):
            if (self.current_year >= self.steo_years[0]) & (self.param_ncrl == 1):
                # Ratio adjustment: scale all gas types proportionally for all integrated years
                # All years >= steo_years[0] are treated the same with ratio adjustment
                # Years > steo_years[0] additionally get shale gas plays adjustment
                self.logger.info('Apply Ratio Adjustment to Realized Prod')
                self.prep_results.adjust_production(mode='year_one')
            elif (self.current_year >= self.steo_years[0]) & (self.integrated_switch == False):
                # Standalone mode: same updates as integrated (ogqngrep, ogdngprd, fed/nonfed,
                # ogngprd, ogregprd, first-STEO-year OGQSHLGAS) using expected NA (ogenagprd)
                self.logger.info('Update regional natural gas production for standalone mode')
                self.prep_results.adjust_production(mode='standalone')
            

        #Calculate outputs that are aggregated across submodules
        if self.current_year >= self.history_year:
            self.logger.info('Aggregate Results Across Submodules')
            self.prep_results.aggregate_results_across_submodules()

        elapsed = time.time() - start_time
        self.prepare_results_runtime += elapsed
        self.logger.info(f'Prepare Results completed in {self.format_time(elapsed)}')

    def run_ngp(self):
        """Assess HSM results to produce Natural Gas Procesing Facility CO2 supply.

        Returns
        -------

        """
        if self.ngp_switch:
            self.logger.info('Running NGP Submodule')
            start_time = time.time()
            self.ngp.run()
            elapsed = time.time() - start_time
            self.ngp_runtime += elapsed
            self.logger.info(f'NGP Submodule completed in {self.format_time(elapsed)}')
        else:
            pass


    def write_pkl(self):
        """Write local variables to PKL

        Returns
        -------

        """
        ###Write intermediate variables
        #Different pickle outputs are written depending on whether fcrl = 0, fcrl = 1, or ncrl=1 (is this a reporting loop)
        #If fcrl=0, do not write pickle outputs
        #If fcrl=1 and ncrl=0, write fcrl pickle outputs
        #If ncrl=1, write ncrl pickle outputs
        if self.integrated_switch == True:
            if (self.param_fcrl == 1) & (self.param_ncrl != 1): # Last regular integration loop
                self.logger.info('Start Writing HSM Intermediate Variables to FCRL PKL')
                self.hsm_vars.write_pkl_variables('fcrl', self.current_year, self.hsm_var_output_path)
                self.logger.info('End Writing HSM Intermediate Variables to FCRL PKL')
            elif self.param_ncrl == 1:  #Reporting Loop
                self.logger.info('Start Writing HSM Intermediate Variables to NCRL PKL')
                self.hsm_vars.write_pkl_variables('ncrl', self.current_year, self.hsm_var_output_path)
                self.logger.info('End Writing HSM Intermediate Variables to NCRL PKL')
            else:
                self.logger.info('No PKL produced in first iterations of history year')
                pass

        pass

    def reset_timing(self):
        """Reset all timing variables to zero.
        
        Returns
        -------
        None
        """
        self.onshore_runtime = 0.0
        self.offshore_runtime = 0.0
        self.alaska_runtime = 0.0
        self.canada_runtime = 0.0
        self.prepare_results_runtime = 0.0
        self.ngp_runtime = 0.0

    def format_time(self, seconds):
        """Format elapsed time in seconds to a human-readable string.

        Parameters
        ----------
        seconds : float
            Elapsed time in seconds

        Returns
        -------
        str
            Formatted time string
        """
        if seconds < 60:
            return f"{seconds:.2f} seconds"
        elif seconds < 3600:
            minutes = int(seconds // 60)
            secs = seconds % 60
            return f"{minutes}m {secs:.1f}s"
        else:
            hours = int(seconds // 3600)
            minutes = int((seconds % 3600) // 60)
            secs = seconds % 60
            return f"{hours}h {minutes}m {secs:.0f}s"

    def get_timing_summary(self):
        """Get a formatted summary of all timing data.

        Returns
        -------
        str
            Formatted timing summary table
        """
        summary = "\n" + "="*50 + "\n"
        summary += "HSM Runtime Summary\n"
        summary += "="*50 + "\n"
        
        if self.onshore_runtime > 0:
            summary += f"Onshore Submodule:     {self.format_time(self.onshore_runtime)}\n"
        if self.offshore_runtime > 0:
            summary += f"Offshore Submodule:    {self.format_time(self.offshore_runtime)}\n"
        if self.alaska_runtime > 0:
            summary += f"Alaska Submodule:      {self.format_time(self.alaska_runtime)}\n"
        if self.canada_runtime > 0:
            summary += f"Canada Submodule:      {self.format_time(self.canada_runtime)}\n"
        if self.prepare_results_runtime > 0:
            summary += f"Prepare Results:       {self.format_time(self.prepare_results_runtime)}\n"
        if self.ngp_runtime > 0:
            summary += f"NGP Submodule:         {self.format_time(self.ngp_runtime)}\n"
        
        total = (self.onshore_runtime + self.offshore_runtime + 
                self.alaska_runtime + self.canada_runtime + 
                self.prepare_results_runtime + self.ngp_runtime)
        
        if total > 0:
            summary += "-"*50 + "\n"
            summary += f"Total Submodule Time:  {self.format_time(total)}\n"
        
        summary += "="*50 + "\n"
        
        return summary

