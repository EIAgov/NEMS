"""Module for reading in historical data and STEO forecasts, and applying this data to Restart variables.

This module reads in historical data and STEO forecasts, reformats the data to match the restart file, and updates the
restart variables. The module operates as follows:

    1. Load in historical data in *load_history* and reformat to match the restart variables.
       NGPLS are converted from OES NGPL regional totals to HSM regional totals

    2. Overwrite restart variables with history in *overwrite_restart_var_history*.

    3. Get historical shares of NGPLs from natural gas production in *calculate_ngpl_region_shares* to be used in STEO
       NGPL calculations.

    4. Load in STEO forecast data in *load_steo_tables* and reformat to match the restart variables, apply NGPL shares calculated
       in *calculate_ngpl_region_shares* here.

    5. Overwrite select restart variables with history in *overwrite_restart_var_steo*.

    6. Aggregate updates to history and steo in "total" or "national" value rows for PMM output tables in *aggregate_pmm_output_totals*.


Input Files
-----------
History input files are mostly derived from the OGSM_Database SAS preprocessor input file, while STEO Input files are
derived from Python preprocessors.


**History Input Files**
    * hist_rfqtdcrd.csv - Historical crude oil production by HSM region
    * hist_ogqshloil.csv - Historical crude oil production by play
    * hist_ogqshlgas.csv - Historical natural gas production by play
    * hist_ngplprd.csv - Historical NGPL production by US state
    * hist_htowells.csv - Historical successful tight oil wells drilled by HSM district
    * hist_htoilprd.csv - Historical tight oil production by HSM district
    * hist_htgwells.csv - Historical successful tight natural gas wells drilled by HSM district
    * hist_htgasprd.csv - Historical tight natural gas production by HSM district
    * hist_htadgprd.csv - Historical tight associated-dissolved natrual gas production by HSM district
    * hist_hsgwells.csv - Historical successful shale gas wells drilled by HSM district
    * hist_hsgasprd.csv - Historical successful shale natural gas wells drilled by HSM district
    * hist_hdryholes.csv - Historical dryholes by HSM district
    * hist_hcowells.csv - Historical successful non-tight oil wells drilled by HSM district
    * hist_hcoilprd.csv - Historical non-tight oil production by HSM district
    * hist_hcgwells.csv - Historical successful non-tight natural gas wells drilled by HSM district
    * hist_hcgasprd.csv - Historical non-tight natural gas production by HSM district
    * hist_hcbmwells.csv - Historical successful coalbed methane natural gas wells drilled by HSM district
    * hist_hcbmprd.csv - Historical coalbed methane natural gas production by HSM district
    * hist_hcadgprd.csv - Historical conventional associated dissolved natural gas production by HSM district
    * hist_ognowell.csv - Historical wells drilled
    * hist_eplp.csv - Historical pentanes production
    * hist_epllpa.csv - Historical propane production
    * hist_epllea.csv - Historical ethane production
    * hist_epllban.csv - Historical butane production
    * hist_epllbai.csv - Historical isobutane production
    * hist_epl2.csv - Historical NGPL production
    * hist_dcrdwhp.csv - Historical crude oil wellhead prices
    * hist_eor_prod.csv - Historical EOR crude oil production
    * hist_lfmm_prod.csv - Historical LFMM region and crude type crude oil production


**STEO Input Files**
    * steo_dcrdwhp.csv - STEO year domestic crude oil wellhead price by region
    * steo_ogngplbu.csv - STEO year butane production by HSM district
    * steo_ogngplet.csv - STEO year ethane production by HSM district
    * steo_ogngplis.csv - STEO year isobutane production by HSM district
    * steo_ogngplpp.csv - STEO year pentanes production by HSM district
    * steo_ogngplpr.csv - STEO year propane production by HSM district
    * steo_ogngplprd.csv - STEO year NGPL production by HSM district
    * steo_rfqtdcrd.csv - STEO year total crude production by HSM region
    * steo_togqshlgas.csv - STEO year natural gas production by select tight oil play
    * steo_togqshloil.csv - STEO year crude oil production by select tight oil play
    * steo_ogenagprd.csv - STEO year natural gas expected production by natural gas type and HSM district
    * steo_ogadgprd.csv - STEO year natural gas associated dissolved production by oil type and HSM district
    * steo_can.csv - STEO year natural gas production by HSM Canada region and type


Model Functions and Class Methods
_________________________________

class (sub.Submodule) - History and STEO submodule for HSM

    * __init__ - Constructor to initialize History and STEO submodule
    * load_history - Reads in history data from DI benchmarked to EIA reporting and reformats data for write to restart file
    * overwrite_restart_var_history - Overwrites Restart variables history
    * calculate_ngpl_region_shares - Breakout EIA History NGPL reporting for STEO years
    * load_steo_tables - Load in STEO benchmark values
    * overwrite_restart_var_steo - Overwrites Restart variables with STEO variables for PMMOUT variables used to calculate price
    * load_steo_adjustments - Placeholder method (kept for backward compatibility, no longer loads data)
    * aggregate_pmm_output_totals - Aggregate updates to history and steo in "total" or "national" value rows for PMM output tables


Output Debug Files
__________________
None


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
    * ogsmout_ogprcoak - Crude oil production by Alaska region
    * ogsmout_ogeorprd - CO2 EOR crude oil production


**Natural Gas Restart Variables**
    * self.restart.ogsmout_ogenagprd - Natural gas expected production by natural gas type and HSM district
    * self.restart.ogsmout_ogrnagprd - Natural gas realized production by natural gas type and HSM district
    * self.restart.ogsmout_ogadgprd - Natural gas associated dissolved production by oil type and HSM district
    * self.restart.ogsmout_ogqshlgas - Natural gas production by select natural gas play
    * self.restart.ogsmout_ogqngrep - Natural gas production by natural gas type


**Crude Oil and Natural Gas Production Restart Variable**
    * self.restart.ogsmout_ogregprd - Total crude oil and natural gas production by production type


**Well Restart Variables**
    * self.restart.ogsmout_ogogwells - Total wells
    * self.restart.ogsmout_ognowell - Total completed wells


**NGPL Production Restart Variables**
    * self.restart.ogsmout_ogngplprd - NGPL production by HSM district
    * self.restart.ogsmout_ogngplet - Ethane production by HSM district
    * self.restart.ogsmout_ogngplpr - Propane production by HSM district
    * self.restart.ogsmout_ogngplbu - Butane production by HSM district
    * self.restart.ogsmout_ogngplis - Isobutane production by HSM district
    * self.restart.ogsmout_ogngplpp - Pentanes production by HSM district


History/STEO Submodule Class Methods
____________________________________

"""
import names as nam
import pandas as pd
import common as com
import submodule as sub
import numpy as np


class hist_steo(sub.Submodule):
    def __init__(self, parent):

        super().__init__(parent, submodule_name='hist_steo')

        #History Variables
        self.hist_rfqtdcrd      = pd.DataFrame()  # HISTORICAL DOMESTIC CRUDE OIL PROD INCLUDING EOR
        self.hist_ogqshloil     = pd.DataFrame()  # HISTORICAL TIGHT OIL PRODUCTION FOR SELECT PLAYS
        self.hist_ogqshlgas     = pd.DataFrame()  # HISTORICAL SHALE GAS PRODUCTION FOR SELECT PLAYS
        self.hist_ngplprd       = pd.DataFrame()  # HISTORICAL NGPL PRODUCTION (MMB)
        self.hist_htowells      = pd.DataFrame()  # HISTORICAL TIGHT OIL WELLS DRILLED
        self.hist_htoilprd      = pd.DataFrame()  # HISTORICAL TIGHT CRUDE OIL PRODUCTION
        self.hist_htgwells      = pd.DataFrame()  # HISTORICAL TIGHT GAS WELLS DRILLED
        self.hist_htgasprd      = pd.DataFrame()  # HISTORICAL NA TIGHT GAS PRODUCTION
        self.hist_htadgprd      = pd.DataFrame()  # HISTORICAL Conventional AD Gas production
        self.hist_htadjprd      = pd.DataFrame()  # HISTORICAL TIGHT AD GAS PRODUCTION
        self.hist_hsgwells      = pd.DataFrame()  # HISTORICAL SHALE GAS WELLS DRILLED
        self.hist_hsgasprd      = pd.DataFrame()  # HISTORICAL NA SHALE GAS PRODUCTION
        self.hist_hdryholes     = pd.DataFrame()  # HISTORICAL DRY HOLES DRILLED
        self.hist_hcowells      = pd.DataFrame()  # HISTORICAL NON-TIGHT OIL WELLS DRILLED
        self.hist_hcoilprd      = pd.DataFrame()  # HISTORICAL NON-TIGHT CRUDE OIL PRODUCTION
        self.hist_hcgwells      = pd.DataFrame()  # HISTORICAL CONVENTIONAL GAS WELLS DRILLED
        self.hist_hcgasprd      = pd.DataFrame()  # HISTORICAL NA CONVENTIONAL GAS PRODUCTION
        self.hist_hcbmwells     = pd.DataFrame()  # HISTORICAL CBM WELLS DRILLED
        self.hist_hcbmprd       = pd.DataFrame()  # HISTORICAL NA COALBED METHANE PRODUCTION
        self.hist_hcadgprd      = pd.DataFrame()  # HISTORICAL CONVENTIONAL AD GAS PRODUCTION
        self.hist_ognowell      = pd.DataFrame()  # HISTORICAL WELL COUNT (MANUALLY SET SEPARATELY OF HSM DATABASE)
        self.hist_eplp          = pd.DataFrame()  # HISTORICAL PENTANES PRODUCTION
        self.hist_epllpa        = pd.DataFrame()  # HISTORICAL PROPANE PRODUCTION
        self.hist_epllea        = pd.DataFrame()  # HISTORICAL ETHANE PRODUCTION
        self.hist_epllban       = pd.DataFrame()  # HISTORICAL BUTANE PRODUCTION
        self.hist_epllbai       = pd.DataFrame()  # HISTORICAL ISOBUTANE PRODUCTION
        self.hist_epl2          = pd.DataFrame()  # OFFICIAL HISTORICAL NGPL COMPOSITION (MBBL/D)
        self.hist_dcrdwhp       = pd.DataFrame()  # HISTORICAL CRUDE OIL WELLHEAD PRICES AND PRODUCTION
        self.hist_eor_prod      = pd.DataFrame()  # EOR PRODUCTION (BBL/D)
        self.hist_lfmm_prod     = pd.DataFrame()  # CRUDE OIL PRODUCTION BY TYPE AND LFMM REGION (MBBl/D)
        self.ngpl_shares        = pd.DataFrame()  #: dataframe of ngpl shares by region based on historical production

        #STEO Variables
        self.steo_dcrdwhp       = pd.DataFrame()  #Average Wellhead Price (nominal $/brl)
        self.steo_ogngplbu      = pd.DataFrame()  #Butane Production (MBBL/D)
        self.steo_ogngplet      = pd.DataFrame()  #Ethane Production (MBBL/D)
        self.steo_ogngplis      = pd.DataFrame()  #Isobutane Production (MBBL/D)
        self.steo_ogngplpp      = pd.DataFrame()  #Pentanes Production (MBBL/D)
        self.steo_ogngplpr      = pd.DataFrame()  #Propane Production (MBBL/D)
        self.steo_ogngplprd     = pd.DataFrame()  #NGPL Production (MBBL/D)
        self.steo_rfqtdcrd      = pd.DataFrame()  #Domestic Crude Oil Production (MMBBL/D)
        self.steo_togqshlgas    = pd.DataFrame()  #Natural Gas Play-level Production, tcf for the first year and adjustment factor for the last two years
        self.steo_togqshloil    = pd.DataFrame()  #Crude Oil Play-level Production, MMBBL/D for the first year and adjustment factor for the last two years
        self.steo_ogenagprd     = pd.DataFrame()  #Natural Gas Expected NA Production (BCF)
        self.steo_ogadgprd      = pd.DataFrame()  #Natural Gas Expected AD Production (BCF)
        self.steo_can           = pd.DataFrame()  #Expected Canada Natural Gas Production (BCF)

        self.hist_ogcoprd_fed   = pd.DataFrame()  #Historical crude production on federal lands
        self.hist_ogngprd_fed   = pd.DataFrame()  #Historical natural gas production on federal lands


    ###History###

    def load_history(self):
        """Reads in history data from Enverus benchmarked to EIA reporting and reformats data for write to restart file.

        Returns
        -------
        self.hist_rfqtdcrd : df
            DataFrame of historical total crude production by HSM region

        self.hist_ogqshloil : df
            DataFrame of crude oil production by select tight oil play

        self.hist_ogqshlgas : df
            DataFrame of natural gas production by select natural gas play

        self.hist_ngplprd : df
            DataFrame of historical NGPL production by state

        self.hist_htowells : df
            DataFrame of historical successful tight oil wells drilled by HSM district

        self.hist_htoilprd : df
            DataFrame of historical tight oil production by HSM district

        self.hist_htgwells : df
            DataFrame of historical successful tight natural gas wells drilled by HSM district

        self.hist_htgasprd : df
            DataFrame of historical tight natrual gas production by HSM district

        self.hist_htadgprd : df
            DataFrame of historical tight associated-dissolved natrual gas production by HSM district

        self.hist_hsgwells : df
            DataFrame of historical successful shale gas wells drilled by HSM district

        self.hist_hsgasprd : df
            DataFrame of historical successful shale natural gas wells drilled by HSM district

        self.hist_hdryholes : df
            DataFrame of historical dryholes by HSM district

        self.hist_hcowells : df
            DataFrame of historical successful non-tight oil wells drilled by HSM district

        self.hist_hcoilprd : df
            DataFrame of historical non-tight oil production by HSM district

        self.hist_hcgwells : df
            DataFrame of historical successful non-tight natural gas wells drilled by HSM district

        self.hist_hcgasprd : df
            DataFrame of historical non-tight natural gas production by HSM district

        self.hist_hcbmwells : df
            DataFrame of historical successful coalbed methane natural gas wells drilled by HSM district

        self.hist_hcbmprd : df
            DataFrame of historical coalbed methane natural gas production by HSM district

        self.hist_hcadgprd : df
            DataFrame of historical conventional associated dissolved natural gas production by HSM district

        self.hist_ognowell : df
            DataFrame of historical wells drilled

        self.hist_eplp : df
            DataFrame of historical pentanes production

        self.hist_epllpa : df
            DataFrame of historical propane production

        self.hist_epllea : df
            DataFrame of historical ethane production

        self.hist_epllban : df
            DataFrame of historical butane production

        self.hist_epllbai : df
            DataFrame of historical isobutane production

        self.hist_epl2 : df
            DataFrame of total historical NGPL production

        self.hist_eor_prod : df
            DataFrame of historical EOR crude oil production

        self.hist_lfmm_prod : df
            DataFrame of historical LFMM region and crude type crude oil production

        self.hist_dcrdwhp : df
            DataFrame of historical crude oil wellhead prices

        Notes
        -----
        """
        ###Read in Historical Setup Table
        temp_filename = self.parent.setup_table.at[nam.hist_setup, nam.filename]
        self.parent.hist_setup_table = com.read_dataframe(self.parent.input_path + temp_filename, index_col=0)

        ###Crude Oil Production
        # Historical domestic crude oil production including EOR (RFQTDCRD) - write to RFQTDCRD
        temp_filename = self.parent.hist_setup_table.at[nam.hist_rfqtdcrd, nam.filename]
        self.hist_rfqtdcrd = com.read_dataframe(self.parent.hist_input_path + temp_filename, index_col=[nam.year])
        # Reformat to match restart file
        # Map region name columns to region numbers using mapping table
        region_mapping = self.parent.mapping[[nam.region_number, nam.region_name]].drop_duplicates()
        region_mapping = region_mapping.set_index(nam.region_name)[nam.region_number].to_dict()
        # Rename columns from region names to region numbers
        self.hist_rfqtdcrd = self.hist_rfqtdcrd.rename(columns=region_mapping)
        self.hist_rfqtdcrd = pd.DataFrame(self.hist_rfqtdcrd.stack())
        self.hist_rfqtdcrd.index.names = self.restart.pmmout_rfqtdcrd.index.names
        self.hist_rfqtdcrd.columns = [nam.value]
        self.hist_rfqtdcrd = self.hist_rfqtdcrd.reorder_levels([1, 0])

        # Historical conventional crude oil production (HCOILPRD) - write to OGOILPRD(I,2,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcoilprd, nam.filename]
        self.hist_hcoilprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Crude Type Index
        self.hist_hcoilprd[2] = 1
        self.hist_hcoilprd = self.hist_hcoilprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcoilprd = pd.DataFrame(self.hist_hcoilprd.stack() / 365)
        self.hist_hcoilprd.index.names = self.restart.ogsmout_ogoilprd.index.names
        self.hist_hcoilprd.columns = [nam.value]

        # Historical tight crude oil production (HTOILPRD) - write to OGOILPRD(I,2,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_htoilprd, nam.filename]
        self.hist_htoilprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Crude Type Index
        self.hist_htoilprd[2] = 2
        self.hist_htoilprd = self.hist_htoilprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_htoilprd = pd.DataFrame(self.hist_htoilprd.stack() / 365)
        self.hist_htoilprd.index.names = self.restart.ogsmout_ogoilprd.index.names
        self.hist_htoilprd.columns = [nam.value]

        # HISTORICAL TIGHT OIL PRODUCTION FOR SELECT PLAYS (OGQSHLOIL) - write to ogqshloil
        temp_filename = self.parent.hist_setup_table.at[nam.hist_ogqshloil, nam.filename]
        self.hist_ogqshloil = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.year])
        # Reformat to match restart file
        self.hist_ogqshloil.columns = list(range(1, len(self.hist_ogqshloil.columns) + 1))
        self.hist_ogqshloil = self.hist_ogqshloil.rename(columns={(len(self.hist_ogqshloil.columns)): 15})
        self.hist_ogqshloil = pd.DataFrame(self.hist_ogqshloil.stack())
        self.hist_ogqshloil.index.names = self.restart.ogsmout_ogqshloil.index.names
        self.hist_ogqshloil.columns = [nam.value]
        self.hist_ogqshloil = self.hist_ogqshloil.reorder_levels([1, 0])

        ###Natural Gas Production
        # HISTORICAL NA CONVENTIONAL GAS PRODUCTION (HCGASPRD) - write to OGENAGPRD(I,1,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcgasprd, nam.filename]
        self.hist_hcgasprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Gas Type Index
        self.hist_hcgasprd[2] = 1
        self.hist_hcgasprd = self.hist_hcgasprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcgasprd = pd.DataFrame(self.hist_hcgasprd.stack())
        self.hist_hcgasprd.index.names = self.restart.ogsmout_ogenagprd.index.names
        self.hist_hcgasprd.columns = [nam.value]

        # HISTORICAL NA TIGHT GAS PRODUCTION (HTGASPRD) - write to OGENAGPRD(I,2,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_htgasprd, nam.filename]
        self.hist_htgasprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Gas Type Index
        self.hist_htgasprd[2] = 2
        self.hist_htgasprd = self.hist_htgasprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_htgasprd = pd.DataFrame(self.hist_htgasprd.stack())
        self.hist_htgasprd.index.names = self.restart.ogsmout_ogenagprd.index.names
        self.hist_htgasprd.columns = [nam.value]

        # HISTORICAL NA SHALE GAS PRODUCTION (HSGASPRD) - write to OGENAGPRD(I,3,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hsgasprd, nam.filename]
        self.hist_hsgasprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Gas Type Index
        self.hist_hsgasprd[2] = 3
        self.hist_hsgasprd = self.hist_hsgasprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hsgasprd = pd.DataFrame(self.hist_hsgasprd.stack())
        self.hist_hsgasprd.index.names = self.restart.ogsmout_ogenagprd.index.names
        self.hist_hsgasprd.columns = [nam.value]

        # HISTORICAL CONVENTIONAL AD GAS PRODUCTION (HCADGPRD) - write to OGADGPRD(I,1,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcadgprd, nam.filename]
        self.hist_hcadgprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Gas Type Index
        self.hist_hcadgprd[2] = 1
        self.hist_hcadgprd = self.hist_hcadgprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcadgprd = pd.DataFrame(self.hist_hcadgprd.stack())
        self.hist_hcadgprd.index.names = self.restart.ogsmout_ogadgprd.index.names
        self.hist_hcadgprd.columns = [nam.value]

        # HISTORICAL TIGHT AD GAS PRODUCTION (HTADGPRD) - write to OGADGPRD(I,2,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_htadgprd, nam.filename]
        self.hist_htadgprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Gas Type Index
        self.hist_htadgprd[2] = 2
        self.hist_htadgprd = self.hist_htadgprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_htadgprd = pd.DataFrame(self.hist_htadgprd.stack())
        self.hist_htadgprd.index.names = self.restart.ogsmout_ogadgprd.index.names
        self.hist_htadgprd.columns = [nam.value]

        # HISTORICAL NA COALBED METHANE PRODUCTION (HCBMPRD) - write to OGENAGPRD(I,4,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcbmprd, nam.filename]
        self.hist_hcbmprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Gas Type Index
        self.hist_hcbmprd[2] = 4
        self.hist_hcbmprd = self.hist_hcbmprd.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcbmprd = pd.DataFrame(self.hist_hcbmprd.stack())
        self.hist_hcbmprd.index.names = self.restart.ogsmout_ogenagprd.index.names
        self.hist_hcbmprd.columns = [nam.value]

        # HISTORICAL SHALE GAS PRODUCTION FOR SELECT PLAYS (OGQSHLGAS) - write to ogqshlgas
        temp_filename = self.parent.hist_setup_table.at[nam.hist_ogqshlgas, nam.filename]
        self.hist_ogqshlgas = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.year])
        # Reformat to match restart file
        self.hist_ogqshlgas.columns = list(range(1, len(self.hist_ogqshlgas.columns) + 1))
        self.hist_ogqshlgas = self.hist_ogqshlgas.rename(columns={(len(self.hist_ogqshlgas.columns)): 15})
        self.hist_ogqshlgas = pd.DataFrame(self.hist_ogqshlgas.stack())
        self.hist_ogqshlgas.index.names = self.restart.ogsmout_ogqshlgas.index.names
        self.hist_ogqshlgas.columns = [nam.value]
        self.hist_ogqshlgas = self.hist_ogqshlgas.reorder_levels([1, 0])

        ###Wells Drilled
        # Historical successful tight oil wells drilled (HTOWELLS) - write to ogogwells(I,2,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_htowells, nam.filename]
        self.hist_htowells = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Well Type Index
        self.hist_htowells[2] = 2
        self.hist_htowells = self.hist_htowells.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_htowells = pd.DataFrame(self.hist_htowells.stack())
        self.hist_htowells.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_htowells.columns = [nam.value]

        # Historical successful non-tight oil wells drilled (HCOWELLS) - write to ogogwells
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcowells, nam.filename]
        self.hist_hcowells = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Well Type Index
        self.hist_hcowells[2] = 1
        self.hist_hcowells = self.hist_hcowells.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcowells = pd.DataFrame(self.hist_hcowells.stack())
        self.hist_hcowells.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_hcowells.columns = [nam.value]

        # Historical conventional gas wells drilled (HCGWELLS) - write to ogogwells(I,3,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcgwells, nam.filename]
        self.hist_hcgwells = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Well Type Index
        self.hist_hcgwells[2] = 3
        self.hist_hcgwells = self.hist_hcgwells.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcgwells = pd.DataFrame(self.hist_hcgwells.stack())
        self.hist_hcgwells.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_hcgwells.columns = [nam.value]

        # Historical tight gas wells drilled (HTGWELLS) - write to ogogwells(I,4,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_htgwells, nam.filename]
        self.hist_htgwells = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Well Type Index
        self.hist_htgwells[2] = 4
        self.hist_htgwells = self.hist_htgwells.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_htgwells = pd.DataFrame(self.hist_htgwells.stack())
        self.hist_htgwells.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_htgwells.columns = [nam.value]

        # Historical successful shale gas drilled (HSGWELLS) - write to ogogwells(I,5,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hsgwells, nam.filename]
        self.hist_hsgwells = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Well Type Index
        self.hist_hsgwells[2] = 5
        self.hist_hsgwells = self.hist_hsgwells.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hsgwells = pd.DataFrame(self.hist_hsgwells.stack())
        self.hist_hsgwells.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_hsgwells.columns = [nam.value]

        # Historical successful CBM wells drilled (HCBMWELLS) - write to ogogwells(I,6,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hcbmwells, nam.filename]
        self.hist_hcbmwells = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        # Set Well Type Index
        self.hist_hcbmwells[2] = 6
        self.hist_hcbmwells = self.hist_hcbmwells.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hcbmwells = pd.DataFrame(self.hist_hcbmwells.stack())
        self.hist_hcbmwells.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_hcbmwells.columns = [nam.value]

        # Historical dry holes drilled (HDRYHOLES) - write to ogogwells(I,7,M)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_hdryholes, nam.filename]
        self.hist_hdryholes = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.district_number])
        #Set Well Type Index
        self.hist_hdryholes[2] = 7
        self.hist_hdryholes = self.hist_hdryholes.set_index([2], append=True)
        # Reformat to match restart file
        self.hist_hdryholes = pd.DataFrame(self.hist_hdryholes.stack())
        self.hist_hdryholes.index.names = self.restart.ogsmout_ogogwells.index.names
        self.hist_hdryholes.columns = [nam.value]


        ###OGNOWELL (Total Wells)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_ognowell, nam.filename]
        self.hist_ognowell =  com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.year])
        self.hist_ognowell.columns = [nam.value]
        self.hist_ognowell.index.names = self.parent.restart.ogsmout_ognowell.index.names


        ###Total NGPL Production
        # Historical NGPL production (mmb) (NGPLPRD) - just used to get NGPL shares
        temp_filename = self.parent.hist_setup_table.at[nam.hist_ngplprd, nam.filename]
        self.hist_ngplprd = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=['ngpl_50'])

        # Drop state and Merge rows
        self.hist_ngplprd = self.hist_ngplprd.drop(['state'], axis=1)
        self.hist_ngplprd.loc[43] = self.hist_ngplprd.loc[43].add(
            self.hist_ngplprd.loc[49])  # put old UTAH & WYOMING category in WY
        self.hist_ngplprd.loc[49] = 0
        self.hist_ngplprd.loc[50] = self.hist_ngplprd.loc[42] * 0.7  # put some volumes in PA
        self.hist_ngplprd.loc[42] = self.hist_ngplprd.loc[42] * 0.3
        temp = self.hist_ngplprd.loc[
            47].copy()  # current have 0 for 2009-2011, need share to be 1 not 0 so put small volume
        temp[temp < 0.1] = 0.1
        self.hist_ngplprd.loc[47] = temp

        # Match to HSM Mapping
        self.hist_ngplprd = self.hist_ngplprd.merge(
            self.parent.mapping[['ngpl_50', nam.district_number, nam.ngpl_region_number]], how='inner', left_index=True,
            right_on='ngpl_50')
        self.hist_ngplprd = self.hist_ngplprd.set_index([nam.district_number])
        self.hist_ngplprd = self.hist_ngplprd.drop(['ngpl_50'], axis=1)

        # Get Temp Share for other ngpl types
        temp_region_totals = self.hist_ngplprd.groupby(nam.ngpl_region_number).sum().copy()
        temp_merge_table = self.hist_ngplprd[[nam.ngpl_region_number]].copy()
        temp_merge_table = temp_merge_table.merge(temp_region_totals,
                                                  how='inner',
                                                  left_on=nam.ngpl_region_number,
                                                  right_index=True)
        temp_ngpl_shares = self.hist_ngplprd.div(temp_merge_table).fillna(0.0)
        temp_ngpl_shares = temp_ngpl_shares.drop(nam.ngpl_region_number, axis=1)

        # Set Alaska and California equal to one district
        temp_ngpl_shares.loc[3] = 0.45
        temp_ngpl_shares.loc[6] = 0.55

        ###NGPL production by type
        def ngpl_prod_type(filename, output_df, restart_var):
            temp_filename = self.parent.hist_setup_table.at[filename, nam.filename]
            output_df = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.year])
            output_df.columns = list(range(1, 17))
            output_df = output_df.transpose()

            # Reformat to match restart file
            output_df = output_df.merge(self.parent.mapping[[nam.ngpl_region_number, nam.district_number]],
                                        how='inner', left_index=True, right_on=nam.ngpl_region_number)
            output_df = output_df.set_index(nam.district_number)
            output_df = output_df.drop(nam.ngpl_region_number, axis=1)
            output_df = output_df.mul(temp_ngpl_shares, axis=1)
            output_df = pd.DataFrame(output_df.stack()) / 1000
            output_df.index.names = restart_var.index.names
            output_df.columns = [nam.value]

            return output_df

        # Official historical NGPL composition (MBD) (NGPL production) (EPL2) - write to ogngplprd
        self.hist_epl2 = ngpl_prod_type(nam.hist_epl2, self.hist_epl2, self.restart.ogsmout_ogngplprd)

        # Historical propane production (EPLLPA) - write to ogngplpr
        self.hist_epllpa = ngpl_prod_type(nam.hist_epllpa, self.hist_epllpa, self.restart.ogsmout_ogngplpr)

        # Historical ethane production (EPLLEA) - write to ogngplet
        self.hist_epllea = ngpl_prod_type(nam.hist_epllea, self.hist_epllea, self.restart.ogsmout_ogngplet)

        # Historical butane production (EPLLBAN) - write to ogngplbu
        self.hist_epllban = ngpl_prod_type(nam.hist_epllban, self.hist_epllban, self.restart.ogsmout_ogngplbu)

        # Historical isobutane production (EPLLBAI) - write to ogngplis
        self.hist_epllbai = ngpl_prod_type(nam.hist_epllbai, self.hist_epllbai, self.restart.ogsmout_ogngplis)

        # Historical pentanes plus production (EPLP) - write to ogngplpp
        self.hist_eplp = ngpl_prod_type(nam.hist_eplp, self.hist_eplp, self.restart.ogsmout_ogngplpp)

        ###HISTORICAL CRUDE OIL WELLHEAD PRICES AND PRODUCTION (DCRDWHP) - write to DCRDWHP
        temp_filename = self.parent.hist_setup_table.at[nam.hist_dcrdwhp, nam.filename]
        self.hist_dcrdwhp = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1, index_col=[nam.year])

        # Get columns
        self.hist_dcrdwhp.columns = list(range(1, len(self.hist_dcrdwhp.columns) + 1))

        # Calculate inflation
        inflation_df = self.parent.rest_mc_jpgdp.copy().loc[(self.parent.rest_mc_jpgdp.index < self.parent.history_year) &
                                                     (self.parent.rest_mc_jpgdp.index >= self.parent.param_baseyr)]
        self.hist_dcrdwhp = self.hist_dcrdwhp.div(inflation_df['value'], axis=0)

        # Reformat to match restart file
        self.hist_dcrdwhp = pd.DataFrame(self.hist_dcrdwhp.stack())
        self.hist_dcrdwhp.index.names = self.restart.pmmout_dcrdwhp.index.names
        self.hist_dcrdwhp.columns = [nam.value]
        self.hist_dcrdwhp = self.hist_dcrdwhp.reorder_levels([1, 0])

        ###Load EOR History
        # Load master EOR production data from input file
        # This file contains historical EOR production by EOR type (Thermal and CO2)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_eor_prod, nam.filename]
        temp_hist_eor_prod_master = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1)
        # Set 'type' column as index for easier row selection by type name
        temp_hist_eor_prod_master = temp_hist_eor_prod_master.set_index('type')

        # Load EOR regional allocation factors from input file
        # Contains thermal_eor_factor and co2_eor_factor for allocating each EOR type across HSM regions (1-7) plus Total (8)
        eor_factors_df = com.read_dataframe(self.parent.hist_input_path + 'hist_eor_factors.csv')
        eor_factors_df = eor_factors_df.set_index('region_number')
        # Store factors for use in overwrite_restart_var_history
        self.eor_factors_df = eor_factors_df.copy()

        # Process Historical EOR Production by Region
        # Extract years of interest and get Thermal and CO2 production separately
        years = list(range(self.parent.param_baseyr, self.parent.history_year + 1))
        temp_thermal_eor = temp_hist_eor_prod_master.loc['Thermal', years].to_frame(name=nam.value)
        temp_co2_eor = temp_hist_eor_prod_master.loc['CO2', years].to_frame(name=nam.value)
        # Store raw EOR data for use in overwrite_restart_var_history
        self.hist_thermal_eor_raw = temp_thermal_eor.copy()
        self.hist_co2_eor_raw = temp_co2_eor.copy()

        # Allocate EOR production across regions using type-specific regional factors
        # For each region: regional_total = (Thermal * thermal_factor) + (CO2 * co2_factor)
        hist_eor_prod_list = []
        for region_num in range(1, 9):
            thermal_factor = eor_factors_df.loc[region_num, 'thermal_eor_factor']
            co2_factor = eor_factors_df.loc[region_num, 'co2_eor_factor']
            # Calculate regional EOR as sum of thermal and CO2 contributions
            regional_thermal = temp_thermal_eor.copy() * thermal_factor
            regional_co2 = temp_co2_eor.copy() * co2_factor
            regional_total = regional_thermal + regional_co2
            regional_total[nam.region_number] = region_num
            regional_total = regional_total.set_index([nam.region_number], append=True)
            regional_total = regional_total.reorder_levels([1, 0])
            hist_eor_prod_list.append(regional_total)
        self.hist_eor_prod = pd.concat(hist_eor_prod_list, ignore_index=False)


        ###Load LFMM History
        temp_filename = self.parent.hist_setup_table.at[nam.hist_crude_lfmm, nam.filename]
        self.hist_lfmm_prod = com.read_dataframe(self.parent.hist_input_path + temp_filename, skiprows=1)
        self.hist_lfmm_prod = self.hist_lfmm_prod.set_index([nam.region_number, nam.well_type_number])
        self.hist_lfmm_prod = pd.DataFrame(self.hist_lfmm_prod.stack())
        self.hist_lfmm_prod.columns = [nam.value]
        self.hist_lfmm_prod.index = self.hist_lfmm_prod.index.set_levels(self.hist_lfmm_prod.index.levels[2].astype(int), level=2)
        self.hist_lfmm_prod.index.names = self.restart.ogsmout_ogcruderef.index.names

        ###Load Federal Production History
        # Historical crude production on federal lands
        # CSV units: mmbbl/year, restart variable units: mmbd (million barrels per day)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_ogcoprd_fed, nam.filename]
        self.hist_ogcoprd_fed = com.read_dataframe(self.parent.hist_input_path + temp_filename)
        # Set index to [year, region_number] and extract production column
        self.hist_ogcoprd_fed = self.hist_ogcoprd_fed.set_index([nam.year, nam.region_number])[['production']]
        # Convert from mmbbl/year to mmbd: divide by 365
        self.hist_ogcoprd_fed['production'] = self.hist_ogcoprd_fed['production'] / 365
        # Convert region_number from float to int
        self.hist_ogcoprd_fed.index = self.hist_ogcoprd_fed.index.set_levels(self.hist_ogcoprd_fed.index.levels[1].astype(int), level=1)
        # Reorder levels to [region_number, year] to match restart structure
        self.hist_ogcoprd_fed = self.hist_ogcoprd_fed.reorder_levels([1, 0])
        # Set column and index names to match restart variable
        self.hist_ogcoprd_fed.columns = [nam.value]
        self.hist_ogcoprd_fed.index.names = self.restart.ogsmout_ogcoprd_fed.index.names

        # Historical natural gas production on federal lands
        # CSV units: bcf/year, restart variable units: tcf (trillion cubic feet per year)
        temp_filename = self.parent.hist_setup_table.at[nam.hist_ogngprd_fed, nam.filename]
        self.hist_ogngprd_fed = com.read_dataframe(self.parent.hist_input_path + temp_filename)
        # Set index to [year, region_number] and extract production column
        self.hist_ogngprd_fed = self.hist_ogngprd_fed.set_index([nam.year, nam.region_number])[['production']]
        # Convert from bcf/year to tcf/year: divide by 1000
        self.hist_ogngprd_fed['production'] = self.hist_ogngprd_fed['production'] / 1000
        # Convert region_number from float to int
        self.hist_ogngprd_fed.index = self.hist_ogngprd_fed.index.set_levels(self.hist_ogngprd_fed.index.levels[1].astype(int), level=1)
        # Reorder levels to [region_number, year] to match restart structure
        self.hist_ogngprd_fed = self.hist_ogngprd_fed.reorder_levels([1, 0])
        # Set column and index names to match restart variable
        self.hist_ogngprd_fed.columns = [nam.value]
        self.hist_ogngprd_fed.index.names = self.restart.ogsmout_ogngprd_fed.index.names

        #Load Alaska and Offshore
        hist_years = list(range(self.parent.param_baseyr, self.parent.history_year + 1))

        #Get Offshore
        rftqdcrd_8 = self.restart.pmmout_rfqtdcrd.xs(8, level = 0, drop_level=False).copy()
        rftqdcrd_8 = rftqdcrd_8 * 365
        rftqdcrd_8[2] = 3

        rftqdcrd_9 = self.restart.pmmout_rfqtdcrd.xs(9, level = 0, drop_level=False).copy()
        rftqdcrd_9 = rftqdcrd_9 * 365
        rftqdcrd_9[2] = 3

        rftqdcrd_10 = self.restart.pmmout_rfqtdcrd.xs(10, level = 0, drop_level=False).copy()
        rftqdcrd_10 = rftqdcrd_10 * 365
        rftqdcrd_10[2] = 7

        #Get Alaska
        rftqdcrd_11 = self.restart.pmmout_rfqtdcrd.xs(11, level = 0, drop_level=False).copy()
        rftqdcrd_11 = rftqdcrd_11 * 365
        rftqdcrd_11[2] = 3

        rftqdcrd_12 = self.restart.pmmout_rfqtdcrd.xs(12, level = 0, drop_level=False).copy()
        rftqdcrd_12 = rftqdcrd_12 * 365
        rftqdcrd_12[2] = 3

        rftqdcrd_13 = self.restart.pmmout_rfqtdcrd.xs(13, level = 0, drop_level=False).copy()
        rftqdcrd_13 = rftqdcrd_13 * 365
        rftqdcrd_13[2] = 3

        #Combine Alaska and Offshore
        ak_off_hist = pd.concat([rftqdcrd_8,rftqdcrd_9,rftqdcrd_10,rftqdcrd_11,rftqdcrd_12,rftqdcrd_13], ignore_index= False).copy()
        ak_off_hist = ak_off_hist.loc[ak_off_hist.index.get_level_values(1).isin(hist_years)]
        ak_off_hist = ak_off_hist.set_index([2], append = True)
        ak_off_hist = ak_off_hist.reorder_levels([0,2,1])
        ak_off_hist.index.names = self.hist_lfmm_prod.index.names

        #Combine Alaska and Offshore with Onshore
        self.hist_lfmm_prod = pd.concat([self.hist_lfmm_prod,ak_off_hist], ignore_index = False)

        pass


    def overwrite_restart_var_history(self):
        """Overwrites Restart variables history.

        Returns
        -------
        self.restart.pmmout_rfqtdcrd : df
            Total crude production by HSM region

        self.restart.pmmout_rfqdcrd : df
            Total crude oil production by HSM region (not including EOR)

        self.restart.ogsmout_ogcrdprd : df
            Crude oil production by HSM region and LFMM crude oil type

        self.restart.ogsmout_ogcruderef : df
            Crude oil production by LFMM crude oil type and region

        self.restart.ogsmout_ogoilprd : df
            Crude oil production by oil type and HSM district

        self.restart.ogsmout_ogqshloil : df
            Crude oil production by select tight oil play

        self.restart.ogsmout_ogprcoak : df
            Crude oil production by Alaska region

        self.restart.ogsmout_ogeorprd : df
            CO2 EOR crude oil production

        self.restart.pmmout_dcrdwhp : df
            Domestic crude oil wellhead price by region

        self.restart.ogsmout_ogenagprd : df
            Natural gas expected production by natural gas type and HSM district

        self.restart.ogsmout_ogrnagprd : df
            Natural gas realized production by natural gas type and HSM district

        self.restart.ogsmout_ogadgprd : df
            Natural gas associated dissolved production by oil type and HSM district

        self.restart.ogsmout_ogngprd : df
            Natural gas production by type

        self.restart.ogsmout_ogqngrep : df
            Natural gas production by natural gas type

        self.restart.ogsmout_ogqshlgas : df
            Natural gas production by select natural gas play

        self.restart.ogsmout_ogregprd : df
            Total crude oil and natural gas production by production type

        self.restart.ogsmout_ogogwells : df
            Total wells

        self.restart.ogsmout_ogngplprd : df
            NGPL production by HSM district

        self.restart.ogsmout_ogngplpp : df
            Pentanes production by HSM district

        self.restart.ogsmout_ogngplpr : df
            Propane production by HSM district

        self.restart.ogsmout_ogngplet : df
            Ethane production by HSM district

        self.restart.ogsmout_ogngplbu : df
            Butane production by HSM district

        self.restart.ogsmout_ogngplis : df
            Isobutane production by HSM district

        self.restart.ogsmout_ogngplprd : df
            NGPL production by HSM district

        self.restart.ogsmout_ogqcrrep : df
            Crude production by oil category

        self.restart.ogsmout_ogcoprd_nonfed : df
            Crude production on non-federal lands

        self.restart.ogsmout_ogcoprd_fed : df
            Crude production on federal lands

        self.restart.ogsmout_ogngprd_nonfed : df
            Natural Gas production on non-federal lands

        self.restart.ogsmout_ogngprd_fed : df
            Natural Gas production on federal lands

        """

        ###Overwrite region-level crude production(including EOR)
        # Cast to float64 for consistent calculations
        self.restart.pmmout_rfqtdcrd['value'] = self.restart.pmmout_rfqtdcrd['value'].astype(np.float64)
        self.restart.pmmout_rfqtdcrd.update(self.hist_rfqtdcrd.astype(self.restart.pmmout_rfqtdcrd.dtypes))

        ###Overwrite OGCOPRD with historical crude production data
        # Extract onshore regions (1-7) and offshore regions (9-10) from pmmout_rfqtdcrd
        # Filter to history years only
        temp_ogcoprd = self.restart.pmmout_rfqtdcrd.copy()
        temp_ogcoprd = temp_ogcoprd[temp_ogcoprd.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7, 9, 10])]
        temp_ogcoprd = temp_ogcoprd[temp_ogcoprd.index.get_level_values(1) <= self.parent.history_year]
        
        # Remap region indices: 9→8, 10→9 (offshore regions)
        temp_ogcoprd = temp_ogcoprd.reset_index()
        temp_ogcoprd.loc[temp_ogcoprd['1'] == 9, '1'] = 8
        temp_ogcoprd.loc[temp_ogcoprd['1'] == 10, '1'] = 9
        temp_ogcoprd = temp_ogcoprd.set_index(['1', '2'])
        temp_ogcoprd.index.names = self.restart.ogsmout_ogcoprd.index.names
        
        # Cast to float64 for consistent calculations
        self.restart.ogsmout_ogcoprd['value'] = self.restart.ogsmout_ogcoprd['value'].astype(np.float64)
        self.restart.ogsmout_ogcoprd.update(temp_ogcoprd.astype(self.restart.ogsmout_ogcoprd.dtypes))


        ###Overwrite region-level crude production(not including EOR)
        temp_hist_eor_df = (self.hist_eor_prod) / 1000000 #set units
        temp_hist_eor_df = temp_hist_eor_df[temp_hist_eor_df.index.isin([1,2,3,4,5,6,7], level=0)] #select regions
        temp_hist_eor_df.index.names = ['1','2'] #rename index
        index_mask = temp_hist_eor_df.index #mask for history
        temp_rest_df = self.restart.pmmout_rfqtdcrd.loc[index_mask].copy()
        temp_rest_df = temp_rest_df.sub(temp_hist_eor_df,axis = 1) #subtract EOR from rfqdcrd
        temp_rest_df.loc[temp_rest_df[nam.value] < 0, nam.value] = 0.0
        
        # Cast to float64 for consistent calculations
        self.restart.pmmout_rfqdcrd['value'] = self.restart.pmmout_rfqdcrd['value'].astype(np.float64)
        self.restart.pmmout_rfqdcrd.update(temp_rest_df.astype(self.restart.pmmout_rfqdcrd.dtypes))


        ###Overwrite LFMM Crude Production History

        ###Overwrite OGCRUDEREF
        temp_ogcruderef = self.hist_lfmm_prod.copy()
        temp_ogcruderef = temp_ogcruderef * 365 / 1000
        temp_ogcruderef = temp_ogcruderef.loc[temp_ogcruderef.index.get_level_values(0).isin([1,2,3,4,5,6,7])]


        ###Get Alaska History
        alaska_prod = self.hist_lfmm_prod.loc[self.hist_lfmm_prod.index.get_level_values(0).isin([11,12,13])].copy()
        alaska_prod = alaska_prod.reset_index(drop=False)
        alaska_prod['1'] = 8
        alaska_prod = alaska_prod.set_index(['1', '2', '3'])
        alaska_prod = alaska_prod.groupby(level = [0,2]).sum()
        alaska_prod['3'] = 3
        alaska_prod = alaska_prod.set_index(['3'], append = True)
        alaska_prod = alaska_prod.reorder_levels([0,2,1])
        alaska_prod.index.names = temp_ogcruderef.index.names
        temp_ogcruderef = pd.concat([temp_ogcruderef, alaska_prod], ignore_index= False)

        # Cast to float64 for consistent calculations
        self.restart.ogsmout_ogcruderef['value'] = self.restart.ogsmout_ogcruderef['value'].astype(np.float64)
        self.restart.ogsmout_ogcruderef.update(temp_ogcruderef.astype(self.restart.ogsmout_ogcruderef.dtypes))


        ###Overwrite district-level crude production
        # Cast to float64 for consistent calculations
        self.restart.ogsmout_ogoilprd['value'] = self.restart.ogsmout_ogoilprd['value'].astype(np.float64)
        self.restart.ogsmout_ogoilprd.update(self.hist_htoilprd.astype(self.restart.ogsmout_ogoilprd.dtypes))
        self.restart.ogsmout_ogoilprd.update(self.hist_hcoilprd.astype(self.restart.ogsmout_ogoilprd.dtypes))


        ###Overwrite crude production history by HSM region and LFMM crude oil type (ogcrdprd)
        temp = self.restart.ogsmout_ogoilprd.copy()
        temp = temp[temp.index.isin([5], level=1)]
        temp = temp.loc[(temp.index.get_level_values(2) <= self.parent.history_year) &
                        (temp.index.get_level_values(2) > 2009)]
        temp = temp.groupby(level=[0,2]).sum().mul(1000)
        temp.index.names = [nam.district_number, nam.year]
        temp = temp.reset_index()
        temp = temp.merge(self.parent.mapping, how='left', on='district_number')

        #Prepare LFMM crude production history by region and year
        hist_lfmm_prod = self.restart.ogsmout_ogcruderef.copy()
        hist_lfmm_prod = hist_lfmm_prod[hist_lfmm_prod.index.get_level_values(2) <= self.parent.history_year]
        hist_lfmm_prod = hist_lfmm_prod * 1000 / 365 # convert from million barrels per year to thousand barrels per day
        hist_lfmm_prod = hist_lfmm_prod.groupby(level=[0,2]).sum()
        hist_lfmm_prod.index.names = [nam.lfmm_region_number, nam.year]
        hist_lfmm_prod = hist_lfmm_prod.reset_index()

        #Normalize og & lf totals
        temp_og = temp.copy().groupby('year')['value'].sum()
        temp_lf = hist_lfmm_prod.copy().groupby('year')['value'].sum()
        normalization_ratio = temp_lf.div(temp_og).dropna().to_frame().rename(columns={'value': 'normalization_ratio'})
        temp = temp.merge(normalization_ratio, how='left', on='year')
        temp['value'] = temp['value'] * temp['normalization_ratio']

        #Merge in LFMM crude production history and calculate ratio for each HSM district
        temp = temp.merge(hist_lfmm_prod, how='left', on=['lfmm_region_number', 'year'], suffixes=['_og', '_lf'])
        temp['ratio'] = temp['value_og'] / temp['value_lf']

        #Prepare LFMM crude production history but include crude this time
        hist_lfmm_prod = self.restart.ogsmout_ogcruderef.copy()
        hist_lfmm_prod = hist_lfmm_prod[hist_lfmm_prod.index.get_level_values(2) <= self.parent.history_year]
        hist_lfmm_prod = hist_lfmm_prod * 1000 / 365
        hist_lfmm_prod.index.names = [nam.lfmm_region_number, nam.lfmm_crude_type, nam.year]
        hist_lfmm_prod = hist_lfmm_prod.reset_index()

        #Merge LFMM crude production histories by LFMM crude type and group by HSM region, update ogsmout_ogcrdprd (1,13)
        temp = temp.merge(hist_lfmm_prod, how='left', on=['lfmm_region_number', 'year'])
        temp['total'] = temp['ratio'] * temp['value']
        temp = temp.groupby(['region_number', 'lfmm_crude_type', 'year'])['total'].sum().to_frame(name='value')
        temp = temp.astype(np.float32)
        self.restart.ogsmout_ogcrdprd.update(temp.astype(self.restart.ogsmout_ogcrdprd.dtypes))

        ###Set ogsmout_ogcrdprd "Total" value
        temp_df = self.restart.ogsmout_ogcrdprd.loc[self.restart.ogsmout_ogcrdprd.index.get_level_values(0).isin(list(range(1,14)))].copy()
        temp_df = temp_df.groupby(level = [1,2]).sum()
        temp_df[2] = 14
        temp_df = temp_df.set_index([2], append = True)
        temp_df = temp_df.reorder_levels([2,0,1])
        temp_df.index.names = self.restart.ogsmout_ogcrdprd.index.names
        self.restart.ogsmout_ogcrdprd.update(temp_df.astype(self.restart.ogsmout_ogcrdprd.dtypes))

        ###Overwrite play-level crude production
        # cast up to float64 (instead of down to float32)
        # Cast to float64 for consistent calculations
        self.restart.ogsmout_ogqshloil['value'] = self.restart.ogsmout_ogqshloil['value'].astype(np.float64)
        self.restart.ogsmout_ogqshloil.update(self.hist_ogqshloil.astype(self.restart.ogsmout_ogqshloil.dtypes))


        ###Overwrite Alaska crude production
        #Get pmmout variables
        temp = self.restart.pmmout_rfqdcrd.copy()
        temp = temp[temp.index.get_level_values(0).isin([11,12,13])]
        temp = temp[temp.index.get_level_values(1) <= self.parent.history_year]

        #Remap index to mask ogsmout_ogprcoak
        temp = temp.reset_index()
        temp['1'] = temp['1'].replace(11, 1)
        temp['1'] = temp['1'].replace(12, 2)
        temp['1'] = temp['1'].replace(13, 3)
        temp = temp.set_index(['1','2'])

        #Change to barrels/year
        temp = temp * 365

        #Write to restart variable
        # cast up to float64 (instead of down to float32)
        # Cast to float64 for consistent calculations
        self.restart.ogsmout_ogprcoak['value'] = self.restart.ogsmout_ogprcoak['value'].astype(np.float64)
        self.restart.ogsmout_ogprcoak.update(temp.astype(self.restart.ogsmout_ogprcoak.dtypes))


        ###Overwrite EOR Production by Region and Type
        # Build EOR production data for all regions (1-8) and EOR types (1=Thermal, 2=CO2, 3=Total)
        # Units: Convert from BBL/D to MBbl/yr (divide by 1000, multiply by 365)
        eor_prod_list = []
        history_years = range(self.restart.parametr_baseyr, self.parent.history_year + 1)
        
        for region_num in range(1, 9):
            thermal_factor = self.eor_factors_df.loc[region_num, 'thermal_eor_factor']
            co2_factor = self.eor_factors_df.loc[region_num, 'co2_eor_factor']
            
            # eor_type=1: Thermal EOR
            regional_thermal = self.hist_thermal_eor_raw.copy() * thermal_factor / 1000 * 365
            regional_thermal[nam.region_number] = region_num
            regional_thermal[nam.eor_type] = 1
            regional_thermal = regional_thermal.set_index([nam.region_number, nam.eor_type], append=True)
            regional_thermal = regional_thermal.reorder_levels([1, 2, 0])
            eor_prod_list.append(regional_thermal)
            
            # eor_type=2: CO2 EOR
            regional_co2 = self.hist_co2_eor_raw.copy() * co2_factor / 1000 * 365
            regional_co2[nam.region_number] = region_num
            regional_co2[nam.eor_type] = 2
            regional_co2 = regional_co2.set_index([nam.region_number, nam.eor_type], append=True)
            regional_co2 = regional_co2.reorder_levels([1, 2, 0])
            eor_prod_list.append(regional_co2)
            
            # eor_type=3: Total EOR (Thermal + CO2)
            regional_total = (self.hist_thermal_eor_raw.copy() * thermal_factor + 
                              self.hist_co2_eor_raw.copy() * co2_factor) / 1000 * 365
            regional_total[nam.region_number] = region_num
            regional_total[nam.eor_type] = 3
            regional_total = regional_total.set_index([nam.region_number, nam.eor_type], append=True)
            regional_total = regional_total.reorder_levels([1, 2, 0])
            eor_prod_list.append(regional_total)
        
        # Combine all EOR production data
        temp_eor_prod = pd.concat(eor_prod_list, ignore_index=False)
        
        # Create a copy to mask for updates
        temp_ogeorprd = self.restart.ogsmout_ogeorprd.copy()
        temp_ogeorprd['value'] = 0  # Default all values to 0
        temp_ogeorprd['value'] = temp_ogeorprd['value'].astype(np.float32)
        
        # Update matching indices for all regions (1-8), EOR types (1-3), and history years
        matching_indices = temp_eor_prod.index.intersection(
            pd.MultiIndex.from_product(
                [range(1, 9), range(1, 4), history_years],
                names=temp_eor_prod.index.names
            )
        )
        
        temp_ogeorprd.loc[matching_indices, 'value'] = temp_eor_prod.loc[matching_indices, 'value'].astype(np.float32)
        
        # Apply the updates to EOR Production
        self.restart.ogsmout_ogeorprd.update(temp_ogeorprd.astype(self.restart.ogsmout_ogeorprd.dtypes))

        ###Overwrite region-level crude price data with historical data
        #Note: This historical data will be used for history years, but projection years
        #will use calculated values from rfcrudewhp (via reg_crude_price) in calculate_prices()
        self.restart.pmmout_dcrdwhp.update(self.hist_dcrdwhp.astype(self.restart.pmmout_dcrdwhp.dtypes))


        ###Overwrite district-level natural gas production
        self.restart.ogsmout_ogenagprd.update(self.hist_htgasprd.astype(self.restart.ogsmout_ogenagprd.dtypes))
        self.restart.ogsmout_ogenagprd.update(self.hist_hsgasprd.astype(self.restart.ogsmout_ogenagprd.dtypes))
        self.restart.ogsmout_ogenagprd.update(self.hist_hcgasprd.astype(self.restart.ogsmout_ogenagprd.dtypes))
        self.restart.ogsmout_ogenagprd.update(self.hist_hcbmprd.astype(self.restart.ogsmout_ogenagprd.dtypes))

        temp = self.restart.ogsmout_ogenagprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp.groupby(level = [0,2]).sum()
        temp[nam.well_type] = 5
        temp = temp.set_index([nam.well_type], append = True)
        temp = temp.reorder_levels([0,2,1])
        temp.index.names = self.restart.ogsmout_ogenagprd.index.names
        self.restart.ogsmout_ogenagprd.update(temp.astype(self.restart.ogsmout_ogenagprd.dtypes))

        # Recategorize historical tight gas (gas_type 2) to shale gas (gas_type 3) in ogenagprd
        # Extract tight gas (gas_type 2) for history years
        history_years_mask_ogen = self.restart.ogsmout_ogenagprd.index.get_level_values(2) <= self.parent.history_year
        tight_gas_mask_ogen = self.restart.ogsmout_ogenagprd.index.get_level_values(1) == 2
        tight_gas_history_mask_ogen = history_years_mask_ogen & tight_gas_mask_ogen

        # Get tight gas values for history years
        temp_tight_gas_ogen = self.restart.ogsmout_ogenagprd.loc[tight_gas_history_mask_ogen].copy()
        if len(temp_tight_gas_ogen) > 0:
            # Reset index to extract district and year, then set gas_type to 3 (shale gas)
            temp_tight_gas_ogen = temp_tight_gas_ogen.reset_index()
            temp_tight_gas_ogen['2'] = 3  # Change gas_type from 2 to 3
            temp_tight_gas_ogen = temp_tight_gas_ogen.set_index(['1', '2', '3'])
            temp_tight_gas_ogen.index.names = self.restart.ogsmout_ogenagprd.index.names
            
            # Get existing shale gas (gas_type 3) for history years
            shale_gas_mask_ogen = self.restart.ogsmout_ogenagprd.index.get_level_values(1) == 3
            shale_gas_history_mask_ogen = history_years_mask_ogen & shale_gas_mask_ogen
            temp_shale_gas_ogen = self.restart.ogsmout_ogenagprd.loc[shale_gas_history_mask_ogen].copy()
            
            # Add tight gas to shale gas using .add() for proper index alignment
            if len(temp_shale_gas_ogen) > 0:
                temp_shale_gas_ogen['value'] = temp_shale_gas_ogen['value'].add(temp_tight_gas_ogen['value'], fill_value=0)
                self.restart.ogsmout_ogenagprd.update(temp_shale_gas_ogen.astype(self.restart.ogsmout_ogenagprd.dtypes))
            else:
                # If no existing shale gas, just add the tight gas as shale gas
                self.restart.ogsmout_ogenagprd.update(temp_tight_gas_ogen.astype(self.restart.ogsmout_ogenagprd.dtypes))
            
            # Zero out tight gas (gas_type 2) for all history years
            self.restart.ogsmout_ogenagprd.loc[tight_gas_history_mask_ogen, 'value'] = 0

        #Realized non-associated natural gas
        self.restart.ogsmout_ogrnagprd.update(self.hist_htgasprd.astype(self.restart.ogsmout_ogrnagprd.dtypes))
        self.restart.ogsmout_ogrnagprd.update(self.hist_hsgasprd.astype(self.restart.ogsmout_ogrnagprd.dtypes))
        self.restart.ogsmout_ogrnagprd.update(self.hist_hcgasprd.astype(self.restart.ogsmout_ogrnagprd.dtypes))
        self.restart.ogsmout_ogrnagprd.update(self.hist_hcbmprd.astype(self.restart.ogsmout_ogrnagprd.dtypes))

        # Recategorize historical tight gas (gas_type 2) to shale gas (gas_type 3) in ogrnagprd
        # Extract tight gas (gas_type 2) for history years
        history_years_mask = self.restart.ogsmout_ogrnagprd.index.get_level_values(2) <= self.parent.history_year
        tight_gas_mask = self.restart.ogsmout_ogrnagprd.index.get_level_values(1) == 2
        tight_gas_history_mask = history_years_mask & tight_gas_mask

        # Get tight gas values for history years
        temp_tight_gas = self.restart.ogsmout_ogrnagprd.loc[tight_gas_history_mask].copy()
        if len(temp_tight_gas) > 0:
            # Reset index to extract district and year, then set gas_type to 3 (shale gas)
            temp_tight_gas = temp_tight_gas.reset_index()
            temp_tight_gas['2'] = 3  # Change gas_type from 2 to 3
            temp_tight_gas = temp_tight_gas.set_index(['1', '2', '3'])
            temp_tight_gas.index.names = self.restart.ogsmout_ogrnagprd.index.names
            
            # Get existing shale gas (gas_type 3) for history years
            shale_gas_mask = self.restart.ogsmout_ogrnagprd.index.get_level_values(1) == 3
            shale_gas_history_mask = history_years_mask & shale_gas_mask
            temp_shale_gas = self.restart.ogsmout_ogrnagprd.loc[shale_gas_history_mask].copy()
            
            # Add tight gas to shale gas using .add() for proper index alignment
            if len(temp_shale_gas) > 0:
                temp_shale_gas['value'] = temp_shale_gas['value'].add(temp_tight_gas['value'], fill_value=0)
                self.restart.ogsmout_ogrnagprd.update(temp_shale_gas.astype(self.restart.ogsmout_ogrnagprd.dtypes))
            else:
                # If no existing shale gas, just add the tight gas as shale gas
                self.restart.ogsmout_ogrnagprd.update(temp_tight_gas.astype(self.restart.ogsmout_ogrnagprd.dtypes))
            
            # Zero out tight gas (gas_type 2) for all history years
            self.restart.ogsmout_ogrnagprd.loc[tight_gas_history_mask, 'value'] = 0

        temp = self.restart.ogsmout_ogrnagprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp.groupby(level = [0,2]).sum()
        temp[nam.well_type] = 5
        temp = temp.set_index([nam.well_type], append = True)
        temp = temp.reorder_levels([0,2,1])
        temp.index.names = self.restart.ogsmout_ogrnagprd.index.names
        self.restart.ogsmout_ogrnagprd.update(temp.astype(self.restart.ogsmout_ogrnagprd.dtypes))

        #Associated disolved natural gas
        self.restart.ogsmout_ogadgprd.update(self.hist_htadgprd.astype(self.restart.ogsmout_ogadgprd.dtypes))
        self.restart.ogsmout_ogadgprd.update(self.hist_hcadgprd.astype(self.restart.ogsmout_ogadgprd.dtypes))

        temp = self.restart.ogsmout_ogadgprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp.groupby(level = [0,2]).sum()
        temp[nam.well_type] = 5
        temp = temp.set_index([nam.well_type], append = True)
        temp = temp.reorder_levels([0,2,1])
        temp.index.names = self.restart.ogsmout_ogadgprd.index.names
        self.restart.ogsmout_ogadgprd.update(temp.astype(self.restart.ogsmout_ogadgprd.dtypes))

        #Total Dry natural gas production by state/district and gas type
        temp = self.restart.ogsmout_ogrnagprd + self.restart.ogsmout_ogadgprd
        self.restart.ogsmout_ogdngprd.update(temp.astype(self.restart.ogsmout_ogdngprd.dtypes))

        ###Overwrite region-level natural gas production
        temp_na_prod = self.restart.ogsmout_ogenagprd.copy()
        temp_ad_prod = self.restart.ogsmout_ogadgprd.copy()

        #Get total prod
        temp_hist_prod = temp_na_prod.copy()
        temp_hist_prod[nam.value] = temp_hist_prod[nam.value] + temp_ad_prod[nam.value]

        #Map district prod to region prod
        temp_hist_prod = temp_hist_prod.xs(5, level = 1)
        temp_hist_prod = temp_hist_prod.reset_index()
        temp_hist_prod.columns = [nam.district_number, nam.year, nam.value]
        temp_hist_prod = temp_hist_prod.merge(self.parent.mapping[[nam.district_number, nam.region_number]],
                                    how = 'left',
                                    on = nam.district_number)
        temp_hist_prod = temp_hist_prod.drop(nam.district_number, axis = 1)

        #Fix Offshore, where HSM regions don't match restart variable regions (Atlantic, which is assigned as 10, produces nothing)
        temp_hist_prod.loc[temp_hist_prod[nam.region_number] == 9, nam.region_number] = 8
        temp_hist_prod.loc[temp_hist_prod[nam.region_number] == 10, nam.region_number] = 9

        #Sum values by Region
        temp_hist_prod = temp_hist_prod.groupby([nam.region_number, nam.year]).sum()
        temp_hist_prod = temp_hist_prod[temp_hist_prod.index.get_level_values(1) <= self.parent.history_year]
        temp_hist_prod.index.names = self.restart.ogsmout_ogngprd.index.names


        #Overwrite ogsmout_ogngprd
        temp_ogngprd = temp_hist_prod[temp_hist_prod.index.get_level_values(0).isin([1,2,3,4,5,6,7,8,9,10])].copy()
        self.restart.ogsmout_ogngprd.update(temp_ogngprd.astype(self.restart.ogsmout_ogngprd.dtypes))

        #Get ogsmout_ogngprd total
        temp = self.restart.ogsmout_ogngprd.copy()
        temp = temp[temp.index.get_level_values(1) <= self.parent.history_year]
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], level=0)]
        temp = temp.groupby(level = 1).sum()
        temp[1] = 11
        temp = temp.set_index([1], append = True)
        temp = temp.reorder_levels([1,0])
        temp.index.names = self.restart.ogsmout_ogngprd.index.names
        self.restart.ogsmout_ogngprd.update(temp.astype(self.restart.ogsmout_ogngprd.dtypes))


        ###Overwrite natural gas production by category
        on_realized_prod_df = self.restart.ogsmout_ogrnagprd.copy()
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(2) <= self.parent.history_year]
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(0) <= 66]  #Take out offshore
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(0) != 3]  #Take out Alaska

        on_ad_prod_df = self.restart.ogsmout_ogadgprd.copy()
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(2) <= self.parent.history_year]
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(0) <= 66]  # Take out offshore
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(0) != 3]  # Take out Alaska

        ###Natural Gas Production by Category
        #Onshore Shale Production plus gas production from tight oil plays
        temp_realized_shale = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby(level=[2]).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1) == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index <= self.parent.history_year]

        temp_realized_shale['value'] = temp_realized_shale['value'].values + temp_ad_prod['value'].values
        temp_realized_shale['1'] = 1
        temp_realized_shale = (temp_realized_shale.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_realized_shale.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_realized_shale.astype(self.restart.ogsmout_ogqngrep.dtypes))

        #Coalbed Methane Production
        temp_realized_cbm = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 4].copy()
        temp_realized_cbm = temp_realized_cbm.groupby(level=[2]).sum()
        temp_realized_cbm['1'] = 2
        temp_realized_cbm = (temp_realized_cbm.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_realized_cbm.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_realized_cbm.astype(self.restart.ogsmout_ogqngrep.dtypes))

        #Tight Production
        temp_realized_tight = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 2].copy()
        temp_realized_tight = temp_realized_tight.groupby(level=[2]).sum()
        temp_realized_tight['1'] = 3
        temp_realized_tight = (temp_realized_tight.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_realized_tight.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_realized_tight.astype(self.restart.ogsmout_ogqngrep.dtypes))

        #Conventional Production
        temp_realized_conv = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 1].copy()
        temp_realized_conv = temp_realized_conv.groupby(level=[2]).sum()
        temp_realized_conv['1'] = 4
        temp_realized_conv = (temp_realized_conv.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_realized_conv.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_realized_conv.astype(self.restart.ogsmout_ogqngrep.dtypes))

        # Other AD Gas Production
        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1).isin([1])].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index <= self.parent.history_year]
        temp_ad_prod['1'] = 5
        temp_ad_prod = (temp_ad_prod.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_ad_prod.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_ad_prod.astype(self.restart.ogsmout_ogqngrep.dtypes))

        ###Offshore Realized Production Adjustment
        # Since NA and AD natgas aren't split up, we need to produce a production ratio here
        off_realized_prod_df = self.restart.ogsmout_ogrnagprd.copy()
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(2) <= self.parent.history_year]
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(0) >= 67]  # Take out onshore
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(0) != 75]  # Take out alaska state offshore
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        off_ad_prod_df = self.restart.ogsmout_ogadgprd.copy()
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(2) <= self.parent.history_year]
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) >= 67]  # Take out onshore
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) != 75]  # Take out alaska state offshore
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        #NA Offshore Production
        temp_realized_na_prod_off = off_realized_prod_df.loc[off_realized_prod_df.index.get_level_values(1) == 5].copy()
        temp_realized_na_prod_off = temp_realized_na_prod_off.groupby(level=[2]).sum()
        temp_realized_na_prod_off['1'] = 6
        temp_realized_na_prod_off = (temp_realized_na_prod_off.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_realized_na_prod_off.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_realized_na_prod_off[[nam.value]].astype(self.restart.ogsmout_ogqngrep.dtypes))

        #AD Offshore Production
        temp_ad_prod = off_ad_prod_df.loc[off_ad_prod_df.index.get_level_values(1) == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index <= self.parent.history_year]
        temp_ad_prod['1'] = 7
        temp_ad_prod = (temp_ad_prod.set_index(['1'], append=True)).reorder_levels([1, 0])
        temp_ad_prod.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp_ad_prod.astype(self.restart.ogsmout_ogqngrep.dtypes))

        ###Alaska Natural Gas Production
        alaska_na_prod = self.restart.ogsmout_ogrnagprd.copy()
        alaska_na_prod = alaska_na_prod[alaska_na_prod.index.get_level_values(2) <= self.parent.history_year]
        alaska_na_prod = alaska_na_prod[alaska_na_prod.index.get_level_values(0).isin([3, 75, 84])] # Alaska incl. offshore
        alaska_na_prod = alaska_na_prod.loc[alaska_na_prod.index.get_level_values(1) == 5].copy()
        alaska_na_prod = alaska_na_prod.groupby(level=2).sum()

        alaska_ad_prod = self.restart.ogsmout_ogadgprd.copy()
        alaska_ad_prod = alaska_ad_prod[alaska_ad_prod.index.get_level_values(2) <= self.parent.history_year]
        alaska_ad_prod = alaska_ad_prod[alaska_ad_prod.index.get_level_values(0).isin([3, 75, 84])] # Alaska incl. offshore
        alaska_ad_prod = alaska_ad_prod.loc[alaska_ad_prod.index.get_level_values(1) == 5].copy()
        alaska_ad_prod = alaska_ad_prod.groupby(level=2).sum()

        alaska_ng_prod = alaska_na_prod + alaska_ad_prod
        alaska_ng_prod['1'] = 8
        alaska_ng_prod = (alaska_ng_prod.set_index(['1'], append=True)).reorder_levels([1, 0])
        alaska_ng_prod.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(alaska_ng_prod.astype(self.restart.ogsmout_ogqngrep.dtypes))

        ###Natural Gas Production by Category
        #Onshore Shale Production plus gas production from tight oil plays
        temp_realized_shale = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 5].copy()

        temp_realized_shale = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby(level=[2]).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1) == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index <= self.parent.history_year]


        ###Aggregate NG Production by category
        temp = self.restart.ogsmout_ogqngrep.copy()
        temp = temp[temp.index.get_level_values(1) <= self.parent.history_year]
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[1] = 9
        temp = temp.set_index([1], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogqngrep.index.names
        self.restart.ogsmout_ogqngrep.update(temp.astype(self.restart.ogsmout_ogqngrep.dtypes))


        ###Overwrite play-level natural gas production
        self.restart.ogsmout_ogqshlgas.update(self.hist_ogqshlgas.astype(self.restart.ogsmout_ogqshlgas.dtypes))

        ###Adjust OGQSHLGAS to match OGQNGREP index 1 for historical years
        # Adjust 'other' category (index 15) to ensure total play-level production matches OGQNGREP shale gas total
        # This ensures consistency between the aggregate (OGQNGREP) and play-level (OGQSHLGAS) shale gas data
        # OGQNGREP is in BCF/year, OGQSHLGAS is in TCF/year
        hist_years_mask = self.restart.ogsmout_ogqshlgas.index.get_level_values(1) <= self.parent.history_year
        hist_years = sorted(self.restart.ogsmout_ogqshlgas.loc[hist_years_mask].index.get_level_values(1).unique())
        
        for year in hist_years:
            # Calculate sum of all OGQSHLGAS plays for this year
            shale_sum = self.restart.ogsmout_ogqshlgas.xs(year, level=1, drop_level=False)['value'].sum()
            
            # Get OGQNGREP index 1 (shale gas total) for this year
            ogqngrep_shale = self.restart.ogsmout_ogqngrep.at[(1, year), 'value']
            
            # Calculate difference: convert OGQSHLGAS sum from TCF to BCF for comparison
            # Then convert difference back to TCF for adjustment
            tempscale = ogqngrep_shale - shale_sum * 1000
            
            # Apply adjustment to index 15 (other category)
            # Use .loc[] for accessing the value, ensuring it's a scalar
            # Cast to the column's dtype to avoid FutureWarning
            value_dtype = self.restart.ogsmout_ogqshlgas['value'].dtype
            self.restart.ogsmout_ogqshlgas.loc[(15, year), 'value'] = (
                self.restart.ogsmout_ogqshlgas.loc[(15, year), 'value'] + tempscale / 1000
            ).astype(value_dtype)


        ###Overwrite Oil and Natural Gas Production by type of production
        # Crude Oil (1 = Primary Crude Oil, 2 = Secondary/EOR Production)
        # Use pre-computed regional EOR data from self.hist_eor_prod
        
        # Get rfqdcrd (crude excluding EOR) for primary production (type=1)
        temp_rfqdcrd = self.restart.pmmout_rfqdcrd.copy()
        temp_rfqdcrd = temp_rfqdcrd.loc[temp_rfqdcrd.index.get_level_values(0) <= 7]
        temp_rfqdcrd = temp_rfqdcrd.loc[temp_rfqdcrd.index.get_level_values(1) <= self.parent.history_year]
        
        # Get regional EOR data (convert from BBL/D to MMBBL/D)
        temp_eor_prod = self.hist_eor_prod.copy() / 1000000
        
        for region in range(1, 8):
            # Get ogregprd template for this region and history years
            temp_ogregprd = self.restart.ogsmout_ogregprd.copy()
            temp_ogregprd = temp_ogregprd.loc[temp_ogregprd.index.get_level_values(2) <= self.parent.history_year]
            temp_ogregprd_primary = temp_ogregprd.loc[(temp_ogregprd.index.get_level_values(0) == region) & 
                                                       (temp_ogregprd.index.get_level_values(1) == 1)].copy()
            temp_ogregprd_eor = temp_ogregprd.loc[(temp_ogregprd.index.get_level_values(0) == region) & 
                                                   (temp_ogregprd.index.get_level_values(1) == 2)].copy()
            
            # Get rfqdcrd and EOR data for this region
            region_rfqdcrd = temp_rfqdcrd.xs(region, level=0)
            region_eor = temp_eor_prod.xs(region, level=0)
            
            # Update primary production (type=1) with rfqdcrd
            temp_ogregprd_primary[nam.value] = region_rfqdcrd[nam.value].values
            self.restart.ogsmout_ogregprd.update(temp_ogregprd_primary.astype(self.restart.ogsmout_ogregprd.dtypes))
            
            # Update EOR production (type=2) with hist_eor_prod
            temp_ogregprd_eor[nam.value] = region_eor[nam.value].values
            self.restart.ogsmout_ogregprd.update(temp_ogregprd_eor.astype(self.restart.ogsmout_ogregprd.dtypes))


        ###Update ogqcrrep (EOR, Conv & Tight, Offshore, Alaska, Anwr, Total) history
        # Use pre-computed EOR data from self.hist_eor_prod for consistency with ogeorprd/ogregprd
        temp_eor_prod = self.hist_eor_prod.copy() / 1000000  # Convert BBL/D to MMBBL/D
        temp_eor_prod = temp_eor_prod.loc[temp_eor_prod.index.get_level_values(0) <= 7]  # Regions 1-7
        temp_eor_prod = temp_eor_prod.loc[temp_eor_prod.index.get_level_values(1) <= self.parent.history_year]  # History only
        temp_only_eor = temp_eor_prod.groupby(level=1).sum() * 365  # Sum by year, convert to MMBBL/yr

        # EOR (category 1)
        temp_only_eor['1'] = 1
        temp_only_eor = (temp_only_eor.set_index(['1'], append=True)).reorder_levels([1,0])
        temp_only_eor.index.names =  self.restart.ogsmout_ogqcrrep.index.names
        self.restart.ogsmout_ogqcrrep.update(temp_only_eor.astype(self.restart.ogsmout_ogqcrrep.dtypes))

        # Calculate temp_excl_eor for Conventional & Tight category (category 2)
        temp_excl_eor = self.restart.pmmout_rfqdcrd.copy().loc[([1,2,3,4,5,6,7], slice(None))].groupby(level=1).sum()
        temp_excl_eor = temp_excl_eor.loc[temp_excl_eor.index <= self.parent.history_year] * 365 # convert to mmbbl/yr

        # Conventional & Tight
        temp_excl_eor['1'] = 2
        temp_excl_eor = (temp_excl_eor.set_index(['1'], append=True)).reorder_levels([1,0])
        temp_excl_eor.index.names =  self.restart.ogsmout_ogqcrrep.index.names
        self.restart.ogsmout_ogqcrrep.update(temp_excl_eor.astype(self.restart.ogsmout_ogqcrrep.dtypes))

        # Offshore
        temp_offshore = self.restart.pmmout_rfqtdcrd.copy().loc[([8,9,10], slice(None))].groupby(level=1).sum() #includes atlantic, GOM, pacific (excludes alaska offshore)
        temp_offshore = temp_offshore.loc[temp_offshore.index <= self.parent.history_year] * 365 # convert to mmbbl/yr
        temp_offshore['1'] = 3
        temp_offshore = (temp_offshore.set_index(['1'], append=True)).reorder_levels([1,0])
        temp_offshore.index.names =  self.restart.ogsmout_ogqcrrep.index.names
        self.restart.ogsmout_ogqcrrep.update(temp_offshore.astype(self.restart.ogsmout_ogqcrrep.dtypes))

        # Alaska
        temp_alaska = self.restart.pmmout_rfqtdcrd.copy().loc[([14], slice(None))].groupby(level=1).sum() # Alaska
        temp_alaska = temp_alaska.loc[temp_alaska.index <= self.parent.history_year] * 365 # convert to mmbbl/yr
        temp_alaska['1'] = 4
        temp_alaska = (temp_alaska.set_index(['1'], append=True)).reorder_levels([1,0])
        temp_alaska.index.names =  self.restart.ogsmout_ogqcrrep.index.names
        self.restart.ogsmout_ogqcrrep.update(temp_alaska.astype(self.restart.ogsmout_ogqcrrep.dtypes))


        ###Apply adjusted realized natural gas volumes to ogsmout_ogregprd (4 = Conventional gas, 5 = Tight Gas, 6 = Shale Gas, 7 = CBM)
        ###Overwrite natural gas production by category
        on_realized_prod_df = self.restart.ogsmout_ogrnagprd.copy()
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(2) <= self.parent.history_year]
        on_realized_prod_df = on_realized_prod_df.reset_index()
        on_realized_prod_df = on_realized_prod_df.merge(self.parent.mapping[[nam.district_number, nam.region_number]],
                                                        how = 'left',
                                                        left_on = '1',
                                                        right_on = nam.district_number)
        on_realized_prod_df = on_realized_prod_df.loc[on_realized_prod_df[nam.region_number] <= 7]
        on_realized_prod_df = on_realized_prod_df.drop([nam.district_number], axis = 1)


        on_ad_prod_df = self.restart.ogsmout_ogadgprd.copy()
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(2) <= self.parent.history_year]
        on_ad_prod_df = on_ad_prod_df.reset_index()
        on_ad_prod_df = on_ad_prod_df.merge(self.parent.mapping[[nam.district_number, nam.region_number]],
                                                        how = 'left',
                                                        left_on = '1',
                                                        right_on = nam.district_number)
        on_ad_prod_df = on_ad_prod_df.loc[on_ad_prod_df[nam.region_number] <= 7]
        on_ad_prod_df = on_ad_prod_df.drop([nam.district_number], axis = 1)


        #Conventional Production
        temp_realized_conv = on_realized_prod_df.loc[on_realized_prod_df['2'] == 1].copy()
        temp_realized_conv = temp_realized_conv.groupby([nam.region_number, '3']).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df['2'] == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby([nam.region_number, '3']).sum()

        temp_realized_conv['value'] = temp_realized_conv['value'].values + temp_ad_prod['value'].values
        temp_realized_conv['2'] = 4
        temp_realized_conv = (temp_realized_conv.set_index(['2'], append=True)).reorder_levels([0,2,1])
        temp_realized_conv.index.names = self.restart.ogsmout_ogregprd.index.names
        self.restart.ogsmout_ogregprd.update((temp_realized_conv / 1000).astype(self.restart.ogsmout_ogregprd.dtypes))

        #Tight Production - recategorize to shale gas (index 6) instead of tight gas (index 5)
        # Extract tight gas from ogrnagprd (gas type 2) - DO NOT modify ogrnagprd itself
        temp_realized_tight = on_realized_prod_df.loc[on_realized_prod_df['2'] == 2].copy()
        temp_realized_tight = temp_realized_tight.groupby([nam.region_number, '3']).sum()

        #Onshore Shale Production plus gas production from tight oil plays
        temp_realized_shale = on_realized_prod_df.loc[on_realized_prod_df['2'] == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby([nam.region_number, '3']).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df['2'] == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby([nam.region_number, '3']).sum()

        # Add tight gas volumes to shale gas (index 6) instead of writing to index 5
        # Use .add() with fill_value=0 to properly align indices and handle missing combinations
        temp_realized_shale['value'] = temp_realized_shale['value'].add(temp_ad_prod['value'], fill_value=0).add(temp_realized_tight['value'], fill_value=0)
        temp_realized_shale['2'] = 6
        temp_realized_shale = (temp_realized_shale.set_index(['2'], append=True)).reorder_levels([0,2,1])
        temp_realized_shale.index.names = self.restart.ogsmout_ogregprd.index.names
        self.restart.ogsmout_ogregprd.update((temp_realized_shale / 1000).astype(self.restart.ogsmout_ogregprd.dtypes))

        #Coalbed Methane Production
        temp_realized_cbm = on_realized_prod_df.loc[on_realized_prod_df['2'] == 4].copy()
        temp_realized_cbm = temp_realized_cbm.groupby([nam.region_number, '3']).sum()
        temp_realized_cbm['2'] = 7
        temp_realized_cbm = (temp_realized_cbm.set_index(['2'], append=True)).reorder_levels([0,2,1])
        temp_realized_cbm.index.names = self.restart.ogsmout_ogregprd.index.names
        self.restart.ogsmout_ogregprd.update((temp_realized_cbm / 1000).astype(self.restart.ogsmout_ogregprd.dtypes))

        # Zero out tight gas (index 5) for all history years after recategorizing to shale gas
        history_years_mask = self.restart.ogsmout_ogregprd.index.get_level_values(2) <= self.parent.history_year
        tight_gas_mask = self.restart.ogsmout_ogregprd.index.get_level_values(1) == 5
        tight_gas_history_mask = history_years_mask & tight_gas_mask
        self.restart.ogsmout_ogregprd.loc[tight_gas_history_mask, 'value'] = 0

        # Crude production on federal lands
        # Update with historical data from CSV
        self.restart.ogsmout_ogcoprd_fed.update(self.hist_ogcoprd_fed.astype(self.restart.ogsmout_ogcoprd_fed.dtypes))

        # Crude production on non-federal lands
        # Calculate for onshore regions (1-7) only: nonfed = total - fed
        temp_ogcoprd = self.restart.ogsmout_ogcoprd.copy()
        temp_ogcoprd = temp_ogcoprd[temp_ogcoprd.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7])]
        temp_ogcoprd = temp_ogcoprd[temp_ogcoprd.index.get_level_values(1) <= self.parent.history_year]

        temp_ogcoprd_fed = self.restart.ogsmout_ogcoprd_fed.copy()
        temp_ogcoprd_fed = temp_ogcoprd_fed[temp_ogcoprd_fed.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7])]
        temp_ogcoprd_fed = temp_ogcoprd_fed[temp_ogcoprd_fed.index.get_level_values(1) <= self.parent.history_year]

        # Calculate non-federal = total - federal
        # Use .sub() with fill_value=0 to handle index alignment properly
        temp_ogcoprd_nonfed = temp_ogcoprd.copy()
        temp_ogcoprd_nonfed['value'] = temp_ogcoprd['value'].sub(temp_ogcoprd_fed['value'], fill_value=0)
        temp_ogcoprd_nonfed.loc[temp_ogcoprd_nonfed['value'] < 0, 'value'] = 0.0

        # Update restart variable
        self.restart.ogsmout_ogcoprd_nonfed.update(temp_ogcoprd_nonfed.astype(self.restart.ogsmout_ogcoprd_nonfed.dtypes))

        # Natural Gas production on federal lands
        # Update with historical data from CSV
        self.restart.ogsmout_ogngprd_fed.update(self.hist_ogngprd_fed.astype(self.restart.ogsmout_ogngprd_fed.dtypes))

        # Natural Gas production on non-federal lands
        # Calculate for onshore regions (1-7) only: nonfed = total - fed
        # Get total natural gas production for onshore regions (1-7) and historical years (units: bcf/yr)
        temp_ogngprd = self.restart.ogsmout_ogngprd.copy()
        temp_ogngprd = temp_ogngprd[temp_ogngprd.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7])]
        temp_ogngprd = temp_ogngprd[temp_ogngprd.index.get_level_values(1) <= self.parent.history_year]

        # Convert from bcf/yr to tcf/yr
        temp_ogngprd['value'] = temp_ogngprd['value'] / 1000

        # Get federal natural gas production for same regions/years (already in tcf)
        temp_ogngprd_fed = self.restart.ogsmout_ogngprd_fed.copy()
        temp_ogngprd_fed = temp_ogngprd_fed[temp_ogngprd_fed.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7])]
        temp_ogngprd_fed = temp_ogngprd_fed[temp_ogngprd_fed.index.get_level_values(1) <= self.parent.history_year]

        # Calculate non-federal = total - federal
        # Use .sub() with fill_value=0 to handle index alignment properly
        temp_ogngprd_nonfed = temp_ogngprd.copy()
        temp_ogngprd_nonfed['value'] = temp_ogngprd['value'].sub(temp_ogngprd_fed['value'], fill_value=0)
        temp_ogngprd_nonfed.loc[temp_ogngprd_nonfed['value'] < 0, 'value'] = 0.0

        # Update restart variable
        self.restart.ogsmout_ogngprd_nonfed.update(temp_ogngprd_nonfed.astype(self.restart.ogsmout_ogngprd_nonfed.dtypes))

        ###Overwrite district-level wells
        self.restart.ogsmout_ogogwells.update(self.hist_htowells.astype(self.restart.ogsmout_ogogwells.dtypes))
        self.restart.ogsmout_ogogwells.update(self.hist_htgwells.astype(self.restart.ogsmout_ogogwells.dtypes))
        self.restart.ogsmout_ogogwells.update(self.hist_hsgwells.astype(self.restart.ogsmout_ogogwells.dtypes))
        self.restart.ogsmout_ogogwells.update(self.hist_hdryholes.astype(self.restart.ogsmout_ogogwells.dtypes))
        self.restart.ogsmout_ogogwells.update(self.hist_hcowells.astype(self.restart.ogsmout_ogogwells.dtypes))
        self.restart.ogsmout_ogogwells.update(self.hist_hcgwells.astype(self.restart.ogsmout_ogogwells.dtypes))
        self.restart.ogsmout_ogogwells.update(self.hist_hcbmwells.astype(self.restart.ogsmout_ogogwells.dtypes))

        # Recategorize historical tight gas wells (well_type 4) to shale gas wells (well_type 5) in ogogwells
        # Extract tight gas wells (well_type 4) for history years
        history_years_mask_wells = self.restart.ogsmout_ogogwells.index.get_level_values(2) <= self.parent.history_year
        tight_wells_mask = self.restart.ogsmout_ogogwells.index.get_level_values(1) == 4
        tight_wells_history_mask = history_years_mask_wells & tight_wells_mask

        # Get tight gas well counts for history years
        temp_tight_wells = self.restart.ogsmout_ogogwells.loc[tight_wells_history_mask].copy()
        if len(temp_tight_wells) > 0:
            # Reset index to extract district and year, then set well_type to 5 (shale gas wells)
            temp_tight_wells = temp_tight_wells.reset_index()
            temp_tight_wells['2'] = 5  # Change well_type from 4 to 5
            temp_tight_wells = temp_tight_wells.set_index(['1', '2', '3'])
            temp_tight_wells.index.names = self.restart.ogsmout_ogogwells.index.names
            
            # Get existing shale gas wells (well_type 5) for history years
            shale_wells_mask = self.restart.ogsmout_ogogwells.index.get_level_values(1) == 5
            shale_wells_history_mask = history_years_mask_wells & shale_wells_mask
            temp_shale_wells = self.restart.ogsmout_ogogwells.loc[shale_wells_history_mask].copy()
            
            # Add tight gas wells to shale gas wells using .add() for proper index alignment
            if len(temp_shale_wells) > 0:
                temp_shale_wells['value'] = temp_shale_wells['value'].add(temp_tight_wells['value'], fill_value=0)
                self.restart.ogsmout_ogogwells.update(temp_shale_wells.astype(self.restart.ogsmout_ogogwells.dtypes))
            else:
                # If no existing shale gas wells, just add the tight gas wells as shale gas wells
                self.restart.ogsmout_ogogwells.update(temp_tight_wells.astype(self.restart.ogsmout_ogogwells.dtypes))
            
            # Zero out tight gas wells (well_type 4) for all history years
            self.restart.ogsmout_ogogwells.loc[tight_wells_history_mask, 'value'] = 0

        ###Overwrite Total historical well count
        for year in self.hist_ognowell.index:
            self.restart.ogsmout_ognowell.loc[year] = self.hist_ognowell.at[year, nam.value]

        ###Overwrite NGPL production
        self.restart.ogsmout_ogngplprd.update(self.hist_ngplprd)
        self.restart.ogsmout_ogngplpp.update(self.hist_eplp.astype(self.restart.ogsmout_ogngplpp.dtypes))
        self.restart.ogsmout_ogngplpr.update(self.hist_epllpa.astype(self.restart.ogsmout_ogngplpr.dtypes))
        self.restart.ogsmout_ogngplet.update(self.hist_epllea.astype(self.restart.ogsmout_ogngplet.dtypes))
        self.restart.ogsmout_ogngplbu.update(self.hist_epllban.astype(self.restart.ogsmout_ogngplbu.dtypes))
        self.restart.ogsmout_ogngplis.update(self.hist_epllbai.astype(self.restart.ogsmout_ogngplis.dtypes))
        self.restart.ogsmout_ogngplprd.update(self.hist_epl2.astype(self.restart.ogsmout_ogngplprd.dtypes))

        pass


    def calculate_ngpl_region_shares(self):
        """EIA STEO reporting for NGPL values has different regions than HSM, this equation splits out the regions
           proportionally so that STEO NGPL regions are matched to HSM regions.

        Returns
        -------
        self.ngpl_shares : df
            DataFrame of historical ngpl shares by HSM regions
        """
        #Format NGPL table
        temp_ngpl = self.restart.ogsmout_ogngplprd.copy()
        temp_ngpl = temp_ngpl.unstack()
        temp_ngpl.columns = list(range(self.parent.param_baseyr,self.parent.final_aeo_year + 1))

        #Merge ngpl table to mapping and retain index
        temp_ngpl = temp_ngpl.merge(self.parent.mapping[[nam.district_number,nam.region_number,nam.ngpl_region_number]],
                                    how = 'left', left_index = True, right_on = nam.district_number).set_index(nam.district_number)

        #Get regional sums of ngpl production for history year
        temp_sum_ngpl = temp_ngpl[[self.parent.history_year, nam.ngpl_region_number]]
        temp_sum_ngpl = temp_sum_ngpl.groupby(nam.ngpl_region_number).sum()
        temp_sum_ngpl.columns = [nam.ngpl_region_sum]

        #Merge to new table with temp_ngpl for history year to get share
        self.ngpl_shares = temp_ngpl[[self.parent.history_year, nam.ngpl_region_number]].merge(temp_sum_ngpl,
            how = 'left', left_on = nam.ngpl_region_number, right_index = True)

        #Get NGPL shares
        self.ngpl_shares[nam.ngpl_share_ratio] = self.ngpl_shares[self.parent.history_year].div(self.ngpl_shares[nam.ngpl_region_sum])
        self.ngpl_shares[nam.ngpl_share_ratio] = self.ngpl_shares[nam.ngpl_share_ratio].fillna(0.0)

        #Hardcode Alaska and California
        self.ngpl_shares.at[3, nam.ngpl_share_ratio] = np.float32(0.45)
        self.ngpl_shares.at[6, nam.ngpl_share_ratio] = np.float32(0.55)

        pass


    ###STEO###

    def load_steo_tables(self):
        """Load in STEO benchmark values.

        Returns
        -------
        self.steo_dcrdwhp : df
            DataFrame of STEO year domestic crude oil wellhead price by region

        self.steo_rfqtdcrd : df
            DataFrame of STEO year total crude production by HSM region

        self.steo_ogenagprd : df
            DataFrame of STEO year natural gas expected production by natural gas type and HSM district

        self.steo_ogadgprd : df
            DataFrame of STEO year natural gas associated dissolved production by oil type and HSM district

        self.steo_can : df
            DataFrame of STEO year natural gas production by HSM Canada region and type

        self.steo_togqshloil : df
            DataFrame of STEO year crude oil production by select tight oil play

        self.steo_togqshlgas : df
            DataFrame of STEO year natural gas production by select tight oil play

        self.steo_ogngplprd : df
            DataFrame of STEO year NGPL production by HSM district

        self.steo_ogngplet : df
            DataFrame of STEO year ethane production by HSM district

        self.steo_ogngplpr : df
            DataFrame of STEO year propane production by HSM district

        self.steo_ogngplbu : df
            DataFrame of STEO year butane production by HSM district

        self.steo_ogngplis : df
            DataFrame of STEO year isobutane production by HSM district

        self.steo_ogngplpp : df
            DataFrame of STEO year pentanes production by HSM district
        """
        #Read in STEO Setup Table
        temp_filename = self.parent.setup_table.at[nam.steo_setup, nam.filename]
        self.steo_setup_table = com.read_dataframe(self.parent.input_path + temp_filename, index_col=0)


        ###Read in STEO Tables

        #Domestic Crude Oil Production (MMBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_rfqtdcrd, nam.filename]
        self.steo_rfqtdcrd = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.region_number])
        #Stack DF and format to match restart file
        self.steo_rfqtdcrd = pd.DataFrame(self.steo_rfqtdcrd.stack())
        self.steo_rfqtdcrd.index.names = ['1', '2']
        self.steo_rfqtdcrd.columns = [nam.value]

        #Crude Oil Play-level Production, MMBBL/D for the first year and adjustment factor for the last two years
        temp_filename = self.steo_setup_table.at[nam.steo_togqshloil, nam.filename]
        self.steo_togqshloil = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.play])
        self.steo_togqshloil.index = [1,2,3,4,5,6,7,8,9,10,15]
        #Stack DF and format to match restart file
        self.steo_togqshloil = pd.DataFrame(self.steo_togqshloil.stack())
        self.steo_togqshloil.index.names = ['1', '2']
        self.steo_togqshloil.columns = [nam.value]

        #Natural Gas Play-level Production, tcf for the first year and adjustment factor for the last two years
        temp_filename = self.steo_setup_table.at[nam.steo_togqshlgas, nam.filename]
        self.steo_togqshlgas = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.play])
        self.steo_togqshlgas.index = [1,2,3,4,5,6,7,8,9,10,15]
        #Stack DF and format to match restart file
        self.steo_togqshlgas = pd.DataFrame(self.steo_togqshlgas.stack())
        self.steo_togqshlgas.index.names = ['1', '2']
        self.steo_togqshlgas.columns = [nam.value]

        #NGPL Production (MBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_ogngplprd, nam.filename]
        self.steo_ogngplprd = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.ngpl_region_number])
        self.steo_ogngplprd = self.steo_ogngplprd.drop(columns=[nam.ngpl_region_name, nam.ngpl_region_description], errors='ignore')
        self.steo_ogngplprd = self.steo_ogngplprd.div(1000, axis = 1) #Convert to MMBBL/D
        #Assign production to HSM districts
        self.steo_ogngplprd = com.assign_ngpls_to_district(self.steo_ogngplprd, self.ngpl_shares, self.parent.steo_years)
        #Stack DF and format to match restart file
        self.steo_ogngplprd = pd.DataFrame(self.steo_ogngplprd.stack())
        self.steo_ogngplprd.index.names = ['1', '2']
        self.steo_ogngplprd.columns = [nam.value]

        #Ethane Production (MBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_ogngplet, nam.filename]
        self.steo_ogngplet = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.ngpl_region_number])
        self.steo_ogngplet = self.steo_ogngplet.drop(columns=[nam.ngpl_region_name, nam.ngpl_region_description], errors='ignore')
        self.steo_ogngplet = self.steo_ogngplet.div(1000, axis=1)  # Convert to MMBBL/D
        #Assign production to HSM districts
        self.steo_ogngplet = com.assign_ngpls_to_district(self.steo_ogngplet, self.ngpl_shares, self.parent.steo_years)
        #Stack DF and format to match restart file
        self.steo_ogngplet = pd.DataFrame(self.steo_ogngplet.stack())
        self.steo_ogngplet.index.names = ['1', '2']
        self.steo_ogngplet.columns = [nam.value]

        #Propane Production (MBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_ogngplpr, nam.filename]
        self.steo_ogngplpr = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.ngpl_region_number])
        self.steo_ogngplpr = self.steo_ogngplpr.drop(columns=[nam.ngpl_region_name, nam.ngpl_region_description], errors='ignore')
        self.steo_ogngplpr = self.steo_ogngplpr.div(1000, axis=1)  # Convert to MMBBL/D
        #Assign production to HSM districts
        self.steo_ogngplpr = com.assign_ngpls_to_district(self.steo_ogngplpr, self.ngpl_shares, self.parent.steo_years)
        #Stack DF and format to match restart file
        self.steo_ogngplpr = pd.DataFrame(self.steo_ogngplpr.stack())
        self.steo_ogngplpr.index.names = ['1', '2']
        self.steo_ogngplpr.columns = [nam.value]

        #Butane Production (MBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_ogngplbu, nam.filename]
        self.steo_ogngplbu = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.ngpl_region_number])
        self.steo_ogngplbu = self.steo_ogngplbu.drop(columns=[nam.ngpl_region_name, nam.ngpl_region_description], errors='ignore')
        self.steo_ogngplbu = self.steo_ogngplbu.div(1000, axis=1)  # Convert to MMBBL/D
        #Assign production to HSM districts
        self.steo_ogngplbu = com.assign_ngpls_to_district(self.steo_ogngplbu, self.ngpl_shares, self.parent.steo_years)
        #Stack DF and format to match restart file
        self.steo_ogngplbu = pd.DataFrame(self.steo_ogngplbu.stack())
        self.steo_ogngplbu.index.names = ['1', '2']
        self.steo_ogngplbu.columns = [nam.value]

        #Isobutane Production (MBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_ogngplis, nam.filename]
        self.steo_ogngplis = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.ngpl_region_number])
        self.steo_ogngplis = self.steo_ogngplis.drop(columns=[nam.ngpl_region_name, nam.ngpl_region_description], errors='ignore')
        self.steo_ogngplis = self.steo_ogngplis.div(1000, axis=1)  # Convert to MMBBL/D
        #Assign production to HSM districts
        self.steo_ogngplis = com.assign_ngpls_to_district(self.steo_ogngplis, self.ngpl_shares, self.parent.steo_years)
        #Stack DF and format to match restart file
        self.steo_ogngplis = pd.DataFrame(self.steo_ogngplis.stack())
        self.steo_ogngplis.index.names = ['1', '2']
        self.steo_ogngplis.columns = [nam.value]

        #Pentanes Production (MBBL/D)
        temp_filename = self.steo_setup_table.at[nam.steo_ogngplpp, nam.filename]
        self.steo_ogngplpp = com.read_dataframe(self.parent.steo_input_path + temp_filename, skiprows=1, index_col=[nam.ngpl_region_number])
        self.steo_ogngplpp = self.steo_ogngplpp.drop(columns=[nam.ngpl_region_name, nam.ngpl_region_description], errors='ignore')
        self.steo_ogngplpp = self.steo_ogngplpp.div(1000, axis=1)  # Convert to MMBBL/D
        #Assign production to HSM districts
        self.steo_ogngplpp = com.assign_ngpls_to_district(self.steo_ogngplpp, self.ngpl_shares, self.parent.steo_years)
        #Stack DF and format to match restart file
        self.steo_ogngplpp = pd.DataFrame(self.steo_ogngplpp.stack())
        self.steo_ogngplpp.index.names = ['1', '2']
        self.steo_ogngplpp.columns = [nam.value]

        pass


    def overwrite_restart_var_steo(self):
        """Overwrites Restart variables with STEO variables for PMMOUT variables used to calculate price.

        Returns
        -------
        self.restart.pmmout_dcrdwhp : df
            Domestic crude oil wellhead price by region

        self.restart.pmmout_rfqtdcrd : df
            Total crude production by HSM region
        """
        #Explicitly cast data to NumPy Float32
        # Use pd.to_numeric to handle any non-numeric values (e.g., column names that may have been stacked)
        self.steo_rfqtdcrd['value'] = pd.to_numeric(self.steo_rfqtdcrd['value'], errors='coerce').astype(np.float32)

        ###Domestic Crude Oil Production (MMBBL/D) - Do this up front to get prices, will need to be done a second time at end of run
        self.restart.pmmout_rfqtdcrd.update(self.steo_rfqtdcrd.astype(self.restart.pmmout_rfqtdcrd.dtypes))

        ###Domestic Crude Oil Production (MMBBL/D) - EOR
        self.restart.pmmout_rfqdcrd.update(self.steo_rfqtdcrd.astype(self.restart.pmmout_rfqdcrd.dtypes))

        #Set Domestic Crude Oil Production for steo years based on history year ratio
        temp_rfqtdcrd = self.restart.pmmout_rfqtdcrd.copy()
        temp_rfqtdcrd = temp_rfqtdcrd[temp_rfqtdcrd.index.get_level_values(0).isin([1,2,3,4,5,6,7])]
        temp_rfqdcrd = self.restart.pmmout_rfqdcrd.copy()
        temp_rfqdcrd = temp_rfqdcrd[temp_rfqdcrd.index.get_level_values(0).isin([1,2,3,4,5,6,7])]


        #Get history year EOR ratio
        temp_rfqtdcrd = temp_rfqtdcrd.xs(self.parent.history_year, level=1).copy()
        temp_eor_ratio = temp_rfqdcrd.xs(self.parent.history_year, level=1).copy()
        temp_eor_ratio = pd.DataFrame(temp_eor_ratio[nam.value] / temp_rfqtdcrd[nam.value])
        temp_eor_ratio.columns = ['eor_ratio']

        #Get steo years for ogsmout_rfqdcrd and apply eor ratio
        temp_rfqdcrd = temp_rfqdcrd[temp_rfqdcrd.index.get_level_values(1).isin(self.parent.steo_years[0:2])]
        temp_rfqdcrd = temp_rfqdcrd.reset_index()
        temp_rfqdcrd = temp_rfqdcrd.merge(temp_eor_ratio, how = 'left', left_on = '1', right_index = True)
        temp_rfqdcrd[nam.value] = temp_rfqdcrd[nam.value] * temp_rfqdcrd['eor_ratio']
        temp_rfqdcrd = temp_rfqdcrd.set_index(['1', '2'])
        self.restart.pmmout_rfqdcrd.update(temp_rfqdcrd[nam.value].astype(self.restart.pmmout_rfqdcrd.dtypes))

        pass


    def load_steo_adjustments(self):
        """Load in STEO overwrite values.

        This method is kept for backward compatibility but no longer loads any data.
        STEO overwrites for side cases are now handled by apply_side_case_steo_overwrites()
        in prepare_results.py, which uses files from input/side_case_owrites/ directory.

        Returns
        ------
        None
        """
        pass


    def aggregate_pmm_output_totals(self):
        """Aggregate updates to history and steo in "total" or "national" value rows for PMM output tables.

            * pmmout_rfqtdcrd row 14 is for Alaska production
            * pmmout_rfqtdcrd row 15 is for Onshore/Offshore production
            * pmmout_rfqtdcrd row 16 is for total production
            * pmmout_dcrdwhp row 14 is for national weighted average price


        Returns
        ------
        self.restart.pmmout_rfqtdcrd : df
            DataFrame of STEO overwrite values for crude oil production

        self.restart.pmmout_dcrdwhp : df
            Oh Domestic crude oil wellhead price by region
        """
        ###Domestic Crude Oil Production (MMBBL/D) aggregates
        #Get Alaska production sum
        temp = self.restart.pmmout_rfqtdcrd.copy()
        temp = temp[temp.index.isin([11,12,13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[1] = 14
        temp = temp.set_index(1, append=True)
        temp = temp.reorder_levels([1, 0])
        self.restart.pmmout_rfqtdcrd.update(temp.astype(self.restart.pmmout_rfqtdcrd.dtypes))

        #Get Onshore/Offshore production sum
        temp = self.restart.pmmout_rfqtdcrd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], level=0)]
        temp = temp.groupby(level = 1).sum()
        temp[1] = 15
        temp = temp.set_index(1, append = True)
        temp = temp.reorder_levels([1,0])
        self.restart.pmmout_rfqtdcrd.update(temp.astype(self.restart.pmmout_rfqtdcrd.dtypes))

        #Get total production sum
        temp = self.restart.pmmout_rfqtdcrd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level = 1).sum()
        temp[1] = 16
        temp = temp.set_index(1, append = True)
        temp = temp.reorder_levels([1,0])
        self.restart.pmmout_rfqtdcrd.update(temp.astype(self.restart.pmmout_rfqtdcrd.dtypes))


        ###Average Wellhead Price (nominal $/brl) Weighted Average for national price
        #Get prices
        temp_price = self.restart.pmmout_dcrdwhp.copy()
        temp_price = temp_price[temp_price.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]

        #Get production weights
        temp_prod = self.restart.pmmout_rfqtdcrd.copy()
        temp_total_prod = temp_prod[temp_prod.index.isin([16], level=0)].reset_index()
        temp_total_prod.columns = [nam.region, nam.year, nam.total_prod]
        temp_prod = temp_prod[temp_prod.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)].reset_index()
        temp_prod.columns = [nam.region, nam.year, nam.value]
        temp_prod = temp_prod.merge(temp_total_prod[[nam.year,nam.total_prod]], how = 'left', on = nam.year)
        temp_prod[nam.value] = temp_prod[nam.value].div(temp_prod[nam.total_prod])
        temp_prod = temp_prod[[nam.value, nam.region, nam.year]].set_index([nam.region, nam.year])

        #Produce weighted average national price
        temp_price = temp_prod.mul(temp_price, axis = 1)
        temp_price = temp_price.groupby(level = 1).sum()
        temp_price[1] = 14
        temp_price = temp_price.set_index(1, append=True)
        temp_price = temp_price.reorder_levels([1, 0])
        self.restart.pmmout_dcrdwhp.update(temp_price.astype(self.restart.pmmout_dcrdwhp.dtypes))

        pass

