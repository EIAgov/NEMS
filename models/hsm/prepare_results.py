"""File for preparing results prior to transmission to the restart file.

Summary
_______
This file contains the operations required to prepare results for transmission to the restart file, these methods include:

    1. aggregate_results_across_submodules -combines production volumes across submodules for cumulative variables.
    2. set_expected_production_to_steo - overwrites calculated production volumes with STEO production values.
    3. adjust_production - respond to actual natural gas demand volumes provided by NGMM.


Model Functions and Class Methods
_________________________________
    * aggregate_results_across_submodules -combines production volumes across submodules for cumulative variables.
    * set_expected_production_to_steo - overwrites calculated production volumes with STEO production values.
    * adjust_production - respond to actual natural gas demand volumes provided by NGMM.


Input Files
-----------
None


Output Debug Files
__________________
None


Output Restart Variables
________________________
None


Notes
-----
None


Prepare Results Methods
_______________________
"""
import names as nam
import pandas as pd
import numpy as np
import common as com
import submodule as sub
import os


def _cast_restart_scalar(restart_df, value):
    """Cast a scalar to match a restart DataFrame's value dtype."""
    if nam.value not in restart_df.columns:
        return value
    return np.asarray(value, dtype=restart_df[nam.value].dtype).item()

class Prep_Results(sub.Submodule):
    """Class for preparing results prior to transmission to the restart file."""
    
    # Region grouping constants for filtering operations
    # Note: These use numeric values as required for DataFrame indexing
    ONSHORE_REGIONS = [1, 2, 3, 4, 5, 6, 7]  # Onshore HSM regions
    OFFSHORE_REGIONS = [8, 9, 10]  # Offshore regions
    ONSHORE_LFMM_REGIONS = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]  # All onshore regions for LFMM calculations
    ALASKA_REGION = 14  # Alaska region
    
    # Production category mapping (for documentation)
    PRODUCTION_CATEGORIES = {1: 'EOR', 2: 'L48_Onshore', 3: 'Offshore', 4: 'Alaska'}
    
    # District boundary constants for production filtering
    ONSHORE_MAX_DISTRICT = 66       # Maximum district number for onshore
    OFFSHORE_MIN_DISTRICT = 67      # Minimum district number for offshore
    
    # Alaska district exclusions (excluded from L48 production calculations)
    ALASKA_ONSHORE_DISTRICT = 3             # Alaska onshore district
    ALASKA_STATE_OFFSHORE_DISTRICT = 75     # Alaska state offshore district
    ALASKA_FEDERAL_OFFSHORE_DISTRICT = 84   # Alaska federal offshore district
    
    # Gas types for ogqngrep (natural gas production by category)
    QNGREP_SHALE = 1        # Shale gas + tight oil associated gas
    QNGREP_CBM = 2          # Coalbed methane
    QNGREP_TIGHT = 3        # Tight gas
    QNGREP_CONV = 4         # Conventional gas
    
    # Column name aliases for label-to-index conversion (maps CSV column names to index types)
    COLUMN_ALIASES = {
        'gastypes': 'gastyp',
        'oiltypes': 'oiltyp',
        'region_number': 'mnumor',
    }
    QNGREP_OTHER_AD = 5     # Other associated-dissolved gas
    QNGREP_OFFSHORE_NA = 6  # Offshore non-associated gas
    QNGREP_OFFSHORE_AD = 7  # Offshore associated-dissolved gas
    QNGREP_ALASKA = 8       # Alaska gas
    QNGREP_TOTAL = 9        # Total gas (sum of 1-8)
    
    # Gas types for non-associated (NA) gas production tables:
    #   - ogrnagprd: Realized Non-Associated Gas PRoDuction (from NGMM feedback)
    #   - ogenagprd: Expected Non-Associated Gas PRoDuction (model calculated)
    # Non-associated gas = gas from gas wells (not produced alongside oil)
    NA_GAS_CONV = 1         # Conventional non-associated gas
    NA_GAS_TIGHT = 2        # Tight gas (non-associated)
    NA_GAS_SHALE = 3        # Shale gas (non-associated)
    NA_GAS_CBM = 4          # Coalbed methane
    NA_GAS_TOTAL = 5        # Total non-associated gas
    
    # Gas types for associated-dissolved (AD) gas production table (ogadgprd):
    # Associated-dissolved gas = gas produced alongside crude oil from oil wells
    AD_GAS_CONV = 1         # Conventional associated-dissolved gas
    AD_GAS_TIGHT = 2        # Associated gas from tight oil plays
    AD_GAS_CO2_EOR = 3      # Associated gas from CO2 EOR operations
    AD_GAS_OTHER_EOR = 4    # Associated gas from other EOR operations
    AD_GAS_TOTAL = 5        # Total associated-dissolved gas
    
    # Gas types for ogregprd (regional production by type)
    REGPRD_CONV = 4         # Conventional gas
    REGPRD_TIGHT = 5        # Tight gas (zeroed out, recategorized to shale)
    REGPRD_SHALE = 6        # Shale gas (includes tight gas)
    REGPRD_CBM = 7          # Coalbed methane
    
    # Well type numbers for filtering operations
    SHALE_TIGHT_WELL_TYPES = [4, 5]    # Shale and tight gas well types
    SHALE_AD_WELL_TYPES = [2, 5]       # Shale and associated-dissolved gas well types
    CONV_WELL_TYPES = [1, 3]           # Conventional gas well types
    CBM_WELL_TYPE = 6                  # Coalbed methane well type
    
    # Conversion factors
    BCF_TO_TCF = 1000000000           # Billion cubic feet to trillion cubic feet conversion
    
    # OGQSHLGAS indices
    OGQSHLGAS_OTHER = 15               # "Other" category index for shale gas plays
    
    @staticmethod
    def _identify_year_columns(df):
        """Identify year columns (numeric column names) vs index columns in a DataFrame.
        
        Returns
        -------
        tuple[list, list]
            (year_cols, index_cols) - lists of column names
        """
        year_cols, index_cols = [], []
        for col in df.columns:
            try:
                year_val = int(col)
                # Only treat as year if >= 1900 (reasonable year range)
                # This prevents single-digit or small numbers from being treated as years
                if year_val >= 1900:
                    year_cols.append(col)
                else:
                    index_cols.append(col)
            except ValueError:
                index_cols.append(col)
        return year_cols, index_cols
    
    def __init__(self, parent):
        super().__init__(parent, submodule_name='prep_results')
        

    def aggregate_results_across_submodules(self):
        """Calculate sum values in restart variable tables that are aggregated across submodules.

        Returns
        -------
        self.parent.restart.ogsmout_ogoilprd : df
            Crude oil production by HSM district

        self.parent.restart.ogsmout_ogenagprd : df
            Expected non-associated natural gas production by HSM district and type

        self.parent.restart.ogsmout_ogqngrep : df
            Natural gas production by natural gas type

        self.parent.restart.ogsmout_ogadgprd : df
            Associated-dissolved natural gas production by HSM district and type

        self.parent.restart.pmmout_rfqtdcrd : df
            Total crude production by region and type (including EOR)

        self.parent.restart.pmmout_rfqdcrd : df
            Total crude production by region and type (not including EOR)

        self.parent.restart.ogsmout_ogjobs : df
            Oil and gas industry jobs (based on legacy equation developed by Dana Van Wagener)

        self.parent.restart.qmore_qngpin : df
            Electricity consumed by natural gas processing plants during carbon capture

        self.parent.restart.ogsmout_ogcrdprd : df
            Crude production by LFMM region and LFMM crude type

        self.parent.restart.ogsmout_ngpco2em : df
            Emissions from natural gas processing plants

        self.parent.restart.ogsmout_ogqcrrep : df
            Crude oil production by oil category

        """
        #Crude Oil Production by district and oil type
        temp = self.parent.restart.ogsmout_ogoilprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=2)]
        temp = temp.groupby(level = [0,2]).sum()
        temp[nam.well_type] = 5
        temp = temp.set_index(nam.well_type, append = True)
        temp = temp.reorder_levels([0, 2, 1])
        temp.index.names = self.parent.restart.ogsmout_ogoilprd.index.names
        self.parent.restart.ogsmout_ogoilprd.update(temp.astype(self.parent.restart.ogsmout_ogoilprd.dtypes))

        #Expected NG Production by HSM district
        temp = self.parent.restart.ogsmout_ogenagprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp.groupby(level = [0,2]).sum()
        temp[nam.well_type] = 5
        temp = temp.set_index([nam.well_type], append = True)
        temp = temp.reorder_levels([0,2,1])
        temp.index.names = self.parent.restart.ogsmout_ogenagprd.index.names
        self.parent.restart.ogsmout_ogenagprd.update(temp.astype(self.parent.restart.ogsmout_ogenagprd.dtypes))

        #NG Production by natural gas category
        temp = self.parent.restart.ogsmout_ogqngrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8], level=0)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.ogsmout_ogqngrep.at[(9, int(self.parent.current_year)), 'value'] = temp['value'].sum()

        # Associated-dissolved (AD) gas production by HSM region and natural gas type
        temp = self.parent.restart.ogsmout_ogadgprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=2)]
        temp = temp.groupby(level = 0).sum()
        temp[nam.well_type] = 5
        temp[nam.year] = int(self.parent.current_year)
        temp = temp.set_index([nam.well_type, nam.year], append = True)
        temp.index.names = self.parent.restart.ogsmout_ogadgprd.index.names
        self.parent.restart.ogsmout_ogadgprd.update(temp.astype(self.parent.restart.ogsmout_ogadgprd.dtypes))

        #Domestic Crude Oil Production by region for LFMM (including EOR)
        temp = self.parent.restart.pmmout_rfqtdcrd.copy()

        temp_ak = temp[temp.index.isin([11, 12, 13], level=0)].copy()
        temp_ak = temp_ak[temp_ak.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.pmmout_rfqtdcrd.at[(14, int(self.parent.current_year)), 'value'] = temp_ak['value'].sum()

        temp_on_off = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], level=0)].copy()
        temp_on_off = temp_on_off[temp_on_off.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.pmmout_rfqtdcrd.at[(15, int(self.parent.current_year)), 'value'] = temp_on_off['value'].sum()

        temp_all = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)].copy()
        temp_all = temp_all[temp_all.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.pmmout_rfqtdcrd.at[(16, int(self.parent.current_year)), 'value'] = temp_all['value'].sum()

        #Domestic Crude Oil Production by region for LFMM (not including EOR)
        temp = self.parent.restart.pmmout_rfqdcrd.copy()

        temp_ak = temp[temp.index.isin([11, 12, 13], level=0)].copy()
        temp_ak = temp_ak[temp_ak.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.pmmout_rfqdcrd.at[(14, int(self.parent.current_year)), 'value'] = temp_ak['value'].sum()

        temp_on_off = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], level=0)].copy()
        temp_on_off = temp_on_off[temp_on_off.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.pmmout_rfqdcrd.at[(15,int(self.parent.current_year)), 'value'] = temp_on_off['value'].sum()

        temp_all = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)].copy()
        temp_all = temp_all[temp_all.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.pmmout_rfqdcrd.at[(16, int(self.parent.current_year)), 'value'] = temp_all['value'].sum()

        #Natural Gas Average Wellhead Price
        temp = self.parent.restart.ogsmout_ogngwhp.copy()
        temp = temp.groupby(level=1).mean()
        temp.index = self.parent.restart.ogsmout_ogpngwhp.index
        self.parent.restart.ogsmout_ogpngwhp.update(temp.astype(self.parent.restart.ogsmout_ogpngwhp.dtypes))

        #Jobs - legacy equation to get jobs number from Macro
        well_value_1 = self.parent.restart.ogsmout_ognowell.at[(int(self.parent.current_year),), nam.value]
        well_value_2 = self.parent.restart.ogsmout_ognowell.at[((int(self.parent.current_year) - 1),), nam.value]
        ogjobs = 152.2320 + (0.006355 * well_value_1) + (0.0086729 * well_value_2)
        self.parent.restart.ogsmout_ogjobs.at[int(self.parent.current_year), 'value'] = _cast_restart_scalar(
            self.parent.restart.ogsmout_ogjobs,
            ogjobs
        )

        #Electricity consumed by natural gas processing plants during carbon capture
        temp = self.restart.qmore_qngpin.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9,10], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.census_divisions] = 11
        temp = temp.set_index([nam.census_divisions], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.qmore_qngpin.index.names
        self.parent.restart.qmore_qngpin.update(temp.astype(self.parent.restart.qmore_qngpin.dtypes))

        #Crude production by LFMM region and LFMM crude type
        temp = self.restart.ogsmout_ogcrdprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=[1,2]).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([2, 0, 1])
        temp.index.names = self.restart.ogsmout_ogcrdprd.index.names
        self.restart.ogsmout_ogcrdprd.update(temp.astype(self.restart.ogsmout_ogcrdprd.dtypes))

        #Electricity consumed by natural gas processing plants during carbon capture
        temp = self.restart.ogsmout_ngpco2em.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9,10], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.census_divisions] = 11
        temp = temp.set_index([nam.census_divisions], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ngpco2em.index.names
        self.parent.restart.ogsmout_ngpco2em.update(temp.astype(self.parent.restart.ogsmout_ngpco2em.dtypes))

        #ogsmout_ogcoprd_nonfed
        temp = self.restart.ogsmout_ogcoprd_nonfed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogcoprd_nonfed.index.names
        self.parent.restart.ogsmout_ogcoprd_nonfed.update(temp.astype(self.parent.restart.ogsmout_ogcoprd_nonfed.dtypes))

        #ogsmout_ogcoprd_fed
        temp = self.restart.ogsmout_ogcoprd_fed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogcoprd_fed.index.names
        self.parent.restart.ogsmout_ogcoprd_fed.update(temp.astype(self.parent.restart.ogsmout_ogcoprd_fed.dtypes))

        # ogsmout_ogngprd_nonfed
        temp = self.restart.ogsmout_ogngprd_nonfed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogngprd_nonfed.index.names
        self.parent.restart.ogsmout_ogngprd_nonfed.update(temp.astype(self.parent.restart.ogsmout_ogngprd_nonfed.dtypes))

        # ogsmout_ogngprd_fed
        temp = self.restart.ogsmout_ogngprd_fed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogngprd_fed.index.names
        self.parent.restart.ogsmout_ogngprd_fed.update(temp.astype(self.parent.restart.ogsmout_ogngprd_fed.dtypes))

        # ogsmout_ogqcrrep
        temp = self.restart.ogsmout_ogqcrrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.oil_category] = 6
        temp = temp.set_index([nam.oil_category], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogqcrrep.index.names
        self.parent.restart.ogsmout_ogqcrrep.update(temp.astype(self.parent.restart.ogsmout_ogqcrrep.dtypes))

        # Explicitly zero out deprecated ANWR index (5) to prevent residual production
        # ANWR production is included in Alaska total (index 4), not separate
        dtype_ogqcrrep = self.parent.restart.ogsmout_ogqcrrep['value'].dtype
        for year in range(self.parent.param_baseyr, self.parent.final_aeo_year + 1):
            self.parent.restart.ogsmout_ogqcrrep.at[(5, year), 'value'] = dtype_ogqcrrep.type(0.0)

        #ogsmout_ogadgprd
        temp_sum_df = self.parent.restart.ogsmout_ogadgprd.copy()
        temp_sum_df = temp_sum_df[temp_sum_df.index.get_level_values('2').isin([1,2,3,4])]
        temp_sum_df = temp_sum_df[temp_sum_df.index.get_level_values('3') == int(self.parent.current_year)]
        temp_sum_df = temp_sum_df.groupby(['1','3']).sum()
        temp_sum_df[nam.gas_type] = 5
        temp_sum_df = temp_sum_df.set_index([nam.gas_type], append = True)
        temp_sum_df = temp_sum_df.reorder_levels([0, 2, 1])
        temp_sum_df.index.names = self.parent.restart.ogsmout_ogadgprd.index.names
        self.parent.restart.ogsmout_ogadgprd.update(temp_sum_df.astype(self.parent.restart.ogsmout_ogadgprd.dtypes))

        pass

    def adjust_fed_nonfed_gas(self):
        """
        Calculates and updates federal and non-federal split adjusted for realized natural gas.

                    * Adjusts total natural gas production for realized natural gas feedback
                    * Maps production to HSM regions
                    * Calculates fed/nonfed gas ratio
                    * Applies fed/nonfed ratio to regional production

        Returns
        -------
        self.restart.ogsmout_ogngprd_nonfed : df
            Natural Gas production on non-federal lands

        self.restart.ogsmout_ogngprd_fed : df
            Natural Gas production on federal lands


        """

        current_year = int(self.parent.current_year)

        # 1. Extract Data for the Current Year
        ogngprd_fed = self.parent.restart.ogsmout_ogngprd_fed.xs(current_year, level=1, drop_level=False).copy()
        ogngprd_nonfed = self.parent.restart.ogsmout_ogngprd_nonfed.xs(current_year, level=1, drop_level=False).copy()

        # calculate natural gas production
        total_ngprd = self.parent.restart.ogsmout_ogrnagprd + self.parent.restart.ogsmout_ogadgprd  # add non-associated and associated-dissolved production
        total_ngprd = total_ngprd.loc[total_ngprd.index.get_level_values(2) == self.parent.current_year]
        total_ngprd = total_ngprd.loc[total_ngprd.index.get_level_values(1) == 5]
        total_ngprd = total_ngprd.droplevel(level=1).reset_index(names=['district_number', 'year'])  # prep for merge

        total_ngprd_hsmreg = total_ngprd.copy().merge(self.parent.mapping, on='district_number')
        total_ngprd_hsmreg = total_ngprd_hsmreg.groupby(['region_number', 'year'])['value'].sum().div(1000).to_frame()
        total_ngprd_hsmreg = total_ngprd_hsmreg[
            ~total_ngprd_hsmreg.index.get_level_values(0).isin([11, 12, 13, 14])].copy() # Drop indices 11-14, Alaska and Total

        # 2. Calculate Federal Ratio
        fed_nonfed_ratio = ogngprd_fed.merge(ogngprd_nonfed, left_index=True, right_index=True,
                                             suffixes=('_fed', '_nonfed'))
        
        # Convert to numeric types to avoid dtype issues
        fed_nonfed_ratio['value_fed'] = pd.to_numeric(fed_nonfed_ratio['value_fed'], errors='coerce')
        fed_nonfed_ratio['value_nonfed'] = pd.to_numeric(fed_nonfed_ratio['value_nonfed'], errors='coerce')
        
        # Calculate fed_ratio with protection against divide-by-zero
        denominator = fed_nonfed_ratio['value_fed'] + fed_nonfed_ratio['value_nonfed']
        fed_nonfed_ratio['fed_ratio'] = np.divide(
            fed_nonfed_ratio['value_fed'].values,
            denominator.values,
            out=np.zeros(len(fed_nonfed_ratio), dtype=float),
            where=denominator.values != 0
        )
        fed_nonfed_ratio = fed_nonfed_ratio[~fed_nonfed_ratio.index.get_level_values(0).isin([11, 12, 13, 14])].copy()  # Drop indices 11-14, Alaska and Total
        # 3. Merge and Filter Data
        total_ngprd_hsmreg.index.names = fed_nonfed_ratio.index.names  # ensure both dataframes have the same index names for merging
        calculate_fed_nonfed = total_ngprd_hsmreg.merge(fed_nonfed_ratio, left_index=True, right_index=True)
        calculate_fed_nonfed.drop(columns=['value_fed', 'value_nonfed'], inplace=True)

        # 4. Recalculate Fed and NonFed production using ratio and realized production
        fed_recalculated = pd.DataFrame()
        nonfed_recalculated = pd.DataFrame()
        fed_recalculated['value'] = calculate_fed_nonfed['value'] * calculate_fed_nonfed['fed_ratio']
        nonfed_recalculated['value'] = calculate_fed_nonfed['value'] * (1 - calculate_fed_nonfed['fed_ratio'])

        # 5. Update Parent's Restart Data
        self.parent.restart.ogsmout_ogngprd_fed.update(fed_recalculated.astype(self.parent.restart.ogsmout_ogngprd_fed.dtypes))
        self.parent.restart.ogsmout_ogngprd_nonfed.update(nonfed_recalculated.astype(self.parent.restart.ogsmout_ogngprd_nonfed.dtypes))

        ###Aggregate NG production by Fed/NonFed
        temp = self.parent.restart.ogsmout_ogngprd_fed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.ogsmout_ogngprd_fed.at[(14, int(self.parent.current_year)), 'value'] = temp['value'].sum()

        ###Aggregate NG production by Fed/NonFed
        temp = self.parent.restart.ogsmout_ogngprd_nonfed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.ogsmout_ogngprd_nonfed.at[(14, int(self.parent.current_year)), 'value'] = temp['value'].sum()


        return

    # =========================================================================
    # Production Adjustment Helper Methods
    # =========================================================================
    
    def _filter_production_by_location(self, source_df, location='onshore', exclude_alaska=True):
        """Filter production DataFrame by geographic location.
        
        Parameters
        ----------
        source_df : pd.DataFrame
            Source production DataFrame with district in index level 0
        location : str
            'onshore' - districts <= ONSHORE_MAX_DISTRICT (66)
            'offshore' - districts >= OFFSHORE_MIN_DISTRICT (67)
        exclude_alaska : bool
            If True, excludes Alaska districts (3, 75, 84)
            
        Returns
        -------
        pd.DataFrame
            Filtered production data for years > history_year
        """
        df = source_df.copy()
        
        # Filter for years after history year
        df = df[df.index.get_level_values(2) > self.parent.history_year]
        
        # Filter by location
        if location == 'onshore':
            df = df[df.index.get_level_values(0) <= self.ONSHORE_MAX_DISTRICT]
        elif location == 'offshore':
            df = df[df.index.get_level_values(0) >= self.OFFSHORE_MIN_DISTRICT]
        
        # Exclude Alaska districts if requested
        if exclude_alaska:
            df = df[df.index.get_level_values(0) != self.ALASKA_ONSHORE_DISTRICT]
            if location == 'offshore':
                df = df[df.index.get_level_values(0) != self.ALASKA_STATE_OFFSHORE_DISTRICT]
                df = df[df.index.get_level_values(0) != self.ALASKA_FEDERAL_OFFSHORE_DISTRICT]
        
        return df

    def _update_ogqngrep_by_category(self, nonassoc_prod_df, assoc_prod_df):
        """Update ogqngrep with natural gas production by category.
        
        Updates categories: shale (1), CBM (2), tight (3), conventional (4), other AD (5)
        
        Parameters
        ----------
        nonassoc_prod_df : pd.DataFrame
            Filtered non-associated gas production (from gas wells, onshore, excluding Alaska)
        assoc_prod_df : pd.DataFrame
            Filtered associated-dissolved gas production (from oil wells, onshore, excluding Alaska)
        """
        current_year = self.parent.current_year
        
        # Helper function to prepare and update a gas category
        def update_category(prod_df, source_gas_type, qngrep_type, assoc_addition=None):
            """Process a single gas category and update ogqngrep."""
            temp = prod_df.loc[prod_df.index.get_level_values(1) == source_gas_type].copy()
            temp = temp.groupby(level=[2]).sum()
            
            # Add associated-dissolved production if specified
            if assoc_addition is not None:
                temp['value'] = temp['value'].values + assoc_addition['value'].values
            
            temp[nam.gas_type] = qngrep_type
            temp = temp.loc[temp.index == current_year]
            temp = (temp.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
            temp.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
            self.parent.restart.ogsmout_ogqngrep.update(
                temp.astype(self.parent.restart.ogsmout_ogqngrep.dtypes))
        
        # Prepare associated gas from tight oil plays for shale category
        temp_assoc_for_shale = assoc_prod_df.loc[assoc_prod_df.index.get_level_values(1) == self.AD_GAS_TIGHT].copy()
        temp_assoc_for_shale = temp_assoc_for_shale.groupby(level=[2]).sum()
        temp_assoc_for_shale = temp_assoc_for_shale.loc[temp_assoc_for_shale.index > self.parent.history_year]
        
        # Update each category
        # Shale (non-assoc type 3 + assoc type 2) -> QNGREP type 1
        update_category(nonassoc_prod_df, self.NA_GAS_SHALE, self.QNGREP_SHALE, temp_assoc_for_shale)
        
        # CBM (non-assoc type 4) -> QNGREP type 2
        update_category(nonassoc_prod_df, self.NA_GAS_CBM, self.QNGREP_CBM)
        
        # Tight (non-assoc type 2) -> QNGREP type 3
        update_category(nonassoc_prod_df, self.NA_GAS_TIGHT, self.QNGREP_TIGHT)
        
        # Conventional (non-assoc type 1) -> QNGREP type 4
        update_category(nonassoc_prod_df, self.NA_GAS_CONV, self.QNGREP_CONV)
        
        # Other associated-dissolved (assoc types 1, 3, 4) -> QNGREP type 5
        temp_other_assoc = assoc_prod_df.loc[assoc_prod_df.index.get_level_values(1).isin(
            [self.AD_GAS_CONV, self.AD_GAS_CO2_EOR, self.AD_GAS_OTHER_EOR])].copy()
        temp_other_assoc = temp_other_assoc.groupby(level=[2]).sum()
        temp_other_assoc = temp_other_assoc.loc[temp_other_assoc.index > self.parent.history_year]
        temp_other_assoc[nam.gas_type] = self.QNGREP_OTHER_AD
        temp_other_assoc = temp_other_assoc.loc[temp_other_assoc.index == current_year]
        temp_other_assoc = (temp_other_assoc.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_other_assoc.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(
            temp_other_assoc.astype(self.parent.restart.ogsmout_ogqngrep.dtypes))

    def _update_offshore_production(self, nonassoc_prod_df, assoc_prod_df):
        """Update ogqngrep with offshore natural gas production.
        
        Updates offshore categories: non-associated offshore (6), associated-dissolved offshore (7)
        
        Parameters
        ----------
        nonassoc_prod_df : pd.DataFrame
            Source non-associated gas production DataFrame (will be filtered for offshore)
        assoc_prod_df : pd.DataFrame
            Source associated-dissolved gas production DataFrame (will be filtered for offshore)
        """
        current_year = self.parent.current_year
        
        # Filter for offshore, excluding Alaska
        off_nonassoc_prod = self._filter_production_by_location(nonassoc_prod_df, location='offshore')
        off_assoc_prod = self._filter_production_by_location(assoc_prod_df, location='offshore')
        
        # Adjust non-associated offshore production (total type 5) -> QNGREP type 6
        temp_nonassoc_off = off_nonassoc_prod.loc[off_nonassoc_prod.index.get_level_values(1) == self.NA_GAS_TOTAL].copy()
        temp_nonassoc_off = temp_nonassoc_off.groupby(level=[2]).sum()
        temp_nonassoc_off[nam.gas_type] = self.QNGREP_OFFSHORE_NA
        temp_nonassoc_off = temp_nonassoc_off.loc[temp_nonassoc_off.index == current_year]
        temp_nonassoc_off = (temp_nonassoc_off.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_nonassoc_off.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(
            temp_nonassoc_off[[nam.value]].astype(self.parent.restart.ogsmout_ogqngrep.dtypes))
        
        # Adjust associated-dissolved offshore production (type 1) -> QNGREP type 7
        temp_assoc_off = off_assoc_prod.loc[off_assoc_prod.index.get_level_values(1) == self.AD_GAS_CONV].copy()
        temp_assoc_off = temp_assoc_off.groupby(level=[2]).sum()
        temp_assoc_off = temp_assoc_off.loc[temp_assoc_off.index > self.parent.history_year]
        temp_assoc_off[nam.gas_type] = self.QNGREP_OFFSHORE_AD
        temp_assoc_off = temp_assoc_off.loc[temp_assoc_off.index == current_year]
        temp_assoc_off = (temp_assoc_off.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_assoc_off.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(
            temp_assoc_off.astype(self.parent.restart.ogsmout_ogqngrep.dtypes))

    def _aggregate_ogqngrep_total(self):
        """Aggregate total NG production by category (sum of types 1-8) -> type 9."""
        current_year = int(self.parent.current_year)
        temp = self.parent.restart.ogsmout_ogqngrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8], level=0)]
        temp = temp[temp.index.isin([current_year], level=1)]
        self.parent.restart.ogsmout_ogqngrep.at[
            (self.QNGREP_TOTAL, current_year), 'value'] = temp['value'].sum()

    def _adjust_ogqshlgas_for_first_steo_year(self):
        """Adjust OGQSHLGAS index 15 to match OGQNGREP index 1 for first STEO year only.
        
        Ensures total play-level shale gas production (OGQSHLGAS) matches 
        aggregate shale gas production (OGQNGREP index 1) by adjusting the 
        'other' category (index 15).
        
        Units: OGQNGREP is in BCF/year, OGQSHLGAS is in TCF/year
        """
        current_year = int(self.parent.current_year)
        
        # Only apply to first STEO year
        if current_year != self.parent.steo_years[0]:
            return
        
        # Calculate sum of all OGQSHLGAS plays for this year
        shale_sum = self.parent.restart.ogsmout_ogqshlgas.xs(
            current_year, level=1, drop_level=False
        )['value'].sum()
        
        # Get OGQNGREP index 1 (shale gas total) for this year
        ogqngrep_shale = self.parent.restart.ogsmout_ogqngrep.at[
            (self.QNGREP_SHALE, current_year), 'value'
        ]
        
        # Calculate difference: convert OGQSHLGAS sum from TCF to BCF for comparison
        # Then convert difference back to TCF for adjustment
        tempscale = ogqngrep_shale - shale_sum * 1000
        
        # Apply adjustment to index 15 (other category)
        self.parent.restart.ogsmout_ogqshlgas.loc[(15, current_year), 'value'] = (
            self.parent.restart.ogsmout_ogqshlgas.loc[(15, current_year), 'value'] 
            + tempscale / 1000
        )

    def _update_ogngprd_by_region(self, nonassoc_source_df, assoc_source_df, mode='integrated'):
        """Update ogngprd with total natural gas production by region.
        
        Combines non-associated gas (from gas wells) and associated-dissolved gas (from oil wells)
        to compute total natural gas production by region. Used by both standalone and
        integrated modes (source is ogenagprd or ogrnagprd respectively).
        
        Parameters
        ----------
        nonassoc_source_df : pd.DataFrame
            Non-associated gas production source (ogrnagprd for realized, ogenagprd for expected)
        assoc_source_df : pd.DataFrame
            Associated-dissolved gas production source (ogadgprd)
        mode : str
            'integrated' - Standard logic with offshore index shift (used for all modes).
        """
        current_year = int(self.parent.current_year)
        
        # Non-associated gas (from gas wells)
        temp_nonassoc = nonassoc_source_df.copy()
        temp_nonassoc = temp_nonassoc.xs(self.NA_GAS_TOTAL, level=1, axis=0)
        temp_nonassoc = temp_nonassoc.xs(current_year, level=1, axis=0)
        temp_nonassoc = pd.merge(temp_nonassoc,
                          self.parent.mapping[[nam.district_number, nam.region_number]],
                          how='left', left_index=True, right_on=nam.district_number)
        temp_nonassoc = temp_nonassoc[[nam.region_number, nam.value]].groupby(nam.region_number).sum()
        
        # Associated-dissolved gas (from oil wells)
        temp_assoc = assoc_source_df.copy()
        temp_assoc = temp_assoc.xs(self.AD_GAS_TOTAL, level=1, axis=0)
        temp_assoc = temp_assoc.xs(current_year, level=1, axis=0)
        temp_assoc = pd.merge(temp_assoc,
                          self.parent.mapping[[nam.district_number, nam.region_number]],
                          how='left', left_index=True, right_on=nam.district_number)
        temp_assoc = temp_assoc[[nam.region_number, nam.value]].groupby(nam.region_number).sum()
        
        # Combine non-associated and associated-dissolved gas for total natural gas
        temp_ng = temp_nonassoc + temp_assoc
        
        # Get total
        ng_total = temp_ng[nam.value].sum()
        temp_ng = temp_ng.loc[list(range(1, 11))]
        temp_ng.loc[11] = ng_total
        
        # Manual shift for offshore regions (8<-9, 9<-10, 10<-0)
        temp_ng.at[(8, nam.value)] = temp_ng.at[(9, nam.value)]
        temp_ng.at[(9, nam.value)] = temp_ng.at[(10, nam.value)]
        temp_ng.at[(10, nam.value)] = 0
        
        # Format for Restart File
        temp_ng[nam.year] = current_year
        temp_ng = temp_ng.set_index([nam.year], append=True)
        temp_ng.index.names = self.parent.restart.ogsmout_ogngprd.index.names
        
        # Assign to Restart Variable
        self.parent.restart.ogsmout_ogngprd.update(
            temp_ng.astype(self.parent.restart.ogsmout_ogngprd.dtypes))

    def _update_ogdngprd_by_district(self, nonassoc_source_df, assoc_source_df):
        """Update ogdngprd with dry natural gas production by district and type.
        
        Combines non-associated gas (from gas wells) and associated-dissolved gas (from oil wells)
        to compute dry natural gas production by HSM district.
        
        Parameters
        ----------
        nonassoc_source_df : pd.DataFrame
            Non-associated gas production source (ogrnagprd for realized, ogenagprd for expected)
        assoc_source_df : pd.DataFrame
            Associated-dissolved gas production source (ogadgprd)
        """
        current_year = int(self.parent.current_year)
        
        # Non-associated gas (from gas wells)
        temp_nonassoc = nonassoc_source_df.copy()
        temp_nonassoc = temp_nonassoc.xs(current_year, level=2, axis=0)
        
        # Get type tables for non-associated gas
        temp_nonassoc_conv = temp_nonassoc.xs(self.NA_GAS_CONV, level=1, drop_level=False).copy()
        temp_nonassoc_tight = temp_nonassoc.xs(self.NA_GAS_TIGHT, level=1, drop_level=False).copy()
        temp_nonassoc_shale = temp_nonassoc.xs(self.NA_GAS_SHALE, level=1, drop_level=False).copy()
        temp_nonassoc_cbm = temp_nonassoc.xs(self.NA_GAS_CBM, level=1, drop_level=False).copy()
        temp_nonassoc_total = temp_nonassoc.xs(self.NA_GAS_TOTAL, level=1, drop_level=False).copy()
        
        # Associated-dissolved gas (from oil wells)
        temp_assoc = assoc_source_df.copy()
        temp_assoc = temp_assoc.xs(current_year, level=2, axis=0)
        
        # Get type tables for associated-dissolved gas
        temp_assoc_conv = temp_assoc.xs(self.AD_GAS_CONV, level=1, drop_level=False).copy()
        # Tight oil associated gas is recategorized to shale for reporting
        temp_assoc_tight = temp_assoc.xs(self.AD_GAS_TIGHT, level=1, drop_level=True).copy()
        temp_assoc_tight[1] = self.NA_GAS_SHALE  # Remap to match non-associated shale type
        temp_assoc_tight = temp_assoc_tight.set_index([1], append=True)
        temp_assoc_total = temp_assoc.xs(self.AD_GAS_TOTAL, level=1, drop_level=False).copy()
        
        # Combine associated and non-associated gas by type
        temp_conv = temp_nonassoc_conv + temp_assoc_conv
        temp_tight = temp_nonassoc_tight + temp_assoc_tight
        temp_total = temp_nonassoc_total + temp_assoc_total
        
        # Recreate table with all gas types
        temp_ng = pd.concat([temp_conv, temp_nonassoc_shale, temp_tight, temp_nonassoc_cbm, temp_total])
        temp_ng[nam.year] = current_year
        temp_ng = temp_ng.set_index([nam.year], append=True)
        temp_ng.index.names = self.parent.restart.ogsmout_ogdngprd.index.names
        
        # Exclude Alaska (NGMM does not use realized non-associated data for Alaska)
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != self.ALASKA_ONSHORE_DISTRICT]
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != self.ALASKA_STATE_OFFSHORE_DISTRICT]
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != self.ALASKA_FEDERAL_OFFSHORE_DISTRICT]
        
        # Write to Restart Variable
        self.parent.restart.ogsmout_ogdngprd.update(
            temp_ng.astype(self.parent.restart.ogsmout_ogdngprd.dtypes))

    def _update_ogregprd(self, nonassoc_source_df, assoc_source_df, zero_tight_all_years=False):
        """Update ogregprd with regional production by gas type.
        
        Combines non-associated gas (from gas wells) and associated-dissolved gas (from oil wells)
        to compute regional production. Recategorizes tight gas to shale for reporting.
        
        Updates gas types: Conventional (4), Shale+Tight (6), CBM (7)
        Zeroes out Tight (5) since it's recategorized to Shale.
        
        Parameters
        ----------
        nonassoc_source_df : pd.DataFrame
            Non-associated gas production source (ogrnagprd for realized, ogenagprd for expected)
        assoc_source_df : pd.DataFrame
            Associated-dissolved gas production source (ogadgprd)
        zero_tight_all_years : bool
            If True, zeros tight gas for all projection years (integrated mode)
            If False, zeros tight gas for current year only (standalone mode)
        """
        current_year = int(self.parent.current_year)
        
        # Prepare non-associated gas production data with region mapping
        nonassoc_prod = nonassoc_source_df.copy()
        nonassoc_prod = nonassoc_prod[nonassoc_prod.index.get_level_values(2) == current_year]
        nonassoc_prod = nonassoc_prod.reset_index()
        nonassoc_prod = nonassoc_prod.merge(
            self.parent.mapping[[nam.district_number, nam.region_number]],
            how='left', left_on='1', right_on=nam.district_number)
        nonassoc_prod = nonassoc_prod.loc[nonassoc_prod[nam.region_number] <= 7]
        nonassoc_prod = nonassoc_prod.drop([nam.district_number], axis=1)
        
        # Prepare associated-dissolved gas production data with region mapping
        assoc_prod = assoc_source_df.copy()
        assoc_prod = assoc_prod[assoc_prod.index.get_level_values(2) == current_year]
        assoc_prod = assoc_prod.reset_index()
        assoc_prod = assoc_prod.merge(
            self.parent.mapping[[nam.district_number, nam.region_number]],
            how='left', left_on='1', right_on=nam.district_number)
        assoc_prod = assoc_prod.loc[assoc_prod[nam.region_number] <= 7]
        assoc_prod = assoc_prod.drop([nam.district_number], axis=1)
        
        # Conventional Production (ogregprd type 4)
        # Non-associated: type 1 (conventional from gas wells)
        # Associated-dissolved: types 1, 3, 4 (conventional AD, CO2 EOR, Other EOR from oil wells)
        temp_conv = nonassoc_prod.loc[nonassoc_prod['2'] == self.NA_GAS_CONV].copy()
        temp_conv = temp_conv.groupby([nam.region_number, '3']).sum()
        
        temp_assoc_conv = assoc_prod.loc[assoc_prod['2'].isin(
            [self.AD_GAS_CONV, self.AD_GAS_CO2_EOR, self.AD_GAS_OTHER_EOR])].copy()
        temp_assoc_conv = temp_assoc_conv.groupby([nam.region_number, '3']).sum()
        
        temp_conv['value'] = temp_conv['value'].values + temp_assoc_conv['value'].values
        temp_conv[nam.gas_type] = self.REGPRD_CONV
        temp_conv = (temp_conv.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_conv.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(
            (temp_conv / 1000).astype(self.parent.restart.ogsmout_ogregprd.dtypes))
        
        # Shale Production (ogregprd type 6) - includes tight gas recategorized to shale
        # Non-associated: type 2 (tight from gas wells) + type 3 (shale from gas wells)
        # Associated-dissolved: type 2 (gas from tight oil plays)
        temp_tight = nonassoc_prod.loc[nonassoc_prod['2'] == self.NA_GAS_TIGHT].copy()
        temp_tight = temp_tight.groupby([nam.region_number, '3']).sum()
        
        temp_shale = nonassoc_prod.loc[nonassoc_prod['2'] == self.NA_GAS_SHALE].copy()
        temp_shale = temp_shale.groupby([nam.region_number, '3']).sum()
        
        temp_assoc_tight = assoc_prod.loc[assoc_prod['2'] == self.AD_GAS_TIGHT].copy()
        temp_assoc_tight = temp_assoc_tight.groupby([nam.region_number, '3']).sum()
        
        # Add tight gas volumes to shale gas (index 6)
        temp_shale['value'] = temp_shale['value'].add(
            temp_assoc_tight['value'], fill_value=0).add(
            temp_tight['value'], fill_value=0)
        temp_shale[nam.gas_type] = self.REGPRD_SHALE
        temp_shale = (temp_shale.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_shale.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(
            (temp_shale / 1000).astype(self.parent.restart.ogsmout_ogregprd.dtypes))
        
        # CBM Production (ogregprd type 7) - non-associated only (no AD CBM)
        temp_cbm = nonassoc_prod.loc[nonassoc_prod['2'] == self.NA_GAS_CBM].copy()
        temp_cbm = temp_cbm.groupby([nam.region_number, '3']).sum()
        temp_cbm[nam.gas_type] = self.REGPRD_CBM
        temp_cbm = (temp_cbm.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_cbm.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(
            (temp_cbm / 1000).astype(self.parent.restart.ogsmout_ogregprd.dtypes))
        
        # Zero out tight gas (index 5) after recategorizing to shale gas
        if zero_tight_all_years:
            # Zero for all projection years
            projection_years_mask = self.parent.restart.ogsmout_ogregprd.index.get_level_values(2) > self.parent.history_year
            tight_gas_mask = self.parent.restart.ogsmout_ogregprd.index.get_level_values(1) == self.REGPRD_TIGHT
            tight_gas_projection_mask = projection_years_mask & tight_gas_mask
            self.parent.restart.ogsmout_ogregprd.loc[tight_gas_projection_mask, 'value'] = 0
        else:
            # Zero for current year only
            current_year_mask = self.parent.restart.ogsmout_ogregprd.index.get_level_values(2) == current_year
            tight_gas_mask = self.parent.restart.ogsmout_ogregprd.index.get_level_values(1) == self.REGPRD_TIGHT
            tight_gas_current_year_mask = current_year_mask & tight_gas_mask
            self.parent.restart.ogsmout_ogregprd.loc[tight_gas_current_year_mask, 'value'] = 0

    def _apply_ratio_adjustment(self):
        """Apply ratio adjustment to realized non-associated gas for all integrated years.
        
        This method calculates a ratio between realized and expected non-associated gas 
        production, then applies that ratio to scale the expected production by type. 
        This is applied to all integrated years (>= steo_years[0]) to ensure HSM production
        matches NGMM feedback while preserving the expected production distribution by gas type.
        
        Source tables:
        - ogrnagprd: Realized Non-Associated Gas PRoDuction (from NGMM feedback)
        - ogenagprd: Expected Non-Associated Gas PRoDuction (HSM calculated)
        """
        current_year = int(self.parent.current_year)
        
        # Get realized non-associated gas production (total, type 5) for current year
        temp_realized = self.restart.ogsmout_ogrnagprd.copy()
        temp_realized = temp_realized.xs(current_year, level=2)
        temp_realized = temp_realized.xs(self.NA_GAS_TOTAL, level=1)
        
        # Get expected non-associated gas production (total, type 5) for current year
        temp_expected = self.restart.ogsmout_ogenagprd.copy()
        temp_expected = temp_expected.xs(current_year, level=2)
        temp_expected = temp_expected.xs(self.NA_GAS_TOTAL, level=1)
        
        # Convert to numeric to avoid dtype issues
        temp_realized['value'] = pd.to_numeric(temp_realized['value'], errors='coerce')
        temp_expected['value'] = pd.to_numeric(temp_expected['value'], errors='coerce')
        
        # Calculate realized/expected production ratio (protect against divide-by-zero)
        temp_prod_ratio = pd.DataFrame(
            np.divide(
                temp_realized['value'].values,
                temp_expected['value'].values,
                out=np.ones(len(temp_realized), dtype=float),
                where=temp_expected['value'].values != 0
            ),
            index=temp_realized.index
        )
        temp_prod_ratio.columns = [nam.realized_prod_ratio]
        
        # Apply ratio to expected non-associated production by type (conv, tight, shale, CBM)
        temp_re = self.restart.ogsmout_ogenagprd.loc[
            self.restart.ogsmout_ogenagprd.index.get_level_values(1).isin(
                [self.NA_GAS_CONV, self.NA_GAS_TIGHT, self.NA_GAS_SHALE, self.NA_GAS_CBM])].copy()
        temp_re = temp_re.loc[temp_re.index.get_level_values(2) == current_year].copy()
        temp_re = temp_re.reset_index()
        temp_re = temp_re.merge(temp_prod_ratio, how='left', left_on='1', right_index=True)
        temp_re['value'] = temp_re['value'] * temp_re['realized_prod_ratio']
        temp_re = temp_re.drop(['realized_prod_ratio'], axis=1)
        temp_re = temp_re.set_index(['1', '2', '3'])
        
        # Update realized non-associated gas with ratio-adjusted values. 
        # Redistributes realized production within each district across gas types to match expected production gas type distributions, while preserving the district totals set by NGMM
        self.restart.ogsmout_ogrnagprd.update(
            temp_re.astype(self.restart.ogsmout_ogrnagprd.dtypes))

    def adjust_production(self, mode: str):
        """Adjust restart variables for natural gas production.
        
        This unified method combines non-associated gas (from gas wells) and 
        associated-dissolved gas (from oil wells) to update various production tables.
        
        Replaces the three separate methods:
        - adjust_vars_for_year_one_prod() -> mode='year_one'
        - adjust_vars_for_realized_prod() -> mode='realized'
        - adjust_regional_natgas_for_standalone() -> mode='standalone'
        
        **Ratio Adjustment Behavior:**
        For all integrated years (mode='year_one' or 'realized'), applies ratio adjustment
        to scale expected production by type to match NGMM realized totals. All years 
        >= steo_years[0] are treated the same with this ratio adjustment.
        
        **Shale Gas Plays Adjustment:**
        For integrated years when current_year > steo_years[0], also calls 
        adjust_onshore_shale_gas_plays_for_realized_prod() to update play-level production,
        well counts, and project drilling parameters.
        
        Parameters
        ----------
        mode : str
            'year_one' - Integrated mode for all years >= steo_years[0]. Applies ratio 
                         adjustment to realized non-associated gas, then updates all 
                         production variables. All integrated years use the same ratio 
                         adjustment approach.
            'realized' - Same as 'year_one' (kept for backward compatibility). Both modes
                         now apply ratio adjustment to all integrated years.
            'standalone' - Standalone mode. Runs the same updates as integrated (ogqngrep,
                           ogdngprd, fed/nonfed, ogngprd, ogregprd, and first-STEO-year
                           OGQSHLGAS "other" adjustment) using expected non-associated gas
                           (ogenagprd). No ratio adjustment applied.
        
        Returns
        -------
        Updates the following restart variables depending on mode:
        - ogsmout_ogqngrep : NG production by category (all modes)
        - ogsmout_ogngprd : NG production by region (all modes)
        - ogsmout_ogdngprd : Dry NG by district/type (all modes)
        - ogsmout_ogregprd : Regional production by type (all modes)
        - ogsmout_ogrnagprd : Realized non-associated gas (integrated modes, via ratio adjustment)
        - ogsmout_ogngprd_fed/nonfed : Fed/nonfed split (all modes)
        - ogsmout_ogqshlgas : First STEO year "other" adjustment (all modes); play-level
          scaling (integrated only, years > steo_years[0])
        - ogsmout_ogogwells : Total wells (integrated only, years > steo_years[0])
        - ogsmout_ognowell : Completed wells (integrated only, years > steo_years[0])
        - ogsmout_ogwellsl48 : Lower 48 wells (integrated only, years > steo_years[0])
        - ogsmout_ogsrl48 : Drilling success rates (integrated only, years > steo_years[0])
        """
        # ========================================================================
        # STEP 1: Determine source data based on mode
        # ========================================================================
        # Non-associated gas: from gas wells (realized = NGMM feedback, expected = HSM calculated)
        # Associated-dissolved gas: from oil wells (always from ogadgprd)
        if mode == 'standalone':
            nonassoc_source = self.parent.restart.ogsmout_ogenagprd  # Expected non-associated gas
        else:
            nonassoc_source = self.parent.restart.ogsmout_ogrnagprd  # Realized non-associated gas
        assoc_source = self.parent.restart.ogsmout_ogadgprd  # Associated-dissolved gas
        
        # ========================================================================
        # STEP 2: Apply ratio adjustment for integrated modes
        # ========================================================================
        # This ensures HSM production matches NGMM feedback while preserving gas type distribution
        if mode != 'standalone':
            self._apply_ratio_adjustment()
        
        # ========================================================================
        # STEP 3: Update production variables (same steps for both modes)
        # ========================================================================
        # Both standalone and integrated run the full update; they differ only by
        # nonassoc_source (expected ogenagprd vs realized ogrnagprd).
        is_integrated = mode != 'standalone'
        is_first_steo_year = self.parent.current_year == self.parent.steo_years[0]
        is_after_first_steo_year = self.parent.current_year > self.parent.steo_years[0]
        
        # Filter non-associated and associated-dissolved production data for onshore
        on_nonassoc_prod = self._filter_production_by_location(nonassoc_source, location='onshore')
        on_assoc_prod = self._filter_production_by_location(assoc_source, location='onshore')
        
        # Update ogqngrep by category (onshore)
        self._update_ogqngrep_by_category(on_nonassoc_prod, on_assoc_prod)
        
        # Update ogqngrep with offshore production
        self._update_offshore_production(nonassoc_source, assoc_source)
        
        # Aggregate ogqngrep total
        self._aggregate_ogqngrep_total()
        
        # Update fed/nonfed gas split
        self.adjust_fed_nonfed_gas()
        
        # Update ogdngprd by district
        self._update_ogdngprd_by_district(nonassoc_source, assoc_source)
        
        # All modes: update ogngprd by region (same logic for both; source differs by mode)
        self._update_ogngprd_by_region(nonassoc_source, assoc_source, mode='integrated')
        
        # All modes: update ogregprd
        # Integrated modes zero tight gas for all projection years, standalone for current year only
        zero_all_years = is_integrated
        self._update_ogregprd(nonassoc_source, assoc_source, zero_tight_all_years=zero_all_years)
        
        # ========================================================================
        # STEP 4: Adjust OGQSHLGAS (shale gas play production) - Year-based logic
        # ========================================================================
        # First STEO year: Adjust "other" category (index 15) to match OGQNGREP totals (all modes)
        if is_first_steo_year:
            # Ensures aggregate shale gas production (OGQNGREP index 1) matches
            # the sum of all OGQSHLGAS plays by adjusting the "other" category
            self._adjust_ogqshlgas_for_first_steo_year()
        elif is_integrated and is_after_first_steo_year:
            # After first STEO year (integrated only): Scale all play-level production,
            # well counts, and project drilling parameters based on realized production ratios
            if self.parent.onshore_switch:
                self.adjust_onshore_shale_gas_plays_for_realized_prod()

    def _calculate_realized_production_ratio(self, current_year):
        """Calculate realized production ratios by district.
        
        Parameters
        ----------
        current_year : int
            Current model year
            
        Returns
        -------
        pd.DataFrame
            DataFrame with production ratios indexed by district number,
            containing column 'realized_prod_ratio'
        """
        # Get realized and expected production for current year, gas type 5 (total)
        realized_prod = self.parent.restart.ogsmout_ogrnagprd.copy()
        realized_prod = realized_prod.xs(current_year, level=2)
        realized_prod = realized_prod.xs(self.NA_GAS_TOTAL, level=1)
        
        expected_prod = self.parent.restart.ogsmout_ogenagprd.copy()
        expected_prod = expected_prod.xs(current_year, level=2)
        expected_prod = expected_prod.xs(self.NA_GAS_TOTAL, level=1)
        
        # Convert to numeric types to avoid dtype issues
        realized_prod['value'] = pd.to_numeric(realized_prod['value'], errors='coerce')
        expected_prod['value'] = pd.to_numeric(expected_prod['value'], errors='coerce')
        
        # Calculate realized production ratios - protect against divide-by-zero
        prod_ratio = pd.DataFrame(
            np.divide(
                realized_prod['value'].values,
                expected_prod['value'].values,
                out=np.ones(len(realized_prod), dtype=float),
                where=expected_prod['value'].values != 0
            ),
            index=realized_prod.index
        )
        prod_ratio.columns = [nam.realized_prod_ratio]
        
        return prod_ratio

    def _apply_ratio_to_production(self, onshore, prod_ratio, current_year):
        """Apply realized production ratios to shale/tight gas production.
        
        Parameters
        ----------
        onshore : Onshore
            Onshore submodule instance
        prod_ratio : pd.DataFrame
            Production ratios indexed by district number
        current_year : int
            Current model year
            
        Returns
        -------
        pd.DataFrame
            Updated shale/tight gas production DataFrame
        """
        # Get shale/tight gas production
        natgas_prod = onshore.natgas_production[[nam.district_number, nam.play, nam.well_type_number, nam.process_code, current_year]].copy()
        shale_tight_prod = natgas_prod[natgas_prod[nam.well_type_number].isin(self.SHALE_TIGHT_WELL_TYPES)].copy()
        
        # Apply realized production ratios
        shale_tight_prod = shale_tight_prod.reset_index().merge(prod_ratio,
                                                                  how='left',
                                                                  left_on=nam.district_number,
                                                                  right_index=True).set_index('index')
        
        shale_tight_prod[current_year] = shale_tight_prod[current_year] * shale_tight_prod[nam.realized_prod_ratio]
        
        # Update onshore.natgas_production
        onshore.natgas_production.update(shale_tight_prod[[current_year]])
        
        return shale_tight_prod

    def _update_ogqshlgas_play_production(self, natgas_prod_adj, current_year):
        """Map adjusted production to OGQSHLGAS play indices and adjust "other" category.
        
        Uses the shale gas play mapping from CSV to iterate through plays instead
        of hardcoding each play.
        
        Parameters
        ----------
        natgas_prod_adj : pd.DataFrame
            Adjusted natural gas production filtered to shale/AD well types
        current_year : int
            Current model year
        """
        onshore = self.parent.onshore
        play_mapping = onshore.shale_gas_index_to_plays
        
        # Get all mapped play numbers for "other" category calculation
        all_mapped_plays = set()
        for play_numbers in play_mapping.values():
            all_mapped_plays.update(play_numbers)
        
        # Apply production to each mapped play
        for output_index, play_numbers in play_mapping.items():
            play_prod = natgas_prod_adj[natgas_prod_adj[nam.play].isin(play_numbers)].copy()
            self.parent.restart.ogsmout_ogqshlgas.at[(output_index, current_year), 'value'] = (
                play_prod[current_year].sum() / self.BCF_TO_TCF
            )
        
        # Calculate "other" category (index 15) - all plays not in mapped plays
        other_prod = natgas_prod_adj[~natgas_prod_adj[nam.play].isin(all_mapped_plays)].copy()
        self.parent.restart.ogsmout_ogqshlgas.at[(self.OGQSHLGAS_OTHER, current_year), 'value'] = (
            other_prod[current_year].sum() / self.BCF_TO_TCF
        )
        
        # Adjust 'other' to make sure total play-level section matches total on Table 14
        shale_sum = self.parent.restart.ogsmout_ogqshlgas.xs(current_year, level=1, drop_level=False).sum()
        tempscale = self.parent.restart.ogsmout_ogqngrep.at[(self.QNGREP_SHALE, current_year), 'value'] - shale_sum * 1000
        # Use .loc[] for accessing the value, ensuring it's a scalar
        self.parent.restart.ogsmout_ogqshlgas.loc[(self.OGQSHLGAS_OTHER, current_year)] = (
            self.parent.restart.ogsmout_ogqshlgas.loc[(self.OGQSHLGAS_OTHER, current_year)] + tempscale / 1000
        )

    def _apply_ratio_to_wells(self, onshore, prod_ratio, current_year):
        """Apply realized production ratios to well counts and round up.
        
        Parameters
        ----------
        onshore : Onshore
            Onshore submodule instance
        prod_ratio : pd.DataFrame
            Production ratios indexed by district number
        current_year : int
            Current model year
            
        Returns
        -------
        pd.DataFrame
            Updated shale/tight wells DataFrame
        """
        # Get shale/tight gas wells
        shale_tight_wells = onshore.wells[[nam.district_number, nam.play, nam.well_type_number, current_year]].copy()
        shale_tight_wells = shale_tight_wells[shale_tight_wells[nam.well_type_number].isin(self.SHALE_TIGHT_WELL_TYPES)].copy()
        
        # Apply realized production ratios
        shale_tight_wells = shale_tight_wells.reset_index().merge(prod_ratio,
                                                                  how='left',
                                                                  left_on=nam.district_number,
                                                                  right_index=True).set_index('index')
        
        shale_tight_wells[current_year] = shale_tight_wells[current_year] * shale_tight_wells[nam.realized_prod_ratio]
        
        # Round well counts
        shale_tight_wells[current_year] = shale_tight_wells[current_year].apply(np.ceil)
        
        # Update onshore.wells
        onshore.wells.update(shale_tight_wells[[current_year]])
        
        return shale_tight_wells

    def _update_restart_well_variables(self, onshore, current_year):
        """Update restart variables for wells (ogogwells, ognowell, ogwellsl48, ogsrl48).
        
        Parameters
        ----------
        onshore : Onshore
            Onshore submodule instance
        current_year : int
            Current model year
        """
        # Adjust Wells and Dryholes for restart file
        onshore.wells[nam.district_number] = onshore.wells[nam.district_number].astype(int)
        onshore.wells[nam.well_type_number] = onshore.wells[nam.well_type_number].astype(int)
        temp = onshore.wells.groupby([nam.district_number, nam.well_type_number]).sum()[[current_year]].stack()
        self.parent.restart.ogsmout_ogogwells['value'].update(temp)
        
        # Add to self.ogsmout_ognowell
        temp = pd.concat([onshore.wells.copy(), onshore.dryholes.copy(), onshore.exploratory_wells.copy()], ignore_index=True)
        if (not temp.empty) & (current_year > self.parent.steo_years[0]):
            self.parent.restart.ogsmout_ognowell.loc[current_year] += temp[[current_year]].sum().values[0]
        
        # Successful Wells and Dryholes by Region
        temp_wells = onshore.wells[[nam.region_number, nam.well_type_number, current_year]].copy()
        temp_total = pd.concat([onshore.wells.copy(), onshore.dryholes.copy()], ignore_index=True)
        temp_total = temp_total[[nam.region_number, nam.well_type_number, current_year]]
        
        # Groupby well_type_number & region - Gas
        temp_total = temp_total.groupby([nam.region_number, nam.well_type_number]).sum()
        temp_wells = temp_wells.groupby([nam.region_number, nam.well_type_number]).sum()
        # Replace zeros with NaN to avoid division by zero
        temp_total = temp_total.replace(0, np.nan)
        temp_sr = temp_wells.div(temp_total, axis=1)
        
        # Apply total wells to restart variables (total wells/dryholes and success rates)
        self.parent.restart.ogsmout_ogwellsl48['value'].update(temp_total.stack())
        self.parent.restart.ogsmout_ogsrl48['value'].update(temp_sr.stack())

    def _apply_ratio_to_projects(self, onshore, prod_ratio):
        """Apply realized production ratios to project drilling parameters.
        
        Parameters
        ----------
        onshore : Onshore
            Onshore submodule instance
        prod_ratio : pd.DataFrame
            Production ratios indexed by district number
            
        Returns
        -------
        pd.DataFrame
            Updated projects DataFrame
        """
        # Get shale/tight gas projects
        projects = onshore.projects.copy()
        shale_tight_projects = projects.loc[projects[nam.well_type_number].isin(self.SHALE_TIGHT_WELL_TYPES)]
        
        # Apply realized production ratios
        shale_tight_projects = shale_tight_projects.reset_index().merge(prod_ratio,
                                                                         how='left',
                                                                         left_on=nam.district_number,
                                                                         right_index=True).set_index('index')
        
        # Redo past_drilling
        shale_tight_projects[nam.past_wells] = shale_tight_projects[nam.past_wells] - shale_tight_projects[nam.last_year_drilling]
        
        # Apply ratio to last year drilling
        shale_tight_projects[nam.last_year_drilling] = shale_tight_projects[nam.last_year_drilling] * shale_tight_projects[nam.realized_prod_ratio]
        shale_tight_projects[nam.last_year_drilling] = shale_tight_projects[nam.last_year_drilling].apply(np.ceil)
        
        # Adjust Past Wells
        shale_tight_projects[nam.past_wells] = shale_tight_projects[nam.past_wells] + shale_tight_projects[nam.last_year_drilling]
        shale_tight_projects[nam.past_wells] = shale_tight_projects[nam.past_wells].astype('int64')
        
        # Update onshore.projects
        onshore.projects.update(shale_tight_projects)
        
        return shale_tight_projects

    def _update_ogregprd_by_gas_type(self, onshore, current_year):
        """Update ogregprd with adjusted production by gas type.
        
        Applies adjusted realized natural gas volumes to ogregprd:
        - 4 = Conventional gas
        - 5 = Tight Gas (zeroed out, recategorized to shale)
        - 6 = Shale Gas
        - 7 = CBM
        
        Parameters
        ----------
        onshore : Onshore
            Onshore submodule instance
        current_year : int
            Current model year
        """
        # Get production by well type and region
        temp = onshore.natgas_production[[nam.well_type_number, nam.region_number, current_year]]
        
        # Conventional gas (well types 1, 3)
        conv_temp = temp[temp[nam.well_type_number].isin(self.CONV_WELL_TYPES)]
        conv_temp = conv_temp.groupby(nam.region_number).sum()
        conv_temp[nam.gas_type] = self.REGPRD_CONV
        conv_temp = conv_temp.set_index([nam.gas_type], append=True)
        self.parent.restart.ogsmout_ogregprd['value'].update(conv_temp[[current_year]].stack() / self.BCF_TO_TCF)
        
        # Tight gas (well type 4) - recategorized to shale gas (index 6)
        tight_temp = temp[temp[nam.well_type_number].isin([4])]
        tight_temp = tight_temp.groupby(nam.region_number).sum()
        tight_temp[nam.gas_type] = self.REGPRD_SHALE  # Recategorize well type 4 (tight gas) to shale gas (index 6)
        tight_temp = tight_temp.set_index([nam.gas_type], append=True)
        self.parent.restart.ogsmout_ogregprd['value'].update(tight_temp[[current_year]].stack() / self.BCF_TO_TCF)
        
        # Shale gas (well types 2, 5)
        shale_temp = temp[temp[nam.well_type_number].isin(self.SHALE_AD_WELL_TYPES)]
        shale_temp = shale_temp.groupby(nam.region_number).sum()
        shale_temp[nam.gas_type] = self.REGPRD_SHALE
        shale_temp = shale_temp.set_index([nam.gas_type], append=True)
        self.parent.restart.ogsmout_ogregprd['value'].update(shale_temp[[current_year]].stack() / self.BCF_TO_TCF)
        
        # CBM (well type 6)
        cbm_temp = temp[temp[nam.well_type_number].isin([self.CBM_WELL_TYPE])]
        cbm_temp = cbm_temp.groupby(nam.region_number).sum()
        cbm_temp[nam.gas_type] = self.REGPRD_CBM
        cbm_temp = cbm_temp.set_index([nam.gas_type], append=True)
        self.parent.restart.ogsmout_ogregprd['value'].update(cbm_temp[[current_year]].stack() / self.BCF_TO_TCF)
        
        # Zero out tight gas (index 5) for current year after routing well type 4 to shale gas
        projection_year_mask = self.parent.restart.ogsmout_ogregprd.index.get_level_values(2) == current_year
        tight_gas_mask = self.parent.restart.ogsmout_ogregprd.index.get_level_values(1) == self.REGPRD_TIGHT
        tight_gas_projection_mask = projection_year_mask & tight_gas_mask
        self.parent.restart.ogsmout_ogregprd.loc[tight_gas_projection_mask, 'value'] = 0

    def adjust_onshore_shale_gas_plays_for_realized_prod(self):
        """Scales onshore natural gas play production to NGMM realized demand.
        
        This method applies realized production ratios to onshore shale/tight gas production,
        well counts, and project drilling parameters to match NGMM feedback. It updates
        play-level production variables and restart variables.
        
        Returns
        -------
        Updates the following restart variables:
        - ogsmout_ogqshlgas : Natural gas production by select natural gas play
        - ogsmout_ogogwells : Total wells
        - ogsmout_ognowell : Total completed wells
        - ogsmout_ogwellsl48 : Total lower 48 wells
        - ogsmout_ogsrl48 : Lower 48 drilling success rates
        - ogsmout_ogregprd : Total crude oil and natural gas production by production type
        
        Also updates onshore internal data structures:
        - natgas_production : Natural gas production by project
        - wells : DataFrame of onshore wells
        - projects : Master DataFrame containing all projects
        """
        onshore = self.parent.onshore
        current_year = int(self.parent.current_year)
        
        # ========================================================================
        # Step 1: Calculate Production Ratios
        # ========================================================================
        prod_ratio = self._calculate_realized_production_ratio(current_year)
        
        # ========================================================================
        # Step 2: Apply Ratios to Production
        # ========================================================================
        self._apply_ratio_to_production(onshore, prod_ratio, current_year)
        
        # ========================================================================
        # Step 3: Update OGQSHLGAS Play Production
        # ========================================================================
        # Get shale/AD gas production for play mapping
        shale_ad_prod = onshore.natgas_production.copy()
        shale_ad_prod = shale_ad_prod[shale_ad_prod[nam.well_type_number].isin(self.SHALE_AD_WELL_TYPES)]
        
        self._update_ogqshlgas_play_production(shale_ad_prod, current_year)
        
        # ========================================================================
        # Step 4: Apply Ratios to Wells
        # ========================================================================
        self._apply_ratio_to_wells(onshore, prod_ratio, current_year)
        
        # ========================================================================
        # Step 5: Update Restart Well Variables
        # ========================================================================
        self._update_restart_well_variables(onshore, current_year)
        
        # ========================================================================
        # Step 6: Apply Ratios to Projects
        # ========================================================================
        self._apply_ratio_to_projects(onshore, prod_ratio)
        
        # ========================================================================
        # Step 7: Update OGREGRD by Gas Type
        # ========================================================================
        self._update_ogregprd_by_gas_type(onshore, current_year)

    def apply_steo_benchmarks_fed_nonfed(self):
        """
        Applies STEO benchmarks to federal and non-federal oil production data.

            *  Extracts relevant data for the current year, including total
               proved reserves (rfqtdcrd) and federal/non-federal oil production
               (ogcoprd_fed, ogcoprd_nonfed).
            *  Calculates the ratio of federal to total (federal + non-federal)
               oil production for each region.
            *  Merges the total proved reserves data with the calculated
               federal/non-federal ratio.
            *  Calculates the federal and non-federal benchmarks by applying the
               federal ratio to the total proved reserves.
            *  Updates the parent's restart data with the calculated federal
               and non-federal benchmarks for oil production.

        Returns
        -------
        self.restart.ogsmout_ogcoprd_fed : df
            Federal oil production with updated benchmarks
        self.restart.ogsmout_ogcoprd_nonfed : df
            Non-federal oil production with updated benchmarks

        """

        current_year = int(self.parent.current_year)

        # 1. Extract Data for the Current Year
        rfqtdcrd = self.parent.restart.pmmout_rfqtdcrd.xs(current_year, level=1, drop_level=False).copy()
        ogcoprd_fed = self.parent.restart.ogsmout_ogcoprd_fed.xs(current_year, level=1, drop_level=False).copy()
        ogcoprd_nonfed = self.parent.restart.ogsmout_ogcoprd_nonfed.xs(current_year, level=1,
                                                                       drop_level=False).copy()

        # 2. Calculate Federal Ratio
        fed_nonfed_ratio = ogcoprd_fed.merge(ogcoprd_nonfed, left_index=True, right_index=True,
                                             suffixes=('_fed', '_nonfed'))
        
        # Convert to numeric types to avoid dtype issues
        fed_nonfed_ratio['value_fed'] = pd.to_numeric(fed_nonfed_ratio['value_fed'], errors='coerce')
        fed_nonfed_ratio['value_nonfed'] = pd.to_numeric(fed_nonfed_ratio['value_nonfed'], errors='coerce')
        
        # Calculate fed_ratio with protection against divide-by-zero
        denominator = fed_nonfed_ratio['value_fed'] + fed_nonfed_ratio['value_nonfed']
        fed_nonfed_ratio['fed_ratio'] = np.divide(
            fed_nonfed_ratio['value_fed'].values,
            denominator.values,
            out=np.zeros(len(fed_nonfed_ratio), dtype=float),
            where=denominator.values != 0
        )

        # 3. Merge and Filter Data
        calculate_fed_nonfed = rfqtdcrd.merge(fed_nonfed_ratio, left_index=True, right_index=True)
        calculate_fed_nonfed = calculate_fed_nonfed[
            calculate_fed_nonfed.index.get_level_values(0) != 14].copy()  # Drop index 14
        calculate_fed_nonfed.drop(columns=['value_fed', 'value_nonfed'], inplace=True)

        # 4. Calculate Benchmarks
        fed_bench = pd.DataFrame()
        nonfed_bench = pd.DataFrame()
        fed_bench['value'] = calculate_fed_nonfed['value'] * calculate_fed_nonfed['fed_ratio']
        nonfed_bench['value'] = calculate_fed_nonfed['value'] * (1 - calculate_fed_nonfed['fed_ratio'])

        # 5. Update Parent's Restart Data
        self.parent.restart.ogsmout_ogcoprd_fed.update(fed_bench.astype(self.parent.restart.ogsmout_ogcoprd_fed.dtypes))
        self.parent.restart.ogsmout_ogcoprd_nonfed.update(nonfed_bench.astype(self.parent.restart.ogsmout_ogcoprd_nonfed.dtypes))

        return

    def _create_debug_comparison_file(self, model_df, steo_df, output_filename, 
                                     mapping_key, index_level_name='region_number'):
        """Creates debug CSV file comparing model vs STEO values with name mappings.
        
        Parameters
        ----------
        model_df : pd.DataFrame
            Model production values (before STEO update)
        steo_df : pd.DataFrame
            STEO production values
        output_filename : str
            Name of the output CSV file (without path)
        mapping_key : str
            Key for parameter mapping dictionary (e.g., 'mnumor_2', 'soplay_ogqshloil')
        index_level_name : str, optional
            Name for the first index level after reset (default: 'region_number')
            
        Returns
        -------
        None
            Writes CSV file to steo_debug directory
        """
        # Merge model and STEO dataframes
        debug_df = model_df.merge(
            steo_df,
            how='left',
            left_index=True,
            right_index=True,
            suffixes=['_model', '_steo']
        ).dropna()
        
        # Convert to numeric types to avoid dtype('O') casting errors
        debug_df['value_model'] = pd.to_numeric(debug_df['value_model'], errors='coerce')
        debug_df['value_steo'] = pd.to_numeric(debug_df['value_steo'], errors='coerce')
        
        # Remove any rows where conversion failed (resulted in NaN)
        debug_df = debug_df.dropna(subset=['value_model', 'value_steo'])
        
        # Calculate STEO adjustment ratio (model/STEO)
        debug_df['steo_adj_ratio'] = np.divide(
            debug_df['value_model'].values,
            debug_df['value_steo'].values,
            out=np.zeros(len(debug_df), dtype=float),
            where=debug_df['value_steo'].values != 0
        )
        
        # Load name mapping from cached parameter mappings
        mappings_dict, _ = self.parent.restart._load_parameter_mappings()
        name_mapping_dict = mappings_dict.get(mapping_key, {})
        
        # Map index values to names (before resetting index for play data)
        if index_level_name == 'play_number':
            # For play-level data, extract play name from index level 0 before resetting
            debug_df['play_name'] = debug_df.index.get_level_values(0).map(name_mapping_dict).fillna('Unknown')
            debug_df_reset = debug_df.reset_index()
            # Reorder columns to put play_name first (matching original code pattern)
            cols = ['play_name'] + debug_df_reset.columns.drop('play_name').tolist()
            debug_df_reset = debug_df_reset[cols]
        else:
            # For region data, reset index first, then map
            debug_df_reset = debug_df.reset_index()
            debug_df_reset = debug_df_reset.rename(columns={debug_df_reset.columns[0]: 'region_number'})
            debug_df_reset['region_name'] = debug_df_reset['region_number'].map(name_mapping_dict).fillna('Unknown')
            # Reorder columns: region_number, region_name, then others
            cols = ['region_number', 'region_name'] + debug_df_reset.columns.drop(['region_number', 'region_name']).tolist()
            debug_df_reset = debug_df_reset[cols]
        
        # Write to CSV
        output_path = self.parent.output_path + 'steo_debug//' + output_filename
        debug_df_reset.to_csv(output_path, index=False)

    def _update_play_level_production(self, steo_source_attr, restart_var_attr, 
                                     debug_filename, mapping_key, is_first_steo_year):
        """Updates play-level production from STEO data.
        
        For the first STEO year, overwrites model values with STEO values.
        For subsequent STEO years, adds STEO values to existing model values.
        
        Parameters
        ----------
        steo_source_attr : str
            Attribute name for STEO data source (e.g., 'steo_togqshloil')
        restart_var_attr : str
            Attribute name for restart variable to update (e.g., 'ogsmout_ogqshloil')
        debug_filename : str
            Name of debug CSV file to create (only for first STEO year)
        mapping_key : str
            Key for parameter mapping dictionary (e.g., 'soplay_ogqshloil')
        is_first_steo_year : bool
            True if current year is the first STEO year
            
        Returns
        -------
        None
            Updates restart variable in place
        """
        current_year = int(self.parent.current_year)
        steo_source = getattr(self.parent.hist_steo, steo_source_attr)
        restart_var = getattr(self.parent.restart, restart_var_attr)
        
        # Extract STEO data for current year
        steo_df = steo_source.xs(current_year, level=1, drop_level=False).copy()
        
        if is_first_steo_year:
            # First STEO year: overwrite with STEO values
            # Get model data BEFORE updating (for debug comparison)
            model_df = restart_var.xs(current_year, level=1, drop_level=False).copy()
            
            # Update restart variable with STEO values
            restart_var.update(steo_df.astype(restart_var.dtypes))
            
            # Create debug comparison file
            self._create_debug_comparison_file(
                model_df, steo_df, debug_filename, mapping_key, index_level_name='play_number'
            )
        else:
            # Subsequent STEO years: add STEO values to existing model values
            model_df = restart_var.xs(current_year, level=1, drop_level=False).copy()
            model_df[nam.value] = (model_df[nam.value] + steo_df[nam.value]).fillna(0.0)
            restart_var.update(model_df.astype(restart_var.dtypes))

    def _update_ngpl_production(self, steo_source_attr, restart_var_attr):
        """Updates NGPL production variable from STEO data.
        
        Parameters
        ----------
        steo_source_attr : str
            Attribute name for STEO data source (e.g., 'steo_ogngplprd')
        restart_var_attr : str
            Attribute name for restart variable to update (e.g., 'ogsmout_ogngplprd')
            
        Returns
        -------
        None
            Updates restart variable in place
        """
        current_year = int(self.parent.current_year)
        steo_source = getattr(self.parent.hist_steo, steo_source_attr)
        restart_var = getattr(self.parent.restart, restart_var_attr)
        
        # Extract STEO data for current year and update restart variable
        steo_df = steo_source.xs(current_year, level=1, drop_level=False).copy()
        restart_var.update(steo_df.astype(restart_var.dtypes))

    def _calculate_eor_ratio_from_history(self, history_year):
        """Calculates EOR ratio from history year data.
        
        The EOR ratio is the ratio of EOR production to total production for each region.
        EOR production is derived as: Total (rfqtdcrd) - Non-EOR (rfqdcrd).
        This ratio is used to calculate non-EOR production for STEO years.
        
        Parameters
        ----------
        history_year : int
            History year to use for calculating the ratio
            
        Returns
        -------
        pd.DataFrame
            DataFrame with 'eor_ratio' column (EOR/Total), indexed by region number
        """
        # Get onshore regions only (regions 1-7)
        total_prod = self.restart.pmmout_rfqtdcrd.copy()
        total_prod = total_prod[total_prod.index.get_level_values(0).isin(self.ONSHORE_REGIONS)]
        
        # pmmout_rfqdcrd is non-EOR production (total excluding EOR)
        non_eor_prod = self.restart.pmmout_rfqdcrd.copy()
        non_eor_prod = non_eor_prod[non_eor_prod.index.get_level_values(0).isin(self.ONSHORE_REGIONS)]
        
        # Extract history year data
        total_prod_history = total_prod.xs(history_year, level=1).copy()
        non_eor_prod_history = non_eor_prod.xs(history_year, level=1).copy()
        
        # Convert to numeric types to avoid dtype('O') casting errors
        non_eor_prod_history[nam.value] = pd.to_numeric(non_eor_prod_history[nam.value], errors='coerce')
        total_prod_history[nam.value] = pd.to_numeric(total_prod_history[nam.value], errors='coerce')
        
        # Calculate actual EOR production = Total - Non-EOR
        eor_prod_history = total_prod_history[nam.value].values - non_eor_prod_history[nam.value].values
        
        # Calculate EOR ratio (EOR production / total production) and preserve region index
        eor_ratio = pd.DataFrame(
            np.divide(
                eor_prod_history,
                total_prod_history[nam.value].values,
                out=np.zeros(len(total_prod_history), dtype=float),
                where=total_prod_history[nam.value].values != 0
            ),
            index=total_prod_history.index,
            columns=['eor_ratio']
        )
        
        return eor_ratio

    def _benchmark_production_by_category(self):
        """Benchmarks crude oil production by category (EOR, L48 Onshore, Offshore, Alaska).
        
        Categories:
        - Category 1: EOR (Enhanced Oil Recovery)
        - Category 2: L48 Onshore (Lower 48 onshore, excluding EOR)
        - Category 3: Offshore
        - Category 4: Alaska
        
        Returns
        -------
        None
            Updates ogsmout_ogqcrrep restart variable in place
        """
        current_year = int(self.parent.current_year)
        
        # Category 1: EOR (Enhanced Oil Recovery)
        # Calculate EOR production for STEO years: EOR = Total (rfqtdcrd) - Non-EOR (rfqdcrd) for onshore regions
        onshore_total = self.parent.restart.pmmout_rfqtdcrd[
            self.parent.restart.pmmout_rfqtdcrd.index.get_level_values('1').isin(self.ONSHORE_REGIONS)
        ].copy()
        onshore_total = onshore_total[onshore_total.index.get_level_values('2').isin([current_year])].copy()
        onshore_total_sum = onshore_total.values.sum()
        
        onshore_non_eor = self.parent.restart.pmmout_rfqdcrd[
            self.parent.restart.pmmout_rfqdcrd.index.get_level_values('1').isin(self.ONSHORE_REGIONS)
        ].copy()
        onshore_non_eor = onshore_non_eor[onshore_non_eor.index.get_level_values('2').isin([current_year])].copy()
        onshore_non_eor_sum = onshore_non_eor.values.sum()
        
        # EOR production = Total - Non-EOR
        eor_production = onshore_total_sum - onshore_non_eor_sum
        # Convert from MMBBL/D to MMBBL/yr and update
        self.parent.restart.ogsmout_ogqcrrep.at[(1, current_year), nam.value] = _cast_restart_scalar(
            self.parent.restart.ogsmout_ogqcrrep,
            eor_production * 365
        )
        
        # Category 2: L48 Onshore (excluding EOR)
        # Use the calculated non-EOR production directly
        # Convert from MMBBL/D to MMBBL/yr and update
        self.parent.restart.ogsmout_ogqcrrep.at[(2, current_year), nam.value] = _cast_restart_scalar(
            self.parent.restart.ogsmout_ogqcrrep,
            onshore_non_eor_sum * 365
        )
        
        # Category 3: Offshore
        offshore_prod = self.parent.restart.pmmout_rfqtdcrd[
            self.parent.restart.pmmout_rfqtdcrd.index.get_level_values('1').isin(self.OFFSHORE_REGIONS)
        ].copy()
        offshore_prod = offshore_prod[offshore_prod.index.get_level_values('2').isin([current_year])].copy()
        offshore_prod_sum = offshore_prod.values.sum()
        # Convert from MMBBL/D to MMBBL/yr and update
        self.parent.restart.ogsmout_ogqcrrep.at[(3, current_year), nam.value] = _cast_restart_scalar(
            self.parent.restart.ogsmout_ogqcrrep,
            offshore_prod_sum * 365
        )
        
        # Category 4: Alaska
        alaska_prod = self.parent.restart.pmmout_rfqtdcrd.at[(self.ALASKA_REGION, current_year), nam.value]
        # Convert from MMBBL/D to MMBBL/yr and update
        self.parent.restart.ogsmout_ogqcrrep.at[(4, current_year), nam.value] = _cast_restart_scalar(
            self.parent.restart.ogsmout_ogqcrrep,
            alaska_prod * 365
        )

    def apply_steo_fixed_benchmarks(self):
        """Overwrites model production with STEO values for first two STEO years.
        
        **When This Runs:**
        - Always called for years in `steo_years[0:2]` (first two STEO years)
        - No flag required - this is the primary STEO benchmarking method
        - Called in `module_unf.py` line 1506 when `current_year in steo_years[0:2]`
        
        **What This Does:**
        - Directly overwrites model-calculated production with STEO forecast values
        - Updates crude oil, natural gas, and NGPL production variables to match STEO
        - This is the first step in STEO benchmarking - sets the baseline STEO values
        
        This method overwrites restart variables with reference case values from CSV files.

        Returns
        -------
        None
            Updates multiple restart variables in place:
            - pmmout_rfqtdcrd: Total crude production (including EOR)
            - pmmout_rfqdcrd: Non-EOR crude production
            - ogsmout_ogcoprd: Crude oil production by lower 48 region (onshore regions 1-7 and offshore regions 9-10)
            - ogsmout_ogqshloil: Tight oil production from select tight oil plays
            - ogsmout_ogqshlgas: Shale natural gas production from select shale natural gas plays
            - ogsmout_ogenagprd: Expected non-associated natural gas (STEO benchmarked)
            - ogsmout_ogrnagprd: Realized natural gas (updated to match ogenagprd)
            - ogsmout_ogngplprd: Total NGPL production by HSM district
            - ogsmout_ogngplet: Ethane production by HSM district
            - ogsmout_ogngplpr: Propane production by HSM district
            - ogsmout_ogngplbu: Butane production by HSM district
            - ogsmout_ogngplis: Isobutane production by HSM district
            - ogsmout_ogngplpp: Pentanes production by HSM district
            - ogsmout_ogregprd: Regional production by type (crude oil types only)
            - ogsmout_ogeorprd: EOR production by region and type
        """
        current_year = int(self.parent.current_year)
        is_first_steo_year = current_year == self.parent.steo_years[0]
        
        # ========================================================================
        # 1. Set Crude Oil Production Levels
        # ========================================================================
        
        # Domestic Crude Oil Production (MMBBL/D) - Total production including EOR
        steo_crude_production = self.parent.hist_steo.steo_rfqtdcrd.xs(
            current_year, level=1, drop_level=False
        ).copy()
        
        # Create debug comparison file for total crude production
        model_crude_production = self.restart.pmmout_rfqtdcrd.xs(
            current_year, level=1, drop_level=False
        ).copy()
        self._create_debug_comparison_file(
            model_crude_production,
            steo_crude_production,
            f'steo_rfqtdcrd_debug_{current_year}.csv',
            'mnumor_2',
            index_level_name='region_number'
        )
        
        # Update total crude production with STEO values
        self.restart.pmmout_rfqtdcrd.update(
            steo_crude_production.astype(self.restart.pmmout_rfqtdcrd.dtypes)
        )
        
        # Domestic Crude Oil Production (MMBBL/D) - non-EOR production
        # Initially set to same as total (will be adjusted below for onshore regions)
        self.restart.pmmout_rfqdcrd.update(
            steo_crude_production.astype(self.restart.pmmout_rfqdcrd.dtypes)
        )
        
        # Set non-EOR production for STEO years based on history year EOR ratio
        # Only adjust if onshore submodule is enabled
        if hasattr(self.parent, 'onshore_switch') and self.parent.onshore_switch:
            # Check if onshore object has the required attribute
            if hasattr(self.parent.onshore, 'steo_eor_annual_growth_rate'):
                # Get onshore production data (regions 1-7)
                total_prod_onshore = self.restart.pmmout_rfqtdcrd.copy()
                total_prod_onshore = total_prod_onshore[
                    total_prod_onshore.index.get_level_values(0).isin(self.ONSHORE_REGIONS)
                ]
                
                # Calculate EOR ratio from history year
                eor_ratio = self._calculate_eor_ratio_from_history(self.parent.history_year)
                
                # Get EOR annual growth rate from onshore configuration
                growth_rate = self.parent.onshore.steo_eor_annual_growth_rate
                max_eor_ratio = 0.99  # Maximum cap to prevent non-EOR from going negative
                
                # Process each STEO year individually to ensure correct application
                for steo_year in self.parent.steo_years[0:2]:
                    # Get STEO year total production values for onshore regions
                    steo_year_total_prod = total_prod_onshore[
                        total_prod_onshore.index.get_level_values(1).isin([steo_year])
                    ].copy()
                    
                    # Apply EOR ratio to calculate non-EOR production for STEO years
                    non_eor_prod_steo = steo_year_total_prod.reset_index()
                    non_eor_prod_steo = non_eor_prod_steo.merge(
                        eor_ratio, how='left', left_on='1', right_index=True
                    )
                    # Fill NaN eor_ratio values with 0 (assume no EOR if ratio missing)
                    non_eor_prod_steo['eor_ratio'] = non_eor_prod_steo['eor_ratio'].fillna(0)
                    
                    # Calculate years from history year to apply growth factor
                    years_from_history = steo_year - self.parent.history_year
                    
                    # Apply annual growth factor to EOR ratio: adjusted_ratio = base_ratio * (1 + growth_rate) ^ years
                    # Cap at maximum to ensure non-EOR production remains positive
                    non_eor_prod_steo['eor_ratio'] = non_eor_prod_steo['eor_ratio'] * ((1 + growth_rate) ** years_from_history)
                    non_eor_prod_steo['eor_ratio'] = non_eor_prod_steo['eor_ratio'].clip(upper=max_eor_ratio)
                    
                    # Calculate non-EOR production: non-EOR = total * (1 - adjusted EOR ratio)
                    non_eor_prod_steo[nam.value] = non_eor_prod_steo[nam.value] * (1 - non_eor_prod_steo['eor_ratio'])
                    non_eor_prod_steo = non_eor_prod_steo.set_index(['1', '2'])
                    self.restart.pmmout_rfqdcrd.update(
                        non_eor_prod_steo[nam.value].astype(self.restart.pmmout_rfqdcrd.dtypes)
                    )
            else:
                # Onshore is enabled but doesn't have the attribute (shouldn't happen in normal operation)
                if hasattr(self.parent, 'logger'):
                    self.parent.logger.warning('Onshore submodule enabled but steo_eor_annual_growth_rate attribute not found. Skipping EOR ratio adjustment.')
        else:
            # Onshore submodule is disabled - skip EOR adjustment, non-EOR production remains equal to total
            if hasattr(self.parent, 'logger'):
                self.parent.logger.info('Onshore submodule disabled. Skipping EOR ratio adjustment for STEO years. Non-EOR production set equal to total production.')
        
        # ========================================================================
        # 2. Benchmark Crude Production by Category
        # ========================================================================
        # Categories: 1=EOR, 2=L48 Onshore, 3=Offshore, 4=Alaska
        self._benchmark_production_by_category()
        
        # ========================================================================
        # 2.5. Populate OGEORPRD for STEO Years
        # ========================================================================
        # Calculate EOR production by region and distribute by EOR type using historical distribution
        for steo_year in self.parent.steo_years[0:2]:
            # Calculate total EOR production by region from rfqtdcrd - rfqdcrd
            for region in self.ONSHORE_REGIONS:
                total_eor_mmbd = (
                    self.restart.pmmout_rfqtdcrd.at[(region, steo_year), 'value'] -
                    self.restart.pmmout_rfqdcrd.at[(region, steo_year), 'value']
                )
                
                # Convert from MMBBL/D to MBbl/yr
                total_eor_mbbl_yr = total_eor_mmbd * 365 * 1000
                
                # Get historical EOR type distribution from history year
                hist_thermal = self.restart.ogsmout_ogeorprd.loc[(region, 1, self.parent.history_year), 'value']
                hist_co2 = self.restart.ogsmout_ogeorprd.loc[(region, 2, self.parent.history_year), 'value']
                hist_total = hist_thermal + hist_co2
                
                # Calculate distribution ratios (handle zero total)
                if hist_total > 0:
                    thermal_ratio = hist_thermal / hist_total
                    co2_ratio = hist_co2 / hist_total
                else:
                    # If no historical EOR, use default distribution (assume all CO2)
                    thermal_ratio = 0.0
                    co2_ratio = 1.0
                
                # Distribute STEO EOR by type
                thermal_eor_mbbl_yr = total_eor_mbbl_yr * thermal_ratio
                co2_eor_mbbl_yr = total_eor_mbbl_yr * co2_ratio
                
                # Populate OGEORPRD for this region and STEO year
                self.restart.ogsmout_ogeorprd.loc[(region, 1, steo_year), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    thermal_eor_mbbl_yr
                )
                self.restart.ogsmout_ogeorprd.loc[(region, 2, steo_year), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    co2_eor_mbbl_yr
                )
                self.restart.ogsmout_ogeorprd.loc[(region, 3, steo_year), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    total_eor_mbbl_yr
                )
            
            # Calculate region 8 (Total) as sum of regions 1-7 for each EOR type
            for eor_type in [1, 2, 3]:
                total_by_type = sum(
                    self.restart.ogsmout_ogeorprd.loc[(region, eor_type, steo_year), 'value']
                    for region in self.ONSHORE_REGIONS
                )
                self.restart.ogsmout_ogeorprd.loc[(8, eor_type, steo_year), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    total_by_type
                )
        
        # ========================================================================
        # 3. Update Play-Level Crude Production
        # ========================================================================
        self._update_play_level_production(
            'steo_togqshloil',
            'ogsmout_ogqshloil',
            f'steo_ogqshloil_debug_{current_year}.csv',
            'soplay_ogqshloil',
            is_first_steo_year
        )
        
        # ========================================================================
        # 4. Adjust Production by LFMM Region
        # ========================================================================
        # Calculate adjustment factor to match STEO onshore production totals
        total_prod = self.parent.restart.pmmout_rfqtdcrd.copy()
        total_prod_current_year = total_prod.xs(current_year, level=1).copy()
        # Sum onshore production (regions 1-13 for LFMM)
        onshore_prod_sum = total_prod_current_year[
            total_prod_current_year.index.get_level_values(0).isin(self.ONSHORE_LFMM_REGIONS)
        ][nam.value].sum()
        
        # Get model's onshore crude reference production
        crude_ref = self.parent.restart.ogsmout_ogcruderef.copy()
        crude_ref_current_year = crude_ref.xs(current_year, level=2)
        # Filter to onshore LFMM regions (regions 1-8)
        crude_ref_onshore = crude_ref_current_year[
            crude_ref_current_year.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7, 8])
        ]
        crude_ref_sum = crude_ref_onshore[nam.value].sum() / 365  # Convert from MMBBL/yr to MMBBL/D
        
        # Calculate STEO adjustment factor
        steo_adjustment_factor = onshore_prod_sum / crude_ref_sum
        
        # Apply adjustment to crude reference production
        crude_ref_adjusted = crude_ref_onshore.mul(steo_adjustment_factor)
        
        # Format for update to restart variable
        crude_ref_adjusted['2'] = current_year
        crude_ref_adjusted = crude_ref_adjusted.set_index(['2'], append=True)
        crude_ref_adjusted.index.names = self.parent.restart.ogsmout_ogcruderef.index.names
        self.parent.restart.ogsmout_ogcruderef.update(
            crude_ref_adjusted.astype(self.parent.restart.ogsmout_ogcruderef.dtypes)
        )
        
        # ========================================================================
        # 5. Update Regional Crude Production
        # ========================================================================
        # Crude oil production by lower 48 region (onshore regions 1-7)
        onshore_crude_prod = self.parent.restart.pmmout_rfqtdcrd.xs(
            current_year, level=1, drop_level=False
        ).copy()
        onshore_crude_prod = onshore_crude_prod[
            onshore_crude_prod.index.get_level_values(0).isin(self.ONSHORE_REGIONS)
        ]
        self.parent.restart.ogsmout_ogcoprd.update(
            onshore_crude_prod.astype(self.parent.restart.ogsmout_ogcoprd.dtypes)
        )
        
        # ========================================================================
        # Crude oil production by offshore regions (Gulf of Mexico and Pacific)
        # ========================================================================
        # Note: Region index mapping between STEO and HSM ogcoprd:
        #   - STEO region 9 (Gulf of Mexico) → HSM ogcoprd[8]
        #   - STEO region 10 (Pacific) → HSM ogcoprd[9]
        #   HSM ogcoprd uses indices 1-11 (skips 0), where index 8=Gulf, 9=Pacific, 10=Atlantic
        offshore_crude_prod = self.parent.hist_steo.steo_rfqtdcrd.xs(
            current_year, level=1, drop_level=False
        ).copy()
        # Filter to only Gulf of Mexico (STEO region 9) and Pacific (STEO region 10)
        offshore_crude_prod = offshore_crude_prod[
            offshore_crude_prod.index.get_level_values(0).isin([9, 10])
        ]
        # Remap STEO region indices to HSM ogcoprd indices:
        # STEO region 9 (Gulf) → HSM ogcoprd[8]
        # STEO region 10 (Pacific) → HSM ogcoprd[9]
        offshore_crude_prod_remapped = offshore_crude_prod.copy()
        offshore_crude_prod_remapped = offshore_crude_prod_remapped.reset_index()
        # Map STEO region 9 → HSM ogcoprd index 8
        offshore_crude_prod_remapped.loc[offshore_crude_prod_remapped['1'] == 9, '1'] = 8
        # Map STEO region 10 → HSM ogcoprd index 9
        offshore_crude_prod_remapped.loc[offshore_crude_prod_remapped['1'] == 10, '1'] = 9
        offshore_crude_prod_remapped = offshore_crude_prod_remapped.set_index(['1', '2'])
        self.parent.restart.ogsmout_ogcoprd.update(
            offshore_crude_prod_remapped.astype(self.parent.restart.ogsmout_ogcoprd.dtypes)
        )
        
        # Benchmark federal and nonfederal production variables
        self.apply_steo_benchmarks_fed_nonfed()
        
        # Regional crude oil and natural gas production by type of production
        # Type 1: Primary crude (non-EOR), Type 2: EOR crude
        for region in self.ONSHORE_REGIONS:
            # Primary crude production (non-EOR)
            self.parent.restart.ogsmout_ogregprd.at[
                (region, 1, current_year), nam.value
            ] = _cast_restart_scalar(
                self.parent.restart.ogsmout_ogregprd,
                self.parent.restart.pmmout_rfqdcrd.at[(region, current_year), 'value']
            )
            
            # EOR production = Total production - Primary production
            self.parent.restart.ogsmout_ogregprd.at[
                (region, 2, current_year), nam.value
            ] = _cast_restart_scalar(
                self.parent.restart.ogsmout_ogregprd,
                self.parent.restart.pmmout_rfqtdcrd.at[(region, current_year), 'value']
                - self.parent.restart.pmmout_rfqdcrd.at[(region, current_year), 'value']
            )
        
        # ========================================================================
        # 6. Update Play-Level Natural Gas Production
        # ========================================================================
        self._update_play_level_production(
            'steo_togqshlgas',
            'ogsmout_ogqshlgas',
            f'steo_ogqshlgas_debug_{current_year}.csv',
            'soplay_ogqshlgas',
            is_first_steo_year
        )
        
        # ========================================================================
        # 7. Update NGPL Production Variables
        # ========================================================================
        # All NGPL variables are updated from STEO data
        self._update_ngpl_production('steo_ogngplprd', 'ogsmout_ogngplprd')  # Total NGPL
        self._update_ngpl_production('steo_ogngplet', 'ogsmout_ogngplet')  # Ethane
        self._update_ngpl_production('steo_ogngplpr', 'ogsmout_ogngplpr')  # Propane
        self._update_ngpl_production('steo_ogngplbu', 'ogsmout_ogngplbu')  # Butane
        self._update_ngpl_production('steo_ogngplis', 'ogsmout_ogngplis')  # Isobutane
        self._update_ngpl_production('steo_ogngplpp', 'ogsmout_ogngplpp')  # Pentanes Plus

    def _load_side_case_csv(self, csv_filename, restart_var_name, is_scalar=False):
        """Load CSV file from side_case_owrites folder and parse headers.
        
        Parameters
        ----------
        csv_filename : str
            Name of CSV file (e.g., 'ogenagprd.csv')
        restart_var_name : str
            Name of restart variable (for error messages)
        is_scalar : bool
            Whether this is a scalar variable (dimension=1, only year index)
            
        Returns
        -------
        pd.DataFrame
            DataFrame with index label columns and year columns, or None if file not found
        """
        csv_path = os.path.join(self.parent.input_path, 'side_case_owrites', csv_filename)
        
        if not os.path.exists(csv_path):
            self.parent.logger.warning(f'CSV file not found for {restart_var_name}: {csv_path}')
            return None
        
        try:
            # Read CSV, skipping first row (description/units)
            df = pd.read_csv(csv_path, skiprows=1, low_memory=False)
            
            # First column is typically a row index, second is the label column
            # If labels have duplicates, use first column as numeric index instead
            first_col = df.columns[0]
            second_col = df.columns[1] if len(df.columns) > 1 else None
            
            if second_col and df[second_col].duplicated().any():
                # Labels have duplicates - use first column as numeric index
                # Rename first column to 'Index' so it gets matched as the primary index
                # Add 1 to convert from 0-based CSV index to 1-based reference table index
                df[first_col] = df[first_col].astype(int) + 1
                df = df.rename(columns={first_col: 'Index'})
                df = df.drop(second_col, axis=1)  # Drop the duplicate label column
            else:
                # No duplicates - drop first column (row index) as before
                df = df.drop(first_col, axis=1)
            
            # For scalar variables, filter out placeholder columns like "index" and "value"
            # These are just row identifiers and shouldn't be processed as index columns
            if is_scalar:
                placeholder_cols = ['index', 'value']
                cols_to_drop = [col for col in df.columns if col.lower() in placeholder_cols]
                if cols_to_drop:
                    df = df.drop(columns=cols_to_drop, errors='ignore')
            
            year_cols, _ = self._identify_year_columns(df)
            if not year_cols:
                self.parent.logger.warning(f'No year columns found in {csv_filename} for {restart_var_name}')
                return None
            
            return df
            
        except Exception as e:
            self.parent.logger.error(f'Error loading CSV file {csv_filename} for {restart_var_name}: {str(e)}')
            return None

    def _load_side_case_overwrite_config(self):
        """Load side case overwrite configuration from CSV.
        
        Returns
        -------
        list[tuple[str, str]]
            List of (csv_filename, restart_var_name) pairs.
        """
        config_path = os.path.join(
            self.parent.input_path, 'side_case_owrites', 'side_case_overwrite_config.csv'
        )
        
        if not os.path.exists(config_path):
            self.parent.logger.warning(f'Side case overwrite config not found: {config_path}')
            return []
        
        try:
            config_df = pd.read_csv(config_path)
            required_cols = {'csv_filename', 'restart_var_name'}
            if not required_cols.issubset(set(config_df.columns)):
                self.parent.logger.error(
                    f'Config missing required columns {required_cols}: {config_path}'
                )
                return []
            
            config_df = config_df.dropna(subset=['csv_filename', 'restart_var_name'])
            config_df['csv_filename'] = config_df['csv_filename'].astype(str).str.strip()
            config_df['restart_var_name'] = config_df['restart_var_name'].astype(str).str.strip()
            
            config_df = config_df[(config_df['csv_filename'] != '') & (config_df['restart_var_name'] != '')]
            
            return list(config_df[['csv_filename', 'restart_var_name']].itertuples(index=False, name=None))
        except Exception as e:
            self.parent.logger.error(f'Error loading side case overwrite config: {str(e)}')
            return []

    def _convert_labels_to_indices(self, df, index_types, var_mapping_path):
        """Convert human-readable labels to numeric indices using parameter reference tables.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame with label columns and year columns
        index_types : list
            List of index type names (e.g., ['ogdist', 'gastyp'])
        var_mapping_path : str
            Path to variable_mapping directory
            
        Returns
        -------
        pd.DataFrame
            DataFrame with numeric indices instead of labels, or None if conversion fails
        """
        if df is None:
            return None
        
        df = df.copy()
        
        # Index types that are year-based and don't need label-to-index conversion
        # (years are already numeric values in the CSV files)
        year_index_types = {'mnumyr', 'year'}
        
        # Load parameter reference tables and create reverse mappings
        label_to_index_maps = {}
        
        for idx_type in index_types:
            # Skip year-related indices - they don't need label conversion
            if idx_type.lower() in year_index_types:
                continue
            
            ref_table_path = os.path.join(var_mapping_path, 'parameter_ref_tables', f'{idx_type}.csv')
            
            if not os.path.exists(ref_table_path):
                self.parent.logger.error(f'Parameter reference table not found: {ref_table_path}')
                return None
            
            try:
                ref_df = pd.read_csv(ref_table_path)
                
                # Reference table format: first column = numeric index, second column = label
                index_col = ref_df.columns[0]
                label_col = ref_df.columns[1] if len(ref_df.columns) > 1 else ref_df.columns[0]
                
                # Create reverse mapping: {label: index_value}
                label_to_index = {}
                for _, row in ref_df.iterrows():
                    label = str(row[label_col]).strip()
                    index_val = int(row[index_col])
                    label_to_index[label] = index_val
                    # Also add case-insensitive version
                    label_to_index[label.lower()] = index_val
                
                label_to_index_maps[idx_type] = label_to_index
                
            except Exception as e:
                self.parent.logger.error(f'Error loading parameter reference table {idx_type}: {str(e)}')
                return None
        
        year_cols, _ = self._identify_year_columns(df)
        
        # Convert label columns to numeric indices
        # Match column names to index types using direct match, aliases, or "Index" fallback
        col_to_index_type = {}
        non_year_index_types = [t for t in index_types if t.lower() not in year_index_types]
        
        # Detect scalar variables (only year index types)
        is_scalar = len(non_year_index_types) == 0
        
        for col in df.columns:
            if col in year_cols:
                continue
            
            col_lower = col.lower()
            # Check direct match or alias match
            matched_type = None
            for idx_type in non_year_index_types:
                if col_lower == idx_type.lower() or self.COLUMN_ALIASES.get(col_lower) == idx_type:
                    matched_type = idx_type
                    break
            
            # Fallback: "Index" column uses first non-year index type
            if matched_type is None and col == 'Index' and non_year_index_types:
                matched_type = non_year_index_types[0]
            
            if matched_type:
                col_to_index_type[col] = matched_type
            elif not is_scalar:
                # Only warn for non-scalar variables - scalar variables may have placeholder columns
                self.parent.logger.warning(f'Could not match column {col} to index type. Available types: {index_types}')
        
        # Convert each label column to numeric indices
        for col, idx_type in col_to_index_type.items():
            if idx_type not in label_to_index_maps:
                continue
            
            label_map = label_to_index_maps[idx_type]
            
            # Convert labels to indices
            def map_label(label):
                label_str = str(label).strip()
                # Try exact match first
                if label_str in label_map:
                    return label_map[label_str]
                # Try case-insensitive
                if label_str.lower() in label_map:
                    return label_map[label_str.lower()]
                # Try to parse as integer (already numeric)
                try:
                    return int(label_str)
                except ValueError:
                    self.parent.logger.warning(f'Label "{label_str}" not found in reference table for {idx_type}')
                    return None
            
            df[col] = df[col].apply(map_label)
            
            # Drop rows where mapping failed
            df = df[df[col].notna()]
        
        return df

    def _reshape_to_restart_format(self, df, var_metadata, first_steo_year):
        """Reshape CSV data to match restart variable MultiIndex structure.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame with numeric index columns and year columns
        var_metadata : dict
            Dictionary with 'index_names' list
        first_steo_year : int
            First STEO year to filter to
            
        Returns
        -------
        pd.DataFrame
            DataFrame with MultiIndex matching restart variable structure, or None if reshape fails
        """
        if df is None or df.empty:
            return None
        
        try:
            year_cols, index_cols = self._identify_year_columns(df)
            
            # Filter to first STEO year only
            if str(first_steo_year) not in year_cols:
                self.parent.logger.warning(f'First STEO year {first_steo_year} not found in CSV data')
                return None
            
            # Get the year column
            year_col = str(first_steo_year)
            
            # Handle scalar variables (no index columns, only year)
            if not index_cols:
                # For scalar variables, create DataFrame with only year column
                result_df = pd.DataFrame({nam.value: df[year_col].values, nam.year: first_steo_year})
                result_df = result_df.set_index([nam.year])
                
                # Rename index level to match restart variable names
                expected_index_names = var_metadata.get('index_names', [])
                if expected_index_names:
                    # Year is the only index for scalar variables
                    year_index_name = 'mnumyr' if 'mnumyr' in expected_index_names else nam.year
                    result_df.index.names = [year_index_name]
            else:
                # Multi-dimensional variables: create DataFrame with index columns and value column
                result_df = df[index_cols + [year_col]].copy()
                result_df = result_df.rename(columns={year_col: nam.value})
                
                # Add year as a column
                result_df[nam.year] = first_steo_year
                
                # Set index: index columns + year
                index_cols_with_year = index_cols + [nam.year]
                result_df = result_df.set_index(index_cols_with_year)
                
                # Rename index levels to match restart variable names
                expected_index_names = var_metadata.get('index_names', [])
                if len(expected_index_names) == len(index_cols):
                    # Build list of new index names: index columns + year
                    new_index_names = []
                    for i, col in enumerate(index_cols):
                        new_index_names.append(expected_index_names[i])
                    # Year is always last, use 'mnumyr' or keep as 'year'
                    year_index_name = 'mnumyr' if 'mnumyr' in expected_index_names else nam.year
                    new_index_names.append(year_index_name)
                    # Use list-based assignment instead of dict
                    result_df.index.names = new_index_names
            
            return result_df
            
        except Exception as e:
            self.parent.logger.error(f'Error reshaping data to restart format: {str(e)}')
            return None

    def apply_side_case_steo_overwrites(self):
        """Overwrite restart variables with reference case values from preprocessed NPZ file.
        
        Only runs for side cases in first STEO year when side_case_steo_overwrite_switch == True.
        Variables that are already handled by apply_steo_fixed_benchmarks() are skipped.
        
        This method loads preprocessed data from input/side_case_owrites/steo_overwrite_data.npz
        and directly overwrites restart variables for the first STEO year only.
        
        The NPZ file is generated by running preprocess_steo_overwrites.py on the cb case
        restart.npz file. This preprocessing step extracts single-year data slices, eliminating
        the need for complex dimension handling at runtime.
        """
        current_year = int(self.parent.current_year)
        first_steo_year = self.parent.steo_years[0]
        
        if current_year != first_steo_year:
            return
        
        # Load preprocessed overwrite data
        npz_path = os.path.join(self.parent.input_path, 'side_case_owrites', 'steo_overwrite_data.npz')
        
        if not os.path.exists(npz_path):
            self.parent.logger.error(
                f'Preprocessed STEO overwrite data not found: {npz_path}. '
                f'Run preprocess_steo_overwrites.py on reference case restart.npz first.'
            )
            return
        
        try:
            overwrite_data = np.load(npz_path, allow_pickle=True)
        except Exception as e:
            self.parent.logger.error(f'Error loading preprocessed STEO overwrite data: {str(e)}')
            return
        
        # Validate metadata - check that STEO year matches
        if '_metadata_steo_year' in overwrite_data.files:
            npz_steo_year = int(overwrite_data['_metadata_steo_year'])
            if npz_steo_year != first_steo_year:
                self.parent.logger.warning(
                    f'STEO year mismatch: NPZ file was generated for year {npz_steo_year}, '
                    f'but current first STEO year is {first_steo_year}. Proceeding anyway.'
                )
        
        # Get list of variable names (excluding metadata keys)
        var_names = [f for f in overwrite_data.files if not f.startswith('_metadata_')]
        
        if not var_names:
            self.parent.logger.warning('No variables found in preprocessed STEO overwrite data. Skipping.')
            return
        
        self.parent.logger.info(f'Applying STEO overwrites for {len(var_names)} variables')
        
        # Process each variable
        updated_count = 0
        for var_name in var_names:
            try:
                # Get restart variable
                restart_var = getattr(self.parent.restart, var_name, None)
                if restart_var is None:
                    self.parent.logger.warning(f'Restart variable {var_name} not found, skipping')
                    continue
                
                # Get preprocessed slice data
                slice_data = overwrite_data[var_name]
                
                # Handle scalar (0-dimensional) data
                if slice_data.ndim == 0:
                    slice_data = float(slice_data)
                
                # Update restart variable for the STEO year
                # Year is always one of the index levels in restart variables
                year_mask = restart_var.index.get_level_values(-1) == first_steo_year
                
                if not year_mask.any():
                    self.parent.logger.warning(f'No data found for year {first_steo_year} in {var_name}, skipping')
                    continue
                
                # Get the rows for this year and update values
                if isinstance(slice_data, (int, float, np.floating, np.integer)):
                    # Scalar variable - single value for the year
                    restart_var.loc[year_mask, nam.value] = restart_var[nam.value].dtype.type(slice_data)
                else:
                    # Array variable - flatten and assign to matching rows
                    flat_data = slice_data.flatten()
                    
                    # Verify data size matches
                    year_data = restart_var.loc[year_mask]
                    if len(flat_data) != len(year_data):
                        self.parent.logger.warning(
                            f'Data size mismatch for {var_name}: preprocessed has {len(flat_data)} values, '
                            f'restart has {len(year_data)} rows for year {first_steo_year}. Skipping.'
                        )
                        continue
                    
                    # Update values - cast to appropriate dtype
                    restart_var.loc[year_mask, nam.value] = flat_data.astype(restart_var[nam.value].dtype)
                
                updated_count += 1
                self.parent.logger.debug(f'Updated {var_name} for year {first_steo_year}')
                
            except Exception as e:
                self.parent.logger.error(f'Error applying overwrite for {var_name}: {str(e)}')
                continue
        
        self.parent.logger.info(f'Successfully updated {updated_count}/{len(var_names)} variables for year {first_steo_year}')
