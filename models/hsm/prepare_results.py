"""File for preparing results prior to transmission to the restart file.

Summary
_______
This file contains the operations required to prepare results for transmission to the restart file, these methods include:

    1. aggregate_results_across_submodules -combines production volumes across submodules for cumulative variables.
    2. set_expected_production_to_steo - overwrites calculated production volumes with STEO production values.
    3. apply_steo_adjustments -  benchmarks HSM results to STEO.
    4. apply_ngmm_realized_prod and adjust_vars_for_realized_prod - respond to actual natural gas demand volumes provided by NGMM.


Model Functions and Class Methods
_________________________________
    * aggregate_results_across_submodules -combines production volumes across submodules for cumulative variables.
    * set_expected_production_to_steo - overwrites calculated production volumes with STEO production values.
    * apply_steo_adjustments -  benchmarks HSM results to STEO.
    * apply_ngmm_realized_prod and adjust_vars_for_realized_prod - respond to actual natural gas demand volumes provided by NGMM.


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

class Prep_Results(sub.Submodule):
    def __init__(self, parent):
        super().__init__(parent, submodule_name='prep_results')
        

    def aggregate_results_across_submodules(self):
        """Calculate sum  values in restart variable tables that are aggregated across submodules.

        Returns
        -------
        self.parent.restart.ogsmout_ogoilprd : df
            Crude oil production by HSM district

        self.parent.restart.ogsmout_ogenagprd : df
            Exepcted non-associated natural gas production by HSM district and type

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
        self.parent.restart.ogsmout_ogoilprd.update(temp)

        #Expected NG Production by HSM district
        temp = self.parent.restart.ogsmout_ogenagprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp.groupby(level = [0,2]).sum()
        temp[nam.well_type] = 5
        temp = temp.set_index([nam.well_type], append = True)
        temp = temp.reorder_levels([0,2,1])
        temp.index.names = self.parent.restart.ogsmout_ogenagprd.index.names
        self.parent.restart.ogsmout_ogenagprd.update(temp)

        #NG Production by natural gas category
        temp = self.parent.restart.ogsmout_ogqngrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8], level=0)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.ogsmout_ogqngrep.at[(9, int(self.parent.current_year)), 'value'] = temp['value'].sum()

        #AD NG production by HSM region and natural gas type
        temp = self.parent.restart.ogsmout_ogadgprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=2)]
        temp = temp.groupby(level = 0).sum()
        temp[nam.well_type] = 5
        temp[nam.year] = int(self.parent.current_year)
        temp = temp.set_index([nam.well_type, nam.year], append = True)
        temp.index.names = self.parent.restart.ogsmout_ogadgprd.index.names
        self.parent.restart.ogsmout_ogadgprd.update(temp)

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
        self.parent.restart.ogsmout_ogpngwhp.update(temp)

        #Jobs
        well_value_1 = self.parent.restart.ogsmout_ognowell.at[(int(self.parent.current_year),), nam.value]
        well_value_2 = self.parent.restart.ogsmout_ognowell.at[((int(self.parent.current_year) - 1),), nam.value]
        ogjobs = 152.2320 + (0.006355 * well_value_1) + (0.0086729 * well_value_2)
        self.parent.restart.ogsmout_ogjobs.at[int(self.parent.current_year), 'value'] = ogjobs

        #Electricity consumed by natural gas processing plants during carbon capture
        temp = self.restart.qmore_qngpin.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9,10], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.census_divisions] = 11
        temp = temp.set_index([nam.census_divisions], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.qmore_qngpin.index.names
        self.parent.restart.qmore_qngpin.update(temp)

        #Crude production by LFMM region and LFMM crude type
        temp = self.restart.ogsmout_ogcrdprd.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=[1,2]).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([2, 0, 1])
        temp.index.names = self.restart.ogsmout_ogcrdprd.index.names
        self.restart.ogsmout_ogcrdprd.update(temp)

        #Electricity consumed by natural gas processing plants during carbon capture
        temp = self.restart.ogsmout_ngpco2em.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9,10], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.census_divisions] = 11
        temp = temp.set_index([nam.census_divisions], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ngpco2em.index.names
        self.parent.restart.ogsmout_ngpco2em.update(temp)

        #ogsmout_ogcoprd_nonfed
        temp = self.restart.ogsmout_ogcoprd_nonfed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogcoprd_nonfed.index.names
        self.parent.restart.ogsmout_ogcoprd_nonfed.update(temp)

        #ogsmout_ogcoprd_fed
        temp = self.restart.ogsmout_ogcoprd_fed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogcoprd_fed.index.names
        self.parent.restart.ogsmout_ogcoprd_fed.update(temp)

        # ogsmout_ogngprd_nonfed
        temp = self.restart.ogsmout_ogngprd_nonfed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogngprd_nonfed.index.names
        self.parent.restart.ogsmout_ogngprd_nonfed.update(temp)

        # ogsmout_ogngprd_fed
        temp = self.restart.ogsmout_ogngprd_fed.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.region_number] = 14
        temp = temp.set_index([nam.region_number], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogngprd_fed.index.names
        self.parent.restart.ogsmout_ogngprd_fed.update(temp)

        # ogsmout_ogqcrrep
        temp = self.restart.ogsmout_ogqcrrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5], level=0)]
        temp = temp.groupby(level=1).sum()
        temp[nam.oil_category] = 6
        temp = temp.set_index([nam.oil_category], append=True)
        temp = temp.reorder_levels([1, 0])
        temp.index.names = self.restart.ogsmout_ogqcrrep.index.names
        self.parent.restart.ogsmout_ogqcrrep.update(temp)

        #ogsmout_ogadgprd
        temp_sum_df = self.parent.restart.ogsmout_ogadgprd.copy()
        temp_sum_df = temp_sum_df[temp_sum_df.index.get_level_values('2').isin([1,2,3,4])]
        temp_sum_df = temp_sum_df[temp_sum_df.index.get_level_values('3') == int(self.parent.current_year)]
        temp_sum_df = temp_sum_df.groupby(['1','3']).sum()
        temp_sum_df[nam.gas_type] = 5
        temp_sum_df = temp_sum_df.set_index([nam.gas_type], append = True)
        temp_sum_df = temp_sum_df.reorder_levels([0, 2, 1])
        temp_sum_df.index.names = self.parent.restart.ogsmout_ogadgprd.index.names
        self.parent.restart.ogsmout_ogadgprd.update(temp_sum_df)

        pass


    def apply_steo_fixed_benchmarks(self):
        """Overwrites expected production with STEO values in first STEO year.

        Returns
        -------
        self.parent.restart.ogsmout_ogqshloil : df
            Tight oil production from select tight oil plays

        self.parent.restart.ogsmout_ogqshlgas : df
            Shale natural gas production from select shale natural gas plays

        self.parent.restart.ogsmout_ogngplprd : df
            Total NGPL production by HSM district

        self.parent.restart.ogsmout_ogngplet : df
            Ethane production by HSM district

        self.parent.restart.ogsmout_ogngplpr : df
            Propoane production by HSM district

        self.parent.restart.ogsmout_ogngplbu : df
            Butane production by HSM district

        self.parent.restart.ogsmout_ogngplis : df
            Isobutane production by HSM district

        self.parent.restart.ogsmout_ogngplpp : df
            Pentanes production by HSM district

        """
        ###Set Crude Oil Production levels
        #Domestic Crude Oil Production (MMBBL/D)
        temp_steo_rfqtdcrd = self.parent.hist_steo.steo_rfqtdcrd.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        debug_rfqtdcrd = self.restart.pmmout_rfqtdcrd.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        debug_rfqtdcrd = debug_rfqtdcrd.merge(temp_steo_rfqtdcrd,
                                              how = 'left',
                                              left_index = True,
                                              right_index = True,
                                              suffixes = ['_model', '_steo']).dropna()
        debug_rfqtdcrd['steo_adj_ratio'] = debug_rfqtdcrd['value_model'] / debug_rfqtdcrd['value_steo']
        debug_rfqtdcrd.to_csv(self.parent.output_path + 'steo_debug//' + 'steo_rfqtdcrd_debug_' + str(self.parent.current_year) + '.csv')
        self.restart.pmmout_rfqtdcrd.update(temp_steo_rfqtdcrd)

        #Domestic Crude Oil Production (MMBBL/D) - EOR
        self.restart.pmmout_rfqdcrd.update(temp_steo_rfqtdcrd)

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
        self.restart.pmmout_rfqdcrd.update(temp_rfqdcrd[nam.value])


        ###Benchmark Crude oil production by oil category:, 1 = EOR, 2 = L48, 3 =Offshore, 4 = AK, (5 = ANWR, deprecated)
        onshore_steo = self.parent.restart.pmmout_rfqtdcrd[self.parent.restart.pmmout_rfqtdcrd.index.get_level_values('1').isin([1, 2, 3, 4, 5, 6, 7])].copy()
        onshore_steo = onshore_steo[onshore_steo.index.get_level_values('2').isin([int(self.parent.current_year)])].copy()
        onshore_steo = onshore_steo.values.sum()
        onshore_steo = onshore_steo - (self.parent.restart.ogsmout_ogqcrrep.at[(1, int(self.parent.current_year)), nam.value] / 365) #Remove CO2 EOR from rfqtdcrd to get ogqcrrep(2, history_year)
        self.parent.restart.ogsmout_ogqcrrep.at[(2, int(self.parent.current_year)), nam.value] = onshore_steo * 365

        offshore_steo = self.parent.restart.pmmout_rfqtdcrd[self.parent.restart.pmmout_rfqtdcrd.index.get_level_values('1').isin([8, 9, 10])].copy()
        offshore_steo = offshore_steo[offshore_steo.index.get_level_values('2').isin([int(self.parent.current_year)])].copy()
        offshore_steo = offshore_steo.values.sum()
        self.parent.restart.ogsmout_ogqcrrep.at[(3, int(self.parent.current_year)), nam.value] = offshore_steo * 365

        ak_steo = self.parent.restart.pmmout_rfqtdcrd.at[(14, int(self.parent.current_year)), nam.value]
        self.parent.restart.ogsmout_ogqcrrep.at[(4, int(self.parent.current_year)), nam.value] = ak_steo * 365


        ###Play-level Crude Production (Year 1 value is set, not adjusted)
        if int(self.parent.current_year) == self.parent.steo_years[0]:
            # Play-level Crude Oil Production
            temp_steo_df = self.parent.hist_steo.steo_togqshloil.xs(int(self.parent.current_year), level=1,drop_level=False).copy()
            temp_debug_df = self.parent.restart.ogsmout_ogqshloil.xs(int(self.parent.current_year), level=1, drop_level=False).copy()
            self.parent.restart.ogsmout_ogqshloil.update(temp_steo_df)

            # Debug
            temp_debug_df = temp_debug_df.merge(temp_steo_df,
                                                  how='left',
                                                  left_index=True,
                                                  right_index=True,
                                                  suffixes=['_model', '_steo']).dropna()
            temp_debug_df['steo_adj_ratio'] = temp_debug_df['value_model'] / temp_debug_df['value_steo']
            temp_debug_df.to_csv(self.parent.output_path + 'steo_debug//' + 'steo_ogqshloil_debug_' + str(self.parent.current_year) + '.csv')

        elif int(self.parent.current_year) >= self.parent.steo_years[1]:
            temp_steo_df = self.parent.hist_steo.steo_togqshloil.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
            temp_ogqshloil_df = self.parent.restart.ogsmout_ogqshloil.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
            temp_ogqshloil_df['value'] = (temp_ogqshloil_df['value'] + temp_steo_df['value']).fillna(0.0)
            self.parent.restart.ogsmout_ogqshloil.update(temp_ogqshloil_df)


        ###Production by LFMM Region
        #Calculate Adjustment Factor for STEO
        temp_rfqtdcrd = self.parent.restart.pmmout_rfqtdcrd.copy()
        temp_ogcruderef = self.parent.restart.ogsmout_ogcruderef.copy()

        #Get Onshore Prod
        temp_rfqtdcrd_sum = temp_rfqtdcrd.xs(int(self.parent.current_year), level = 1).copy()
        temp_rfqtdcrd_sum = temp_rfqtdcrd_sum[temp_rfqtdcrd_sum.index.get_level_values(0).isin([1,2,3,4,5,6,7,8,9,10,11,12,13])]
        temp_rfqtdcrd_sum = temp_rfqtdcrd_sum[nam.value].copy().sum()

        #Get Onshore ogcruderef (Other submodule values are set in offshore.py and alaska.py)
        temp_ogcruderef = temp_ogcruderef.xs(int(self.parent.current_year), level = 2)
        temp_ogcruderef = temp_ogcruderef[temp_ogcruderef.index.get_level_values(0).isin([1,2,3,4,5,6,7,8])]
        temp_ogcruderef_sum = temp_ogcruderef[nam.value].sum()
        temp_ogcruderef_sum = temp_ogcruderef_sum / 365

        #Get STEO Adjustment Factor
        steo_adj = temp_rfqtdcrd_sum/temp_ogcruderef_sum

        #Apply Adjustment
        temp_ogcruderef = temp_ogcruderef.mul(steo_adj)

        #Format for Update to Restart Variable
        temp_ogcruderef['2'] = int(self.parent.current_year)
        temp_ogcruderef = temp_ogcruderef.set_index(['2'], append = True)
        temp_ogcruderef.index.names = self.parent.restart.ogsmout_ogcruderef.index.names
        self.parent.restart.ogsmout_ogcruderef.update(temp_ogcruderef)


        ###Crude oil production by lower 48 region
        temp = self.parent.restart.pmmout_rfqtdcrd.xs(int(self.parent.current_year), level=1, drop_level=False).copy()
        temp = temp[temp.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7])].copy()
        self.parent.restart.ogsmout_ogcoprd.update(temp)

        #Switch regions 6 & 7 to match LFMM
        self.parent.restart.ogsmout_ogcoprd.at[(7, int(self.parent.current_year)), 'value'] = temp.at[(6, int(self.parent.current_year)), nam.value]
        self.parent.restart.ogsmout_ogcoprd.at[(6, int(self.parent.current_year)), 'value'] = temp.at[(7, int(self.parent.current_year)), nam.value]

        #Crude oil production by HSM district

        #Regional crude oil and natural gas production by type of production
        for region in list(range(1, 8)):

            if region <= 4:
                self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value']

                self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqtdcrd.at[(region, int(self.parent.current_year)), 'value'] - \
                self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value']

            elif region == 5:
                self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value'] + \
                self.parent.restart.ogsmout_ogqcrrep.at[(1, int(self.parent.current_year)), 'value'] / 365 / 1000

                self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqtdcrd.at[(region, int(self.parent.current_year)), 'value'] - \
                (self.parent.restart.ogsmout_ogqcrrep.at[(1, int(self.parent.current_year)), 'value'] / 365 / 1000) - \
                self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value']

            elif region == 6:
                self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqdcrd.at[(7, int(self.parent.current_year)), 'value']

                self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqtdcrd.at[(7, int(self.parent.current_year)), 'value'] - \
                self.parent.restart.pmmout_rfqdcrd.at[(7, int(self.parent.current_year)), 'value']

            elif region == 7:
                self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqdcrd.at[(6, int(self.parent.current_year)), 'value']

                self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                self.parent.restart.pmmout_rfqtdcrd.at[(6, int(self.parent.current_year)), 'value'] - \
                self.parent.restart.pmmout_rfqdcrd.at[(6, int(self.parent.current_year)), 'value']


        ###Play-Level Natural Gas Production
        if int(self.parent.current_year) == self.parent.steo_years[0]:
            temp_steo_df = self.parent.hist_steo.steo_togqshlgas.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
            temp_debug_df = self.parent.restart.ogsmout_ogqshlgas.xs(int(self.parent.current_year), level=1, drop_level=False).copy()
            self.parent.restart.ogsmout_ogqshlgas.update(temp_steo_df)

            # Debug
            temp_debug_df = temp_debug_df.merge(temp_steo_df,
                                                how='left',
                                                left_index=True,
                                                right_index=True,
                                                suffixes=['_model', '_steo']).dropna()
            temp_debug_df['steo_adj_ratio'] = temp_debug_df['value_model'] / temp_debug_df['value_steo']
            temp_debug_df.to_csv(self.parent.output_path + 'steo_debug//' + 'steo_ogqshlgas_debug_' + str(self.parent.current_year) + '.csv')

        elif int(self.parent.current_year) >= self.parent.steo_years[1]:
            temp_steo_df = self.parent.hist_steo.steo_togqshlgas.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
            temp_ogqshlgas_df = self.parent.restart.ogsmout_ogqshlgas.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
            temp_ogqshlgas_df['value'] = (temp_ogqshlgas_df['value'] + temp_steo_df['value']).fillna(0.0)
            self.parent.restart.ogsmout_ogqshlgas.update(temp_ogqshlgas_df)


        ###Average Wellhead Price (nominal $/brl)
        self.restart.pmmout_dcrdwhp.update(self.parent.hist_steo.steo_dcrdwhp)


        ### NGPLs
        #NGPL Production (MBBL/D)
        temp_df = self.parent.hist_steo.steo_ogngplprd.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        self.parent.restart.ogsmout_ogngplprd.update(temp_df)

        #Ethane Production (MBBL/D)
        temp_df = self.parent.hist_steo.steo_ogngplet.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        self.parent.restart.ogsmout_ogngplet.update(temp_df)

        #Propane Production (MBBL/D)
        temp_df = self.parent.hist_steo.steo_ogngplpr.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        self.parent.restart.ogsmout_ogngplpr.update(temp_df)

        #Butane Production (MBBL/D)
        temp_df = self.parent.hist_steo.steo_ogngplbu.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        self.parent.restart.ogsmout_ogngplbu.update(temp_df)

        #Isobutane Production (MBBL/D)
        temp_df = self.parent.hist_steo.steo_ogngplis.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()
        self.parent.restart.ogsmout_ogngplis.update(temp_df)

        #Pentanes Plus Production (MBBL/D)
        temp_df = self.parent.hist_steo.steo_ogngplpp.xs(int(self.parent.current_year), level=1, drop_level=False).copy()
        self.parent.restart.ogsmout_ogngplpp.update(temp_df)

        pass


    def apply_steo_adjustments(self):
        """Applies STEO adjustments to smooth transition from STEO forecasts to HSM projections over STEO years and freeze side cases.

            * Oil production is benched to rfqtdcrd
            * Natural gas production is benched to ogenagprd

        Returns
        -------
        self.parent.restart.pmmout_rfqtdcrd : df
            Total crude production by region and type (including EOR)

        self.parent.restart.ogsmout_ogqcrrep : df
            Crude oil production by oil category

        self.parent.restart.ogsmout_ogqshloil : df
            Tight oil production from select tight oil plays

        self.parent.restart.ogsmout_ogcruderef : df
            Crude oil production by LFMM crude oil type and region

        self.parent.restart.ogsmout_ogcoprd : df
            Crude oil production by lower 48 region

        self.parent.restart.pmmout_rfqdcrd : df
            Total crude oil production by HSM region (not including EOR)

        self.parent.restart.ogsmout_ogenagprd : df
            Expected non-associated natural gas production by HSM district and type

        self.parent.restart.ogsmout_ogrnagprd : df
            Natural gas realized production by natural gas type and HSM district

        self.parent.restart.ogsmout_ogadgprd : df
            Associated-dissolved natural gas production by HSM district and type

        self.parent.restart.ogsmout_ogngprd : df
            Natural gas production by type

        self.parent.restart.ogsmout_ogngplprd : df
            Total NGPL production by HSM district

        self.parent.restart.ogsmout_ogngplet : df
            Ethane production by HSM district

        self.parent.restart.ogsmout_ogngplpr : df
            Propoane production by HSM district

        self.parent.restart.ogsmout_ogngplbu : df
            Butane production by HSM district

        self.parent.restart.ogsmout_ogngplis : df
            Isobutane production by HSM district

        self.parent.restart.ogsmout_ogngplpp : df
            Pentanes production by HSM district
        """
        ###Crude Production Adjustments
        #Crude production by region (including EOR)
        temp_steo_rfqtdcrd = self.parent.hist_steo.steo_o_rfqtdcrd.xs(int(self.parent.current_year), level = 1, drop_level = False).copy()

        temp_df = self.parent.restart.pmmout_rfqtdcrd.copy()
        temp_df = temp_df.merge(temp_steo_rfqtdcrd['value'], how = 'right', left_index = True, right_index = True, suffixes=('', '_adjustment'))
        temp_df['value'] = temp_df['value'] + temp_df['value_adjustment']
        self.parent.restart.pmmout_rfqtdcrd.update(temp_df)


        if all(i <= 0 for i in temp_df['value'].tolist()): #Don't run if no adjustments
            pass
        else:
            
            ###Benchmark Crude oil production by oil category:, 1 = EOR, 2 = L48, 3 =Offshore, 4 = AK, (5 = ANWR, deprecated)
            onshore_steo = self.parent.restart.pmmout_rfqtdcrd[self.parent.restart.pmmout_rfqtdcrd.index.get_level_values('1').isin([1, 2, 3, 4, 5, 6, 7])].copy()
            onshore_steo = onshore_steo[onshore_steo.index.get_level_values('2').isin([int(self.parent.current_year)])].copy()
            onshore_steo = onshore_steo.values.sum()
            onshore_steo = onshore_steo - (self.parent.restart.ogsmout_ogqcrrep.at[(1, int(self.parent.current_year)), nam.value] / 365) #Remove CO2 EOR from rfqtdcrd to get ogqcrrep(2, history_year)
            self.parent.restart.ogsmout_ogqcrrep.at[(2, int(self.parent.current_year)), nam.value] = onshore_steo * 365

            offshore_steo = self.parent.restart.pmmout_rfqtdcrd[self.parent.restart.pmmout_rfqtdcrd.index.get_level_values('1').isin([8, 9, 10])].copy()
            offshore_steo = offshore_steo[offshore_steo.index.get_level_values('2').isin([int(self.parent.current_year)])].copy()
            offshore_steo = offshore_steo.values.sum()
            self.parent.restart.ogsmout_ogqcrrep.at[(3, int(self.parent.current_year)), nam.value] = offshore_steo * 365

            ak_steo = self.parent.restart.pmmout_rfqtdcrd.at[(14, int(self.parent.current_year)), nam.value]
            self.parent.restart.ogsmout_ogqcrrep.at[(4, int(self.parent.current_year)), nam.value] = ak_steo * 365


            ###Production by LFMM Region
            #Calculate Adjustment Factor for STEO
            temp_rfqtdcrd = self.parent.restart.pmmout_rfqtdcrd.copy()
            temp_ogcruderef = self.parent.restart.ogsmout_ogcruderef.copy()

            #Get Onshore Prod
            temp_rfqtdcrd_sum = temp_rfqtdcrd.xs(int(self.parent.current_year), level = 1).copy()
            temp_rfqtdcrd_sum = temp_rfqtdcrd_sum[temp_rfqtdcrd_sum.index.get_level_values(0).isin([1,2,3,4,5,6,7,8,9,10,11,12,13])]
            temp_rfqtdcrd_sum = temp_rfqtdcrd_sum[nam.value].copy().sum()

            #Get Onshore ogcruderef (Other submodule values are set in offshore.py and alaska.py)
            temp_ogcruderef = temp_ogcruderef.xs(int(self.parent.current_year), level = 2)
            temp_ogcruderef = temp_ogcruderef[temp_ogcruderef.index.get_level_values(0).isin([1,2,3,4,5,6,7,8])]
            temp_ogcruderef_sum = temp_ogcruderef[nam.value].sum()
            temp_ogcruderef_sum = temp_ogcruderef_sum / 365

            #Get STEO Adjustment Factor
            steo_adj = temp_rfqtdcrd_sum/temp_ogcruderef_sum

            #Apply Adjustment
            temp_ogcruderef = temp_ogcruderef.mul(steo_adj)

            #Format for Update to Restart Variable
            temp_ogcruderef['2'] = int(self.parent.current_year)
            temp_ogcruderef = temp_ogcruderef.set_index(['2'], append = True)
            temp_ogcruderef.index.names = self.parent.restart.ogsmout_ogcruderef.index.names
            self.parent.restart.ogsmout_ogcruderef.update(temp_ogcruderef)


            ###Crude oil production by lower 48 region
            temp = self.parent.restart.pmmout_rfqtdcrd.xs(int(self.parent.current_year), level=1, drop_level=False).copy()
            temp = temp[temp.index.get_level_values(0).isin([1, 2, 3, 4, 5, 6, 7])].copy()
            self.parent.restart.ogsmout_ogcoprd.update(temp)

            #Switch regions 6 & 7 to match LFMM
            self.parent.restart.ogsmout_ogcoprd.at[(7, int(self.parent.current_year)), 'value'] = temp.at[(6, int(self.parent.current_year)), nam.value]
            self.parent.restart.ogsmout_ogcoprd.at[(6, int(self.parent.current_year)), 'value'] = temp.at[(7, int(self.parent.current_year)), nam.value]

            #Crude oil production by HSM district

            #Regional crude oil and natural gas production by type of production
            for region in list(range(1, 8)):

                if region <= 4:
                    self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value']

                    self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqtdcrd.at[(region, int(self.parent.current_year)), 'value'] - \
                    self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value']

                elif region == 5:
                    self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value'] + \
                    self.parent.restart.ogsmout_ogqcrrep.at[(1, int(self.parent.current_year)), 'value'] / 365 / 1000

                    self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqtdcrd.at[(region, int(self.parent.current_year)), 'value'] - \
                    (self.parent.restart.ogsmout_ogqcrrep.at[(1, int(self.parent.current_year)), 'value'] / 365 / 1000) - \
                    self.parent.restart.pmmout_rfqdcrd.at[(region, int(self.parent.current_year)), 'value']

                elif region == 6:
                    self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqdcrd.at[(7, int(self.parent.current_year)), 'value']

                    self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqtdcrd.at[(7, int(self.parent.current_year)), 'value'] - \
                    self.parent.restart.pmmout_rfqdcrd.at[(7, int(self.parent.current_year)), 'value']

                elif region == 7:
                    self.parent.restart.ogsmout_ogregprd.at[(region, 1, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqdcrd.at[(6, int(self.parent.current_year)), 'value']

                    self.parent.restart.ogsmout_ogregprd.at[(region, 2, int(self.parent.current_year)), nam.value] = \
                    self.parent.restart.pmmout_rfqtdcrd.at[(6, int(self.parent.current_year)), 'value'] - \
                    self.parent.restart.pmmout_rfqdcrd.at[(6, int(self.parent.current_year)), 'value']

                ###NA Natural Gas Production
                if int(self.parent.current_year) in list(
                        self.parent.hist_steo.steo_o_na_ng.columns):  # Natural gas adjustment only occurs for current history year and first STEO year
                    # Get current year STEO adjustment values
                    temp_steo_o_na_ng = self.parent.hist_steo.steo_o_na_ng[[int(self.parent.current_year)]].copy()

                    # Write Sum to Restart variable
                    sum_rest_df = temp_steo_o_na_ng.copy()
                    sum_rest_df[nam.well_type] = 5
                    sum_rest_df[nam.year] = int(self.parent.current_year)
                    sum_rest_df = sum_rest_df.set_index([nam.well_type, nam.year], append=True)
                    sum_rest_df.index.names = self.parent.restart.ogsmout_ogenagprd.index.names
                    sum_rest_df.columns = [nam.value]
                    self.parent.restart.ogsmout_ogenagprd.update(sum_rest_df)

                    # Rename Columns
                    temp_steo_o_na_ng.columns = ['steo_adj']

                    # Get temp restart variable for current year
                    temp_df = self.parent.restart.ogsmout_ogenagprd.copy()
                    temp_df = temp_df.xs(int(self.parent.current_year), level=2)

                    # Fill total level 5 since aggregation happens at the end of the HSM cycle
                    temp_df = temp_df.reset_index()
                    temp_df = temp_df[temp_df['2'].isin([1, 2, 3, 4])]

                    # Get Sum df
                    sum_df = temp_df.groupby('1').sum().drop('2', axis=1)

                    # Merge total sum and district-level production dfs
                    temp_df = temp_df.reset_index(drop=False)
                    temp_df = temp_df.merge(temp_steo_o_na_ng, how='left', left_on='1', right_index=True)
                    temp_df = temp_df.merge(sum_df, how='left', left_on='1', right_index=True, suffixes=('', '_sum'))

                    # Calculate STEO-adjusted value as a factor of STEO production and calculated production and write to debug
                    temp_df['value'] = temp_df['steo_adj'] * (temp_df['value'] / temp_df['value_sum'])
                    temp_df = temp_df.replace([np.inf, -np.inf, np.nan], 0.0)
                    adj_factor_df = temp_df.copy()
                    adj_factor_df = adj_factor_df.rename(columns={'1': nam.district_number, '2': nam.gas_type})
                    adj_factor_df[nam.year] = int(self.parent.current_year)
                    adj_factor_df = adj_factor_df.set_index([nam.district_number, nam.gas_type, nam.year])
                    adj_factor_df['adj_factor'] = (adj_factor_df['value_sum'] / adj_factor_df['steo_adj']).fillna(
                        1.0)  # For debugging
                    if self.parent.debug_switch == 1:
                        adj_factor_df.to_csv(
                            self.parent.output_path + 'steo_debug/' + 'hsm_steo_nagas_adjustment_factor_' + str(
                                int(self.parent.current_year)) + '.csv')

                    # Prepare write to restart file
                    temp_df['3'] = int(self.parent.current_year)
                    temp_df = temp_df.set_index(['1', '2', '3'])
                    temp_df = temp_df[['value']]

                    # Write to restart file
                    self.parent.restart.ogsmout_ogenagprd.update(temp_df)
                    self.parent.restart.ogsmout_ogrnagprd.update(temp_df)


                else:
                    pass

                ###AD Natural Gas Production
                if int(self.parent.current_year) in list(
                        self.parent.hist_steo.steo_o_ad_ng.columns):  # Natural gas adjustment only occurs for current history year and first STEO year
                    # Get current year STEO adjustment values
                    temp_steo_o_ad_ng = self.parent.hist_steo.steo_o_ad_ng[[int(self.parent.current_year)]].copy()

                    # Write Sum to Restart variable
                    sum_rest_df = temp_steo_o_ad_ng.copy()
                    sum_rest_df[nam.well_type] = 5
                    sum_rest_df[nam.year] = int(self.parent.current_year)
                    sum_rest_df = sum_rest_df.set_index([nam.well_type, nam.year], append=True)
                    sum_rest_df.index.names = self.parent.restart.ogsmout_ogenagprd.index.names
                    sum_rest_df.columns = [nam.value]
                    self.parent.restart.ogsmout_ogadgprd.update(sum_rest_df)

                    # Rename Columns
                    temp_steo_o_ad_ng.columns = ['steo_adj']

                    # Get temp restart variable for current year
                    temp_df = self.parent.restart.ogsmout_ogadgprd.copy()
                    temp_df = temp_df.xs(int(self.parent.current_year), level=2)

                    # Fill total level 5 since aggregation normally happens at the end of the HSM cycle
                    temp_df = temp_df.reset_index()
                    temp_df = temp_df[temp_df['2'].isin([1, 2, 3, 4])]

                    # Get Sum df
                    sum_df = temp_df.groupby('1').sum().drop('2', axis=1)

                    # Merge total sum and district production dfs
                    temp_df = temp_df.merge(temp_steo_o_ad_ng, how='left', left_on='1', right_index=True)
                    temp_df = temp_df.merge(sum_df, how='left', left_on='1', right_index=True, suffixes=('', '_sum'))

                    # Calculate adjusted value as a factor of STEO prodtion and calculated production and write to restart file
                    temp_df['value'] = temp_df['steo_adj'] * (temp_df['value'] / temp_df['value_sum']).fillna(0.0)
                    adj_factor_df = temp_df.copy()
                    adj_factor_df = adj_factor_df.rename(columns={'1': nam.district_number, '2': nam.gas_type})
                    adj_factor_df[nam.year] = int(self.parent.current_year)
                    adj_factor_df = adj_factor_df.set_index([nam.district_number, nam.gas_type, nam.year])
                    adj_factor_df = (adj_factor_df['value_sum'] / adj_factor_df['steo_adj']).fillna(
                        1.0)  # For debugging
                    if self.parent.debug_switch == 1:
                        adj_factor_df.to_csv(
                            self.parent.output_path + 'steo_debug/' + 'hsm_steo_adgas_adjustment_factor_' + str(
                                int(self.parent.current_year)) + '.csv')

                    # Write to restart file
                    temp_df['3'] = int(self.parent.current_year)
                    temp_df = temp_df.set_index(['1', '2', '3'])
                    temp_df = temp_df[['value']]
                    self.parent.restart.ogsmout_ogadgprd.update(temp_df)
                else:
                    pass

        ###Set Natural gas production by natural gas type to OGENAGPRD and OGADGPRD for STEO years
        #Get temp NA Prod
        temp_na = self.parent.restart.ogsmout_ogenagprd.xs(5, level = 1, drop_level= True).copy()
        temp_na = temp_na.xs(int(self.parent.current_year), level = 1, drop_level = True)

        #Get temp AD Prod
        temp_ad = self.parent.restart.ogsmout_ogadgprd.xs(5, level = 1, drop_level= True).copy()
        temp_ad = temp_ad.xs(int(self.parent.current_year), level = 1, drop_level = True)

        #Get total gas prod by region
        temp_total = temp_na + temp_ad
        temp_total = temp_total.merge(self.parent.mapping[[nam.district_number, nam.region_number]],
                                      how = 'left',
                                      left_index = True,
                                      right_on = nam.district_number)
        temp_total = temp_total.groupby([nam.region_number]).sum()

        #Drop Alaska rows and district column
        temp_total = temp_total[[nam.value]]
        temp_total = temp_total.loc[temp_total.index <= 10]

        #Prepare Index for merge
        temp_total[nam.year] = int(self.parent.current_year)
        temp_total = temp_total.set_index([nam.year], append = True)
        temp_total.index.names = self.parent.restart.ogsmout_ogngprd.index.names

        #Set OGNPRD equal to NA Prod + AD Prod
        self.parent.restart.ogsmout_ogngprd.update(temp_total)

        #Get L48 Sum total
        temp_df = self.parent.restart.ogsmout_ogngprd.loc[self.parent.restart.ogsmout_ogngprd.index.get_level_values(0).isin([1,2,3,4,5,6,7])].copy()
        temp_df = temp_df.xs(int(self.parent.current_year), level = 1)
        temp_sum = temp_df[nam.value].sum()
        self.parent.restart.ogsmout_ogngprd.at[(11, int(self.parent.current_year)), nam.value] = temp_sum
       
        
        ###NGPL Production Adjustments
        def calculate_ngpl_adjustment(ngpl_type, restart_variable):
            #Get temp ngpl adjustment df
            temp_ngpl_adj = self.parent.hist_steo.steo_o_ngpls.at[ngpl_type, int(self.parent.current_year)]

            #Get temp ngpl_df
            temp_df = restart_variable.copy()
            temp_df = temp_df.xs(int(self.parent.current_year), level = 1, drop_level = False)

            #Get NGPL sum df
            sum_ngpls = temp_df['value'].sum()

            #Merge adjustment value and sum production
            temp_df['temp_ngpl_adj'] = temp_ngpl_adj
            temp_df['value_sum'] = sum_ngpls

            #Calculate district NGPL adjustment based on proportion of sum of NGPLs
            temp_df['value'] = temp_df['value'] * (temp_df['temp_ngpl_adj'] / temp_df['value_sum'])
            temp_df = temp_df[['value']]
            restart_variable.update(temp_df)

            return restart_variable


        #Total NGPL Production
        self.parent.restart.ogsmout_ogngplprd = calculate_ngpl_adjustment(nam.total_ngpls, self.parent.restart.ogsmout_ogngplprd)

        #Ethane
        self.parent.restart.ogsmout_ogngplet = calculate_ngpl_adjustment(nam.ethane, self.parent.restart.ogsmout_ogngplet)

        #Propane
        self.parent.restart.ogsmout_ogngplpr = calculate_ngpl_adjustment(nam.propane, self.parent.restart.ogsmout_ogngplpr)

        #Butane
        self.parent.restart.ogsmout_ogngplbu = calculate_ngpl_adjustment(nam.butane, self.parent.restart.ogsmout_ogngplbu)

        #Isobutane
        self.parent.restart.ogsmout_ogngplis = calculate_ngpl_adjustment(nam.isobutane, self.parent.restart.ogsmout_ogngplis)

        #Pentanes Plus
        self.parent.restart.ogsmout_ogngplpp = calculate_ngpl_adjustment(nam.pentanes_plus, self.parent.restart.ogsmout_ogngplpp)

        pass


    def apply_ngmm_realized_prod(self):
        """Apply NGMM realized production to HSM.

            * Comparison of realized production from NGMM (ogrnagprd) and expected production produced by HSM (ogenagprd)
            * Check to determine which HSM districts have sufficient expected shale natgas production to be redistributed
              against differences in realized production
            * HSM districts which have sufficient shale natgas production have shale production changed to match realized demand from NGMM
            * Those which do not have realized demand from NGMM distributed across all fuel types
            * These changes are then written to the restart file

        Returns
        -------
        self.parent.restart.ogsmout_ogrnagprd : df
            Natural gas realized production by natural gas type and HSM district
        """
        ###Get local natural gas production tables and mask for year
        #Get temp production dfs
        expected_prod_df = self.parent.restart.ogsmout_ogenagprd.copy()
        realized_prod_df = self.parent.rest_ogrnagprd.copy()

        #Mask for current model year
        expected_prod_df = expected_prod_df[expected_prod_df.index.get_level_values(2) >= (self.parent.history_year + 1)]
        realized_prod_df = realized_prod_df[realized_prod_df.index.get_level_values(2) >= (self.parent.history_year + 1)]

        #Get total district-level production
        e_total_prod_df = expected_prod_df.xs(5, level=1, axis=0).copy()
        r_total_prod_df = realized_prod_df.xs(5, level=1, axis=0).copy()

        #Compare realized prod to expected prod
        prod_check = realized_prod_df.copy() - expected_prod_df.copy()
        prod_check_sum = prod_check.xs(5, level=1, axis=0).copy()
        ###Update realized production by manipulating shale gas volumes where shale volumes are less than the difference between expected and realized production
        #Get just shale realized production for adjustments
        temp_e_shale_prod_df = expected_prod_df.xs(3, level=1, axis=0).copy()

        #Mask for when expected shale production + realized_production > expected production so that realized shale production is not set to a negative value
        prod_check_sum['value'] = prod_check_sum['value'].copy() + temp_e_shale_prod_df['value'].copy()
        prod_check_mask = prod_check_sum['value'] >= 0 #Check to make sure shale prod won't be negative
        shale_prod_mask = temp_e_shale_prod_df['value'] > 0.1 #Set to 0.1 so we're not adjusting for negligible values
        shale_adj_mask = prod_check_mask & shale_prod_mask

        #Create reindexed realized production df to apply prod_check masking
        nom_index = temp_e_shale_prod_df[shale_adj_mask].copy()
        nom_index = nom_index.index #get masked index

        temp_e_prod_df = expected_prod_df.copy()
        temp_e_prod_df = temp_e_prod_df.reset_index(drop=False).set_index(['1', '3']) #Drop gas type index so we can match to the masked index
        temp_e_prod_df = temp_e_prod_df.loc[nom_index]
        temp_e_prod_df = temp_e_prod_df.set_index(['2'], append = True) #reapply index
        temp_e_prod_df = temp_e_prod_df.reorder_levels([0, 2, 1]) #reorder levels

        #Apply shale adjustments (set temp_r_shale_adjustments equal to the difference between expected and realized production if possible, if not, ratio)
        shale_adj_df = temp_e_shale_prod_df[shale_adj_mask].copy()
        shale_adj_df['value'] = shale_adj_df['value'] + r_total_prod_df['value'] - e_total_prod_df['value']
        shale_adj_df['2'] = 3
        shale_adj_df = shale_adj_df.set_index(['2'], append = True)
        shale_adj_df = shale_adj_df.reorder_levels([0, 2, 1])

        #Update realized production using shale gas adjustments
        realized_prod_df.update(temp_e_prod_df)
        realized_prod_df.update(shale_adj_df)


        ###Update realized production based on ratio where shale volumes are less than the difference between expected and realized production
        ratio_index = temp_e_shale_prod_df[~shale_adj_mask].copy()
        ratio_index = ratio_index.index

        #Create merged dataframe of expected production by gastype/district and expected production by district aggregated
        temp_merged_prod = expected_prod_df.copy()
        temp_merged_prod = temp_merged_prod.reset_index(drop = False).set_index(['1', '3'])
        temp_merged_prod = temp_merged_prod.merge(e_total_prod_df, how = 'left', left_index = True, right_index = True, suffixes=('', '_total_expected'))
        temp_merged_prod = temp_merged_prod.merge(r_total_prod_df, how = 'left', left_index = True, right_index = True, suffixes=('', '_total_realized'))
        temp_merged_prod = temp_merged_prod.loc[ratio_index]
        temp_merged_prod = temp_merged_prod.set_index(['2'], append = True)
        temp_merged_prod = temp_merged_prod.reorder_levels([0, 2, 1])

        #Produce ratio of production by gas_type/district to production by district
        temp_merged_prod['e_prod_ratio'] = (temp_merged_prod['value'] / temp_merged_prod['value_total_expected'])

        #Fix nans and infinity values (set shale and total ratios = 1.0, everything else to 0)
        temp_shale_sum_df = temp_merged_prod[temp_merged_prod.index.get_level_values('2').isin([2, 5])].copy()
        temp_shale_sum_df = temp_shale_sum_df.fillna(1.0)
        temp_shale_sum_df = temp_shale_sum_df.replace([np.inf, -np.inf], 1.0)
        temp_merged_prod.update(temp_shale_sum_df)

        temp_other_df = temp_merged_prod[temp_merged_prod.index.get_level_values('2').isin([2, 5])].copy()
        temp_other_df = temp_other_df.fillna(0.0)
        temp_other_df = temp_other_df.replace([np.inf, -np.inf], 0.0)
        temp_merged_prod.update(temp_other_df)

        #Apply expected production ratio to realized production where shale production < the difference between realized and expected production
        temp_merged_prod['value'] = temp_merged_prod['value_total_realized'].mul(temp_merged_prod['e_prod_ratio'])

        #Update temp realized production
        realized_prod_df.update(temp_merged_prod)


        ###Set gas type 5 equal to sum(1:4)
        temp_sum_df = realized_prod_df.copy()
        temp_sum_df = temp_sum_df[temp_sum_df.index.get_level_values('2').isin([1,2,3,4])]
        temp_sum_df = temp_sum_df[temp_sum_df.index.get_level_values('3') == int(self.parent.current_year)]
        temp_sum_df = temp_sum_df.groupby(['1','3']).sum()
        temp_sum_df[nam.gas_type] = 5
        temp_sum_df = temp_sum_df.set_index([nam.gas_type], append = True)
        temp_sum_df = temp_sum_df.reorder_levels([0, 2, 1])
        temp_sum_df.index.names = realized_prod_df.index.names

        #Update temp realized production
        realized_prod_df.update(temp_sum_df)

        ###Update the Restart Variable
        self.parent.restart.ogsmout_ogrnagprd.update(realized_prod_df)

        pass


    def adjust_vars_for_year_one_prod(self):
        """Adjust Restart variables so that they reflect STEO year one input production.

        Returns
        -------
        self.parent.restart.ogsmout_ogqngrep : df
            Natural gas production by natural gas type

        self.parent.restart.ogsmout_ogngprd : df
            Natural gas production by type

        self.parent.restart.ogsmout_ogdngprd : df
            Dry natural gas production by state/district and gas type


        """
        # Create Realized Production temp table and mask for years
        on_na_prod_df = self.parent.restart.ogsmout_ogenagprd.copy()
        on_na_prod_df = on_na_prod_df[
            on_na_prod_df.index.get_level_values(2) > self.parent.history_year]
        on_na_prod_df = on_na_prod_df[
            on_na_prod_df.index.get_level_values(0) <= 66]  # Take out offshore
        on_na_prod_df = on_na_prod_df[on_na_prod_df.index.get_level_values(0) != 3]  # Take out Alaska

        on_ad_prod_df = self.parent.restart.ogsmout_ogadgprd.copy()
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(2) > self.parent.history_year]
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(0) <= 66]  # Take out offshore
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(0) != 3]  # Take out Alaska


        ### Realized Production types 1-4
        ###Get temp dfs for expected prod, realized prod, wells, natgas prod, and projects
        #Realized Prod
        temp_realized_prod = self.restart.ogsmout_ogrnagprd.copy()
        temp_realized_prod = temp_realized_prod.xs(int(self.parent.current_year), level = 2)
        temp_realized_prod = temp_realized_prod.xs(5, level = 1)

        #Expected Prod
        temp_expected_prod = self.restart.ogsmout_ogenagprd.copy()
        temp_expected_prod = temp_expected_prod.xs(int(self.parent.current_year), level=2)
        temp_expected_prod = temp_expected_prod.xs(5, level = 1)

        #Get realized_production ratios
        temp_prod_ratio = temp_realized_prod.div(temp_expected_prod, axis = 0)
        temp_prod_ratio = temp_prod_ratio.replace([np.inf, -np.inf, np.nan], 1.0)
        temp_prod_ratio.columns = [nam.realized_prod_ratio]

        temp_re = self.restart.ogsmout_ogenagprd.loc[self.restart.ogsmout_ogenagprd.index.get_level_values(1).isin([1,2,3,4])].copy()
        temp_re = temp_re.loc[temp_re.index.get_level_values(2) == int(self.parent.current_year)].copy()
        temp_re = temp_re.reset_index()
        temp_re = temp_re.merge(temp_prod_ratio,
                                how = 'left',
                                left_on = '1',
                                right_index = True)
        temp_re['value'] = temp_re['value'] * temp_re['realized_prod_ratio']
        temp_re = temp_re.drop(['realized_prod_ratio'], axis = 1)
        temp_re = temp_re.set_index(['1','2','3'])
        self.restart.ogsmout_ogrnagprd.update(temp_re)


        ###Natural Gas Production by Category
        # Adjust onshore Shale Production plus gas production from tight oil plays to match realized prod
        temp_realized_shale = on_na_prod_df.loc[on_na_prod_df.index.get_level_values(1) == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby(level=[2]).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1) == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index > self.parent.history_year]

        temp_realized_shale['value'] = temp_realized_shale['value'].values + temp_ad_prod['value'].values
        temp_realized_shale[nam.gas_type] = 1
        temp_realized_shale = temp_realized_shale.loc[temp_realized_shale.index == self.parent.current_year]
        temp_realized_shale = (temp_realized_shale.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_shale.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_shale)

        # Adjust coalbed methane production to match realized prod
        temp_realized_cbm = on_na_prod_df.loc[on_na_prod_df.index.get_level_values(1) == 4].copy()
        temp_realized_cbm = temp_realized_cbm.groupby(level=[2]).sum()
        temp_realized_cbm[nam.gas_type] = 2
        temp_realized_cbm = temp_realized_cbm.loc[temp_realized_cbm.index == self.parent.current_year]
        temp_realized_cbm = (temp_realized_cbm.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_cbm.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_cbm)

        # Adjust tight production to match realized prod
        temp_realized_tight = on_na_prod_df.loc[on_na_prod_df.index.get_level_values(1) == 2].copy()
        temp_realized_tight = temp_realized_tight.groupby(level=[2]).sum()
        temp_realized_tight[nam.gas_type] = 3
        temp_realized_tight = temp_realized_tight.loc[temp_realized_tight.index == self.parent.current_year]
        temp_realized_tight = (temp_realized_tight.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_tight.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_tight)

        # Adjust conventional Production to match realized prod
        temp_realized_conv = on_na_prod_df.loc[on_na_prod_df.index.get_level_values(1) == 1].copy()
        temp_realized_conv = temp_realized_conv.groupby(level=[2]).sum()
        temp_realized_conv[nam.gas_type] = 4
        temp_realized_conv = temp_realized_conv.loc[temp_realized_conv.index == self.parent.current_year]
        temp_realized_conv = (temp_realized_conv.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_conv.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_conv)

        # Adjust other AD natgas Production to match realized prod
        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1) == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index > self.parent.history_year]
        temp_ad_prod[nam.gas_type] = 5
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index == self.parent.current_year]
        temp_ad_prod = (temp_ad_prod.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_ad_prod.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_ad_prod)

        ###Offshore Realized Production Adjustment
        # Since NA and AD natgas aren't split up, we need to produce a production ratio here
        off_realized_prod_df = self.parent.restart.ogsmout_ogrnagprd.copy()
        off_realized_prod_df = off_realized_prod_df[
            off_realized_prod_df.index.get_level_values(2) > self.parent.history_year]
        off_realized_prod_df = off_realized_prod_df[
            off_realized_prod_df.index.get_level_values(0) >= 67]  # Take out onshore
        off_realized_prod_df = off_realized_prod_df[
            off_realized_prod_df.index.get_level_values(0) != 75]  # Take out alaska state offshore
        off_realized_prod_df = off_realized_prod_df[
            off_realized_prod_df.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        off_ad_prod_df = self.parent.restart.ogsmout_ogadgprd.copy()
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(2) > self.parent.history_year]
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) >= 67]  # Take out onshore
        off_ad_prod_df = off_ad_prod_df[
            off_ad_prod_df.index.get_level_values(0) != 75]  # Take out alaska state offshore
        off_ad_prod_df = off_ad_prod_df[
            off_ad_prod_df.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        # Adjust NA offshore production to match realized prod
        temp_realized_na_prod_off = off_realized_prod_df.loc[off_realized_prod_df.index.get_level_values(1) == 5].copy()
        temp_realized_na_prod_off = temp_realized_na_prod_off.groupby(level=[2]).sum()
        temp_realized_na_prod_off[nam.gas_type] = 6
        temp_realized_na_prod_off = temp_realized_na_prod_off.loc[
            temp_realized_na_prod_off.index == self.parent.current_year]
        temp_realized_na_prod_off = (temp_realized_na_prod_off.set_index([nam.gas_type], append=True)).reorder_levels(
            [1, 0])
        temp_realized_na_prod_off.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_na_prod_off[[nam.value]])

        # Adjust AD offshore production to match realized prod
        temp_ad_prod = off_ad_prod_df.loc[off_ad_prod_df.index.get_level_values(1) == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index > self.parent.history_year]
        temp_ad_prod[nam.gas_type] = 7
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index == self.parent.current_year]
        temp_ad_prod = (temp_ad_prod.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_ad_prod.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_ad_prod)

        ###Aggregate NG production by category
        temp = self.parent.restart.ogsmout_ogqngrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8], level=0)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.ogsmout_ogqngrep.at[(9, int(self.parent.current_year)), 'value'] = temp['value'].sum()

        ###Adjust total natgas production by Region to match realized prod
        # Non-Associated
        temp_na = self.parent.restart.ogsmout_ogrnagprd.copy()
        temp_na = temp_na.xs(5, level=1, axis=0)
        temp_na = temp_na.xs(int(self.parent.current_year), level=1, axis=0)
        temp_na = pd.merge(temp_na,
                           self.parent.mapping[[nam.district_number, nam.region_number]],
                           how='left',
                           left_index=True, right_on=nam.district_number)
        temp_na = temp_na[[nam.region_number, nam.value]].groupby(nam.region_number).sum()

        # Associated Dissolved
        temp_ad = self.parent.restart.ogsmout_ogadgprd.copy()
        temp_ad = temp_ad.xs(5, level=1, axis=0)
        temp_ad = temp_ad.xs(int(self.parent.current_year), level=1, axis=0)
        temp_ad = pd.merge(temp_ad,
                           self.parent.mapping[[nam.district_number, nam.region_number]],
                           how='left',
                           left_index=True, right_on=nam.district_number)
        temp_ad = temp_ad[[nam.region_number, nam.value]].groupby(nam.region_number).sum()

        # Combine NA and AD gas
        temp_ng = temp_na + temp_ad

        # Get total
        ng_total = temp_ng[nam.value].sum()
        temp_ng = temp_ng.loc[list(range(1, 11))]
        temp_ng.loc[11] = ng_total

        # Fix Offshore which is misaligned from restart
        temp_ng.at[(8, nam.value)] = temp_ng.at[(9, nam.value)]
        temp_ng.at[(9, nam.value)] = temp_ng.at[(10, nam.value)]
        temp_ng.at[(10, nam.value)] = 0

        # Format for Restart File
        temp_ng[nam.year] = int(self.parent.current_year)
        temp_ng = temp_ng.set_index([nam.year], append=True)
        temp_ng.index.names = self.parent.restart.ogsmout_ogngprd.index.names

        # Assign to Restart Variable
        self.parent.restart.ogsmout_ogngprd.update(temp_ng)

        ###Adjust dry natural gas production by HSM district and natgas type to match realized prod
        ###Total natgas production
        # Non-Associated
        temp_na = self.parent.restart.ogsmout_ogrnagprd.copy()
        temp_na = temp_na.xs(int(self.parent.current_year), level=2, axis=0)

        # Get Type tables
        temp_na_conv = temp_na.xs(1, level=1, drop_level=False).copy()
        temp_na_shale = temp_na.xs(2, level=1, drop_level=False).copy()
        temp_na_tight = temp_na.xs(3, level=1, drop_level=False).copy()
        temp_na_cbm = temp_na.xs(4, level=1, drop_level=False).copy()
        temp_na_total = temp_na.xs(5, level=1, drop_level=False).copy()

        # Associated Dissolved
        temp_ad = self.parent.restart.ogsmout_ogadgprd.copy()
        temp_ad = temp_ad.xs(int(self.parent.current_year), level=2, axis=0)
        # Get Type tables
        temp_ad_conv = temp_ad.xs(1, level=1, drop_level=False).copy()
        temp_ad_tight = temp_ad.xs(2, level=1, drop_level=True).copy()
        temp_ad_tight[1] = 3
        temp_ad_tight = temp_ad_tight.set_index([1], append=True)
        temp_ad_total = temp_ad.xs(5, level=1, drop_level=False).copy()

        # Combine AD and NA gas
        temp_conv = temp_na_conv + temp_ad_conv
        temp_tight = temp_na_tight + temp_ad_tight
        temp_total = temp_na_total + temp_ad_total

        # Recreate table
        temp_ng = pd.concat([temp_conv, temp_na_shale, temp_tight, temp_na_cbm, temp_total])
        temp_ng[nam.year] = int(self.parent.current_year)
        temp_ng = temp_ng.set_index([nam.year], append=True)
        temp_ng.index.names = self.parent.restart.ogsmout_ogdngprd.index.names

        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != 3] #Take out alaska, NGMM does not use ogrnagprd for Alaska
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != 75]  # Take out alaska state offshore
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        # Write to Restart Variable
        self.parent.restart.ogsmout_ogdngprd.update(temp_ng)

        ###Apply realized natural gas volumes to ogsmout_ogregprd (4 = Conventional gas, 5 = Tight Gas, 6 = Shale Gas, 7 = CBM)
        # Overwrite natural gas production by category
        on_na_prod_df_df = self.parent.restart.ogsmout_ogenagprd.copy()
        on_na_prod_df_df = on_na_prod_df_df[
            on_na_prod_df_df.index.get_level_values(2) == int(self.parent.current_year)]
        on_na_prod_df_df = on_na_prod_df_df.reset_index()
        on_na_prod_df_df = on_na_prod_df_df.merge(
            self.parent.mapping[[nam.district_number, nam.region_number]],
            how='left',
            left_on='1',
            right_on=nam.district_number)
        on_na_prod_df_df = on_na_prod_df_df.loc[on_na_prod_df_df[nam.region_number] <= 7]
        on_na_prod_df_df = on_na_prod_df_df.drop([nam.district_number], axis=1)

        on_ad_prod_df = self.parent.restart.ogsmout_ogadgprd.copy()
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(2) == int(self.parent.current_year)]
        on_ad_prod_df = on_ad_prod_df.reset_index()
        on_ad_prod_df = on_ad_prod_df.merge(self.parent.mapping[[nam.district_number, nam.region_number]],
                                            how='left',
                                            left_on='1',
                                            right_on=nam.district_number)
        on_ad_prod_df = on_ad_prod_df.loc[on_ad_prod_df[nam.region_number] <= 7]
        on_ad_prod_df = on_ad_prod_df.drop([nam.district_number], axis=1)

        # Conventional Production
        temp_realized_conv = on_na_prod_df_df.loc[on_na_prod_df_df['2'] == 1].copy()
        temp_realized_conv = temp_realized_conv.groupby([nam.region_number, '3']).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df['2'] == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby([nam.region_number, '3']).sum()

        temp_realized_conv['value'] = temp_realized_conv['value'].values + temp_ad_prod['value'].values
        temp_realized_conv[nam.gas_type] = 4
        temp_realized_conv = (temp_realized_conv.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_conv.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update((temp_realized_conv / 1000))

        # Tight Production
        temp_realized_tight = on_na_prod_df_df.loc[on_na_prod_df_df['2'] == 2].copy()
        temp_realized_tight = temp_realized_tight.groupby([nam.region_number, '3']).sum()
        temp_realized_tight[nam.gas_type] = 5
        temp_realized_tight = (temp_realized_tight.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_tight.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(temp_realized_tight / 1000)

        # Onshore Shale Production plus gas production from tight oil plays
        temp_realized_shale = on_na_prod_df_df.loc[on_na_prod_df_df['2'] == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby([nam.region_number, '3']).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df['2'] == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby([nam.region_number, '3']).sum()

        temp_realized_shale['value'] = temp_realized_shale['value'].values + temp_ad_prod['value'].values
        temp_realized_shale[nam.gas_type] = 6
        temp_realized_shale = (temp_realized_shale.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_shale.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(temp_realized_shale / 1000)

        # Coalbed Methane Production
        temp_realized_cbm = on_na_prod_df_df.loc[on_na_prod_df_df['2'] == 4].copy()
        temp_realized_cbm = temp_realized_cbm.groupby([nam.region_number, '3']).sum()
        temp_realized_cbm[nam.gas_type] = 7
        temp_realized_cbm = (temp_realized_cbm.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_cbm.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(temp_realized_cbm / 1000)

        pass


    def adjust_vars_for_realized_prod(self):
        """Adjust Restart variables so that they reflect final model year realized production.

        Returns
        -------
        self.parent.restart.ogsmout_ogqngrep : df
            Natural gas production by natural gas category

        self.parent.restart.ogsmout_ogngprd : df
            Natural gas production by Lower 48 region

        self.parent.restart.ogsmout_ogdngprd : df
            Dry natural gas production by state/district and gas type


        """
        #Create Realized Production temp table and mask for years
        on_realized_prod_df = self.parent.restart.ogsmout_ogrnagprd.copy()
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(2) > self.parent.history_year]
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(0) <= 66] #Take out offshore
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(0) != 3] #Take out Alaska

        on_ad_prod_df = self.parent.restart.ogsmout_ogadgprd.copy()
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(2) > self.parent.history_year]
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(0) <= 66] #Take out offshore
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(0) != 3] #Take out Alaska


        ###Natural Gas Production by Category
        #Adjust onshore Shale Production plus gas production from tight oil plays to match realized prod
        temp_realized_shale = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby(level = [2]).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1) == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index > self.parent.history_year]

        temp_realized_shale['value'] = temp_realized_shale['value'].values + temp_ad_prod['value'].values
        temp_realized_shale[nam.gas_type] = 1
        temp_realized_shale = temp_realized_shale.loc[temp_realized_shale.index == self.parent.current_year]
        temp_realized_shale = (temp_realized_shale.set_index([nam.gas_type], append = True)).reorder_levels([1, 0])
        temp_realized_shale.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_shale)

        #Adjust coalbed methane production to match realized prod
        temp_realized_cbm = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 4].copy()
        temp_realized_cbm = temp_realized_cbm.groupby(level=[2]).sum()
        temp_realized_cbm[nam.gas_type] = 2
        temp_realized_cbm = temp_realized_cbm.loc[temp_realized_cbm.index == self.parent.current_year]
        temp_realized_cbm = (temp_realized_cbm.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_cbm.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_cbm)

        #Adjust tight production to match realized prod
        temp_realized_tight = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 2].copy()
        temp_realized_tight = temp_realized_tight.groupby(level=[2]).sum()
        temp_realized_tight[nam.gas_type] = 3
        temp_realized_tight = temp_realized_tight.loc[temp_realized_tight.index == self.parent.current_year]
        temp_realized_tight = (temp_realized_tight.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_tight.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_tight)

        #Adjust conventional Production to match realized prod
        temp_realized_conv = on_realized_prod_df.loc[on_realized_prod_df.index.get_level_values(1) == 1].copy()
        temp_realized_conv = temp_realized_conv.groupby(level=[2]).sum()
        temp_realized_conv[nam.gas_type] = 4
        temp_realized_conv = temp_realized_conv.loc[temp_realized_conv.index == self.parent.current_year]
        temp_realized_conv = (temp_realized_conv.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_conv.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_conv)

        #Adjust other AD natgas Production to match realized prod
        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df.index.get_level_values(1).isin([1,3,4])].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index > self.parent.history_year]
        temp_ad_prod[nam.gas_type] = 5
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index == self.parent.current_year]
        temp_ad_prod = (temp_ad_prod.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_ad_prod.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_ad_prod)


        ###Offshore Realized Production Adjustment
        #Since NA and AD natgas aren't split up, we need to produce a production ratio here
        off_realized_prod_df = self.parent.restart.ogsmout_ogrnagprd.copy()
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(2) > self.parent.history_year]
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(0) >= 67] #Take out onshore
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(0) != 75] #Take out alaska state offshore
        off_realized_prod_df = off_realized_prod_df[off_realized_prod_df.index.get_level_values(0) != 84] #Take out alaska federal offshore

        off_ad_prod_df = self.parent.restart.ogsmout_ogadgprd.copy()
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(2) > self.parent.history_year]
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) >= 67]  # Take out onshore
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) != 75]  # Take out alaska state offshore
        off_ad_prod_df = off_ad_prod_df[off_ad_prod_df.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        #Adjust NA offshore production to match realized prod
        temp_realized_na_prod_off = off_realized_prod_df.loc[off_realized_prod_df.index.get_level_values(1) == 5].copy()
        temp_realized_na_prod_off = temp_realized_na_prod_off.groupby(level=[2]).sum()
        temp_realized_na_prod_off[nam.gas_type] = 6
        temp_realized_na_prod_off = temp_realized_na_prod_off.loc[temp_realized_na_prod_off.index == self.parent.current_year]
        temp_realized_na_prod_off = (temp_realized_na_prod_off.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_realized_na_prod_off.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_realized_na_prod_off[[nam.value]])

        #Adjust AD offshore production to match realized prod
        temp_ad_prod = off_ad_prod_df.loc[off_ad_prod_df.index.get_level_values(1) == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby(level=[2]).sum()
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index > self.parent.history_year]
        temp_ad_prod[nam.gas_type] = 7
        temp_ad_prod = temp_ad_prod.loc[temp_ad_prod.index == self.parent.current_year]
        temp_ad_prod = (temp_ad_prod.set_index([nam.gas_type], append=True)).reorder_levels([1, 0])
        temp_ad_prod.index.names = self.parent.restart.ogsmout_ogqngrep.index.names
        self.parent.restart.ogsmout_ogqngrep.update(temp_ad_prod)


        ###Aggregate NG production by category
        temp = self.parent.restart.ogsmout_ogqngrep.copy()
        temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8], level=0)]
        temp = temp[temp.index.isin([int(self.parent.current_year)], level=1)]
        self.parent.restart.ogsmout_ogqngrep.at[(9, int(self.parent.current_year)), 'value'] = temp['value'].sum()


        ###Adjust total natgas production by Region to match realized prod
        #Non-Associated
        temp_na = self.parent.restart.ogsmout_ogrnagprd.copy()
        temp_na = temp_na.xs(5, level = 1, axis = 0)
        temp_na = temp_na.xs(int(self.parent.current_year), level = 1, axis = 0)
        temp_na = pd.merge(temp_na,
                       self.parent.mapping[[nam.district_number, nam.region_number]],
                       how = 'left',
                       left_index= True, right_on = nam.district_number)
        temp_na = temp_na[[nam.region_number,nam.value]].groupby(nam.region_number).sum()

        #Associated Dissolved
        temp_ad = self.parent.restart.ogsmout_ogadgprd.copy()
        temp_ad = temp_ad.xs(5, level = 1, axis = 0)
        temp_ad = temp_ad.xs(int(self.parent.current_year), level = 1, axis = 0)
        temp_ad = pd.merge(temp_ad,
                       self.parent.mapping[[nam.district_number, nam.region_number]],
                       how = 'left',
                       left_index= True, right_on = nam.district_number)
        temp_ad = temp_ad[[nam.region_number,nam.value]].groupby(nam.region_number).sum()

        #Combine NA and AD gas
        temp_ng = temp_na + temp_ad

        #Get total
        ng_total = temp_ng[nam.value].sum()
        temp_ng = temp_ng.loc[list(range(1,11))]
        temp_ng.loc[11] = ng_total

        #Fix Offshore which is misaligned from restart
        temp_ng.at[(8, nam.value)]     = temp_ng.at[(9, nam.value)]
        temp_ng.at[(9, nam.value)]     = temp_ng.at[(10, nam.value)]
        temp_ng.at[(10, nam.value)]    = 0

        #Format for Restart File
        temp_ng[nam.year] = int(self.parent.current_year)
        temp_ng = temp_ng.set_index([nam.year], append = True)
        temp_ng.index.names = self.parent.restart.ogsmout_ogngprd.index.names

        #Assign to Restart Variable
        self.parent.restart.ogsmout_ogngprd.update(temp_ng)


        ###Adjust dry natural gas production by HSM district and natgas type to match realized prod
        ###Total natgas production
        #Non-Associated
        temp_na = self.parent.restart.ogsmout_ogrnagprd.copy()
        temp_na = temp_na.xs(int(self.parent.current_year), level = 2, axis = 0)

        #Get Type tables
        temp_na_conv = temp_na.xs(1,level = 1, drop_level= False).copy()
        temp_na_shale = temp_na.xs(2,level = 1, drop_level= False).copy()
        temp_na_tight = temp_na.xs(3,level = 1, drop_level= False).copy()
        temp_na_cbm = temp_na.xs(4,level = 1, drop_level= False).copy()
        temp_na_total = temp_na.xs(5,level = 1, drop_level= False).copy()

        #Associated Dissolved
        temp_ad = self.parent.restart.ogsmout_ogadgprd.copy()
        temp_ad = temp_ad.xs(int(self.parent.current_year), level = 2, axis = 0)
        #Get Type tables
        temp_ad_conv = temp_ad.xs(1,level = 1, drop_level= False).copy()
        temp_ad_tight = temp_ad.xs(2,level = 1, drop_level= True).copy()
        temp_ad_tight[1] = 3
        temp_ad_tight = temp_ad_tight.set_index([1], append = True)
        temp_ad_total = temp_ad.xs(5,level = 1, drop_level= False).copy()

        #Combine AD and NA gas
        temp_conv = temp_na_conv + temp_ad_conv
        temp_tight = temp_na_tight + temp_ad_tight
        temp_total = temp_na_total + temp_ad_total

        #Recreate table
        temp_ng = pd.concat([temp_conv,temp_na_shale, temp_tight, temp_na_cbm, temp_total])
        temp_ng[nam.year] = int(self.parent.current_year)
        temp_ng = temp_ng.set_index([nam.year], append = True)
        temp_ng.index.names = self.parent.restart.ogsmout_ogdngprd.index.names

        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != 3] #Take out alaska, NGMM does not use ogrnagprd for Alaska
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != 75]  # Take out alaska state offshore
        temp_ng = temp_ng[temp_ng.index.get_level_values(0) != 84]  # Take out alaska federal offshore

        #Write to Restart Variable
        self.parent.restart.ogsmout_ogdngprd.update(temp_ng)
        
        
        ###Apply realized natural gas volumes to ogsmout_ogregprd (4 = Conventional gas, 5 = Tight Gas, 6 = Shale Gas, 7 = CBM)
        #Overwrite natural gas production by category
        on_realized_prod_df = self.parent.restart.ogsmout_ogrnagprd.copy()
        on_realized_prod_df = on_realized_prod_df[on_realized_prod_df.index.get_level_values(2) == int(self.parent.current_year)]
        on_realized_prod_df = on_realized_prod_df.reset_index()
        on_realized_prod_df = on_realized_prod_df.merge(
            self.parent.mapping[[nam.district_number, nam.region_number]],
            how='left',
            left_on='1',
            right_on=nam.district_number)
        on_realized_prod_df = on_realized_prod_df.loc[on_realized_prod_df[nam.region_number] <= 7]
        on_realized_prod_df = on_realized_prod_df.drop([nam.district_number], axis=1)

        on_ad_prod_df = self.parent.restart.ogsmout_ogadgprd.copy()
        on_ad_prod_df = on_ad_prod_df[on_ad_prod_df.index.get_level_values(2) == int(self.parent.current_year)]
        on_ad_prod_df = on_ad_prod_df.reset_index()
        on_ad_prod_df = on_ad_prod_df.merge(self.parent.mapping[[nam.district_number, nam.region_number]],
                                            how='left',
                                            left_on='1',
                                            right_on=nam.district_number)
        on_ad_prod_df = on_ad_prod_df.loc[on_ad_prod_df[nam.region_number] <= 7]
        on_ad_prod_df = on_ad_prod_df.drop([nam.district_number], axis=1)

        #Conventional Production
        temp_realized_conv = on_realized_prod_df.loc[on_realized_prod_df['2'] == 1].copy()
        temp_realized_conv = temp_realized_conv.groupby([nam.region_number, '3']).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df['2'] == 1].copy()
        temp_ad_prod = temp_ad_prod.groupby([nam.region_number, '3']).sum()

        temp_realized_conv['value'] = temp_realized_conv['value'].values + temp_ad_prod['value'].values
        temp_realized_conv[nam.gas_type] = 4
        temp_realized_conv = (temp_realized_conv.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_conv.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update((temp_realized_conv / 1000))

        #Tight Production
        temp_realized_tight = on_realized_prod_df.loc[on_realized_prod_df['2'] == 2].copy()
        temp_realized_tight = temp_realized_tight.groupby([nam.region_number, '3']).sum()
        temp_realized_tight[nam.gas_type] = 5
        temp_realized_tight = (temp_realized_tight.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_tight.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(temp_realized_tight / 1000)

        #Onshore Shale Production plus gas production from tight oil plays
        temp_realized_shale = on_realized_prod_df.loc[on_realized_prod_df['2'] == 3].copy()
        temp_realized_shale = temp_realized_shale.groupby([nam.region_number, '3']).sum()

        temp_ad_prod = on_ad_prod_df.loc[on_ad_prod_df['2'] == 2].copy()
        temp_ad_prod = temp_ad_prod.groupby([nam.region_number, '3']).sum()

        temp_realized_shale['value'] = temp_realized_shale['value'].values + temp_ad_prod['value'].values
        temp_realized_shale[nam.gas_type] = 6
        temp_realized_shale = (temp_realized_shale.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_shale.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(temp_realized_shale / 1000)

        #Coalbed Methane Production
        temp_realized_cbm = on_realized_prod_df.loc[on_realized_prod_df['2'] == 4].copy()
        temp_realized_cbm = temp_realized_cbm.groupby([nam.region_number, '3']).sum()
        temp_realized_cbm[nam.gas_type] = 7
        temp_realized_cbm = (temp_realized_cbm.set_index([nam.gas_type], append=True)).reorder_levels([0, 2, 1])
        temp_realized_cbm.index.names = self.parent.restart.ogsmout_ogregprd.index.names
        self.parent.restart.ogsmout_ogregprd.update(temp_realized_cbm / 1000)

        pass
        