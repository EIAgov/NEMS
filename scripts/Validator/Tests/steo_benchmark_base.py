from DataModel.Model import Model
import pandas as pd

class STEOBenchmarkBase:
    #column_headers=['AEO Name', 'STEO Name', 'Tolerance', 'Multiplier', 'Multiplier_LeapYear', 'grpby_sign_aeo', 'n_steo']
    column_headers=['AEO Name', 'STEO Name', 'Tolerance', 'Tolerance_EndYear', 'Multiplier', 'Multiplier_LeapYear', 'grpby_sign_aeo', 'n_steo']
    rpt_header = 'Group By'
    # group_by_flag: (0, -1) for AEO group, 1 for multiple STEO group
    # grpby_sign_aeo = (1, -1) for AEO group
    # n_steo = 0/1 for 1 or many STEO for the mapping record

    def __init__(self, catg='ngmm', start_year=2020, end_year=2023):
        self.mapping_catg = catg
        self.start_year = start_year
        self.end_year = end_year

    def load_table(self, flag):
        if flag == 1:
            df = Model.getInstance().files.csv.load_table_entire()
            #df = model.files.csv.load_table_entire()

            year_columns = [str(i) for i in range(self.start_year, self.end_year+1)]
            colns = ['VarNam2', 'Geogr'] + year_columns
            df = df.loc[:, colns]
            df.rename(columns={'VarNam2': self.column_headers[0]}, inplace='True')
            df[self.column_headers[0]] = df[self.column_headers[0]].str.strip()
            df[self.column_headers[0]] = df[self.column_headers[0]].str.upper()

        elif flag == 2:
            df = Model.getInstance().files.csv_steo.load_table()
            #df = model.files.csv_steo.load_table()

            df.columns = df.iloc[0]
            df = df.iloc[1: , :]
            df = df.drop(range(1,4))

            year_columns = [str(i) for i in range(self.start_year, self.end_year+1)]
            colns = ['AAAA_DATEX'] + year_columns
            df = df.loc[:, colns]
            df.rename(columns={'AAAA_DATEX': self.column_headers[1]}, inplace='True')
            #df[self.column_headers[1]].str.strip()
            df[self.column_headers[1]] = df[self.column_headers[1]].str.strip()
            df[self.column_headers[1]] = df[self.column_headers[1]].str.upper()
        return df

    def is_leap_year(self, year):
        return (year%4 == 0) and (year %100 != 0 or year %100 == 0)

    def assemble_mapping_record(self, data, df):
        d = {self.column_headers[0]: data[0], \
             self.column_headers[1]: data[1], \
             self.column_headers[2]: data[2], \
             self.column_headers[3]: data[3], \
             self.column_headers[4]: data[4], \
             self.column_headers[5]: data[5], \
             self.column_headers[6]: data[6], \
             self.column_headers[7]: data[7]}
        # in aeo2024_py311 Pandas 2.0.2, df.append is deepreciated, use list and df.concat() instead:
        df = pd.concat([df, pd.DataFrame.from_records([d])], ignore_index=True)
        # in aeo2023_py37_b Pandas, df.append() was supported:
        #df = df.append(d, ignore_index = True)

        return df

    def handle_multiple_steo(self, df, idx, df2, aeo_tmp_name=0):
        # STEO only has + combination, no -
        s = df[self.column_headers[1]][idx]
        result = [x.strip() for x in s.split('+')]
        for i in range(1,len(result)+1):
            if (aeo_tmp_name==0):
                d = (df[self.column_headers[0]][idx], result[i-1], df[self.column_headers[2]][idx], \
                     df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                     df[self.column_headers[5]][idx], 1, 1)
            else:
                d = (str(aeo_tmp_name), result[i-1], df[self.column_headers[2]][idx], \
                     df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                     df[self.column_headers[5]][idx], 1, 1)

            df2 = self.assemble_mapping_record(d, df2)

        return df2

    def handle_multiple_aeo(self, df, idx, df2, steo_tmp_name=0):
        # AEO can have both + and - combination
        s = df[self.column_headers[0]][idx]
        result = [x.strip() for x in s.split('+')]
        if (steo_tmp_name > 0):
            for i in range(1,len(result)+1):
                # handle liquides: (PSD000:tr_in_ngl_propan + PSD000:tr_in_ole_propy - PSD000:tr_ex_ngl_propan - PSD000:tr_ex_ole_propy ))
                # test1: (PSD000:trade_imports - PSD000:trade_exports))
                minus = [x.strip() for x in result[i-1].split('-')]
                #minus = [x.strip() for x in result[1].split('-')]
                if len(minus) > 1:
                    d = (minus[0], str(steo_tmp_name), df[self.column_headers[2]][idx], \
                         df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                         df[self.column_headers[5]][idx], 1, 0)
                    df2 = self.assemble_mapping_record(d, df2)

                    for j in range(2, len(minus)+1):
                        d = (minus[j-1], str(steo_tmp_name), df[self.column_headers[2]][idx], \
                             df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                             df[self.column_headers[5]][idx], -1, 0)
                        df2 = self.assemble_mapping_record(d, df2)
                else:
                    d = (result[i-1], str(steo_tmp_name), df[self.column_headers[2]][idx], \
                         df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                         df[self.column_headers[5]][idx], 1, 0)
                    df2 = self.assemble_mapping_record(d, df2)
            return df2 # exit the if loop (steo_tmp_name > 0)

        # now (n AEO : 1 STEO):
        for i in range(1,len(result)+1):
            # handle liquides: (PSD000:tr_in_ngl_propan + PSD000:tr_in_ole_propy - PSD000:tr_ex_ngl_propan - PSD000:tr_ex_ole_propy ))
            # test1: (PSD000:trade_imports - PSD000:trade_exports))
            minus = [x.strip() for x in result[i-1].split('-')]
            #minus = [x.strip() for x in result[1].split('-')]
            if len(minus) > 1:
                d = (minus[0], df[self.column_headers[1]][idx], df[self.column_headers[2]][idx], \
                     df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                     df[self.column_headers[5]][idx], 1, 0)
                df2 = self.assemble_mapping_record(d, df2)

                for j in range(2, len(minus)+1):
                    d = (minus[j-1], df[self.column_headers[1]][idx], df[self.column_headers[2]][idx], \
                         df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                         df[self.column_headers[5]][idx], -1, 0)
                    df2 = self.assemble_mapping_record(d, df2)
            else:
                d = (result[i-1], df[self.column_headers[1]][idx], df[self.column_headers[2]][idx], \
                     df[self.column_headers[3]][idx], df[self.column_headers[4]][idx], \
                     df[self.column_headers[5]][idx], 1, 0)
                df2 = self.assemble_mapping_record(d, df2)

        return df2

    def refresh_float_type(self, df, suffix='', idx_list=[]):
        year_columns = [str(i)+suffix for i in range(self.start_year, self.end_year+1)]
        df[year_columns] = df[year_columns].astype(float)
        for i in idx_list:
            df[self.column_headers[i]] = df[self.column_headers[i]].astype(float)
        return df

    def load_table_mapping(self):
        if self.mapping_catg == 'ngmm':
            df = Model.getInstance().files.csv_aeo2steo_ngmm.load_table()
        elif self.mapping_catg == 'cmm':
            df = Model.getInstance().files.csv_aeo2steo_cmm.load_table()
        elif self.mapping_catg == 'emm':
            df = Model.getInstance().files.csv_aeo2steo_emm.load_table()
        elif self.mapping_catg == 'blg':
            df = Model.getInstance().files.csv_aeo2steo_blg.load_table()
        elif self.mapping_catg == 'ind':
            df = Model.getInstance().files.csv_aeo2steo_ind.load_table()
        elif self.mapping_catg == 'itg':
            df = Model.getInstance().files.csv_aeo2steo_itg.load_table()
        elif self.mapping_catg == 'liq':
            df = Model.getInstance().files.csv_aeo2steo_liq.load_table()
        elif self.mapping_catg == 'ogsm':
            df = Model.getInstance().files.csv_aeo2steo_ogsm.load_table()
        elif self.mapping_catg == 'mco':
            df = Model.getInstance().files.csv_aeo2steo_mco.load_table()
        else:
            df = Model.getInstance().files.csv_aeo2steo_ngmm.load_table()
        #df = model.files.csv_aeo2steo_emm.load_table()

        df = df[self.column_headers[:6]]
        df[self.column_headers[0]].str.strip()
        df[self.column_headers[1]].str.strip()
        df[self.column_headers[0]] = df[self.column_headers[0]].str.upper()
        df[self.column_headers[1]] = df[self.column_headers[1]].str.upper()
        df[self.column_headers[2]] = df[self.column_headers[2]].str.rstrip('%').astype(float)/100.00
        df[self.column_headers[3]] = df[self.column_headers[3]].str.rstrip('%').astype(float)/100.00
        df[self.column_headers[4]] = df[self.column_headers[4]].astype(float)
        df[self.column_headers[5]] = df[self.column_headers[5]].astype(float)

        # now parse data with +,- cases
        df2 = pd.DataFrame(columns = self.column_headers)

        df3_aeo = pd.DataFrame(columns = self.column_headers)
        df3_steo = pd.DataFrame(columns = self.column_headers)

        compute_name_cnt = 0
        for idx, row in df.iterrows():
            s = df[self.column_headers[0]][idx]
            n_aeo_flag = s.find('+')
            n_aeo_flag += s.find('-')

            s = df[self.column_headers[1]][idx]
            n_steo_flag = s.find('+')

            if (n_steo_flag ==-1):
                #(1 AEO or n AEO) to (1 STEO)
                # parsing for the NGMM case "NGS000:ea_PipelineFuel + NGS000:ea_liquefactexp"

                df2=self.handle_multiple_aeo(df, idx, df2)

            elif (n_steo_flag >-1) and (n_aeo_flag == -2):
                #(1 AEO):(n STEO)
                # parsing 1 AEO or N AEO like EMM case (REN000:x2_AllSunnyOut,soepgen_us + sochgen_us + sodtp_us)
                df2 = self.handle_multiple_steo(df, idx, df2)

            elif (n_steo_flag >-1) and (n_aeo_flag > -2):
                #(n AEO):(m STEO)
                compute_name_cnt += 1
                df3_aeo = self.handle_multiple_aeo(df, idx, df3_aeo, compute_name_cnt)
                df3_steo = self.handle_multiple_steo(df, idx, df3_steo, compute_name_cnt)

        return df2, df3_aeo, df3_steo

    def assemble_computed_tables(self, df_unifapi, df_steo, df_map_aeo, df_map_steo):
        df_merge1 = pd.merge(
            df_map_aeo,
            df_unifapi,
            how="left",
            on='AEO Name'
        )
        df_merge1 = df_merge1.dropna(how='any')
        df_merge1 = df_merge1.drop(self.column_headers[2:6], axis=1)
        df_merge1 = df_merge1.drop(self.column_headers[7], axis=1) #drop n_steo
        df_merge1 = self.refresh_float_type(df_merge1, suffix='', idx_list=[6])

        # calculate +,- on AEO part
        temp = df_merge1[df_merge1[self.column_headers[6]] == -1]
        if len(temp) != 0:
            t1 = df_merge1[self.column_headers[0:2]]
            t2 = df_merge1.select_dtypes(include=['float64'])

            for i in range(self.start_year, self.end_year+1):
                t2[str(i)] = t2[str(i)]*t2[self.column_headers[6]]

            df_merge1 = pd.concat([t1, t2], axis=1)

        # AEO part ready
        df_merge1.rename(columns={self.column_headers[1]:'grpby'}, inplace='True')

        # process STEO part
        df_merge2 = pd.merge(
            df_map_steo,
            df_steo,
            how="left",
            on='STEO Name'
        )
        df_merge2 = df_merge2.dropna(how='any')
        df_merge2 = df_merge2.drop(self.column_headers[6], axis=1) #drop grpby_sign_aeo
        df_merge2.rename(columns={self.column_headers[0]:'grpby'}, inplace='True')
        df_merge2 = self.refresh_float_type(df_merge2, suffix='', idx_list=[7])

        for i in range(self.start_year, self.end_year+1):
            df_merge1[str(i)] = df_merge1.groupby('grpby')[str(i)].transform('sum')
            df_merge2[str(i)] = df_merge2.groupby('grpby')[str(i)].transform('sum')

        # now merge these two parts:
        df_merge = pd.merge(
            df_merge1,
            df_merge2,
            how="left",
            on='grpby',
            suffixes=("_aeo", "_steo")
        )

        return df_merge

    def calcuate_discrepency(self, df, grpby_aeo_or_steo = 'aeo'):
        for i in range(self.start_year, self.end_year+1):
            a = str(i) + '_aeo'
            s = str(i) + '_steo'
            a2 = str(i) + '_aeo2'
            s2 = str(i) + '_steo2'
            if grpby_aeo_or_steo == 'aeo':
                # group by to handle the 'NGS000:ea_PipelineFuel + NGS000:ea_liquefactexp' case
                df[a2] = df.groupby(self.column_headers[1])[a].transform('sum')
                if (self.is_leap_year(i)):
                    df[str(i)] = (df[a2]/(df[s]*df[self.column_headers[5]])) - 1
                else:
                    df[str(i)] = (df[a2]/(df[s]*df[self.column_headers[4]])) - 1

            elif grpby_aeo_or_steo == 'steo':
                # to handle EMM case (REN000:x2_AllSunnyOut,soepgen_us + sochgen_us + sodtp_us)
                df[s2] = df.groupby(self.column_headers[0])[s].transform('sum')

                if (self.is_leap_year(i)):
                    df[str(i)] = (df[a]/(df[s2]*df[self.column_headers[5]])) - 1
                else:
                    df[str(i)] = (df[a]/(df[s2]*df[self.column_headers[4]])) - 1

            else:
                # n AEO: m STEO relationship. Data has been grouped
                if (self.is_leap_year(i)):
                    df[str(i)] = (df[a]/(df[s]*df[self.column_headers[5]])) - 1
                else:
                    df[str(i)] = (df[a]/(df[s]*df[self.column_headers[4]])) - 1

        if grpby_aeo_or_steo == 'aeo':
            df[self.rpt_header] = df[self.column_headers[1]] #'by STEO'
        elif grpby_aeo_or_steo == 'steo':
            df[self.rpt_header] = df[self.column_headers[0]] #'by AEO'
        else:
            df[self.rpt_header] = df['grpby']

        year_columns = [str(i) for i in range(self.start_year, self.end_year+1)]
        h = [self.column_headers[0], self.column_headers[1], self.column_headers[2], self.column_headers[3], self.rpt_header] + year_columns
        df_discrepancy = df[h]

        return df_discrepancy

    def get_aeo_steo(self, df_unifapi, df_steo):
        df_map, df_map_aeo, df_map_steo = self.load_table_mapping()
        #df_map = model.files.csv_aeo2steo_emm.load_table()
        #df_unifapi = model.files.csv.load_table_entire()
        #df_steo = model.files.csv_steo.load_table()

        df_merge = pd.merge(
            df_map,
            df_unifapi,
            how="left",
            on='AEO Name'
        )
        df_merge = df_merge.dropna(how='any')
        df_merge = self.refresh_float_type(df_merge, suffix='', idx_list=[6,7])

        temp = df_merge[df_merge[self.column_headers[6]] == -1]
        if len(temp) != 0:
            t1 = df_merge[self.column_headers[0:2]]
            t2 = df_merge.select_dtypes(include=['float64'])

            for i in range(self.start_year, self.end_year+1):
                t2[str(i)] = t2[str(i)]*t2[self.column_headers[6]]

            df_merge = pd.concat([t1, t2], axis=1)

        df_merge = pd.merge(
            df_merge,
            df_steo,
            how="left",
            on='STEO Name',
            suffixes=("_aeo", "_steo")
        )
        df_merge = df_merge.dropna(how='any')

        df_merge = self.refresh_float_type(df_merge, suffix='_aeo')
        df_merge = self.refresh_float_type(df_merge, suffix='_steo', idx_list=[6,7])

        df_part1 = df_merge[df_merge[self.column_headers[7]] == 0]
        df_part2 = df_merge[df_merge[self.column_headers[7]] == 1]

        # calculate 1-to-1 or 1-to-many cases
        df_part1[self.rpt_header] = ''; df_part2[self.rpt_header] = ''
        if len(df_part1) > 0:
            df_part1 = self.calcuate_discrepency(df_part1, 'aeo')
        if len(df_part2) > 0:
            df_part2 = self.calcuate_discrepency(df_part2, 'steo')

        # in aeo2024_py311 Pandas 2.0.2, df.append is deepreciated, use list and df.concat() instead:
        d = df_part2.to_dict('records')
        df_result = pd.concat([df_part1, pd.DataFrame.from_records([d])], ignore_index=True)
        # in aeo2023_py37_b Pandas, df.append() was supported:
        #df_result = df_part1.append(df_part2)

        df_result = df_result.reset_index(drop=True)

        df_result2 = self.assemble_computed_tables(df_unifapi, df_steo, df_map_aeo, df_map_steo)
        df_result2[self.rpt_header] = ''
        #df_result2 = self.calcuate_discrepency(df_result2, 'steo')
        df_result2 = self.calcuate_discrepency(df_result2, 'grpby')

        # in aeo2024_py311 Pandas 2.0.2, df.append is deepreciated, use list and df.concat() instead:
        d = df_result2.to_dict('records')
        df_discrepancy = pd.concat([df_result, pd.DataFrame.from_records([d])], ignore_index=True)
        # in aeo2023_py37_b Pandas, df.append() was supported:
        #df_discrepancy = df_result.append(df_result2)

        df_discrepancy = df_discrepancy.reset_index(drop=True)

        return df_discrepancy