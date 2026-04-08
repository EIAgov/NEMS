import pandas as pd

def analysis_report_main(file_a, file_b, out_loc, alldif):
    '''
    The main function for the analysis report between runs, now including VarNam2 comparison and duplicate checks.
    '''
    # Assume the following columns, when combined, create a unique row identifier:
    # 'TableNumber', 'RowNum', 'RegionNum', 'VarName', 'VarNam2', 'Geogr'
    id_cols = ['TableNumber', 'RowNum', 'RegionNum', 'VarName', 'VarNam2', 'Geogr']
    value_cols = ['GLabel', 'Gunits', 'RowFmt', 'DaType', 'SubDat', 'Sector', 'SubSec', 'Source', 'SubSrc'] # Other descriptive cols

    # Read CSVs and create a unique ID
    dfa = pd.read_csv(file_a)
    dfb = pd.read_csv(file_b)

    # Convert ID columns to string for concatenation to avoid type issues and NaN handling
    for col in id_cols:
        dfa[col] = dfa[col].astype(str)
        dfb[col] = dfb[col].astype(str)

    dfa['unique_id'] = dfa[id_cols].agg('_'.join, axis=1)
    dfb['unique_id'] = dfb[id_cols].agg('_'.join, axis=1)


    # --- Duplicate checking for unique_id ---
    duplicates_a_id = dfa[dfa.duplicated(subset=['unique_id'], keep=False)]
    duplicates_b_id = dfb[dfb.duplicated(subset=['unique_id'], keep=False)]

    if not duplicates_a_id.empty:
        print(f"Duplicates found in {file_a} based on 'unique_id':")
        print(duplicates_a_id[['VarNam2', 'GLabel', 'unique_id']].sort_values(by='unique_id').to_string())
    if not duplicates_b_id.empty:
        print(f"\nDuplicates found in {file_b} based on 'unique_id':")
        print(duplicates_b_id[['VarNam2', 'GLabel', 'unique_id']].sort_values(by='unique_id').to_string())

    # Hardcode the analysis years
    years = range(2025, 2051)
    years = ["{:04d}".format(x) for x in years]

    # --- Identify unique_id unique to each file ---
    unique_id_in_a = set(dfa['unique_id'].unique())
    unique_id_in_b = set(dfb['unique_id'].unique())

    only_in_a_id = list(unique_id_in_a - unique_id_in_b)
    only_in_b_id = list(unique_id_in_b - unique_id_in_a)

    # Filter original dataframes to get full rows for unique_id
    df_only_in_a = dfa[dfa['unique_id'].isin(only_in_a_id)].copy()
    df_only_in_b = dfb[dfb['unique_id'].isin(only_in_b_id)].copy()

    # --- Proceed with numerical difference analysis for common unique_id ---
    common_unique_id = list(unique_id_in_a.intersection(unique_id_in_b))

    # Filter dataframes for common unique_id and set index
    dfa_common_indexed = dfa[dfa['unique_id'].isin(common_unique_id)].set_index('unique_id')
    dfb_common_indexed = dfb[dfb['unique_id'].isin(common_unique_id)].set_index('unique_id')

    # Align dfb to dfa's index for direct subtraction.
    # This is safe because 'unique_id' is now truly unique.
    dfb_common_indexed_aligned = dfb_common_indexed.reindex(dfa_common_indexed.index)


    # Calculate differences directly from these aligned indexed dataframes
    temp_df = abs(dfa_common_indexed[years] - dfb_common_indexed_aligned[years])
    temp_df['orig_sum'] = dfa_common_indexed[years].sum(axis=1)

    # Join with relevant metadata from dfa_common_indexed
    # Ensure all columns required for the final report are selected from dfa_common_indexed
    # or constructed from it.
    analysis_cols_from_dfa = id_cols + value_cols # All descriptive columns

    # Select these from dfa_common_indexed and join with temp_df
    # temp_df already has the unique_id as its index.
    # The join should be efficient as both are indexed by unique_id.
    temp_df2 = temp_df.join(dfa_common_indexed[analysis_cols_from_dfa])

    temp_df2['absolute_difference'] = temp_df2[years].sum(axis=1)
    temp_df2['relative_diff'] = (temp_df2['absolute_difference'] / temp_df2['orig_sum']).fillna(0)

    # Add raw year values (aYYYY, bYYYY) by joining back to the original indexed common DFs
    # Using .reindex on the Series to ensure alignment with temp_df2's index
    for i in years:
        temp_df2[f'a{i}'] = dfa_common_indexed[i].reindex(temp_df2.index)
        temp_df2[f'b{i}'] = dfb_common_indexed_aligned[i].reindex(temp_df2.index)


    # Reset index to make unique_id a column again for output and filtering
    temp_df3 = temp_df2.drop(years + ['orig_sum'], axis=1).reset_index()

    # Sort by relative difference, and output all rows
    temp_df4 = temp_df3.sort_values(by='relative_diff', ascending=False)

    # Larger relative differences for 'United States' and specific conditions
    tempdf_us = temp_df3[(temp_df3['absolute_difference'] > 1) &
                         (temp_df3['relative_diff'] > .01) &
                         (temp_df3['Geogr'].str.lower() == 'united states') &
                         (temp_df3['TableNumber'] != '150')].copy()
    tempdf_us2 = tempdf_us.sort_values(by=['relative_diff'], ascending=[0])
    affected_tables_count = len(tempdf_us['TableNumber'].unique())

    # Summarize the US stats
    df_less_col = tempdf_us2[['TableNumber', 'absolute_difference', 'relative_diff']]
    val_counts_df = df_less_col['TableNumber'].value_counts()
    summary_table_df = pd.merge(val_counts_df.reset_index(), df_less_col.groupby('TableNumber').sum().reset_index(), on='TableNumber')
    summary_table_df.columns = ['Table_Number', 'RowsWithDiffs', 'SumOfDiffs', 'SumOfRelDiff']


    # Text stats
    summary_text_list = []
    summary_text_list.append(['Stats for the run',])
    summary_text_list.append(['File A:', file_a])
    summary_text_list.append(['File B:',file_b])
    summary_text_list.append(['',''])
    summary_text_list.append(['--- Row Comparison (based on unique ID) ---', ''])
    summary_text_list.append(['Number of rows unique to File A:', len(only_in_a_id)])
    summary_text_list.append(['Number of rows unique to File B:', len(only_in_b_id)])
    summary_text_list.append(['Number of common rows for numerical comparison:', len(common_unique_id)])
    summary_text_list.append(['',''])
    summary_text_list.append(['Limited Set Stats',''])
    summary_text_list.append(['-Changed Tables (Limited Set):', affected_tables_count])
    summary_text_list.append(['-Rows with Differences (Limited Set):', len(tempdf_us2)])
    summary_text_list.append(['*Limited set is: US total, change greater than 1 and 1%, no table 150',])
    summary_text_list.append(['*Table below summarizes the limited set'])
    text_df = pd.DataFrame(summary_text_list)

    # Write out excel files
    outfile = pd.ExcelWriter(out_loc, engine="xlsxwriter")

    text_df.to_excel(outfile, 'Summary', index=False, header=False)
    summary_table_df.to_excel(outfile, 'Summary', index=False, startrow=len(text_df)+2)

    # New sheets for unique_id specific comparisons
    if not df_only_in_a.empty:
        df_only_in_a.to_excel(outfile, "Rows_Only_A", index=False)
    if not df_only_in_b.empty:
        df_only_in_b.to_excel(outfile, "Rows_Only_B", index=False)

    # Existing difference sheets
    tempdf_us2.to_excel(outfile, "limited_differences", index=False)
    if alldif == True:
        temp_df4.to_excel(outfile, "all_differences", index=False)
    else:
        print("All Differences not required")

    # Format summary sheet
    sum_sheet = outfile.sheets['Summary']
    col_list = [["A:A",20],
                ["B:B",15],
                ["C:C",15],
                ["D:D",15]
                ]
    for i in col_list:
        sum_sheet.set_column(i[0], i[1])

    outfile.close()

if __name__ == '__main__':
    file_a = r"T:\output\aeo2026\ref2026\d091025v\NEMSref2026.unif.api.csv"
    file_b = r"T:\output\aeo2026\ref2026\d090925x\NEMSref2026.unif.api.csv"
    out_loc = r'C:\Users\jmw\Desktop\diff_testnew.xlsx'
    analysis_report_main(file_a, file_b, out_loc, False)