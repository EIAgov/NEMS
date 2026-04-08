import pandas as pd
import numpy as np
import re

def replace_history(d_base_fixed: dict, user: str = None) -> dict:
    """
    Processes a nested dictionary `d_base_fixed` to apply history-based rules
    for zeroing out or replacing data.

    This function reads mapping data from MERNEMSMapping.csv and MER time-series data
    from MER_20251125.xlsx. It then iterates through the 'TN XXX' keys in d_base_fixed
    to perform the following:
    1. Standardize 'History' column from 'dflayin' to a numeric format.
    2. Identify 'IROWS' to keep (History = 1 or 2).
    3. Identify 'IROWS' to replace with MER data (History = 2).
    4. Zero out values in d_base_fixed for 'IROWS' that are not identified as 'keepers'.
    5. Replace values for 'IROWS' with History = 2 using corresponding MER data.

    Args:
        d_base_fixed (dict): A nested dictionary containing the base data to be modified.
                             Expected structure:
                             {
                                 "TN XXX": {
                                     "IROWS": list of strings,
                                     year: {irow: value, ...},
                                     ...
                                 },
                                 ...
                             }
        user (str, optional): An identifier for the user performing the operation.
                              Defaults to None. (Currently not used in logic, for context/logging).

    Returns:
        dict: The modified d_base_fixed dictionary after applying all rules.
    """
    # Define the range of years for processing
    start_year = 1990
    end_year = 2024 # This should ideally be replaced with `table_spec` or derived as needed

    # Read in layin.csv and grab RN rows
    # NOTE: `dflayin` is loaded from a file as expected in a production environment.
    dflayin = pd.read_csv("input/layin.csv")

    # --- CRITICAL STEP: Standardize 'History' column to numeric early ---
    dflayin['History_Numeric'] = pd.to_numeric(dflayin['History'], errors='coerce')

    # Prepare histcols_base: Filter for CM = "RN" and non-blank IROWS
    histcols_all_data = dflayin.copy()
    histcols_all_data['Table Number_Processed'] = pd.to_numeric(histcols_all_data['Table Number'], errors='coerce')
    histcols_all_data.dropna(subset=['Table Number_Processed'], inplace=True)
    histcols_all_data['Table Number'] = histcols_all_data['Table Number_Processed'].astype(int).astype(str)

    histcols_base = histcols_all_data[
        (histcols_all_data["CM"] == "RL") &
        (histcols_all_data["IROWS"].notna())
    ].copy()
    if pd.api.types.is_string_dtype(histcols_base["IROWS"]):
        histcols_base = histcols_base[histcols_base["IROWS"] != ''].copy()
        
    tablenum_base = histcols_all_data[
        (histcols_all_data["CM"] == "RN")
    ].copy()
    tablenum_base = tablenum_base[['Row Label', 'Table Number']]

    # --- Get MER to NEMS Mapping ---
    try:
        df_mer_nems_mapping = pd.read_csv("input/MERNEMSMapping.csv")
    except FileNotFoundError:
        print("Error: input/MERNEMSMapping.csv not found. Please ensure the file is in the correct directory.")
        df_mer_nems_mapping = pd.DataFrame(columns=["MER Mapping", "AEO Mapping"])
    except Exception as e:
        print(f"An error occurred while reading MERNEMSMapping.csv: {e}")
        df_mer_nems_mapping = pd.DataFrame(columns=["MER Mapping", "AEO Mapping"])
    # Get NEMS Vars from MER mapping
    aeo_mapping_series = df_mer_nems_mapping["AEO Mapping"].copy()
    
    # --- NEW: Simplify and Filter aeo_mapping_series ---
    if not aeo_mapping_series.empty:
        simplified_aeo_values = []
        for aeo_entry in aeo_mapping_series:
            # Check for complicated entries (containing arithmetic operators)
            # This regex looks for +, -, *, or / followed by spaces or other characters
            if re.search(r'[+\-*/]', aeo_entry):
                simplified_aeo_values.append(np.nan) # Mark for dropping by setting to NaN
            else:
                # If not complicated, keep only the part before '/'
                simplified_aeo_values.append(aeo_entry.split('/')[0].strip())

        # Create a new Series with simplified values, then drop NaNs
        aeo_mapping_series = pd.Series(simplified_aeo_values, index=aeo_mapping_series.index).dropna()
    else:
        aeo_mapping_series = pd.Series(dtype=str) # Ensure it's an empty Series of string type if originally empty

    # Drop TOTAL., .A, and spacing in MER column
    if not df_mer_nems_mapping.empty:
        df_mer_nems_mapping["MER Mapping"] = df_mer_nems_mapping["MER Mapping"].str.replace("TOTAL.", "", regex=False)
        df_mer_nems_mapping["MER Mapping"] = df_mer_nems_mapping["MER Mapping"].str.replace(".A", "", regex=False)
        df_mer_nems_mapping["MER Mapping"] = df_mer_nems_mapping["MER Mapping"].str.replace(" ", "", regex=False)    

    # --- Read in MER data source Excel file ---
    try:
        df_mer_data_source = pd.read_excel(r"input\MER_20251125.xlsx")
        df_mer_data_source = df_mer_data_source.drop_duplicates(keep='first') # double check the keep='first'
        df_mer_data_source.set_index("Row Labels", inplace=True)
    except FileNotFoundError:
        print("Error: input/MER_20251125.xlsx not found. Please ensure the file is in the correct directory.")
        df_mer_data_source = pd.DataFrame()
    except KeyError:
        print("Error: 'Row Labels' column not found in MER_20251125.xlsx. Cannot set index.")
        df_mer_data_source = pd.DataFrame()
    except Exception as e:
        print(f"An error occurred while reading MER_20251125.xlsx: {e}")
        df_mer_data_source = pd.DataFrame()

    # --- Dynamically creating combined mer_data for replacement from mapping and source ---
    mer_rows = []
    full_aeo_mapping_string_notmatch = []
    if not df_mer_nems_mapping.empty and not df_mer_data_source.empty:
            
        for index, mapping_row in df_mer_nems_mapping.iterrows():
            full_mer_mapping_string = mapping_row["MER Mapping"] # e.g., "TETCEUS/10^3"
            full_aeo_mapping_string = mapping_row["AEO Mapping"] # e.g. TCE000:ga_Total
                
            # Get the table number and IROWS from the layin by searching the AEO Mapping
            # the match only searches for single aeo table IDs, not calculations
            match = re.match(r"^[A-Za-z0-9:_]+$", full_aeo_mapping_string)
            
            if match:
                # get the "RN" and "DRNAME" from the aeo_mapping_string and try to find the table number and irows
                aeo_mapping_string = full_aeo_mapping_string.split("000:")
                table_num = tablenum_base[tablenum_base["Row Label"] == aeo_mapping_string[0]]
                
                # Search the MER mapping for calculations using search and fullmatch for calculations that are one variable / 1000
                search = re.search(r"[+\-*/()]", full_mer_mapping_string)
                fullmatch = re.fullmatch(r"^[A-Z0-9_]+/1000$", full_mer_mapping_string)
                
                # in some cases there will be two table numbers for the same "RN", just loop through them and search if the "DRNAME" is in that table
                for index, table_num_row in table_num.iterrows():
                    filtered_hiscols_all_data = histcols_all_data[histcols_all_data["Table Number"] == table_num_row["Table Number"]]
                    
                    # if "DRNAME" is in that table, create an entry, else don't
                    if (filtered_hiscols_all_data["DRNAME"].str.contains(aeo_mapping_string[1], case=False, na=False)).any():
                        filtered_hiscols_all_data_row = filtered_hiscols_all_data[filtered_hiscols_all_data["DRNAME"].str.contains(aeo_mapping_string[1], case=False, na=False)]
                        entry = {"Table Number": table_num_row["Table Number"], "IROWS": filtered_hiscols_all_data_row["IROWS"].iloc[0]}
                        
                        # loop through the years and calculate the value using eval, else return nan
                        for year in range(start_year, end_year + 1):
                            if str(year) in df_mer_data_source.columns:
                                # goes through the search for calculations and for calculations is only one variable/1000
                                # this avoid using .eval for single variable/1000 calculations which is slower
                                if search:
                                    if fullmatch:
                                        mer_series_id = full_mer_mapping_string.split("/")[0]
                                        entry[year] = df_mer_data_source.loc[mer_series_id, str(year)] / 1000
                                    else:
                                        # the data needs to be in dataframe and transposed so that the var names are the columns for .eval
                                        mer_data_source_row = df_mer_data_source[str(year)].to_frame().T
                                        calculated_mer_value = mer_data_source_row.eval(full_mer_mapping_string)
                                        entry[year] = calculated_mer_value.iloc[0]
                                else:
                                    # if it's just a one to one mapping
                                    entry[year] = df_mer_data_source.loc[full_mer_mapping_string, str(year)]
                            else:
                                entry[year] = np.nan # Year column not in source
                        mer_rows.append(entry)
            else:
                # this is a list of the aeo_mapping_strings that did not match, meaning that it could be a calculation
                full_aeo_mapping_string_notmatch.append(mapping_row)

    # Construct mer_data DataFrame for efficient lookup
    if mer_rows:
        mer_data = pd.DataFrame(mer_rows)
        mer_data['Table Number'] = mer_data['Table Number'].astype(str)
    else:
        mer_data = pd.DataFrame(columns=["Table Number", "IROWS"] + list(range(start_year, end_year + 1)))
    
    # set the index for the mer_data and sort the index for efficient lexsort
    mer_data_indexed = mer_data.set_index(["Table Number", "IROWS"])
    mer_data_indexed = mer_data_indexed.sort_index()

    # --- Main Logic: Iterate through d_base_fixed and apply rules ---
    for outer_key in d_base_fixed.keys():
        # Only process keys that are in the "TN XXX" format
        if outer_key.startswith("TN ") and outer_key[3:].isdigit():
            tn_num_str = str(int(outer_key.split(" ")[1])) # e.g., "TN 001" -> "1"

            current_tn_histcols = histcols_base[histcols_base["Table Number"] == tn_num_str].copy()

            # Identify IROWS that should *prevent* zeroing out for this TN: History_Numeric is 1 or 2.
            irows_to_keep = set(
                current_tn_histcols[
                    (current_tn_histcols['History_Numeric'] == 1) |
                    (current_tn_histcols['History_Numeric'] == 2)
                ]["IROWS"].unique()
            )

            # Identify IROWS that should be replaced with MER data (History_Numeric == 2)
            irows_to_replace = set(
                current_tn_histcols[
                    current_tn_histcols['History_Numeric'] == 2
                ]["IROWS"].unique()
            )

            # Get the IROWS associated with this outer_key in d_base_fixed
            histcols_tablenum = histcols_all_data[histcols_all_data["Table Number"] == tn_num_str]
            d_base_fixed_irows = set(histcols_tablenum.get("IROWS", []))
            d_base_fixed_irows_num = {item for item in d_base_fixed_irows if isinstance(item, str) and item.isdigit()}

            # Identify IROWS in d_base_fixed[outer_key] that are NOT in irows_to_keep.
            # These are the ones that will be zeroed out.
            irows_to_zero_out = d_base_fixed_irows_num - irows_to_keep

            # --- Apply the zeroing logic ---
            if irows_to_zero_out:
                for year in range(start_year, end_year + 1):
                    if year in d_base_fixed[outer_key]: # Check if the year exists in d_base_fixed for this outer_key
                        for irow in irows_to_zero_out:
                            if int(irow) in d_base_fixed[outer_key][year]:
                                # Check if the IROW exists for this year
                                d_base_fixed[outer_key][year][int(irow)] = 0

            # --- Apply MER replacement logic for History_Numeric == 2 ---
            if irows_to_replace:
                for year in range(start_year, end_year + 1):
                    if year in d_base_fixed[outer_key]:
                        # Check if the year exists in d_base_fixed for this outer_key
                        for irow_replace in irows_to_replace:
                            if int(irow_replace) in d_base_fixed[outer_key][year]:
                                # Check if the IROW exists for this year
                                # Check if the (Table Number, IROWS) combination exists in our prepared mer_data_indexed
                                if (tn_num_str, irow_replace) in mer_data_indexed.index:
                                    # get the mer value from the data indexed using the table number and IROWS for the year
                                    mer_value = mer_data_indexed.loc[(tn_num_str, irow_replace), year]
                                    # check if region 11, national total, is in the table, else the regions are all 0
                                    # NOTE: the mer_value is using the [0] due to some DRNAME use the same IROWS, causing 2 values to given when doing .loc
                                    if 11 in d_base_fixed[outer_key][year][int(irow_replace)]:
                                        df_regional_total = sum(d_base_fixed[outer_key][year][int(irow_replace)][0:9])
                                        # loop through each of the regions to calculate the weight of the region to adjust the mer_value
                                        for i in range(1, len(d_base_fixed[outer_key][year][int(irow_replace)]-1)):
                                            d_base_fixed[outer_key][year][int(irow_replace)][i] = mer_value[0] * (d_base_fixed[outer_key][year][int(irow_replace)][i]/df_regional_total)
                                        # set the mer_value as the national
                                        d_base_fixed[outer_key][year][int(irow_replace)][11] = mer_value[0]
                                    else:
                                        d_base_fixed[outer_key][year][int(irow_replace)] = mer_value[0]
                                else:
                                    # Warning: MER data combination not found in processed `mer_data`
                                    # Suppress for cleaner function output, or implement logging
                                    print(f"can't write {outer_key}, IROW:{irow_replace}")
                            else:
                                print(f"can't write {outer_key}, IROW:{irow_replace}")
                    else: 
                        print(f"can't write {year}")

    return d_base_fixed