""" SZO updated on 7/22/2024 to handle multi-block issues
"""

import os
import numpy as np
import pandas as pd

# import time
# from RW_preprocessor_sme import preprocessor_sme

from RW_debug import main_logger, log_execution

# main_logger = setup_logging('main', 'RW_log.log', int(config.get('debugging', 'debug_screen')) == 1)


# @log_execution(main_logger, message='read_var')
def read_var(varname, dat, variablelisting):

    # my_var = varname
    # common_block = variablelisting.loc[my_var.upper(), 'Common Block Name']
    # my_dim = variablelisting.loc[my_var.upper(), 'Dimension Params']
    # my_dim_list = variablelisting.loc[my_var.upper(), 'Extra Indeces']
    # # another approach: my_dim_list = varrange.loc[my_var.upper()]

    # Mike modified on 1/5/2024
    my_var = varname
    temp = variablelisting.loc[my_var.upper(), "Common Block Name"]
    if isinstance(temp, str):
        common_block = temp
        my_dim = variablelisting.loc[my_var.upper(), "Dimension Params"]
        my_dim_list = variablelisting.loc[my_var.upper(), "Extra Indeces"]
    else:
        # print(f'multiple common blocks for: {varname}')
        # iloc[0] for P variables which are in two common blocks
        common_block = temp.iloc[0]
        my_dim = variablelisting.loc[my_var.upper(), "Dimension Params"].iloc[0]
        my_dim_list = variablelisting.loc[my_var.upper(), "Extra Indeces"].iloc[0]
    assert isinstance(common_block, str)

    if len(my_dim) == 1:
        df = dat[f"{common_block}/{my_var.lower()}"]
        df.index += 1
        df = df.reset_index().rename(columns={"index": my_dim[0]})

    elif len(my_dim) == 2:
        df = dat[f"{common_block}/{my_var.lower()}"]
        df.index += 1
        df.columns += 1
        df = df.reset_index().rename(columns={"index": my_dim[0]})
        df = df.set_index(my_dim[:-1]).reset_index()

    elif len(my_dim) == 3:
        L = []
        for i, j in enumerate(my_dim_list[0]):
            df = dat[f"{common_block}/{my_var.lower()}/{i}"]
            df.index += 1
            df.columns += 1
            df = df.reset_index().rename(columns={"index": my_dim[1]})
            df.loc[:, my_dim[0]] = i + 1
            L.append(df.copy())
        df = pd.concat(L).set_index(my_dim[:-1]).reset_index()

    elif len(my_dim) == 4:
        L = []
        for i, j in enumerate(my_dim_list[0]):
            for k, m in enumerate(my_dim_list[1]):
                # print(j, m)
                df = dat[f"{common_block}/{my_var.lower()}/{i}/{k}"]
                df.index += 1
                df.columns += 1
                df = df.reset_index().rename(columns={"index": my_dim[2]})
                df.loc[:, my_dim[0]] = i + 1
                df.loc[:, my_dim[1]] = k + 1
                L.append(df.copy())
        df = pd.concat(L).set_index(my_dim[:-1]).reset_index()

    # ---------
    # construct new dataframe with:
    #     convert non-year index items to strings;
    #     convert years from 1-based to 1990-based
    my_dim_dict = {}
    for i, j in enumerate(my_dim):
        my_dim_dict[j] = {j + 1: my_dim_list[i][j] for j in range(len(my_dim_list[i]))}

    df2 = df.copy()
    if len(my_dim) == 1:
        i = my_dim[0]
        df2[i] = df2[i].apply(lambda x: my_dim_dict[i][x])
        df2 = df2.rename(columns=my_dim_dict[my_dim[0]])
    else:
        for i in my_dim[:-1]:
            df2[i] = df2[i].apply(lambda x: my_dim_dict[i][x])
        df2 = df2.rename(columns=my_dim_dict[my_dim[-1]])

    # --------
    # prefix integer column names with 'M'
    if len(my_dim) == 1:
        i = my_dim[0]
        if str(i).isdigit():
            temp = f"M{i}"
            df = df.rename(columns={i: f"M{i}"})
        else:
            temp = i
        df = df.set_index(temp)
        df2 = df2.set_index(my_dim[0])

    elif len(my_dim) > 1:
        temp = []
        for i in my_dim[:-1]:
            if str(i).isdigit():
                temp.append(f"M{i}")
                df = df.rename(columns={i: f"M{i}"})
            else:
                temp.append(i)
        # df = df.set_index(my_dim[: -1])
        df = df.set_index(temp)
        df2 = df2.set_index(my_dim[:-1])

        if str(my_dim[-1]).isdigit():
            temp = f"M{str(my_dim[-1])}"
        else:
            temp = str(my_dim[-1])
        df.columns.name = temp
        df2.columns.name = temp
    # --------

    # return raw dataframe and 'prettified' dataframe
    return df, df2


@log_execution(main_logger, message="read_stuff")
def read_stuff(dat, my_vars):
    variablelisting = dat["_variablelisting"]

    # #for varname in nems_vars['sup_prd']['v']:
    # for varname in my_vars:
    #     df = read_var(varname, dat, variablelisting)
    #     dfd[varname] = df.copy()
    dfd = {}
    for varname in my_vars:
        # print(varname)
        try:
            df, df2 = read_var(varname, dat, variablelisting)
            dfd[varname] = df.copy()
        # except:
        #     # Ignore dimensions, constant variables, and values
        #     print(f'varname==========: {varname}')
        except Exception as e:
            # Stop execution if the variable is not available in restart
            raise Exception(f"Error reading variable '{varname}': {str(e)}")

    # if table has dimension >= 2 and MNUMYR is an elemment, then make MNUMYR the columns
    # note: need a nonsense string that is guaranetted not to be in the dimension elements
    for k in dfd.keys():
        my_dim = variablelisting.loc[k, "Dimension Params"]
        x = len(my_dim)
        if "MNUMYR" in my_dim:
            if x == 1:
                dfd[k] = dfd[k].T

            elif (x == 2) and (my_dim[-1] != "MNUMYR"):
                dfd[k] = dfd[k].T

            elif (x > 2) and (my_dim[-1] != "MNUMYR"):
                nonsense_string = "nonsense_" + "_".join(my_dim)
                z = dfd[k].copy()
                df_long = pd.melt(
                    z.reset_index(),
                    id_vars=z.index.names,
                    value_vars=z.columns,
                    value_name=nonsense_string,
                )
                my_index = [
                    i for i in df_long.columns if i not in ["MNUMYR", nonsense_string]
                ]
                dfd[k] = pd.pivot_table(
                    df_long,
                    values=nonsense_string,
                    index=my_index,
                    columns=["MNUMYR"],
                    aggfunc="sum",
                )

            else:
                # x >= 2, but MNUMYR is in the columns (last dimension element)
                pass

    """# Moved to preprocessor"""
    # # Read coefficients
    # RW_coefficients_path = os.path.join(os.getcwd(), 'input', 'RW_coefficients.csv')
    # df = pd.read_csv(RW_coefficients_path).set_index('name')
    # df.columns = df.columns.str.strip()
    # d_coef = df.T.to_dict()
    # d_coef = {key: value['value'] for key, value in d_coef.items()}
    # dfd.update(d_coef)

    # # Populate dfd with SME calculation results
    # dfd = preprocessor_sme(dfd, table_id)

    # dat.close()

    return dfd


# parse_parametr_include_file---------------------------------------------------
@log_execution(main_logger, message="parse_parameter_file")
def parse_parameter_file(f_in):  # ='parametr.txt'):
    parametr_include_file = f_in

    with open(parametr_include_file) as file:
        lines = [line.strip() for line in file]

    d = {}
    for i in lines:
        if i.startswith("PARAMETER("):
            z = i[i.find("(") + 1 : i.find(")")].split("=")
            try:
                # print(i)
                d.update({z[0]: int(z[1])})
            except:

                s = z[1].split("+")
                temp = []
                for j in s:
                    if not j.isdigit():
                        temp.append(str(d[j]))
                    else:
                        temp.append(j)
                # eval should be safe here, right?
                d.update({z[0]: eval("+".join(temp))})

    return d


@log_execution(main_logger, message="format_path_csv")
def get_table_format_csv(format_path_csv):
    """Read the 'layin.csv' file and perform column renaming and manipulation.

    1/30/2024 update:
        Add additional columns & rows to df_format to provide enough table info to generate RAN
            cols: 'Ftab Code for this row'
            rows: include 'all rows' defined by RAN's 'RTTRow'

            These cols and rows will be filted out later to generate formatted tables and all_row_csv

    Parameters
    ----------
    format_path_csv : str
        Path to the csv file.

    Returns
    -------
    pd.DataFrame
        DataFrame containing table format spec.
    """

    main_logger.info("\n   ------- get_table_format_csv")

    # 'useful' columns and their new names
    cols = {
        "CM": "CM",
        "Row Label": "Label",
        "Table Number": "TableNumber",
        "IROWS": "IROWS",
        "VarName": "VarName",
        "DRNAME": "VarNam2",
        "Graph Label": "GLabel",
        "Units String": "Gunits",
        "ROWFMT": "RowFmt",
        "Data Type (long)": "DaType",
        "Sub Data Type (long)": "SubDat",
        "Sector (long)": "Sector",
        "Sub Sector (long)": "SubSec",
        "Source (long)": "Source",
        "Sub Source (long)": "SubSrc",
        "Geography (long)": "Geography Override",
        "Geography": "Geogr",
        "DRForm": "DRForm",  # Added for RAN 'RGForm'
        "Var units": "Var units",
        #'Ftab Code for this row': 'Expression',
        # Additional columns to distinish from the above - not used
        "Data Type": "DaType_short",
        "Sub Data Type": "SubDat_short",
        "Sector": "Sector_short",
        "Sub Sector": "SubSec_short",
        "Source": "Source_short",
        "Sub Source": "SubSrc_short",
    }

    df_format = pd.read_csv(format_path_csv).fillna("").rename(columns=cols)

    for i in df_format.index:
        try:
            df_format.loc[i, "TableNumber"] = int(df_format.loc[i, "TableNumber"])
        except:
            pass

    # Fix df_format: get useful columns only
    df_format = df_format[cols.values()].copy()

    # Insert uid columns
    df_format.insert(0, "uid", "")

    # Select useful rows
    df_format = df_format[
        df_format["CM"].isin(["RN", "TF", "HD", "RH", "RL", "FN", "TN"])
    ]

    # Remove blank table numbers
    table_numbers = list(filter(lambda x: x != "", df_format["TableNumber"].unique()))
    for i in table_numbers:
        a = df_format[df_format["TableNumber"] == i]

        # Some tables are blank !!!
        if a.shape[0] > 1:
            # create and fill 'table_name' column
            table_name = a.loc[a["CM"] == "RN", "Label"].iloc[0]

            df_format.loc[a.index, "table_name"] = table_name

            # fill any blanks in the 'ROWFMT' column with the table's default row-format
            if "TF" in a["CM"].unique():
                rowfmt = a.loc[a["CM"] == "TF", "Label"].iloc[0]
            else:
                # print(f'No TF specifed for table: {i}. Using 2 as a default for now.')
                rowfmt = 2

            # Updated on 5/1/2004
            # b = a[(a['VarNam2'] != '') & (a['RowFmt'] == '')]
            b = a[a["RowFmt"] == ""]  # RL with no VarName should not excluded

            df_format.loc[b.index, "RowFmt"] = rowfmt

            # Generate uid for data rows only
            df_format.loc[b.index, "uid"] = (
                df_format.loc[b.index, "table_name"]
                + ":"
                + df_format.loc[b.index, "VarNam2"]
            )

            df_format.loc[b.index, "uid"] = df_format.loc[b.index, "VarNam2"]

            df_format.loc[df_format["VarNam2"] == "", "uid"] = ""

            # Generate RowNum for RL rows - needed by all row csv file
            df_format.loc[a.index, "RowNum"] = (
                np.where(df_format.loc[a.index, "CM"] == "RL", 1, 0)
                .cumsum()
                .astype(int)
            )
        else:  # blank tables
            pass

    # Insert a new column RegionNum as the 3rd column
    df_format.insert(2, "RegionNum", 0)

    # Update on 4/29/2024 to work with RAN
    df_format["Expression"] = ""
    # df_format.to_excel('df_get_table_format_csv.xlsx')

    return df_format


@log_execution(main_logger, message="get_table_format")
def get_table_format(format_path):
    """Read the 'layin.xls' file and perform column renaming and manipulation.

    1/30/2024 update:
        Add additional columns & rows to df_format to provide enough table info to generate RAN
            cols: 'Ftab Code for this row'
            rows: include 'all rows' defined by RAN's 'RTTRow'

            These cols and rows will be filted out later to generate formatted tables and all_row_csv

    Parameters
    ----------
    format_path : str
        Path to the Excel file.

    Returns
    -------
    pd.DataFrame
        DataFrame containing table format spec.

    # Note
    # ----
    # For now, we will read in the 'Fortran-related' layin.xls file
    # Eventually, that file will be replaced by something more 'Python-related'

    """

    main_logger.info("\n---------- get_table_format")

    # Select, rename, and reorder columns for foramtted tables and all row csv file
    cols = {
        "CM": "CM",
        "Row Label:34 Chars in text output, 47 Chars Max": "Label",
        "Table Number": "TableNumber",
        "IROWS": "IROWS",
        "VarName": "VarName",
        "DRNAME: 16 Chars": "VarNam2",
        "Graph Label.  Suggested Format: {Table Subject : Row Heading : Subheading : Rowlabel}": "GLabel",
        "Units String": "Gunits",
        "ROWFMT": "RowFmt",
        "Data Type": "DaType",
        "Sub Data Type": "SubDat",
        "Sector": "Sector",
        "Sub Sector": "SubSec",
        "Source": "Source",
        "Sub Source": "SubSrc",
        "Geography Override": "Geography Override",
        "Geogr": "Geogr",
        "DRForm": "DRForm",  # Added for RAN 'RGForm'
        "Var units": "Var units",
        "Ftab Code for this row": "Expression",
    }

    df_format = (
        pd.read_excel(format_path, sheet_name="layin").fillna("").rename(columns=cols)
    )

    # Fix df_format: get useful columns only
    df_format = df_format[cols.values()].copy()

    # Insert uid columns
    df_format.insert(0, "uid", "")

    # Select rows based on the setting of spreadsheet column A
    df_format = df_format[
        df_format["CM"].isin(["RN", "TF", "HD", "RH", "RL", "FN", "TN"])
    ]

    # Remove blank table numbers
    table_numbers = list(filter(lambda x: x != "", df_format["TableNumber"].unique()))
    for i in table_numbers:
        a = df_format[df_format["TableNumber"] == i]

        # Some tables are blank !!!
        if a.shape[0] > 1:
            # create and fill 'table_name' column
            table_name = a.loc[a["CM"] == "RN", "Label"].iloc[0]

            df_format.loc[a.index, "table_name"] = table_name

            # fill any blanks in the 'ROWFMT' column with the table's default row-format
            if "TF" in a["CM"].unique():
                rowfmt = a.loc[a["CM"] == "TF", "Label"].iloc[0]
            else:
                # print(f'No TF specifed for table: {i}. Using 2 as a default for now.')
                rowfmt = 2

            # Updated on 5/1/2004
            # b = a[(a['VarNam2'] != '') & (a['RowFmt'] == '')]
            b = a[a["RowFmt"] == ""]  # RL with no VarName should not excluded

            df_format.loc[b.index, "RowFmt"] = rowfmt

            # Generate uid for data rows only
            df_format.loc[b.index, "uid"] = (
                df_format.loc[b.index, "table_name"]
                + ":"
                + df_format.loc[b.index, "VarNam2"]
            )

            df_format.loc[b.index, "uid"] = df_format.loc[b.index, "VarNam2"]

            df_format.loc[df_format["VarNam2"] == "", "uid"] = ""

            # Generate RowNum for RL rows - needed by all row csv file
            df_format.loc[a.index, "RowNum"] = (
                np.where(df_format.loc[a.index, "CM"] == "RL", 1, 0)
                .cumsum()
                .astype(int)
            )
        else:  # blank tables
            pass

    # Insert a new column RegionNum as the 3rd column
    df_format.insert(2, "RegionNum", 0)

    return df_format


@log_execution(main_logger, message="normalize_format")
def normalize_format(table_info, HD_num=3):
    """
    Aligns the tables specified in the layin so that each of them have
    the same "structure" of HD row (HD_num=3), and RH row (one):

        Insert new HD rows to a table if it has less than HD_num (3)
        Insert a RH row to a table if it doesn't have an RH row

    And set the level as follows:

        HD1: 1
        HD2: 2
        HD3: 3
        RH: 4

    Parameters
    ----------
    table_info : pandas.DataFrame
        The table information DataFrame.
    HD_num : int
        The number of HD rows required for each table.

    Returns
    -------
    pandas.DataFrame
        The updated table information DataFrame.

    Note
    ----
    Tables specified in layin have various counts of HD rows (1~2), and RH row (0~1).
    This function returns a updated layin df with 3 HD rows and one RH rows for every table.
    Normalize/Standardize tables is important for generating longthin files and RAN file.
    """

    main_logger.info("\n   ------- normalize_format")

    df = table_info
    df["Level"] = 0  #''
    not_blank_mask = df["TableNumber"].str.strip().ne("")
    df = df[not_blank_mask]
    # Group the DataFrame by 'TableNumber'
    # grouped = df.groupby('TableNumber').filter(lambda x: x['TableNumber'].any() != 0)
    grouped = df.groupby("TableNumber")

    # New DataFrame to store the updated rows
    df_norm = pd.DataFrame(columns=df.columns)

    # Iterate over each group
    for _, group in grouped:
        table_num = group["TableNumber"].iloc[0]

        # Get RN, TF rows
        rn_tf_row = group[(group["CM"] == "RN") | (group["CM"] == "TF")]

        # Check the number of 'HD' rows in the group
        hd_count = group["CM"].eq("HD").sum()

        if hd_count < HD_num:  # 3:

            # Calculate the number of rows to insert
            rows_to_insert = HD_num - hd_count

            # Get the indices of the 'HD' rows
            hd_indices = group.index[group["CM"] == "HD"]

            # Find the last 'HD' row and its following non-'HD' rows
            last_hd_index = hd_indices[-1]
            # following_non_hd_indices = group.index[(group.index > last_hd_index)]

            # Insert new HD rows between the last 'HD' row and its following non-'HD' rows
            new_hd_rows = pd.DataFrame(
                {
                    "TableNumber": [table_num] * rows_to_insert,
                    "CM": ["HD"] * rows_to_insert,
                }
            )
            hd_3rows = pd.concat([group.loc[hd_indices], new_hd_rows]).reset_index(
                drop=True
            )
        else:
            hd_3rows = group[group["CM"] == "HD"].copy()

        # Set DRForm to HD rows for def add_style
        #   hard-code bold blue font for table captions
        #   character 3: size, character 4: color (b: blue)
        hd_3rows.loc[hd_3rows.index[0], "DRForm"] = "B07000"
        hd_3rows.loc[hd_3rows.index[1], "DRForm"] = "p05000"
        hd_3rows.loc[hd_3rows.index[2], "DRForm"] = "p05000"

        # Also check 'RH' row
        rh_index = group.index[group["CM"] == "RH"]
        if rh_index.empty:
            # If no 'RH' rows in the group, insert a new row after HD rows
            rh_row = pd.DataFrame({"TableNumber": [table_num], "CM": ["RH"]})

        else:
            rh_row = group[group["CM"] == "RH"].copy()

        # Set DRForm to RH row for def add_style
        #   character 3: size, character 4: color(0: black)
        rh_row.loc["DRForm"] = "B05000"

        following_non_hd_rh_indices = group.index[
            (group["CM"] != "N")
            & (group["CM"] != "TF")
            & (group["CM"] != "HD")
            & (group["CM"] != "RH")
        ]
        # Assign 'Level' to HD and RH
        #     1: HD row 1
        #     2: HD row 2
        #     3: HD row 3
        #     4: RH row
        hd_3rows["Level"] = hd_3rows.reset_index().index + 1
        rh_row["Level"] = rh_row.reset_index().index + 4

        df_norm = pd.concat(
            [
                df_norm,
                rn_tf_row,
                hd_3rows,
                rh_row,
                # new_rh_row,
                group.loc[following_non_hd_rh_indices],
            ]
        ).reset_index(drop=True)

    # # Concatenate the original rows that are not in any group
    # remaining_rows = df[~df.index.isin(df_norm.index)]
    # df_norm = pd.concat([df_norm, remaining_rows]).reset_index(drop=True)

    df_norm = df_norm.fillna("")
    # df_norm.to_excel('normalized.xlsx')

    # df_norm.to_excel('df_normalize_format.xlsx')

    return df_norm  # table_info


@log_execution(main_logger, message="insert_citation")
def insert_citation(df_format, citation_path,user):
    """
    Inserts citation values from a text file into the 'Label' and 'Gunits' columns of the input DataFrame.
    Replaces '####' with the dollar year of 'main_historical_year'.

    Parameters
    ----------
    df_format : pandas.DataFrame
        The DataFrame to modify.
    citation_path : str
        The file path to the citation text file.

    Returns
    -------
    pandas.DataFrame
        The modified DataFrame with citations inserted.
    """

    # df = pd.read_csv(citation_path, delimiter='\t')
    # Fixed to get first row "AEO_ID","AEO2023",AEO Identifier
    df = pd.read_csv(citation_path, delimiter="\t", header=None)

    # Add a new row scenario_and_datekey to the last of the DataFrame
    # s = 'scenario_and_datekey,' + '"' + scen_date + '"'
    # df.loc[len(df)] = [s]
    for index, row in df.iterrows():
        # Split the string into three substrings
        #   e.g., 'm__m, "2012", Macro base year dollars (GDP)'
        parts = row.iloc[0].split('"')

        # Get citation and value
        citiation = parts[0][0:-1]
        value = parts[1]

        # Updated on 7/23/2024
        # Get Macro base year dollars (GDP)
        if citiation == "m__m":
            MACYR = int(value)

        df_format["Label"] = df_format["Label"].str.replace(citiation, value)
        df_format["Label"] = df_format["Label"].str.replace("scenario_and_datekey", user.ScenDate)
        df_format["Gunits"] = df_format["Gunits"].str.replace(citiation, value)

    # Replace '####' with dollar year of 'main_historical_year'
    #  e.g., '"main_historical_year","2022",main historical year'
    main_hist_year = (
        df[df.iloc[:, 0].str.contains("main_historical_year")]
        .iloc[0]
        .iloc[0]
        .split('"')[1]
    )
    df_format["Label"] = df_format["Label"].str.replace("####", main_hist_year)
    df_format["Gunits"] = df_format["Gunits"].str.replace("####", main_hist_year)

    return df_format, MACYR


def indcarb(dfd, con, CARBON_OR_2):

    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"] if CARBON_OR_2 == "CO2" else 1

    # Emission factors by fuel
    emis = {
        # 1: dfd['EM_ELEC'].loc[9,11] / dfd['QELAS'].loc[11] * C_CO2_FACTOR,
        2: dfd["ENGIN"] * C_CO2_FACTOR,
        3: dfd["ECLIN"] * C_CO2_FACTOR,
        4: dfd["EMCIN"] * C_CO2_FACTOR,
        5: dfd["ECLIN"] * C_CO2_FACTOR,
        6: dfd["ERLIN"] * C_CO2_FACTOR,
        7: dfd["EDSIN"] * C_CO2_FACTOR,
        8: dfd["ELGIN"] * C_CO2_FACTOR,
        9: dfd["EMGIN"] * C_CO2_FACTOR,
        10: dfd["ESGIN"] * C_CO2_FACTOR,
        11: dfd["EPCIN"] * C_CO2_FACTOR,
        12: dfd["EPFIN"] * 0,  # Dummy variable
        13: dfd["EPFIN"] * C_CO2_FACTOR,
        14: dfd["EKSIN"] * C_CO2_FACTOR,
        15: dfd["EOTIN"] * C_CO2_FACTOR,
        16: dfd["ENQNGPF"] * C_CO2_FACTOR,
        17: dfd["ENQLGPF"] * C_CO2_FACTOR,
        18: dfd["ENQLGPF"] * 0,  # Dummy variable
    }
    # Initialize carbon emissions total
    indcarb_total = 0.0

    # Calculation loop for fuels 2 to 18
    for i in range(2, 19):
        # INDUSTRY CONSUMPTION, 18 Fuels, Regions 1:4 and US:5
        # indcarb_total += con.loc[i, 5] * emis[i] * dfd["TRIL_TO_QUAD_rwpre"]
        series = con.loc[i].loc[5].copy()
        series.index = emis[i].index
        indcarb_total += series * emis[i] * dfd["TRIL_TO_QUAD_rwpre"]

    # Calculate electricity emissions on a census region basis
    divs = [
        (1, 2),
        (3, 4),
        (5, 7),
        (8, 9),
    ]  # Starting and ending census division numbers

    for ir, (DS, DE) in enumerate(divs, start=1):

        emis[1] = (
            dfd["EM_ELEC"].loc[9, DS : DE + 1, :].sum()
            / dfd["QELAS"].loc[DS : DE + 1].sum()
            * C_CO2_FACTOR
        )
        series = con.loc[1, ir]
        series.index = emis[1].index
        indcarb_total += series * emis[1] * dfd["TRIL_TO_QUAD_rwpre"]

    return indcarb_total


def sum_CarbCap_vars(dfd):
    # FOR AEO2025, fix any national totals that are missing. Remove for AEO2026
    try:
        dfd['CC_INDY'].loc[(slice(None), 11), :] = (
            dfd['CC_INDY'].loc[(slice(None), slice(1, 9)), :].groupby('M10').sum()
            .values  # Circumvent misaligned dataframe indices
        )
        dfd['CC_ELEC'].loc[(slice(None), 11), :] = (
            dfd['CC_ELEC'].loc[(slice(None), slice(1, 9)), :].groupby('M5').sum()
            .values  # Circumvent misaligned dataframe indices
        )
    except KeyError:
        pass
    return dfd