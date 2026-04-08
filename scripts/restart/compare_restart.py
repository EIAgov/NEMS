# -*- coding: utf-8 -*-
"""
Created Mar 04 2025
Modified July 10 2025 

@author: Claire Su
@author: Greg Miller

This module provides functions for loading, comparing, and reporting differences
between two .npz restart files. It can be run directly from the command line
or imported into other scripts (e.g., a GUI).
"""
import os
import argparse
import sys
import numpy as np

DEFAULT_TOLERANCE       = 0.0001
DEFAULT_OUTPUT_DIR      = ".\\output"
DEFAULT_REPORT_FILENAME = "result_report.csv"

def get_report_path(output_dir=DEFAULT_OUTPUT_DIR, filename=DEFAULT_REPORT_FILENAME):
    """
    Constructs and prepares the path for the output report file.

    Ensures that the specified output directory exists, creating it if necessary.

    Parameters
    ----------
    output_dir : str, optional
        The directory where the report file will be saved.
        Defaults to `DEFAULT_OUTPUT_DIR`.
    filename : str, optional
        The name of the report file. Defaults to `DEFAULT_REPORT_FILENAME`.

    Returns
    -------
    str
        The absolute path to the report file.
    """
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    return os.path.join(output_dir, filename)

def get_npz_files(input_dir):
    """
    Retrieves a list of .npz files from the specified input directory and its subfolders.

    Parameters
    ----------
    input_dir : str
        The path to the directory to search for .npz files.

    Returns
    -------
    list of str
        A list of absolute paths to the discovered .npz files.

    Raises
    ------
    FileNotFoundError
        If the specified `input_dir` does not exist.
    ValueError
        If no .npz files are found within the `input_dir`.
    """
    npz_files = []
    if not os.path.exists(input_dir):
        raise FileNotFoundError(f"Input directory not found: {input_dir}")
    for root, _, files in os.walk(input_dir):
        for file in files:
            if file.endswith('.npz'):
                npz_files.append(os.path.join(root, file))
    if len(npz_files) <= 0:
        raise ValueError(f"No NPZ files found. ({len(npz_files)})")
    else:
        return npz_files

def load_npz_data(npz_file):
    """
    Loads data from a .npz file into a dictionary.

    Parameters
    ----------
    npz_file : str
        The absolute path to the .npz file.

    Returns
    -------
    dict
        A dictionary containing the data loaded from the .npz file.

    Raises
    ------
    ValueError
        If the .npz file cannot be loaded (e.g., file corruption, incorrect format).
    """
    try:
        npz_data = dict(np.load(npz_file, allow_pickle=True))
        return npz_data
    except Exception as e:
        raise ValueError(f"Failed to load NPZ data from file: {npz_file}.`r`n{e}")

def do_comparison(npz_data1, npz_data2, tolerance=DEFAULT_TOLERANCE):
    """
    Compare 2 one or multi-dimensional arrays element-wise while ignoring NaN and infinity 
    values, and identify indices where the relative difference between corresponding elements
    exceeds the given tolerance.

    Parameters
    ----------
    npz_data1 : numpy.ndarray
        First numerical data array
    npz_data2 : numpy.ndarray
        Second numerical data array
    tolerance : float, optional
        Tolerance threshold for detecting significant differences.
        Defaults to `DEFAULT_TOLERANCE`.

    Returns
    -------
    tuple of list of list
        A tuple of two lists:
        - The first list will contain indices where `npz_data1 / npz_data2` 
            difference exceeds tolerance.
        - The second list will contain indices where `npz_data2 / npz_data1` 
            difference exceeds tolerance.
    """
    # Initialize ratio arrays with ones to prevent unintended bias in calculations
    ratio1 = np.ones_like(npz_data1, dtype=np.float64)
    ratio2 = np.ones_like(npz_data2, dtype=np.float64)

    # Create a mask to ignore NaN and inf values 
    # i.e. only compare finite values. When a value is NaN or inf, isfinite() returns False)
    valid_mask = (np.isfinite(npz_data1) & np.isfinite(npz_data2))

    # further mask to avoid division by zero
    mask1 = valid_mask & (npz_data1 != 0)
    mask2 = valid_mask & (npz_data2 != 0)

    # compute saf division where valid
    ratio1[mask1] = npz_data1[mask1] / npz_data2[mask1]
    ratio2[mask2] = npz_data2[mask2] / npz_data1[mask2]

    # Find indices where abs(ratio - 1) > tol
    diff1 = np.argwhere(np.abs(ratio1 - 1) > tolerance)
    diff2 = np.argwhere(np.abs(ratio2 - 1) > tolerance)
    return diff1.tolist(), diff2.tolist()

def get_diff_list(npz_data1, npz_data2, tolerance=DEFAULT_TOLERANCE, vars=[]):
    """
    Compares numerical arrays within two NPZ datasets and identifies differing variables.

    The function iterates over all numerical keys, excluding keys beginning with '_' (metadata).
    Calls `do_comparison()` to find elements with significant differences, recording the 
    variable name when a discrepancy is found. The function also identifies if a key
    is missing or the data has a shape mismatch.

    Iterates over all numerical keys (excluding metadata keys starting with "_")
    in the datasets. Calls `do_comparison()` to find elements with significant
    differences and records the variable names where discrepancies exist.
    Also flags `MISSING_IN_SECOND_FILE` and `SHAPE_MISMATCH` issues.

    Parameters
    ----------
    npz_data1 : dict
        The first dataset loaded from an .npz file.
    npz_data2 : dict
        The second dataset loaded from an .npz file.
    tolerance : float, optional
        The tolerance threshold for detecting significant numerical differences.
        Defaults to `DEFAULT_TOLERANCE`.
    vars      : list
        A list of vars given to search if the detailed_flag is True.

    Returns
    -------
    problem_keys  : list of str
        A list of variable names (or descriptive tags like "MISSING_IN_SECOND_FILE/var")
        where differences or structural issues exceed the specified tolerance.
    array_cords   : list of str
        A list of array loc of the problem keys.
    calc_string   : list of str
        A list of strings that shows the values of the problem keys from the two restarts.
    diff_value    : list
        A list of differences from the comparison.

    Notes
    -------
    Retained code for debugging
    Paste below problem_keys.append() (around line 155)
    '''
    # DO NOT delete. Code for debug to check if the key's value is processed:
    # 1. using warnings.catch_warnings to catch and handle the warning
    import warnings
    with warnings.catch_warnings(record=True) as caught_warnings:
        warnings.simplefilter("always", RuntimeWarning)  # Ensure warnings are always shown
        # now execute the code below to trigger the warning
        diff1, diff2 = do_comparison(npz_data1[i], npz_data2[i], tol)
        if caught_warnings:
            print("Caught a warning:", caught_warnings[0].message)
            print(i)    #MXPBLK/xpalmg, WRENEW/wwntd
        else:
            continue
    # 2. catch ZeroDivisionError. The except condition will not hit but I backup the code for future troubleshoots
    try:
        diff1, diff2 = do_comparison(npz_data1[i], npz_data2[i], tol)
    except ZeroDivisionError:
        print(i)
    '''
    """
    # Exclude npz_data1['_variablelisting'] and sort the keys
    ls = [i for i in npz_data1.keys() if not i.startswith("_")]
    ls.sort()
    
    if not vars:  # Checks if vars_list is empty
        print("Variable list is empty, skip filtering.")
        pass
    else:
        print("Filtering data based on provided variables.")
        filtered_items = []
        # lowercase the ls and vars for filter
        ls_lower = [i.lower() if isinstance(i, str) else i for i in ls]
        vars = [i.lower() if isinstance(i, str) else i for i in vars]
        filtered_items=[]
        for var in vars:
            if var == "":
                pass
            else:
                if any(var in item for item in ls_lower):
                    for k in range(len(ls_lower)):
                        if ls_lower[k].startswith(var):
                            filtered_items.append(ls[k])
                else:
                    print("variable not found in the variable listing:", var) 
        ls = filtered_items

    # Iterate througth the key fuel variables and check
    problem_keys = []
    array_cords = []
    calc_string = []
    diff_value = []
    for i in ls:
        # Skip non-numeric (skip string, boolean) data type arrays
        if not np.issubdtype(npz_data1[i].dtype, np.number):
            continue

        # check element in both
        if i not in npz_data2:
            problem_keys.append(f"MISSING_IN_SECOND_FILE/{i}")
            continue

        # check array shape matches
        if npz_data1[i].shape != npz_data2[i].shape:
            problem_keys.append(f"SHAPE_MISMATCH/{i}")
            continue

        # compare the elements
        diff1, diff2 = do_comparison(npz_data1[i], npz_data2[i], tolerance)
        if diff1 or diff2:
            # get the values and the difference 
            for index in diff1:
                npz_data1_val = npz_data1[i][tuple(index)]
                npz_data2_val = npz_data2[i][tuple(index)]
                npz_data_diff_val = npz_data1_val - npz_data2_val
                
                # filter out flat differences that are smaller than 0.0001
                if abs(npz_data_diff_val) > 0.0001:
                    # append the information into a list
                    problem_keys.append(i)
                    array_cords.append('"' + str(index) + '"')
                    calc_string.append(f"({str(npz_data1_val if npz_data1_val else 0)}) - ({str(npz_data2_val if npz_data2_val else 0)})")
                    diff_value.append(npz_data_diff_val)

    return problem_keys, array_cords, calc_string, diff_value
    

def write_report(problem_keys, array_cords, calc_string, diff_value, report_path, detailed_flag = False):
    """
    Generates the output CSV report based on the identified problem keys.

    Writes a header "COMMON_BLOCK,VARIABLE" followed by each problem key,
    attempting to split it into block and variable for structured output.

    Parameters
    ----------
    problem_keys  : list of str
        A list of strings representing the identified differences,
        potentially including paths like "BLOCK/VARIABLE".
    array_cords   : list of str
        A list of array loc of the problem keys.
    calc_string   : list of str
        A list of strings that shows the values of the problem keys from the two restarts.
    diff_value    : list
        A list of differences from the comparison.
    report_path   : str
        The absolute path where the CSV report will be saved.
    detailed_flag : boolean
        A switch for generating detailed comparison.
    """
    if detailed_flag == True:
        report_path = report_path.replace(".csv","_detailed.csv")
    
    with open(report_path, "w", encoding="UTF8") as f:
        if detailed_flag == True:
            f.write(f"COMMON_BLOCK,VARIABLE,Array Loc,Comparison,Difference (tol: {DEFAULT_TOLERANCE*100}%)\n")
            for i in range(len(problem_keys)):
                s_parts = problem_keys[i].split("/")
                if len(s_parts) == 2:
                    f.write(f"{s_parts[0]},{s_parts[1]},{array_cords[i]},{calc_string[i]},{diff_value[i]}\n")
                else:
                    f.write(f"N/A,{i}")
        else:
            f.write(f"COMMON_BLOCK,VARIABLE\n")
            problem_keys = list(set(problem_keys))
            problem_keys.sort()
            for i in range(len(problem_keys)):
                s_parts = problem_keys[i].split("/")
                if len(s_parts) == 2:
                    f.write(f"{s_parts[0]},{s_parts[1]}\n")
                else:
                    f.write(f"N/A,{i}")
    return

def compare_npz_files(file1_path, file2_path, tolerance=DEFAULT_TOLERANCE, vars=[]):
    """
    Drives the comparison process for two specific NPZ files.

    Loads the data, performs the comparison, and returns the list of
    problematic keys and any informational messages.

    Parameters
    ----------
    file1_path : str
        The absolute path to the first .npz file.
    file2_path : str
        The absolute path to the second .npz file.
    tolerance : float, optional
        The numerical tolerance for the comparison. Defaults to `DEFAULT_TOLERANCE`.
    vars      : list
        A list of vars given to search if the detailed_flag is True.

    Returns
    -------
    tuple
        A tuple containing:
        - list of str: `problem_keys` identified during comparison.
        - list of str: `messages` that provide context or status of the comparison.

    Raises
    ------
    ValueError
        If either of the input files are not found or cannot be loaded.
    """
    messages = []

    if not os.path.exists(file1_path):
        raise ValueError(f"First restart file not found: {file1_path}")
    if not os.path.exists(file2_path):
        raise ValueError(f"Second restart file not found: {file2_path}")

    npz_data1 = load_npz_data(file1_path)
    npz_data2 = load_npz_data(file2_path)

    messages.append(f"Comparing: {os.path.basename(file1_path)} \
                   and {os.path.basename(file2_path)}")
    problem_keys, array_cords, calc_string, diff_value  = get_diff_list(npz_data1, npz_data2, tolerance, vars)
    return problem_keys, array_cords, calc_string, diff_value, messages

def select_restart(files):
    """
    Asks the user to pick 2 restarts if there is more than 2 restart files in the input.

    Parameters
    ----------
    files : list of strings 
        list of restart files.
        
    Returns
    -------
    tuple
        A tuple containing:
        - list of str: restart files that the user selected. 
    """
    
    print("Restarts in the inputs folder:")
    for i, filename in enumerate(files):
        print(f"{i+1}. {filename}")
        
    selected_indices = []
    selected_files = []
    while len(selected_indices) < 2:
        try:
            prompt = f"Enter number to corresponding restart {len(selected_indices) + 1} to select (or 'q' to quit): "
            user_input = input(prompt).strip().lower()
            
            if user_input == 'q':
                print("Exiting.")
                sys.exit(1)
            
            choice = int(user_input)
            if 1 <= choice <= len(files):
                if choice in selected_indices:
                    print("This restart has already been selected.")
                else:
                    selected_indices.append(choice)
                    selected_files.append(files[choice - 1])
                    print(f"'{files[choice - 1]}' selected.")
            else:
                print(f"Invalid number. Enter a number between 1 and {len(files)}.")
        except ValueError:
            print("Invalid input. Enter a number or 'q'.")

    return tuple(selected_files)

def enter_vars(detailed_flag):
    """
    If the detailed_flag is true, ask the user a list of variables to view or skip to view all. 

    Parameters
    ----------
    detailed_flag : boolean
        A switch for generating detailed comparison.
        
    Returns
    -------
    variables     : list of strings
        A list of variables generated from the user's input. 
    """ 
    if detailed_flag == True:
        user_input = input("Please enter a list of variables, in COMMON_BLOCK or COMMON_BLOCK/VARIABLE (ex. AB32/ab_allbank_avl) separated by commas (press Enter to skip): ")
        if user_input.strip() == "":
            print("No variables entered.")
            return []
        else:
            variables = [var.strip() for var in user_input.split(',')]
            
            print("You entered the following variables:", variables)
            return variables
    else:
        return []
        
def cli_mode():
    """
    Main function for command-line interface execution.

    Parses command-line arguments, orchestrates the NPZ file comparison,
    and prints results and status messages to the console. This function
    retains the original command-line functionality of the script.
    """
    parser = argparse.ArgumentParser(description="Compares two .npz restart files for numerical" \
    " differences.")

    # -r to specify individual restart files
    parser.add_argument(
        "-r", "--restart_files", dest="restart_files", type=str, nargs='+',
        help="Paths to the two .npz restart files to compare. Example: -r file1.npz file2.npz",
        required=False
    )

    # -i / --input_dir to read npz files from a directory
    parser.add_argument(
        "-i", "--input_dir", dest="input_dir", type=str, default=".\\input",
        help="Directory to search for .npz files if -r is not used (default: .\\input)."
    )

    # -t / -tolerance to specify tolerance value
    parser.add_argument(
        "-t", "--tolerance", type=float, default=DEFAULT_TOLERANCE,
        help=f"Numerical tolerance for comparison (default: {DEFAULT_TOLERANCE})"
    )

    # -o / --output_dir to specify location for csv file output
    parser.add_argument(
        "-o", "--output_dir", type=str, default=DEFAULT_OUTPUT_DIR,
        help=f"Directory to save the report file (default: {DEFAULT_OUTPUT_DIR})."
    )
    args = parser.parse_args()

    # try to pull in npz files from args
    file_paths = []
    if args.restart_files:
        file_paths = args.restart_files
    else:
        try:
            file_paths = get_npz_files(args.input_dir)
        except FileNotFoundError as e:
            print(f"Error: {e}", file=sys.stderr)
            sys.exit(1)

    # less than 2 npz?
    if len(file_paths) < 2:
        print("Error: Two NPZ files are requried for comparison.")
        sys.exit(1)
    # check if there is more than 2 restarts, if so ask the user which 2 to compare
    elif len(file_paths) > 2:
        file1_path, file2_path = select_restart(file_paths)
    else:
        file1_path = file_paths[0]
        file2_path = file_paths[1]
        
    file1_name = os.path.split(file1_path)[-1]
    file2_name = os.path.split(file2_path)[-1]
    print(f"Comparing {file1_name} vs {file2_name}")
    report_path = get_report_path(args.output_dir, file1_name.split(".")[0] + "-vs-" + file2_name.split(".")[0] + ".csv")

    # Set this to true for detailed results, False if standard.
    detailed_flag = False
    
    # ask user to enter vars if true
    vars = enter_vars(detailed_flag)

    # compare npz files
    try:
        problem_keys, array_cords, calc_string, diff_value, _ = compare_npz_files(file1_path, file2_path, args.tolerance, vars)
        if not problem_keys:
            print("Happy! No differences detected!")
            with open(report_path, 'w', encoding="UTF8") as f:
                f.write("")
        else:
            write_report(problem_keys, array_cords, calc_string, diff_value, report_path, detailed_flag)
            print("Differences detected!")
        print("Done comparing NPZ files.")

    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)

    except Exception as e:
        print(f"Unhandled exception: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    cli_mode()
