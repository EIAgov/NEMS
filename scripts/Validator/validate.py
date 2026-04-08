import os
import pandas as pd
from Controller import ReportFormatter
from DataModel.Model import Model

from Tests import create_test_prep as create_test_prep

def main():
    #pass in the directory name as input
    print('You are running the NEMS output validation script.')

    # grab the info of path of output\validator folder and the output root 
    validator_dir = os.path.dirname(os.path.abspath(__file__))
    output_root_dir = os.path.dirname(validator_dir)
    print(f"Running from {output_root_dir}")

    run_validate(validator_dir, output_root_dir)

def get_lastyr_entry(filename="scedes.all"):
    """
    Goes up one folder level, finds the specified file,
    reads its content, extracts the value associated with 'LASTYR',
    and attempts to convert it to an integer.

    Args:
        filename (str): The name of the file to search for.

    Returns:
        int or None: The integer value associated with 'LASTYR' if found and
                     successfully converted, otherwise None.
    """
    current_dir = os.path.dirname(os.path.abspath(__file__))

    # Go up one folder level initially
    initial_parent_dir = os.path.dirname(current_dir)

    # Determine the final base directory for the file
    base_dir_for_file = initial_parent_dir

    # Check if the name of the initial_parent_dir ends with 'Validator'
    if os.path.basename(initial_parent_dir).lower().endswith("validator"):
        # If it does, go up one more level
        base_dir_for_file = os.path.dirname(initial_parent_dir)
        print(f"Parent directory ends with 'Validator'. Moving up an additional level to: {base_dir_for_file}")
    else:
        print(f"Parent directory does not end with 'Validator'. Searching in: {base_dir_for_file}")


    # Construct the full path to the file
    file_path = os.path.join(base_dir_for_file, filename)
    
    lastyr_str_value = None
    lastyr_int_value = None

    try:
        with open(file_path, 'r') as f:
            for line in f:
                if "LASTYR" in line:
                    if '=' in line:
                        key, value = line.strip().split('=', 1)
                        if key.strip() == "LASTYR":
                            lastyr_str_value = value.strip()
                            break
                    elif ' ' in line and line.strip().startswith("LASTYR"):
                        parts = line.strip().split(maxsplit=1)
                        if len(parts) > 1 and parts[0] == "LASTYR":
                            lastyr_str_value = parts[1]
                            break
    except FileNotFoundError:
        print(f"Error: The file '{filename}' was not found at '{file_path}'.")
        return None
    except Exception as e:
        print(f"An error occurred during file reading: {e}")
        return None

    if lastyr_str_value is not None:
        try:
            lastyr_int_value = int(lastyr_str_value)
        except ValueError:
            print(f"Warning: Could not convert '{lastyr_str_value}' to an integer.")
            return None
    else:
        print("LASTYR entry not found in the file.")

    return lastyr_int_value
    
def run_validate(validator_dir, output_root_dir):
    import pytest

    os.chdir(validator_dir)
    # validator_report.xml shall exist in the same folder of this validate.py file
    xml_report_path = os.path.join((validator_dir), 'validator_report.xml')
    print(xml_report_path)
    # run the preprocess for test_compare_totals.py
    df_keys = pd.read_csv('input\\validator_subset_keys.csv')
    scedyear = get_lastyr_entry()
    if scedyear != df_keys['end_year'].iloc[0]:
        print("Run is shorter than 2050, changing tests to go to scedes LastYr instead")
        df_keys.loc[:,"end_year"] = scedyear
    ls_tests = df_keys['test']
    create_test_prep.create_test_compare_functions(ls_tests, 'Tests\\test_compare_totals.py')

    # run the preprocess for test_inequality_check.py
    df_keys = pd.read_csv('input\\validator_inequality_keys.csv')
    if scedyear != df_keys['end_year'].iloc[0]:
        print("Run is shorter than 2050, changing tests to go to scedes LastYr instead")
        df_keys.loc[:,"end_year"] = scedyear
    ls_tests = df_keys['test']
    create_test_prep.create_test_inequality_functions(ls_tests, 'Tests\\test_inequality_check.py')

    model = Model.getInstance()
    model.load_files(output_root_dir)

    # Tests are run here
    pytest.main(["-o", "junit_family=xunit1", "-v", f"--junitxml={xml_report_path}"])

    # Report is formatted here
    ReportFormatter.main()

if __name__ == '__main__':
    main()