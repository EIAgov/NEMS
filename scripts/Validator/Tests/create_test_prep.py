"""
A preprocessor to the test_compare_totals.py that edits the .py file to add test functions for each
of the tests in the validator_subset_keys.csv.

This shortens the lines needed for adding tests and tests are written in automatically. 
"""

import pandas as pd

def create_test_compare_functions(ls, pyfile):
    """
    Creates the test definitions from a list given to the .py file. 

    Parameters
    ----------
    ls : list
        list of the test names which includes (record_property)
    pyfile : string
        the name of the .py file
    """    
    
    # saves the content of the .py file
    with open(pyfile, 'r') as f:
        contents = f.read()
        
    # splits the contents to get contents before any test function is written for debugging
    ls_parts = contents.split("def test_")
    
    # writes the content of the .py file and then loops through the list to write the function
    with open(pyfile, 'w') as f:
        f.write(ls_parts[0])
        for i in ls:
            s_test = i.replace("(record_property)", "")
            f.write(f"\ndef {i}:\n\n")
            f.write(f"    test = \'{s_test}\'\n")
            f.write("    h = StatusHelper()\n")
            f.write("    h.skip_test(h.get_status(test))\n")
            f.write("    s_discrepancy, DISCREPANCY_TOLERANCE, start_year, str_total, str_subset, ls_total, ls_concat_str, str_totnum, str_subnum, str_subreg, str_sub_units, str_total_units = calculate_subset(test)\n\n")
            f.write("    return_errors(s_discrepancy, DISCREPANCY_TOLERANCE, record_property, test, start_year, str_total, str_subset, ls_total, ls_concat_str, str_totnum, str_subnum, str_subreg, str_sub_units, str_total_units)\n")

def create_test_inequality_functions(ls, pyfile):
    """
    Creates the test definitions from a list given to the .py file. 

    Parameters
    ----------
    ls : list
        list of the test names which includes (record_property)
    pyfile : string
        the name of the .py file
    """    
    
    # saves the content of the .py file
    with open(pyfile, 'r') as f:
        contents = f.read()
        
    # splits the contents to get contents before any test function is written for debugging
    ls_parts = contents.split("def test_")
    
    # writes the content of the .py file and then loops through the list to write the function
    with open(pyfile, 'w') as f:
        f.write(ls_parts[0])
        for i in ls:
            s_test = i.replace("(record_property)", "")
            f.write(f"\ndef {i}:\n\n")
            f.write(f"    test = \'{s_test}\'\n")
            f.write("    h = StatusHelper()\n")
            f.write("    h.skip_test(h.get_status(test))\n")
            f.write("    df, df_boolean, ls_year_columns, ls_tables, ls_regions, start_year, str_name, str_inequality, str_units, abs_flag = check_inequality(test) \n\n")
            f.write("    return_errors(record_property, df, df_boolean, ls_year_columns, ls_tables, ls_regions, start_year, str_inequality, str_name, test, str_units, abs_flag)\n")
    
if __name__ == "__main__":
    """
    stand-alone test. 
    """
    pyfile = "test_compare_totals.py"
    
    df_keys = pd.read_csv("validator_subset_keys.csv")
    ls_tests = df_keys['test']
    
    create_test_compare_functions(ls_tests, pyfile)
    
    pyfile = "test_inequality_check.py"
    
    df_keys = pd.read_csv("validator_inequality_keys.csv")
    ls_tests = df_keys['test']
    
    create_test_inequality_functions(ls_tests, pyfile)
    
    