# Validator 
## Purpose:
Currently many output files of a NEMS run are manually checked and verified for correctness. The Validator’s purpose is to automate this process. The NEMS validator will behave similarly to the existing WEPS validator. 

## Usage:
The validator will be run automatically as part of a NEMS run. When launching a NEMS run no additional action is required by the user to run the validator. 
A report named “validator_report.xlsx” will be placed in the NEMS run output folder. The report is an excel workbook, where the first sheet is a summary of all tests and the pass/fail status of the test. Each test that fails will have an additional sheet detailing the errors encountered in the test. 

## Current tests:

### test_total_supply_equals_total_demand
Checks Ftab's table of total energy demand and supply. Tests if (total supply - total demand) is smaller than a discrepancy tolerance.

### test_nohup_contains_no_errors  
Tests that Nohup.out files (in the main, p1, p2, and p3 folders) do not contain any error messages.

### test_convergence
Is the final cycle gpa >= 3.90

### test_gas_model_aimms_files_exist 
Check that P1/ngas/fromAIMMS folder has files for every year 2016-2050.

### test_coal_model_aimms_files_exist
Check that P2/coal/fromAIMMS folder has files for every year 2019-2050.

### test_restore_model_aimms_files_exist
Check that P2/rest/fromAIMMS folder has files for every yer 2018-2050.

### test_lfmm_gams_model_gdx_files_exist
check that the P1 folder has lfmm_pYEAR.gdx files for all years 2010-2050.

### test_ctus_gams_model_ctssoln_file_exists
Check that P2 folder has a ctssoln.gdx file.



## Contributing: 
The next step for the Validator is adding more tests. Any output file of NEMS or FTAB can be tested, the Integration team just needs to be made aware of which files and values you want to be validated. 
If you are currently manually checking the output of a NEMS run, let the Integration Team know (by emailing Kyle.Morley@eia.gov) and we can add a test to the validator. 
