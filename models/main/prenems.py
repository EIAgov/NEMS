import os
import sys
import parse_scedes
import restart_load
import steo_load
import conversion_factors_load, seds_load, risk_load
import PyFilerWrapper as pfw
from logging_utilities import print_it, prenems_setup_logs
import subprocess
import imodel_helper
import ngplprice
import pandas as pd

sys.path.append("../converge")

from converge import conv_data_preprocess

MODULE_NAME = "prenems.py"


def nems_runtype():

    """Povides NEMS model list based on runtype
    
    Provides the assigned models by jognems (sequential) or parnems (parallel)
    runs. For jognems, all available models are listed. for Parnems each model
    are separated by the partition (p1, p2, p3) the run in.
    
    Parameters
    ----------
    None.
    
    Returns
    -------
    dict
        dictionary of the jognems and parnems models sorted by partition
    str
        string determining if current run is sequential (s) or parallel (p1, p2, p3)

    """

    # Creates a list of models by their runtype. 
    # Sequential (jognems) lists all available models
    # Parallel (parnems) lists the models that run only in their partition
    jogpar = \
          {"s" : ["CDM", "CMM", "EMM", "HMM", "HSM", "IDM", "IEM", "LFMM", "MAM", "NGMM", "RDM", "RFM", "TDM", "CCATS"],
           "p1": ["CDM", "HSM", "IDM", "IEM", "LFMM", "MAM", "NGMM", "RDM", "TDM", "CCATS", "HMM"],
           "p2": ["CDM", "CMM", "EMM", "RDM", "RFM"],
           "p3": ["IDM"]}

    # Determines the current run type based on the folder path.
    # Folder path ending with p1, p2, or p3 have are assigned with p1, p2 ,p3 string
    # Folder path ending with any  string other than p1, p2, p3 are assigned the "s" string
    temp = os.path.basename(os.path.normpath(os.getcwd()))
    if temp in ["p1", "p2", "p3"]:
        runtype = temp
    else:
        runtype = "s"
    
    assert runtype in list(jogpar.keys())

    return jogpar, runtype
    
def set_nems_modules(user):
    
    """determines the models and their order for the NEMS run
    
    Uses the user.jogpar (dict) and user.runtype (str) to determine the models 
    that can run in the specific run type, parnem (p1, p2, p3) or jognem (s)
    and the order to run. The function checks against the user.SCEDES (dict) 
    for SCEDES flags (e.g. EXE, EXI, EXT, etc.) for which models are turned on.
    
    Parameters
    ----------
    user : SimpleNamespace
        user class object, containing dict with NEMS
    
    Returns
    -------
    list
        list of models to run in the specified order

    """
    
    # Dictinary of module characters that trails the "EX" SCEDES key (e.g. EXE, EXI, EXT)
    module_map = {"M": "MAM",
                  "C": "CMM",
                  "R": "RDM",
                  "K": "CDM",
                  "I": "IDM",
                  "T": "TDM",
                  "E": "EMM",
                  "W": "IEM",
                  "L": "HSM",
                  "G": "NGMM",
                  "O": "LFMM",
                  "N": "RFM",
                  "H": "HMM",
                  "Q": "CCATS"}

    # Returns the list of all models that can run in this specific runtype
    x = user.jogpar[user.runtype]  
    module_scedes = {module_map[k[2:]]: v for k, v in user.SCEDES.items() if k.startswith("EX") and len(k)==3} 
    module_on = [k for k, v in module_scedes.items() if (k in x) and (str(v)=="1")]
    
    # creates a dictionary of scedes that start with "ORD" string that will be used to determine the order of the modules
    temp = {module_map[k[3:]]: int(v) for k, v in user.SCEDES.items() if k.startswith("ORD")}
    # sorts the models order determined by the scedes key (e.g. ORDM, ORDC, ORDT)
    module_order = list(dict(sorted(temp.items(), key=lambda item: item[1])).keys())

    # creates a final list of models to run in the determined order
    z = [i for i in module_order if i in module_on]

    if not z:
        with open("no_modules_on.txt", "w") as f:
            f.write("")

    return z


def scenario_date_setup(pyfiler, CURIRUN):
    """Extract scenario name and datekey from folder setup and assign to pyfiler object

    Determines if the runtype is parnem or jognem based on folder structure before
    extracting the scenario name (e.g. ref2025_solo) and datekey (e.g. d120524g).
    Checks to make sure scenario name derived from scedes file name is 15 characters or less.
    Program will exit if scenario name exceeds 15 characters. Function will assign variables 
    to pyfiler object. Pyfiler variables assigned values:

        pyfiler.nchar.scen (e.g. ref2025_solo)
        pyfiler.nchar.date (e.g. d120524g)
        pyfiler.nchar.scen_date (e.g. ref2025_solo.d120524g)

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module

    CURIRUN : int
        Current NEMS cycle number

    Returns
    -------
    None.

    """

    # extract scendate info from directory path and determine if jognem or parnem run
    cwd_split = os.getcwd().split("\\")
    if cwd_split[-1] in ["p1", "p2", "p3"]:
        scen, date = cwd_split[-3], cwd_split[-2]
    else:
        scen, date = cwd_split[-2], cwd_split[-1]

    # Check if scenario name and datekey individually and combined exceeds character limit
    # If either or both exceeds the character limit, the string will be truncated
    # 20 characters for scenario name
    # 8 characters for datekey
    # Total not to exceed 29, including 'dot' separator: 20 + "." + 8 = 29 characters
    # This is because excel, where the reports are produced, can only handle tabnames =< 31 characters
    # Check if scenario name exceeds 20 character limit. If exceed, program exits.
    if len(scen) > 20:
        print_it(CURIRUN, f"WARNING: SCEDES file name {scen} exceeded the 20 character limit. Exiting NEMS NOW", MODULE_NAME)
        os.sys.exit()
    # Check if datekey exceeds 8 characters limit
    if len(date) > 8:
        print_it(CURIRUN, "WARNING: date name > 8 characters will be truncated in pyfiler", MODULE_NAME)
    
    # Assigns strings to pyfiler variables
    scen_adj = scen[:21].ljust(21)
    date_adj = date[:8].ljust(8)
    scen_date_adj = f"{scen_adj.strip()}.{date_adj.strip()}"[:29].ljust(29)
    pyfiler.utils.scen = scen_adj
    pyfiler.nchar.date = date_adj
    pyfiler.nchar.scen_date = scen_date_adj


def get_runmod(models):
    """Pass in a desire execution model list, generate and return a 1/0 value (turn on/off) imodel run list.

    Parameters
    ----------
    models : list
        list of NEMS models activated to run for this Cycle. Full model list is as follows but can be shorter depending 
        on SCEDES setting and Parnem or Jognem run:
        ['MAM', 'CMM', 'RDM', 'CDM', 'IDM', 'TDM', 'EMM', 'IEM', 'HSM', 'NGMM', 'LFMM', 'RFM', 'HMM', 'CCATS']

    Returns
    -------
    list
        an imodel list with 1/0 values (0 = OFF, 1 = ON) corresponding to the NEMS models activated in this run
    
    """

    # Create an empty list of 0 values by the total number of available NEMS models (as of AEO2025, 14 models)
    result=[0] * 14
    # Retrieve a dict variable with model name key to imodel int pair
    name_to_imodel_dict=imodel_helper.set_model_dicts()

    # Iterate through 'models' list for activated NEMS model and turns corresponding switch to 1 if model is activated
    for i in models:
        model_key=name_to_imodel_dict.get(i)
        result[model_key-1]=1

    # Check if EMM (result[6]) is ON and turns RFM (result[11]) ON. Both are required to run together
    if result[6]==1: result[11]=1

    return result

def load_WEPS_data(scedes):
    """
    Load the WEPS data from rw-all_series_uid.csv to a dataframe.

    Parameters
    ----------
    scedes : dict
        dictionary of scedes keys and value pairs parsed from SCEDES.ALL file

    Returns
    -------
    df_data: dataframe
        WEPS data
    """    
    
    # get the file path from scedes
    file = scedes['RW-ALL_SERIES_UIDN'].replace('$NEMS/','').replace('/','\\').lower()
    
    try:
        # read file path
        df_data = pd.read_csv(file)
        
    except FileNotFoundError:
        # print that the file is not founded.
        print(f"{file} is not in the directory.")
    
    return df_data


def nems_setup(pyfiler1, cur_cycle_num):

    """Setting up NEMS variables and user object before running NEMS cycle
    
    Setup NEMS run by reading all required files into memory in the following order

    1.  Read in scedes file into dictionary
    2.  Read in Restart file
    3.  Zero out known nan and inf variables in pyfiler
    4.  Create user class that carries data from:
            SCEDES
    5.  Create jognem and parnem model list and determine current runtype
    6.  Reset SCEDES flags for NEMS modules to ZERO (e.g. pyfiler.ncrtl.exe = 0)
    7.  Turns on only relevant SCEDES flag for NEMS modules
    8.  Setup scenario name and date key
    9.  Load Conversion Factor into pyd
    10. Load SEDS Factor into pyd
    11. Load Risk into pyd
    12. Read in STEO data into pyfiler memory
    13. Pass data from scedes to pyfiler and assign current cycle number
    14. Load Dictionary into user object and NEMSVardf for NEMS Report Writer
    15. Load in Convergence Variables from CSV
    16. Connect to EMM Oracle based on SCEDES flag input
    17. Run hswrk.bat
    18. Assign runmod flags to pyfiler
    19. Initialize Inter-Cycle (intercv) flags based on scedes model configuration
    20. Read any remaining data specific to the Integrating Model
    21. Prenems print setup essential messages / logs
    22. Execute Natural Gas Plant Liquids (NGPL) to estimate ethane and propane prices
    23. Load rw_all_series.csv, WEPS data, to the user object


    Parameters
    ----------

    pyfiler1 : module
        pyfiler fortran module

    cur_cycle_num : int
        Current NEMS cycle number


    Returns
    -------
    
    SimpleNamespace
        user class object, containing dict with NEMS

    """

    # 1. Read in scedes file
    print_it(cur_cycle_num, "reading in 'scedes.all file", MODULE_NAME)
    scedes = parse_scedes.parse_scedes_all("scedes.all")

    # 2. Read in the restart file
    ## Restart file read-in has 3 different conditions to account for

    ## a. NRUNS > 1
    ### When the NEMS run has has more than 1 cycle, setup will copy a 
    ### restart file into the root directory for jognem or p1, p2, p3
    ### folders for parnem. Restart file is renamed to 'restart.in'
    ### When NRUNS > 1, input restart file name is always 'restart.in'

    ## b. NRUNS == 1 (jognem, p1, and p2)
    ### Only for jognem, p1, and p2 when NRUNS == 1, setup will copy a
    ### restart file into the "input" folder at the root directory for 
    ### jognem or "p1/input" and "p2/input" for parnem. Restart file is 
    ### renamed to 'restarti.<ext>' where <ext> is the file extension
    ### set in the scedes file under "RESTARTN" variable

    ## b. NRUNS == 1 (p3)
    ### Only for p3 when NRUNS == 1, the restart input file used in p3
    ### when NRUNS == 1 is generated from tfiler.exe by combining the 
    ### restart file output from p1 and p2. The output from tfiler.exe
    ### is hardcoded in tfiler.files as "Restarto   p3/input/restarti.unf\n"

    print_it(cur_cycle_num, f"reading in restart file", MODULE_NAME)
    if scedes['NRUNS'] == '1':

        tmp_restart = scedes['RESTARTN'].split('/')[-1]
        tmp_restart_extension = tmp_restart.split('.')[1]

        # Activate only whehn NRUNS == 1 and currently running p3
        # Overwrite the tmp_restart_extension from the scedes file
        # with hardcoded 'unf' string
        if os.getcwd().split('\\')[-1] == 'p3':
            tmp_restart_extension = 'npz'
            
        input_restart_filename = 'input/restarti.' + tmp_restart_extension

    else:
        input_restart_filename = 'restart.in'
    
    # Call function to read in restart file
    # Must read in restart file into pyfiler1 to initialise variables
    restart_load.read_restart_file(input_restart_filename, pyfiler1)
    
    # 3. Create user class
    print_it(cur_cycle_num, f"creating user class", MODULE_NAME)
    user = parse_scedes.create_user_class(scedes)

    # 4. Create jognem and parnem model list and determine current runtype
    print_it(cur_cycle_num, f"creating nems run model list", MODULE_NAME)
    user.jogpar, user.runtype = nems_runtype()

    # 5. Create list of NEMS model order to run
    print_it(cur_cycle_num, f"creating nems model list run order", MODULE_NAME)
    user.module_order = set_nems_modules(user)

    # 6. Reset SCEDES flags for NEMS modules to ZERO
    print_it(cur_cycle_num, f"Reset pyfiler model flags to zero", MODULE_NAME)
    restart_load.reset_module_scedes_flags(pyfiler1)

    # 7. Turns on only relevant SCEDES flag for NEMS modules
    print_it(cur_cycle_num, f"Set pyfiler model flags based on SCEDES flag", MODULE_NAME)
    restart_load.set_module_scedes_flags(user, pyfiler1)

    # 8. Setup scenario name and date key
    print_it(cur_cycle_num, f"Set pyfiler scenario name and date key", MODULE_NAME)
    scenario_date_setup(pyfiler1, cur_cycle_num)

    # 9. Load Conversion Factor into pyd
    print_it(cur_cycle_num, f"Load conversion factor", MODULE_NAME)
    conversion_factors_load.main(user, pyfiler1)

    # 10. Load SEDS Factor into pyd
    print_it(cur_cycle_num, f"Load SEDS data", MODULE_NAME)
    seds_load.main(user, pyfiler1)

    # 11. Load Risk into pyd
    print_it(cur_cycle_num, f"Load Risk data", MODULE_NAME)
    risk_load.main(user, pyfiler1)

    # 12. Read in STEO data
    print_it(cur_cycle_num, f"Load STEO data", MODULE_NAME)
    steo_load.read_all_steo_vars(user, pyfiler1)

    # 13. Pass data from scedes to pyfiler
    print_it(cur_cycle_num, f"Pass data from scedes to pyfiler", MODULE_NAME)
    restart_load.scedes_to_pyfiler(user, pyfiler1)
    print_it(cur_cycle_num, f"Assign Cycle number to pyfiler", MODULE_NAME)
    # 13. Assign current cycle number
    pyfiler1.cycleinfo.curirun = int(cur_cycle_num)

    # 14. Load Dictionary into user object and NEMSVardf for NEMS Report Writer
    print_it(cur_cycle_num, f"Load Dictionary into user object and NEMSVardf", MODULE_NAME)
    NEMSFortTable, NEMSAttributesTable = pfw.ParseDict("input/dict.txt")
    NEMSVardf = pfw.RetrieveVarDim(NEMSFortTable, NEMSAttributesTable)
    ## user.NEMSVardf used in nems_flow.py to write out restart.npz
    user.NEMSVardf = NEMSVardf
    ## Write to .csv for NEMS Report Writer
    NEMSVardf.to_csv('NEMSVardf.csv', index = True)

    # 15 Load in Convergence Variables from CSV
    print_it(cur_cycle_num, f"Load in Convergence Variables from CSV", MODULE_NAME)
    input_path_convergence = os.path.join(os.getcwd(), 'converge', 'input', 'conv_')
    df_conv, df_rlx, all_vars, vars_conv, vars_rlx = conv_data_preprocess.read_conv_file(input_path_convergence)
    # user.CONV_DAT contains dictionary of pandas dataframes and lists used by conv_main.main
    user.CONV_DAT = {"df_conv" : df_conv, 
                     "df_rlx" : df_rlx, 
                     "all_vars" : all_vars, 
                     "vars_conv" : vars_conv, 
                     "vars_rlx" : vars_rlx}
    # user.CONV_VAR (lower case of variable list derived from "all_vars") used by PyfilerToHDF5.main
    user.CONV_VAR = [x.lower() for x in user.CONV_DAT["all_vars"]]
    # Load IMODEL convergence into dataframe
    user.CONV_IMODEL = pd.read_csv(input_path_convergence + 'IMODEL.csv', index_col='NMODEL')

    # 16. Connect to EMM Oracle based on SCEDES flag input
    print_it(cur_cycle_num, f"Connect to EMM Oracle based on SCEDES flag input", MODULE_NAME)
    if int(user.SCEDES['ORACLESW']) == 0:
        pyfiler1.resetosw("ORCLECP ",0)
        pyfiler1.resetosw("ORCLEFP ",0)
        pyfiler1.resetosw("ORCLEFD ",0)

    # 17. Run hswrk.bat
    print_it(cur_cycle_num, "run 'hswrk.bat'", MODULE_NAME)
    subprocess.run('hswrk.bat', shell=True)

    # 18. Assign runmod flags to pyfiler
    print_it(cur_cycle_num, "Assign runmod flags to pyfiler", MODULE_NAME)
    pyfiler1.ncntrl.runmod = get_runmod(user.module_order)

    # 19. Initialize Inter-Cycle (intercv) flags based on scedes model configuration
    print_it(cur_cycle_num, "Initialize flags used for Inter-Cycle Convergence", MODULE_NAME)
    restart_load.init_continue_flags_intercv(user, pyfiler1)
    
    # 20. Read any remaining data specific to the Integrating Model
    print_it(cur_cycle_num, "Read any remaining data specific to the Integrationg Model", MODULE_NAME)
    pyfiler1.initialize_api()

    # 21. Execute Natural Gas Plant Liquids (NGPL) to estimate ethane and propane prices
    RUNRNGPL=int(user.SCEDES['RUNRNGPL'])
    if RUNRNGPL==1:
        print_it(cur_cycle_num, "run 'ngpl_stat_price'", MODULE_NAME)
        #call stat_price
        pyfiler1.ngpl_stat_price_run.go_ngpl_stat_price()
        ngplprice.main(str(pyfiler1.cycleinfo.curirun))
        pyfiler1.ngpl_stat_price_run.go_ngpl_stat_price_reporting()
    else:
        print_it(cur_cycle_num, '("  Not running Python program for propane and ethane pricing by request")', MODULE_NAME)

    # 22. Prenems print setup essential messages / logs
    prenems_setup_logs(pyfiler1, user, cur_cycle_num)
    
    # 23. Load WEPS data into user object
    user.WEPSdata = load_WEPS_data(scedes)

    return user