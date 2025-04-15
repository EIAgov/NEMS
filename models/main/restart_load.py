def read_restart_file(input_restart_filename, pyfiler):

    """Reads in the restart file (e.g. restart.unf, restart.rlx) into memory. Creates a required empty restart.unf file


    Parameters
    ----------
    input_restart_file_name : str
        the name or path to the restart file
        e.g.:
        restart.unf
        input/restart.rlx

    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.

    """

    # This block creates the empty restart.unf file
    with open('restart.unf', 'w+') as fp:
        pass
    

    pyfiler.utils.read_filer(input_restart_filename)

def write_restart_file(output_restart_filename, pyfiler):

    """Write out the restart file (e.g. restart.unf, restart.rlx) into memory. Creates a required empty restart.unf file


    Parameters
    ----------
    input_restart_file_name : str
        the name or path to the restart file
        e.g.:
        restart.unf
        input/restart.rlx

    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.

    """
    
    pyfiler.utils.write_filer(output_restart_filename)


def reset_module_scedes_flags(pyfiler):

    """Resets all module flags to Zero

    pyfiler.ncntrl.exw : int
        ncntrl flag for IEM
    pyfiler.ncntrl.exm : int
        ncntrl flag for MAM
    pyfiler.ncntrl.exr : int
        ncntrl flag for RDM
    pyfiler.ncntrl.exk : int
        ncntrl flag for CDM
    pyfiler.ncntrl.exi : int
        ncntrl flag for IDM
    pyfiler.ncntrl.ext : int
        ncntrl flag for TDM
    pyfiler.ncntrl.exe : int
        ncntrl flag for EMM
    pyfiler.ncntrl.exc : int
        ncntrl flag for CMM
    pyfiler.ncntrl.exl : int
        ncntrl flag for HSM
    pyfiler.ncntrl.exg : int
        ncntrl flag for NGMM
    pyfiler.ncntrl.exo : int
        ncntrl flag for LFMM
    pyfiler.ncntrl.exn : int
        ncntrl flag for RFM
    pyfiler.ncntrl.exh : int
        ncntrl flag for HMM
    pyfiler.ncntrl.exq : int
        ncntrl flag for CCATS

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.


    """
    
    pyfiler.ncntrl.exw = 0  # IEM
    pyfiler.ncntrl.exm = 0  # MAM
    pyfiler.ncntrl.exr = 0  # RDM
    pyfiler.ncntrl.exk = 0  # CDM
    pyfiler.ncntrl.exi = 0  # IDM
    pyfiler.ncntrl.ext = 0  # TDM
    pyfiler.ncntrl.exe = 0  # EMM
    pyfiler.ncntrl.exc = 0  # CMM
    pyfiler.ncntrl.exl = 0  # HSM  
    pyfiler.ncntrl.exg = 0  # NGMM
    pyfiler.ncntrl.exo = 0  # LFMM
    pyfiler.ncntrl.exn = 0  # RFM
    pyfiler.ncntrl.exh = 0  # HMM
    pyfiler.ncntrl.exq = 0  # CCATS


def set_module_scedes_flags(user, pyfiler):
    """Sets all module flags to values passed from scedes.all
    
    Values read in from scedes.all file into the dictionary user.SCEDES are
    assigned to their corresponding pyfiler.ncntrl model flags.
    
    This function checks if the corresnponding model is turned on from the user.SCEDES
    variable. It then checks if the model can run in the current partition (p1, p2, p3, s)
    NEMS runtype. If the correspoding SCEDES flag is on and the model runs in the current 
    partition, then 1 is assigned to the pyfiler.ncntrl model flag as ON.

    pyfiler.ncntrl.exw : int
        ncntrl flag for IEM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exm : int
        ncntrl flag for MAM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exr : int
        ncntrl flag for RDM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exk : int
        ncntrl flag for CDM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exi : int
        ncntrl flag for IDM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.ext : int
        ncntrl flag for TDM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exe : int
        ncntrl flag for EMM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exc : int
        ncntrl flag for CMM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exl : int
        ncntrl flag for HSM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exg : int
        ncntrl flag for NGMM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exo : int
        ncntrl flag for LFMM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exn : int
        ncntrl flag for RFM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exh : int
        ncntrl flag for HMM (0 is OFF, 1 is ON)
    pyfiler.ncntrl.exq : int
        ncntrl flag for CCATS (0 is OFF, 1 is ON)

    Parameters
    ----------
    user : SimpleNamespace
        user class object, containing dict with NEMS runtime information
    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.

    """
    po = user.SCEDES

    # INTERNATIONAL (IEM)
    if po["EXW"] == "1" and ('IEM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exw = 1

    # MACROECONOMIC (MAM)
    if po["EXM"] == "1" and ('MAM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exm = 1
    
    # RESIDENTIAL (RDM)
    if po["EXR"] == "1" and ('RDM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exr = 1
    
    # COMMERCIAL (CDM)
    if po["EXK"] == "1" and ('CDM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exk = 1
    
    # INDUSTRIAL (IDM)
    if po["EXI"] == "1" and ('IDM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exi = 1
    
    # TRANSPORTATION (TDM)
    if po["EXT"] == "1" and ('TDM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.ext = 1
    
    # UTILITY (EMM)
    if po["EXE"] == "1" and ('EMM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exe = 1
    
    # COAL SUPPLY (CMM)
    if po["EXC"] == "1" and ('CMM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exc = 1
    
    # OIL AND GAS SUPPLY (HSM)
    if po["EXL"] == "1" and ('HSM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exl = 1
    
    # GAS TRAN & DIST (NGMM)
    if po["EXG"] == "1" and ('NGMM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exg = 1
    
    # PETROLEUM REFINERY (LFMM)
    if po["EXO"] == "1" and ('LFMM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exo = 1
    
    # RENEWABLES (RFM)
    if po["EXN"] == "1" and ('RFM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exn = 1
    
    # HYDROGEN MARKET MOD (HMM)
    if po["EXH"] == "1" and ('HMM' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exh = 1
    
    # CCATS (CCATS)
    if po["EXQ"] == "1" and ('CCATS' in user.jogpar[user.runtype]):
        pyfiler.ncntrl.exq = 1


def scedes_to_pyfiler(user, pyfiler):
    
    """Sets all pyfiler.ncntrl runtime flags to values passed from scedes.all
    
    Values read in from scedes.all file into the dictionary user.SCEDES are
    assigned to their corresponding pyfiler.ncntrl runtime flags.
    


    Parameters
    ----------
    user : SimpleNamespace
        user class object, containing dict with NEMS runtime information
    pyfiler : module
        pyfiler fortran object

    Returns
    -------
    None.

    """
    
    po = user.SCEDES

    pyfiler.ncntrl.maxitr       = po['MAXITR']      # MAXIMUM ITERATIONS
    pyfiler.ncntrl.dbdump       = po['DBDUMP']      # DATABASE DUMP/YR SW (0->OFF,1->ON) (DEF=0)
    pyfiler.ncntrl.lastcalyr    = po['LASTYR']      # LAST FORECAST YEAR as Year (EG. 2020)
    pyfiler.ncntrl.lastyr       = int(po['LASTYR']) - 1990 + 1      # LAST FORECAST YEAR INDEX  (EG. 29)
    pyfiler.ncntrl.prtdbgw      = po['PRTDBGW']     # PRINT DEBUG, INTERNATIONAL
    pyfiler.ncntrl.prtdbgm      = po['PRTDBGM']     # PRINT DEBUG, MACROECONOMIC
    pyfiler.ncntrl.prtdbgr      = po['PRTDBGR']     # PRINT DEBUG, RESIDENTIAL
    pyfiler.ncntrl.prtdbgk      = po['PRTDBGK']     # PRINT DEBUG, COMMERCIAL
    pyfiler.ncntrl.prtdbgi      = po['PRTDBGI']     # PRINT DEBUG, INDUSTRIAL
    pyfiler.ncntrl.prtdbgt      = po['PRTDBGT']     # PRINT DEBUG, TRANSPORTATION
    pyfiler.ncntrl.prtdbge      = po['PRTDBGE']     # PRINT DEBUG, ELECTRICITY
    pyfiler.ncntrl.prtdbgc      = po['PRTDBGC']     # PRINT DEBUG, COAL
    pyfiler.ncntrl.prtdbgl      = po['PRTDBGL']     # PRINT DEBUG, OIL AND GAS SUPPLY
    pyfiler.ncntrl.prtdbgg      = po['PRTDBGG']     # PRINT DEBUG, NATURAL GAS TRANS.& DISTR
    pyfiler.ncntrl.prtdbgo      = po['PRTDBGO']     # PRINT DEBUG, REFINERY
    pyfiler.ncntrl.prtdbgn      = po['PRTDBGN']     # PRINT DEBUG, RENEWABLE
    pyfiler.ncntrl.prtdbgh      = po['PRTDBGH']     # PRINT DEBUG, Hydrogen
    pyfiler.ncntrl.prtdbgq      = po['PRTDBGQ']     # PRINT DEBUG, CCATS
    pyfiler.ncntrl.ijumpyr      = po['IJUMPYR']     # End of forecast horizon - index
    pyfiler.ncntrl.ijumpcalyr   = po['IJUMPCAL']    # End of forecast horizon - calendar year
    pyfiler.ncntrl.firsyr       = int(po['FIRSYR']) - 1990 + 1      # FIRST FORECAST YEAR INDEX (EG. 2)
    pyfiler.ncntrl.frctol       = po['FRCTOL']      # MINIMUM FRACTIONAL CONVERGENCE TOLERANCE
    pyfiler.ncntrl.abstol       = po['ABSTOL']      # MINIMUM ABSOLUTE  CONVERGENCE TOLERANCE
    pyfiler.ncntrl.rlxpc        = po['RLXPC']       # RELAXATION FRACTION
    pyfiler.ncntrl.nyrs         = po['NYRS']        # NUMBER OF GROWTH YEARS FOR EXPECTATIONS
    pyfiler.ncntrl.i4site       = po['I4SITE']      # FORESIGHT OPT (1:MYOPIC, 2: ADAPTIVE, 3: PERFECT)
    pyfiler.ncntrl.i4scnt       = po['I4SCNT']      # FORESIGHT CONTROL: (1: MAIN, 2: SUBMODULE)
    pyfiler.ncntrl.irelax       = po['IRELAX']      # OPTION TO RUN HEURISTIC ROUTINE TO SPEED CONVERGENCE
    pyfiler.ncntrl.itimng       = po['ITIMNG']      # TIMING SWITCH (ITIMNG=1 MEANS TIMING ON)
    pyfiler.ncntrl.wwop         = po['WWOP']        # WORLD OIL PRICE CASE
    pyfiler.ncntrl.mmac         = po['MMAC']        # MACRO CASE
    pyfiler.ncntrl.history      = po['HISTORY']     # OPTION TO OVERWRITE 1990 DATA W/SEDS DATA (1=TRUE)
    pyfiler.ncntrl.modelon      = po['MODELON']     # MODELS NEVER OFF SWITCH(0->OFF,1->ON)(DEF=0)
    pyfiler.ncntrl.ecpstart     = po['ECPSTART']    # START YEAR FOR ECP MODULE (DEF=1)
    pyfiler.ncntrl.macfdbk      = po['MACFDBK']     # MACROECONOMIC FEEDBACK SWITCH
    pyfiler.ncntrl.elassw       = po['ELASSW']      # ELASTICITY SWITCH (0->OFF, 1->ON)
    pyfiler.ncntrl.dsmswtch     = po['DSMSWTCH']    # DEM SIDE MGMT SWITCH(0->OFF,1->ON)
    pyfiler.ncntrl.yearpr       = po['YEARPR']      # FOR REPORTING, YEAR DOLLARS
    pyfiler.ncntrl.scalpr       = pyfiler.macout.mc_jpgdp[int(po['YEARPR']) - 1987]     # Setting deflator value for reporting year dollars !PCWGDP SCALING FROM 1987$ TO YEARPR $
    pyfiler.cycleinfo.numiruns  = po['NRUNS']       # Maximum cycles in run