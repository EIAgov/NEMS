# -*- coding: utf-8 -*-
"""
Created on Feb 01 2024

@author: Claire Su
"""
import os
import sys
import re

sys.path.append(os.getcwd() + '\\PyFiler')


def read_all_steo_vars(user,pyfiler):
    """Read in steovars.csv, call steovars_convert() to compute and update the values in the referenced pyfiler.
    If excoal flag and EXC both are turned on, call steovars_write_to_clsteo() to write out clsteo.txt file
    If exngaim flag and EXG both are turned on, call steovars_write_to_ngsteo() to write out ngsteo.txt file

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
    excoal = int(pyfiler.ncntrl.exc)    #excoal=1
    exngaim = int(pyfiler.ncntrl.exg)   #exngaim=1

    UNIT_STEO_IN='./input/steovars.csv'
    steovr=int(pyfiler.other.steovr) #443 #steovr value from includes file
    BASEYR=pyfiler.utils.baseyr #1990
    list_steo_var_name,STEO_VARS_KEEP=steovars_steolist_read_in(steovr)
    '''
    # keep for debug:
    with open(r'steodbug_list_steo_var_name.csv','w+') as f:
        for idx,line in enumerate(list_steo_var_name):
            f.write(f'{idx},{line}\n')
    with open(r'steodbug_STEO_VARS_KEEP.csv','w+') as f:
        for idx,line in enumerate(STEO_VARS_KEEP):
            f.write(f'{idx},{line}\n')
    '''

    result_dict, STEO_VARS_FOUND, data = steovars_clean_up(UNIT_STEO_IN, BASEYR, steovr, list_steo_var_name, STEO_VARS_KEEP,pyfiler)
    with open(r'steodbug_STEO_VARS_FOUND.csv','w+') as f:
        for idx,line in enumerate(STEO_VARS_FOUND):
            f.write(f'{idx},{line}\n')

    '''
    ! doing some conversions to make things a little easier for the models
    ! STEO has natural gas and electricity in units per day while AEO has units per year
    ! also:  using STEO days and STEO deflator to do these conversions for enhanced STEO consistency
    '''
    pyfiler = steovars_convert(BASEYR, result_dict, STEO_VARS_FOUND, pyfiler, data)

    if pyfiler.ncntrl.exc == 1 and excoal != 0: #!  but only if AIMMS coal model is on
        #!  write out clsteo.txt file:  for now, emulate exact file format
        steovars_write_to_clsteo(result_dict, pyfiler)

    if pyfiler.ncntrl.exg == 1 and exngaim != 0:
        #!  List of variables to write to AIMMS NGMM:
        steovars_write_to_ngsteo(result_dict, pyfiler)

def write_aimms_variable(variable_prefix, increment_yrs, data_array,beg_yr):
    """Compose and return the data info string for the AIMMS variable.

    Sample code to invoked this method:
    WRITE_AIMMS_VARIABLE(EUNIT,"NGCCU_ENC(YEAR)   ", LASTSTEOYEAR-1989,NGCCU_ENC(1:LASTSTEOYEAR-1989),1990)

    Parameters
    ----------
    variable_prefix : str
        the prefix string to indicate the data belongs to which variable
    increment_yrs : int
        the increment of years
    data_array : array
        a float array of data
    beg_yr : int
        the start year

    Returns:
    -------
    str
        for example:
        NGCCU_ENC(YEAR) := data {1990 : 4.17233,  1991 : 4.03930,  1992 : 3.96209,  1993 : 4.15989};
    """
    d=''
    for y in range(0,increment_yrs):
        d=d+f'{y+beg_yr:6d} : {data_array[y]:14.5f},'

    s=f'{variable_prefix} := data '+'{ '+d[:-1]+' };\n'
    return s

def steovars_write_to_ngsteo(result_dict, pyfiler):
    """Write NGMM specific messages to ngas/ngsteo.txt file.

    Parameters
    ----------
    result_dict : array
        a data array

    pyfiler : module
        pyfiler fortran module
    
    Returns
    -------
    None.
    """
    yrs = result_dict['LASTSTEOYEAR']-1989
    po = pyfiler.other
    with open(r'ngas/ngsteo.txt', 'w+') as f:
        #!  Format to write them in:
        #!  NGCCU_ENC:= data { 1.0, 1.1, 1.2, etc: put the entire list of data elements separated by comments };
        f.write(write_aimms_variable("NGCCU_ENC(YEAR)",yrs,po.ngccu_enc[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_ESC(YEAR)",yrs,po.ngccu_esc[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_MAC(YEAR)",yrs,po.ngccu_mac[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_MTN(YEAR)",yrs,po.ngccu_mtn[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_NEC(YEAR)",yrs,po.ngccu_nec[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_PAC(YEAR)",yrs,po.ngccu_pac[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_SAC(YEAR)",yrs,po.ngccu_sac[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_WNC(YEAR)",yrs,po.ngccu_wnc[0:yrs],1990))
        f.write(write_aimms_variable("NGCCU_WSC(YEAR)",yrs,po.ngccu_wsc[0:yrs],1990))
        f.write(write_aimms_variable("NGCCUUS(YEAR)",yrs,po.ngccuus[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_ENC(YEAR)",yrs,po.ngicu_enc[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_ESC(YEAR)",yrs,po.ngicu_esc[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_MAC(YEAR)",yrs,po.ngicu_mac[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_MTN(YEAR)",yrs,po.ngicu_mtn[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_NEC(YEAR)",yrs,po.ngicu_nec[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_PAC(YEAR)",yrs,po.ngicu_pac[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_SAC(YEAR)",yrs,po.ngicu_sac[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_WNC(YEAR)",yrs,po.ngicu_wnc[0:yrs],1990))
        f.write(write_aimms_variable("NGICU_WSC(YEAR)",yrs,po.ngicu_wsc[0:yrs],1990))
        f.write(write_aimms_variable("NGICUUS(YEAR)",yrs,po.ngicuus[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_ENC(YEAR)",yrs,po.ngrcu_enc[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_ESC(YEAR)",yrs,po.ngrcu_esc[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_MAC(YEAR)",yrs,po.ngrcu_mac[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_MTN(YEAR)",yrs,po.ngrcu_mtn[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_NEC(YEAR)",yrs,po.ngrcu_nec[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_PAC(YEAR)",yrs,po.ngrcu_pac[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_SAC(YEAR)",yrs,po.ngrcu_sac[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_WNC(YEAR)",yrs,po.ngrcu_wnc[0:yrs],1990))
        f.write(write_aimms_variable("NGRCU_WSC(YEAR)",yrs,po.ngrcu_wsc[0:yrs],1990))
        f.write(write_aimms_variable("NGRCUUS(YEAR)",yrs,po.ngrcuus[0:yrs],1990))
        f.write(write_aimms_variable("NGEUDUS(YEAR)",yrs,po.ngeudus[0:yrs],1990))
        f.write(write_aimms_variable("NGHHUUS(YEAR)",yrs,po.nghhuus[0:yrs],1990))
        f.write(write_aimms_variable("NGSFPUS(YEAR)",yrs,po.ngsfpus[0:yrs],1990))
        f.write(write_aimms_variable("NGLPPUS(YEAR)",yrs,po.nglppus[0:yrs],1990))
        f.write(write_aimms_variable("NGACPUS(YEAR)",yrs,po.ngacpus[0:yrs],1990))
        f.write(write_aimms_variable("NGTCPUS(YEAR)",yrs,po.ngtcpus[0:yrs],1990))
        f.write(write_aimms_variable("BALIT(YEAR)",yrs,po.balit[0:yrs],1990))
        f.write(write_aimms_variable("NGNWPUS(YEAR)",yrs,po.ngnwpus[0:yrs],1990))
        f.write(write_aimms_variable("NGEXPUS_LNG(YEAR)",yrs,po.ngexpus_lng[0:yrs],1990))
        f.write(write_aimms_variable("NGEXPUS_PIPE(YEAR)",yrs,po.ngexpus_pipe[0:yrs],1990))
        f.write(write_aimms_variable("NGEXPUS(YEAR)",yrs,po.ngexpus[0:yrs],1990))
        f.write(write_aimms_variable("NGIMPUS(YEAR)",yrs,po.ngimpus[0:yrs],1990))
        f.write(write_aimms_variable("NGIMPUS_LNG(YEAR)",yrs,po.ngimpus_lng[0:yrs],1990))
        f.write(write_aimms_variable("NGIMPUS_PIPE(YEAR)",yrs,po.ngimpus_pipe[0:yrs],1990))

def steovars_write_to_clsteo(result_dict, pyfiler):
    """Write Coal specific messages to coal/clsteo.txt file.

    Parameters
    ----------
    result_dict : array
        a data array
    pyfiler : module
        pyfiler fortran module
    
    Returns
    -------
    None.

    """
    steo_def_1987=result_dict['STEO_DEF_1987']
    po=pyfiler.other
    with open(r'coal/clsteo.txt', 'w+') as f:
        f.write("  COMPOSITE TABLE:\n")
        f.write("  steoyr  AppalachiaLimit   InteriorLimit   WestLimit  ElecPriceSTEO   ElecTonsSTEO  WasteCoalSTEO  CokeTonsSTEO  IndustrialTonsSTEO CokingExpSTEO SteamExpSTEO ImportsSTEO EOYStocksEPEast EOYStocksEPMidW EOYStocksEPSouth EOYStocksEPWest EOYStocksPrim EOYStocksSecd\n")
        f.write("!------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
        for i in range(0,result_dict['LASTSTEOYEAR']-1990+1):
            s=f"{i+1990:6d}{po.clprpar_ton[i]:16.3f}{po.clprpir_ton[i]:16.3f}{po.clprpwr_ton[i]:14.3f}{po.cleudus[i]/steo_def_1987*po.gdpdius[i]:13.4f}{po.clepcon_ton[i]:16.3f}{po.clwcpus_ton[i]:13.2f}{po.clkcpus_ton[i]:16.3f}{po.clycpus_ton[i]:16.3f}{po.clexpmc_ton[i]:18.3f}{po.clexpsc_ton[i]:13.3f}{po.climp48_ton[i]:12.3f}{po.clps_ep_ne[i]:16.3f}{po.clps_ep_mw[i]:16.3f}{po.clps_ep_so[i]:17.3f}{po.clps_ep_we[i]:16.3f}{po.clsdpus[i]:14.3f}{po.clstpus[i]:14.3f}"
            f.write(s+'\n')


def steovars_convert(BASEYR,result_dict, STEO_VARS_FOUND,pyfiler,data):
    """Use the STEO values in memory, compute and convert the units, and update the values in the referenced pyfiler.
    
    Also call steovars_debug_write_random() and write_out_steodbug(), to random write statements into STEODBUG.txt to check versus input file.

    Parameters
    ----------
    BASEYR : int
        the base year
    result_dict : array
        a data array
    STEO_VARS_FOUND : list
        list of integers where 1/0 for the STEO var not found or found. 1 means not found
    pyfiler : module
        pyfiler fortran module
    data : dict
        a dict contains steovars.csv data value

    Returns
    -------
    module
        pyfiler fortran module

    
    """
    LASTSTEOYEAR=result_dict['LASTSTEOYEAR']
    steo_def_1987=result_dict['STEO_DEF_1987']  #57.0045
    #BASEYR=1990

    po=pyfiler.other
    end_yr=LASTSTEOYEAR-1989
    for y in range(0,end_yr):
        #po.balit[0:end_yr]   =  po.balit[0:end_yr]   * po.zsajqus[0:end_yr] / 1000.   # natural gas balancing item
        po.balit[y]   =  data['balit'][y]   * data['zsajqus'][y] / 1000.   # natural gas balancing item
        po.ngexpus_lng[y]  =  data['ngexpus_lng'][y]  * data['zsajqus'][y] / 1000. # liquefied natural gas exports
        po.ngexpus_pipe[y] =  data['ngexpus_pipe'][y] * data['zsajqus'][y] / 1000. # pipeline natural gas exports
        po.ngexpus[y] =  data['ngexpus'][y] * data['zsajqus'][y] / 1000.   # natural gas exports
        po.ngimpus[y] =  data['ngimpus'][y] * data['zsajqus'][y] / 1000.   # natural gas imports
        po.ngimpus_lng[y]  =  data['ngimpus_lng'][y]  * data['zsajqus'][y] / 1000. # liquefied natural gas imports
        po.ngimpus_pipe[y] =  data['ngimpus_pipe'][y] * data['zsajqus'][y] / 1000. # pipeline natural gas imports
        po.ngnwpus[y] =  data['ngnwpus'][y] * data['zsajqus'][y] / 1000.   # natural gas net withdrawals from inventory
        po.ngprpus[y] =  data['ngprpus'][y] * data['zsajqus'][y] / 1000.   # natural gas total dry production
        po.ngrcpus[y] =  data['ngrcpus'][y] * data['zsajqus'][y] / 1000.   # natural gas consumption, residential

        po.ngrcp_enc[y] =  data['ngrcp_enc'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_esc[y] =  data['ngrcp_esc'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_mac[y] =  data['ngrcp_mac'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_mtn[y] =  data['ngrcp_mtn'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_nec[y] =  data['ngrcp_nec'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_pac[y] =  data['ngrcp_pac'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_sac[y] =  data['ngrcp_sac'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_wnc[y] =  data['ngrcp_wnc'][y] * data['zsajqus'][y] / 1000.
        po.ngrcp_wsc[y] =  data['ngrcp_wsc'][y] * data['zsajqus'][y] / 1000.

        #NGCCPUS(I) =  NGCCPUS(I) * ZSAJQUS(I) / 1000.   !  natural gas consumption, commercial
        po.ngccpus[y] =  data['ngccpus'][y] * data['zsajqus'][y] / 1000.   #  natural gas consumption, commercial
        po.ngccp_enc[y] =  data['ngccp_enc'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_esc[y] =  data['ngccp_esc'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_mac[y] =  data['ngccp_mac'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_mtn[y] =  data['ngccp_mtn'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_nec[y] =  data['ngccp_nec'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_pac[y] =  data['ngccp_pac'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_sac[y] =  data['ngccp_sac'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_wnc[y] =  data['ngccp_wnc'][y] * data['zsajqus'][y] / 1000.
        po.ngccp_wsc[y] =  data['ngccp_wsc'][y] * data['zsajqus'][y] / 1000.
        po.nginx[y]   =  data['nginx'][y]   * data['zsajqus'][y] / 1000.   #  natural gas consumption, industrial (including lease and plant fuel)
        po.nglppus[y] =  data['nglppus'][y] * data['zsajqus'][y] / 1000.   #  natural gas lease and plant fuel
        po.ngvhpus[y] =  data['ngvhpus'][y] * data['zsajqus'][y] / 1000.   #  natural gas vehicle use
        po.ngacpus[y] =  data['ngacpus'][y] * data['zsajqus'][y] / 1000.   #  natural gas pipeline and distribution use
        po.ngepcon[y] =  data['ngepcon'][y] * data['zsajqus'][y] / 1000.   #  natural gas consumption, electric power
        po.ngsfpus[y] =  data['ngsfpus'][y] * data['zsajqus'][y] / 1000.   #  supplemental gaseous fuels supply
        po.ngtcpus[y] =  data['ngtcpus'][y] * data['zsajqus'][y] / 1000.   #  natural gas consumption
        #   convert these from billion kilowatthours per day to billion kilowatthours
        po.epeopus[y] =  data['epeopus'][y] * data['zsajqus'][y]      # total electric power sector generation
        po.cltopus[y] =  data['cltopus'][y] * data['zsajqus'][y]      #   from coal
        po.ngtopus[y] =  data['ngtopus'][y] * data['zsajqus'][y]      #   from natural gas
        po.nutopus[y] =  data['nutopus'][y] * data['zsajqus'][y]      #   from nuclear power
        po.patopus[y] =  data['patopus'][y] * data['zsajqus'][y]      #   from petroleum
        po.hptopus[y] =  data['hptopus'][y] * data['zsajqus'][y]      #   from pumped storage
        po.hvtopus[y] =  data['hvtopus'][y] * data['zsajqus'][y]      #   from conventional hydropower
        po.getopus[y] =  data['getopus'][y] * data['zsajqus'][y]      #   from geothermal
        po.sotopus[y] =  data['sotopus'][y] * data['zsajqus'][y]      #   from all solar
        po.wwtopus[y] =  data['wwtopus'][y] * data['zsajqus'][y]      #   from wood and other biomass
        po.tseopus[y] =  data['tseopus'][y] * data['zsajqus'][y]      # total electricity generation
        po.eldupus[y] =  data['eldupus'][y] * data['zsajqus'][y]      # direct use of electricity by non-electric power sector
        po.elnipus[y] =  data['elnipus'][y] * data['zsajqus'][y]      # net imports of electricity
        po.exrcpus[y] =  data['exrcpus'][y] * data['zsajqus'][y]      # residential electricity sales
        po.exrcp_enc[y] =  data['exrcp_enc'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_esc[y] =  data['exrcp_esc'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_hak[y] =  data['exrcp_hak'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_mac[y] =  data['exrcp_mac'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_mtn[y] =  data['exrcp_mtn'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_nec[y] =  data['exrcp_nec'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_pac[y] =  data['exrcp_pac'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_sac[y] =  data['exrcp_sac'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_wnc[y] =  data['exrcp_wnc'][y] * data['zsajqus'][y] / 1000.
        po.exrcp_wsc[y] =  data['exrcp_wsc'][y] * data['zsajqus'][y] / 1000.
        po.exccpus[y] =  data['exccpus'][y] * data['zsajqus'][y]      # commercial electricity sales
        po.exccp_enc[y] =  data['exccp_enc'][y] * data['zsajqus'][y] / 1000.
        po.exccp_esc[y] =  data['exccp_esc'][y] * data['zsajqus'][y] / 1000.
        po.exccp_hak[y] =  data['exccp_hak'][y] * data['zsajqus'][y] / 1000.
        po.exccp_mac[y] =  data['exccp_mac'][y] * data['zsajqus'][y] / 1000.
        po.exccp_mtn[y] =  data['exccp_mtn'][y] * data['zsajqus'][y] / 1000.
        po.exccp_nec[y] =  data['exccp_nec'][y] * data['zsajqus'][y] / 1000.
        po.exccp_pac[y] =  data['exccp_pac'][y] * data['zsajqus'][y] / 1000.
        po.exccp_sac[y] =  data['exccp_sac'][y] * data['zsajqus'][y] / 1000.
        po.exccp_wnc[y] =  data['exccp_wnc'][y] * data['zsajqus'][y] / 1000.
        po.exccp_wsc[y] =  data['exccp_wsc'][y] * data['zsajqus'][y] / 1000.
        po.exicpus[y] =  data['exicpus'][y] * data['zsajqus'][y]      # industrial electricity sales
        po.exacpus[y] =  data['exacpus'][y] * data['zsajqus'][y]      # transportation electricity sales
        po.extcpus[y] =  data['extcpus'][y] * data['zsajqus'][y]      # total electricity sales
        po.estxpus[y] =  data['estxpus'][y] * data['zsajqus'][y]      # total electricity consumption
        po.tdlopus[y] =  data['tdlopus'][y] * data['zsajqus'][y]      # transmission & distribution losses and unaccounted for

        #!   convert these from million kilowatthours per day to billion kilowatthours
        #       ! electric power sector generation
        # There is no CLEAP_US in steovars.csv anymore, so data['clep_us'][y] etc. columns doesn't exist. Later shall also remove their common block declaration
        # ref: main.f#L11109
        '''
        '''
        #  no longer convert small scale solar, steo changed units
        #   SODTP_US(I) =  SODTP_US(I) * ZSAJQUS(I) / 1000. #  from small scale solar
        '''
        # end-use generation
        '''
        # convert from thousand btu per kilowatthour to btu per kilowatthour

        #! price conversions
        #!   convert these from nominal dollars per barrel to 1987 dollars per barrel
        po.brepuus[y] =  data['brepuus'][y] * steo_def_1987 / data['gdpdius'][y]        # brent crude oil
        po.wtipuus[y] =  data['wtipuus'][y] * steo_def_1987 / data['gdpdius'][y]        # west texas intermediate crude oil
        po.raimuus[y] =  data['raimuus'][y] * steo_def_1987 / data['gdpdius'][y]        # average imported refiner inquisition
        #   convert these from nominal cents per gallon to 1987 dollars per gallon
        po.d2rcaus[y] =  data['d2rcaus'][y] * steo_def_1987 / data['gdpdius'][y] / 100. # retail heating oil incl taxes
        po.dsrtuus[y] =  data['dsrtuus'][y] * steo_def_1987 / data['gdpdius'][y] / 100. # retail diesel fuel incl taxes
        po.jktcuus[y] =  data['jktcuus'][y] * steo_def_1987 / data['gdpdius'][y] / 100. # jet fuel from refiner to end user
        po.mgeiaus[y] =  data['mgeiaus'][y] * steo_def_1987 / data['gdpdius'][y] / 100. # retail gasoline all grades incl taxes
        po.mgrarus[y] =  data['mgrarus'][y] * steo_def_1987 / data['gdpdius'][y] / 100. # retail regular motor gasoline incl taxes
        po.rftcuus[y] =  data['rftcuus'][y] * steo_def_1987 / data['gdpdius'][y] / 100. # no. 6 residual fuel oil
        #   convert these from nominal dollars per million btu to 1987 dollars per million btu
        po.cleudus[y] =  data['cleudus'][y] * steo_def_1987 / data['gdpdius'][y]        # coal, electric power
        po.ngeudus[y] =  data['ngeudus'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, electric power
        po.nghhuus[y] =  data['nghhuus'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas at henry hub
        #   convert these from nominal dollars per thousand cubic feet to 1987 dollars per thousand cubic feet
        po.ngccu_enc[y] =  data['ngccu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, east north central
        po.ngccu_esc[y] =  data['ngccu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, east south central
        po.ngccu_mac[y] =  data['ngccu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, middle atlantic
        po.ngccu_mtn[y] =  data['ngccu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, mountain
        po.ngccu_nec[y] =  data['ngccu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, new england
        po.ngccu_pac[y] =  data['ngccu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, pacific
        po.ngccu_sac[y] =  data['ngccu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, south atlantic
        po.ngccu_wnc[y] =  data['ngccu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, west north central
        po.ngccu_wsc[y] =  data['ngccu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial, west south central
        po.ngccuus[y] =  data['ngccuus'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, commercial
        po.ngicu_enc[y] =  data['ngicu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, east north central
        po.ngicu_esc[y] =  data['ngicu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, east south central
        po.ngicu_mac[y] =  data['ngicu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, middle atlantic
        po.ngicu_mtn[y] =  data['ngicu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, mountain
        po.ngicu_nec[y] =  data['ngicu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, new england
        po.ngicu_pac[y] =  data['ngicu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, pacific
        po.ngicu_sac[y] =  data['ngicu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, south atlantic
        po.ngicu_wnc[y] =  data['ngicu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, west north central
        po.ngicu_wsc[y] =  data['ngicu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial, west south central
        po.ngicuus[y] =  data['ngicuus'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, industrial
        po.ngrcu_enc[y] =  data['ngrcu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, east north central
        po.ngrcu_esc[y] =  data['ngrcu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, east south central
        po.ngrcu_mac[y] =  data['ngrcu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, middle atlantic
        po.ngrcu_mtn[y] =  data['ngrcu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, mountain
        po.ngrcu_nec[y] =  data['ngrcu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, new england
        po.ngrcu_pac[y] =  data['ngrcu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, pacific
        po.ngrcu_sac[y] =  data['ngrcu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, south atlantic
        po.ngrcu_wnc[y] =  data['ngrcu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, west north central
        po.ngrcu_wsc[y] =  data['ngrcu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential, west south central
        po.ngrcuus[y] =  data['ngrcuus'][y] * steo_def_1987 / data['gdpdius'][y]        # natural gas, residential
        #   convert these from nominal cents per kilowatthour btu to 1987 cents per kilowatthour
        po.escmu_enc[y]  =  data['escmu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, east north central
        po.escmu_esc[y]  =  data['escmu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, east south central
        po.escmu_mac[y]  =  data['escmu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, middle atlantic
        po.escmu_mtn[y]  =  data['escmu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, mountain
        po.escmu_nec[y]  =  data['escmu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, new england
        po.escmu_pac[y]  =  data['escmu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, pacific
        po.escmu_sac[y]  =  data['escmu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, south atlantic
        po.escmu_wnc[y]  =  data['escmu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, west north central
        po.escmu_wsc[y]  =  data['escmu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity, west south central
        po.escmuus[y] =  data['escmuus'][y] * steo_def_1987 / data['gdpdius'][y]        # commercial electricity
        po.esicu_enc[y]  =  data['esicu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, east north central
        po.esicu_esc[y]  =  data['esicu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, east south central
        po.esicu_mac[y]  =  data['esicu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, middle atlantic
        po.esicu_mtn[y]  =  data['esicu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, mountain
        po.esicu_nec[y]  =  data['esicu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, new england
        po.esicu_pac[y]  =  data['esicu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, pacific
        po.esicu_sac[y]  =  data['esicu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, south atlantic
        po.esicu_wnc[y]  =  data['esicu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, west north central
        po.esicu_wsc[y]  =  data['esicu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity, west south central
        po.esicuus[y] =  data['esicuus'][y] * steo_def_1987 / data['gdpdius'][y]        # industrial electricity
        po.esrcu_enc[y]  =  data['esrcu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, east north central
        po.esrcu_esc[y]  =  data['esrcu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, east south central
        po.esrcu_mac[y]  =  data['esrcu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, middle atlantic
        po.esrcu_mtn[y]  =  data['esrcu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, mountain
        po.esrcu_nec[y]  =  data['esrcu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, new england
        po.esrcu_pac[y]  =  data['esrcu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, pacific
        po.esrcu_sac[y]  =  data['esrcu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, south atlantic
        po.esrcu_wnc[y]  =  data['esrcu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, west north central
        po.esrcu_wsc[y]  =  data['esrcu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity, west south central
        po.esrcuus[y] =  data['esrcuus'][y] * steo_def_1987 / data['gdpdius'][y]        # residential electricity
        po.estcu_enc[y]  =  data['estcu_enc'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, east north central
        po.estcu_esc[y]  =  data['estcu_esc'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, east south central
        po.estcu_mac[y]  =  data['estcu_mac'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, middle atlantic
        po.estcu_mtn[y]  =  data['estcu_mtn'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, mountain
        po.estcu_nec[y]  =  data['estcu_nec'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, new england
        po.estcu_pac[y]  =  data['estcu_pac'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, pacific
        po.estcu_sac[y]  =  data['estcu_sac'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, south atlantic
        po.estcu_wnc[y]  =  data['estcu_wnc'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, west north central
        po.estcu_wsc[y]  =  data['estcu_wsc'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, west south central
        po.estcuus[y] =  data['estcuus'][y] * steo_def_1987 / data['gdpdius'][y]        # electricity, all sector average

    #!  some random write statements to check versus input file:
    s=steovars_debug_write_random(po,0,LASTSTEOYEAR-BASEYR+1)
    write_out_steodbug(s)

    return pyfiler

def steovars_debug_write_random(po,start_idx,end_idx):
    """Use the definied random target STEO group, return the statements to allow check versus the STEO input file value.

    Parameters
    ----------
    po : array
        the referenced pyfiler.other
    start_idx : int
        the start index (the target start year we are interested to check)
    end_idx : int
        the end index (the target end year we are interested to check)

    Returns
    -------
    str
        the composed string to be written out
    """

    msg=["ARTCBUS","MBFPPUS","MBRIPUS","KSTCPUS","LUTCPUS","WXTCPUS","OHNIPUS","OHRIPUS","PAGLPUS","RFNPPUS","RAIMUUS","SOCMGEN_US","ZWHD_WSC"]

    for i in range(start_idx,end_idx):
        msg[0]+=f'{po.artcbus[i]:12.6f}'
        msg[1]+=f'{po.mbfppus[i]:12.6f}'
        msg[2]+=f'{po.mbripus[i]:12.6f}'
        msg[3]+=f'{po.kstcpus[i]:12.6f}'
        msg[4]+=f'{po.lutcpus[i]:12.6f}'
        msg[5]+=f'{po.wxtcpus[i]:12.6f}'
        msg[6]+=f'{po.ohnipus[i]:12.6f}'
        msg[7]+=f'{po.ohripus[i]:12.6f}'
        msg[8]+=f'{po.paglpus[i]:12.6f}'
        msg[9]+=f'{po.rfnppus[i]:12.6f}'
        msg[10]+=f'{po.raimuus[i]:12.6f}'
        msg[11]+=f'{po.socmgen_us[i]:12.6f}'
        msg[12]+=f'{po.zwhd_wsc[i]:12.6f}'

    s=''
    for i in msg:
        s+=i+'\n'
    return s

def steovars_clean_up(steovars_file, BASEYR, steovr, list_steo_var_name, STEO_VARS_KEEP, pyfiler):
    """Perform feature Engineering on steovars.csv file and call steovars_write_meta() to write out the cleaned steovars.csv as steovars_meta.csv

    Parameters
    ----------
    steovars_file : str
        the file path of the steovars.csv
    BASEYR : int
        the base year. For example, 1990
    steovr : int
        the STEO variable number
    list_steo_var_name : list
        the variable name list of STEO
    STEO_VARS_KEEP : list
        the kept STEO vars list
    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    dict
        a dict contains LASTSTEOYEAR, steo_def_1987 etc. etc. setting info
    list
        the int list with 1/0 for the STEO var not found or found. 1 means not found
    dict
        a dict contains steovars.csv data value

    """
    lines = open(steovars_file,'r').readlines()
    lines=[x.strip() for x in lines]

    # skip the lines start with 'TIMESTAMP','AAA_OFFSET',or 'AAAA_DATEX'
    removed=['TIMESTAMP','AAA_OFFSET','AAAA_DATEX']
    for i in removed:
        pos=len(i)
        lines =[x for x in lines if x[:pos] != i]

    #  some data ends before the last STEO year.  this, putting a 0 at the end of lines ending with a comma,
    #  is an attempt to read those lines without hitting end-of-line early
    #  (filled cells in the final column don't have a comma following them when saved comma-delimited)
    # for example: COPC_NI
    lines=[x+'0' if x[-1]==',' else x for x in lines]

    FIRSTSTEOYEAR=0
    LASTSTEOYEAR=0
    SKIPYEARS=0
    MNUMYR=61
    STEO_DEF_1987 =0

    dict_STEO_VARS_FOUND={}
    STEO_VARS_FOUND=[1]*steovr   # will get turned to 0 when the variable is filled
    data_in_memory={}
    for idx, line in enumerate(lines):
        # log a warning message if a line has no more than 999 chars. Or, move to next line if the line is empty
        steo_eol=len(line)
        if steo_eol > 999: write_out_steodbug(f"  Possible problem:  end of line={steo_eol} approaching maximum length of line read=999")
        if steo_eol ==0:
            write_out_steodbug(f"  Interesting problem in which the end of line is before the beginning of line.  Line={idx+1}")
            continue

        data=line.split(',')
        data[0]=data[0].strip()
        # find the info of first year and last year
        if data[0]=='':
            FIRSTSTEOYEAR=int(data[1])
            LASTSTEOYEAR=int(data[-1])
            write_out_steodbug(f" dates, first year = {FIRSTSTEOYEAR}\n"+f" dates, last year = {LASTSTEOYEAR}")
            SKIPYEARS = BASEYR - FIRSTSTEOYEAR

            steovars_write_meta(['STEO\YEAR']+data[SKIPYEARS+1:],'w+')
            data_in_memory['STEO\YEAR']=data[SKIPYEARS+1:]
            continue

        # find the info of STEOMONTH
        # in mmm yyyy format such as "Oct 2023"; current plan is to check in STEO file a15bbb as steovars.csv every month
        elif data[0]=='STEOMONTH':
            STEO_MONTH=data[1][:3]
            write_out_steodbug(f" STEO month found = {STEO_MONTH}")
            continue

        # now variable line, read in variable name and data
        #  initialize read variable so lack of entry is 0 rather than the last read value for that year
        #  this variable is dimensioned to cover the years in the STEO file, which begins before 1990
        #  all the years are read.  it will only transfer over the applicable years
        else:
            KEEP_CHECKING=True
            write_out_steodbug(f" STEO variable #{data[0]}# data begins at position={len(data[0])+2}  ends at position={len(line)}")

            for i in range(0, steovr):

                if data[0] not in list_steo_var_name:
                    dict_STEO_VARS_FOUND.update({data[0]:1})
                    continue
                elif KEEP_CHECKING==True and data[0] ==STEO_VARS_KEEP[i]:
                    '''
                    indices = [i for i, x in enumerate(STEO_VARS_KEEP) if x == data[0]]
                    i=indices[0]
                    '''
                    write_out_steodbug(f"  STEO variable {data[0]} found at equivalenced position {i}")
                    STEO_VARS_FOUND[i]=0  #  will get turned to 0 when the variable is filled (This is original main.f design)

                    # now default 0 for those empty STEO value among BASEYR to LASTYR
                    data=steovars_refill_default_zero(data)

                    KEEP_CHECKING=False

                    # compute EQ_STEO_VARS and write into common block
                    #DO IYEAR=FIRSTSTEOYEAR+SKIPYEARS,LASTSTEOYEAR
                    for y in range(FIRSTSTEOYEAR+SKIPYEARS,LASTSTEOYEAR+1):  #range(1990,2024)
                        # EQ_STEO_VARS((I-1)*MNUMYR+IYEAR-BASEYR+1) = STEO_IN(IYEAR-FIRSTSTEOYEAR+1)
                        pyfiler.other.eq_steo_vars[(i-1+1)*MNUMYR+y-BASEYR]=data[y-FIRSTSTEOYEAR+1]

                    # special handling 'GDPDIUS'
                    if data[0]=='GDPDIUS':
                        STEO_DEF_1987 = data[1987-FIRSTSTEOYEAR+1]    #row 529, column N. ie. year 1987
                        write_out_steodbug(f"  The 1987 implicit price deflator={STEO_DEF_1987}")

                    # now update the dict_STEO_VARS_FOUND
                    dict_STEO_VARS_FOUND.update({data[0]:0})

                    steovars_write_meta(data[0:1]+data[SKIPYEARS+1:])
                    s=data[0].lower()
                    data_in_memory[s]=data[SKIPYEARS+1:]

    # now EOF
    write_out_steodbug(f"  End of STEO file reached after {len(lines)+len(removed)} variables (rows) read.")
    if sum(STEO_VARS_FOUND) > 0:
        for i in range(0,steovr):
            if STEO_VARS_FOUND[i]==1: write_out_steodbug(f"  variable {STEO_VARS_KEEP[i]} not Encountered when it was an expected STEO variable")

    result_dict={'FIRSTSTEOYEAR':FIRSTSTEOYEAR,'LASTSTEOYEAR':LASTSTEOYEAR,'SKIPYEARS':SKIPYEARS,'MNUMYR':MNUMYR,'STEO_DEF_1987':STEO_DEF_1987}
    return result_dict, STEO_VARS_FOUND, data_in_memory

def steovars_write_meta(out_msgs, mode='a'):
    """"A centralized printout method, to write messages to steovars_meta.csv file

    Parameters
    ----------
    out_msg : str
        the message to print out
    mode : str, optional
        write or append mode. Defaults to 'a'.

    Returns
    -------
    None

    """
    out_msg = ",".join(str(e) for e in out_msgs)
    with open('steovars_meta.csv',mode) as f:
        f.write(out_msg+'\n')

def steovars_refill_default_zero(data):
    """Process the passed over data and fill with zero (0) on those empty SETO values among BASEYR to LASTYR

    Parameters
    ----------
    data : array
        the raw STEO values

    Returns
    -------
    array
        the processed STEO values
    """
    # now default 0 for those empty STEO values among BASEYR to LASTYR
    FIRST_COL=data[0]
    s=data[1:]
    tmp=[0.0 if x.strip()=='' else float(x) for x in s]

    data=[]
    data.append(FIRST_COL)
    data.extend(tmp)
    return data

def steovars_steolist_read_in(steovr):
    """Read in steolist.txt, parse and compare with variables in common block. Then return a list of STEO variable name list,
    and STEO_VARS_KEEP array. Call write_out_steodbug() and write error messages to STEODBUG.txt file.

    Parameters
    ----------
    steovr : int
        the number of STEO variables

    Returns
    -------
    list
        a list of STEO variable name list
    list
        an array of list variables with the order
    """
    #!   writes will go to STEODBUG.txt except ones with "Problem" (or "problem") in string
    UNIT_STEO_IN='./input/steolist.txt'
    write_out_steodbug("  Reading file with list of variables",'w+')

    lines = open(UNIT_STEO_IN,'r').readlines()
    find_flag=0
    idx_beg=0
    idx_end=len(lines)-1
    write_out_steodbug("  Reading list of variables:  found beginning of list")
    for idx, line in enumerate(lines):
        if line[0]=='!' and line[1]=='@':
            if find_flag==0:
                find_flag=1
                idx_beg=idx
            else:
                find_flag=2
                idx_end=idx
        continue

    # extract out the lines in-between '!@'
    new_list=lines[idx_beg+1:idx_end]
    new_list=[x.rstrip() for x in new_list]
    new_list=[x for x in new_list if x[0:1] !='!']

    steo_count=0
    STEO_VARS_KEEP=['']*steovr
    list_steo_var_name=[]
    word='(MNUMYR)'
    if len(new_list) > steovr:
        write_out_steodbug(f"  Problem:  Number of variables in common block greater than STEOVR({steovr}): {len(new_list)} variables read.")
        os.sys.exit()

    for idx, line in enumerate(new_list):
        match_obj = re.search(word, line, re.IGNORECASE)
        if match_obj:

            # now use '(MNUMYR)' location to back track variable name. Need to deduct the last char white space.
            e=match_obj.start()-2
            b=line[:e+1].rfind(' ')
            steo_var_name=line[b+1:e+1]
            list_steo_var_name.append(steo_var_name)
            STEO_VARS_KEEP[idx]=steo_var_name
            write_out_steodbug(f"  Reading list of variables:  found variable name:  {steo_var_name}")

            steo_count+=1
            #if steo_count > steovr:
            if len(list_steo_var_name) > steovr:
                write_out_steodbug(f"  Problem:  Number of variables in common block greater than STEOVR({steovr}): {steo_count} variables read.")
                os.sys.exit()

    write_out_steodbug(f" End of list found after {steo_count} variable names found.")
    return list_steo_var_name,STEO_VARS_KEEP

def write_out_steodbug(out_msg, mode='a'):
    """"A centralized log printout method, to write messages to STEODEBUG.txt file

    Parameters
    ----------
    out_msg : str
        the message to print out
    mode : str, optional
        write or append mode. Defaults to 'a'.
    
    Returns
    -------
    None.

    """
    out_file='STEODBUG'
    with open(out_file+'.txt',mode) as f:
        f.write(out_msg+'\n')

if __name__ == '__main__':
    print("Please load from the main program script. Exit now...")
    os.sys.exit()
