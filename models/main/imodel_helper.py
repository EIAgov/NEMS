# -*- coding: utf-8 -*-
"""
Created on Mar 15 2024

@author: Claire Su
"""

def set_model_dicts():
    """Set up a mapping dictionary, to prepare for imodel and model name lookup. The dict has model names as key and imodel integers as value
    
    INTEGER EXW       !=RUNMOD( 1) EXECUTE WORLD  (INTERNATIONAL), IEM
    INTEGER EXM       !=RUNMOD( 2) EXECUTE MAC    (MACROECONOMIC), MAM
    INTEGER EXR       !=RUNMOD( 3) EXECUTE RESD   (RESIDENTIAL)
    INTEGER EXK       !=RUNMOD( 4) EXECUTE COMM   (COMMERCIAL)
    INTEGER EXI       !=RUNMOD( 5) EXECUTE IND    (INDUSTRIAL)
    INTEGER EXT       !=RUNMOD( 6) EXECUTE TRAN   (TRANSPORTATION)
    INTEGER EXE       !=RUNMOD( 7) EXECUTE UTIL   (UTILITY)
    INTEGER EXC       !=RUNMOD( 8) EXECUTE COAL   (COAL SUPPLY)
    INTEGER EXL       !=RUNMOD( 9) EXECUTE WELL   (OIL AND GAS SUPPLY), HSM
    INTEGER EXG       !=RUNMOD(10) EXECUTE PIPE   (GAS TRANS.& DISTR.)
    INTEGER EXO       !=RUNMOD(11) EXECUTE REFINE (PETROLEUM REFINERY)
    INTEGER EXN       !=RUNMOD(12) EXECUTE RENEW  (RENEWABLES), RFM
    INTEGER EXH       !=RUNMOD(13) EXECUTE Hydrogen
    INTEGER EXH       !=RUNMOD(13) EXECUTE CCATS

    Returns
    -------
    dict
        a dictionary to look up model number by a model name. ex.{'IEM': 1, 'MAM': 2, 'RDM': 3, ...}
    """
    #imodel_to_name_dict = {1:'WORLD', 2:'MAC', 3:'RESD', 4:'COMM', 5:'IND', 6:'TRAN',7:'UTIL',8:'COAL',9:'WELL',10:'NGMM',11:'LFMM',12:'RENEW',13:'HYDROGEN'}
    imodel_to_name_dict = {1:'IEM', 2:'MAM', 3:'RDM', 4:'CDM', 5:'IDM', 6:'TDM',7:'EMM',8:'CMM',9:'HSM',10:'NGMM',11:'LFMM',12:'RFM',13:'HMM',14:'CCATS'}
    name_to_imodel_dict = {value:key for key, value in imodel_to_name_dict.items()}
    return name_to_imodel_dict

def get_imodel(name_to_imodel_dict, current_nems_model):
    """Look up the model name to imodel mapping dictionary by a model name and get its corresponding imodel number

    Parameters
    ----------
    name_to_imodel_dict : dict
        a dictionary to look up model number by a model name. ex.{'IEM': 1, 'MAM': 2, 'RDM': 3, ...}
    
    current_nems_model : str
        the model name

    Returns
    -------
    int
        the imodel number
    """
    result = name_to_imodel_dict.get(current_nems_model, -999)
    return result

def get_subr_name(name_to_imodel_dict,imodel):
    """Look up the imodel name to name mapping dictionary by an imodel name and get its corresponding model name

    Parameters
    ----------
    name_to_imodel_dict : dict
        a dictionary to look up model number by a model name. ex. {1:'IEM', 2:'MAM', 3:'RDM',...}
    
    imodel : int
        the imodel number

    Returns
    -------
    str
        the model name
    """
    imodel_to_name_dict= {value:key for key, value in name_to_imodel_dict.items()}
    result = imodel_to_name_dict.get(imodel, "Not Found")
    return result
