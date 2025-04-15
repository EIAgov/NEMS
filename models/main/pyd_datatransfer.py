import numpy as np
import os
import time

def timenems(func):
    """timing decorator used to log the function execution time
    
    Parameters
    ----------
    func : callable
        the function to be decorated / timed
        
    Returns
    -------
    callable
        a wrapper function that adds extra functionality before and after
        the execution of the original function
        
    """
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        result = func(*args, **kwargs)
        end = time.perf_counter()
        elapsed = end - start
        print(f'Function {func.__name__!r} executed in {(elapsed):.6f}ns')
        
        return result
    
    return wrapper

@timenems
def copy_tables_between_pyd(pyfiler_write_out, pyfiler_read_in):
    """transfers data from one pyfiler module to another pyfiler module using commonblocks
    
    This is to transfer everything using commoblocks EXCEPT:
        other, utils, and anything else listed in the "commonblock_exclude_list" variable
    
    Parameters
    ----------
    pyfiler_write_out : module
        pyfiler fortran module to copy data from
        
    pyfiler_read_in : module
        pyfiler fortran module to copy data to
        
    Returns
    -------
    None.
    
    """
    
    # Create an empty list
    copy_list = []
    
    # Append a list of common block names exist in both pyfiler modules
    for i in set(dir(pyfiler_write_out)).intersection(set(dir(pyfiler_read_in))):
        if i[0] != '_':
            copy_list.append(i)

    for i in set(dir(pyfiler_write_out)).symmetric_difference(set(dir(pyfiler_read_in))):
        if i[0]!="_":
            copy_list.append(i)
    
    # list of common blocks to exclude from the pyfiler data transfer
    commonblock_exclude_list = [
        "ncntrl", "qsblk", "uefpout", "uettout", "indrep", "indout", "apmore", "bldglrn",
        "rlx", "cycleinfo", "pmmftab", "emablk", "csapr", "e111d", "aponroad", "hmmblk",
        "rggi", "mpblk", "emission", "bifurc", "mxqblk", "ampblk", "tcs45q", "efpout",
        "aeusprc", "continew", "coalprc", "convfact", "ngtdmout", "resdrep", "uso2grp",
        "emeblk", "intout", "qblk", "angtdm", "comparm", "qonroad", "indrep2", "rscon",
        "lfmmout", "coalemm", "eusprc", "ab32", "regco2", "nchar", "commrep", "coalout",
        "ponroad", "uecpout", "avepas", "qmore", "coalrep", "epmbank", "wrenew", "ghgrep",
        "uefdout", "converge", "mcdetail", "ngtdmrep", "tranrep", "pmmout", "acoalprc",
        "rseff", "macout", "cogen", "mxpblk", "pmmrpt", "ogsmout", "dsmtfefp", "pmore",
        "ngrpt", "udatout"
        ]

    # Remove the 'utils' and 'other' modules from the commonblock list
    remove_list = ['utils', 'other']
    remove_list.extend(commonblock_exclude_list)
    copy_list = list(set(copy_list) - set(remove_list))
    
    # Iterate through the commonblock list to copy data from one pyfiler module to the other
    for i in copy_list:
        try:
            for j in dir(getattr(pyfiler_write_out, i)):
                #print(i, " : ", j)
                if j[0] != '_':
                    try:
                        p1var = np.copy(getattr(getattr(pyfiler_write_out, i), j))
                        setattr(getattr(pyfiler_read_in, i), j, p1var)
                    except AttributeError:
                        pass
                    except TypeError:
                        pass
        except AttributeError:
            pass


@timenems
def copy_equivalences_between_pyd(pyfiler_write_out, pyfiler_read_in):
    """transfers data from one pyfiler module to another pyfiler module using fortran equivalence
    
    
    Parameters
    ----------
    pyfiler_write_out : module
        pyfiler fortran module to copy data from
        
    pyfiler_read_in : module
        pyfiler fortran module to copy data to
        
    Returns
    -------
    None.
    
    """
    copy_list = [] # list to hold block names to copy between pyd(s)
    eq_list = []

    copy_list = [
        'MQARRY','MPARRY','PMOREQ','QMOREQ','MXQARRY','MXPARRY','MQSARY','NCNTRL_EQUIV','CYC_INF_EQ', 'NCHAR_EQ','EQ_CLOUT',
        'EQ_CLREP','EQ_CGOUT','EFPDAT','UEFPDAT','UEFDDAT','UDATDAT','UECPDAT','DSMEFDAT','ULDSMDAT','UETTDAT',
        'EQ_EMOUT','EQ_EAOUT','EQ_INOUT','EQ_INREP','EQ_INREP2','EQ_ITOUT','EQ_NTOUT','CLPRCOUT', #' EMMCLOUT', 
        'EMMCLOUT','EQ_CONVERGE',
        'OQARRY','OPARRY','PELOUT','EQ_RSREP','EQ_RSCON','EQ_RSEFF','EQ_TRREP','EQ_CMPRM','EQ_CMREP','EQ_MCOUT',
        'EQ_MCDTL','EQ_NTREP','EQ_NGRPT','EQ_OGOUT','EQ_LFMMOUT','EQ_RFOUT','EQ_RFRPT','EQ_RFFTAB','WRARRY',
        'EQ_CFOUT','EQ_EMEBLK','PMOREQ','CLPRCOUT','PELOUT','EQ_EMEBLK','EQ_SO2GRP','EQ_BldgLrn','EQ_BIFURC','EQ_EPMBANK',
        'EQ_GHGREP','OPARRY','HMARRY','EQ_REGCO2','EQ_CONT','EQ_AB32','EQ_RGGI','EQ_CSAPR','EQ_E111D','TCS45QEQ'
        ] # equivalence common block names (see filer.f)
    
    commonblock_copy_list = [
        'QBLK','MPBLK','PMORE','QMORE','MXQBLK','MXPBLK','QSBLK','NCNTRL','CYCLEINFO','NCHAR','COALOUT',
        'COALREP','COGEN','EFPOUT','UEFPOUT','UEFDOUT','UDATOUT','UECPOUT','DSMTFEFP','ULDSMOUT','UETTOUT',
        'EMISSION','EMABLK','INDOUT','INDREP','INDREP2','INTOUT','ANGTDM','COALPRC','COALEMM','CONVERGE',
        'QONROAD','PONROAD','EUSPRC','RESDREP','RSCON','RSEFF','TRANREP','COMPARM','COMMREP','MACOUT',
        'MCDETAIL','NGTDMOUT','NGTDMREP','NGRPT','OGSMOUT','LFMMOUT','PMMOUT','PMMRPT','PMMFTAB','WRENEW',
        'CONVFACT','AMPBLK','APMORE','ACOALPRC','AEUSPRC','EMEBLK','USO2GRP','BldgLrn','BIFURC','EPMBANK',
        'GHGREP','APONROAD','HMMBLK','REGCO2','CONTINEW','AB32','RGGI','CSAPR','E111D','TCS45Q'
        ] # common block names
    
    in_pyfiler_other = [
        'PARAMETR','AMPBLK','ANGTDM','RSCON','ACOALPRC','APONROAD','AEUSPRC'
        ] # stored in other
    
    equivalences_from_includes = [
        'CLPRCOUT','PELRSOUT','PELCMOUT','PELINOUT','PELOUT','MPRC','MPARRY','PMOREQ','EQ_NTOUT','MUPRC',
        'SMUPRC','SGASPRICE','OPARRY','EP$LF','EPAVOID','BUFFER','R_CL_UNITS','EMMCLOUT','QCLCLNR','HCLCLNR',
        'GCLCLNR','PSLCLNR','PHGCLNR','PCACLNR','HRTCLNR','RCLCLNR','SCLCLNR','BCLCLNR','CCLCLNR','XCLCLNR',
        'EQ_CLOUT','CLPRCOUT','EQ_CLREP','EQ_CGOUT','EQ_CMREP','EQ_CMPRM','EQ_CONT','EQ_CONVERGE','EQ_CFOUT','EQ_CSAPR',
        'CHRNUM0','VALUE0','VALUE0','BASE','UEITAJ','NMARCST','ELHGHT','MARCST','DSMEFDAT','EQ_E111D',
        'PELOUTN','EFPDAT','UOBKVL','EGTXRT','ESCWPP','REVREQ','DEMREP','EQ_EAOUT','EQ_EMOUT','ALPRCOUT',
        'APELRSOUT','APELCMOUT','APELINOUT','APELOUT','AMPRC','AMPARRY','APMOREQ','AMUPRC','ASGASPRICE','AOPARRY',
        'PELRSOUT','PELCMOUT','PELINOUT','PELOUT','UPFUEL','HMARRY','EQ_INOUT','QELINOUT','EQ_INREP','EQ_INREP2',
        'EQ_ITOUT','EQ_LFMMOUT','ULOAD','EQ_MCOUT','EQ_MCPRM','EQ_MCDTL','MPRC','MPARRY','PMOREQ','MXPRC',
        'MXPARRY','MXQTY','MXQARRY','NCNTRL_EQUIV','NCHAR_EQ','CYC_INF_EQ','EQ_NGRPT','EQ_NTOUT','MUPRC','SMUPRC',
        'TRQPLDV','TRQPRAIL','TRQPSHIP','SGASPRICE','EQ_NTREP','EQ_OGOUT','WMXCP','WC_NP','EQ_RFFTAB','EQ_RFOUT',
        'EQ_RFRPT','OPARRY','MQTY','MQARRY','QMOREQ','OQARRY','MQCEN','MQSARY','EQ_RSREP','EQ_RSCON',
        'EQ_RSEFF','EQ_STEO_VARS','TCS45QEQ','EQ_TRREP','UDATDAT','UECPDAT','UEFDDAT','MUQTY','SMUQTY','SGASQUANT',
        'UEFPDAT','UETTDAT','ULDSMDAT','EQ_SO2GRP','WDSUP_Q','WDSUP_P','WRARRY', 'EQ_CCATSOUT'
    ]
    
    # using list comprehension to concat and make lowercase
    eq_list = [y.lower() for x in [equivalences_from_includes, copy_list] for y in x]
    
    # drops duplicate from the list and sort them in ascending alphabetically
    eq_list = sorted(list(set(eq_list)))

    for n in eq_list:
        
        if (hasattr(pyfiler_write_out.utils, n) and hasattr(pyfiler_read_in.utils, n)):
            # Note that np.copy makes a deepcopy of object
            setattr(pyfiler_read_in.utils, n, np.copy(getattr(pyfiler_write_out.utils, n)))
        if (hasattr(pyfiler_write_out.other, n) and hasattr(pyfiler_read_in.other, n)): 
            # Note that np.copy makes a deepcopy of object 
            setattr(pyfiler_read_in.other, n, np.copy(getattr(pyfiler_write_out.other, n)))



if __name__ == '__main__':
    print("Please load from the main program script. Exit now...")
    os.sys.exit()