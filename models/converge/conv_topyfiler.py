import numpy as np

def values_back_to_pyfiler(pyfiler1, dat):
    """copies data from dict of dataframes back to PyFiler memory

    Parameters
    ----------
    pyfiler1 : module
        pyfiler fortran module
    
    dat : dict
        dict of convergence variables in dataframes
    """
    master_list=dat.keys()

    for var in list(master_list):

        element=var.split("/")
        # Skip if commonblock and variable are 'NCNTRL/cnvtst'
        if var == 'NCNTRL/cnvtst':
            continue

        # Skip if variable is 1 dimensional
        if (len(element)==1):
            continue

        # Copy if variable is 2 dimensional
        if len(element) == 2:
            tmp_value = np.copy(dat[var].values)
            setattr(getattr(pyfiler1, element[0].lower()), element[1], tmp_value)
        
        # Copy if variable is 3 dimensional
        elif len(element) == 3:
            if element[2].isnumeric():
                tmp_index_region = int(element[2])
                tmp_value = np.copy(dat[var].values)

                try:
                    getattr(getattr(pyfiler1, element[0].lower()), element[1])[tmp_index_region] = tmp_value
                except AttributeError:
                    getattr(pyfiler1.utils, element[1])[tmp_index_region] = tmp_value