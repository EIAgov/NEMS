import pandas as pd
import numpy as np
#dat=pd.HDFStore(r"D:\output\ark\ref2024\d070824d\converge\input\converge_in.2026.1.hdf5")
def values_back_to_pyfiler(pyfiler1, dat):
    '''
    Sends values from the hdf back to pyfiler
    '''
    master_list=dat.keys()
    skips=[]

    for var in list(master_list):

        element=var.split("/")
        if element[0] in skips:
            continue
        if var == 'NCNTRL/cnvtst':
            continue
        if (len(element)==1):
            continue
        if len(element) == 2:
            tmp_value = np.copy(dat[var].values)
            setattr(getattr(pyfiler1, element[0].lower()), element[1], tmp_value)
        elif len(element) == 3:
            if element[2].isnumeric():
                tmp_index_region = int(element[2])
                tmp_value = np.copy(dat[var].values)

                try:
                    getattr(getattr(pyfiler1, element[0].lower()), element[1])[tmp_index_region] = tmp_value
                except AttributeError:
                    getattr(pyfiler1.utils, element[1])[tmp_index_region] = tmp_value