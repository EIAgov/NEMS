import os
import numpy as np
import pandas as pd


def filter_npz(varlist_file, npd):
    """filters out select variables from dictionary based on list of variables

    Parameters
    ----------
    varlist_file : str
        path to csv file that lists variables from P2 to carry over
    
    npd : dict
        dict of variables in numpy arrays
    
    Returns
    -------
    dict
        dict of numpy arrays filtered by list
    """
    
    # read in csv file and setup as pandas.DataFrame
    varlist =  pd.read_csv(varlist_file, header = None)
    
    # set header
    varlist = varlist.set_axis(['Block', 'Var', 'Format'], axis='columns')
    
    # drop "format" columns
    varlist.drop('Format', axis=1, inplace=True)
    
    # remove any comment rows (keys that start with*)
    varlist = varlist[~varlist['Block'].str.contains(r"\*")]
    
    #Get the Block and Var value only, drop 'LABEL=' and 'VAR='
    varlist['Block'] = varlist['Block'].str.split('=').str[1]
    varlist['Var'] = varlist['Var'].str.split('=').str[1]
    
    #combine block and var to make keys
    combined = varlist['Block'] + '/' + varlist['Var'].str.lower()
    allowed_keys = list(combined)
    
    # drop all keys that ends with "/all"
    allowed_keys = [s for s in allowed_keys if not s.endswith("/all")]
    
    # get all the blocks where var=ALL to and add all block/keys to allowed keys
    all_variables = varlist[varlist['Var'] =='ALL']
    all_variables_keys = [key for key in npd.keys() if key.split('/')[0] in list(all_variables['Block']) ]
    allowed_keys.extend(all_variables_keys)
    
    # filter and select variables from dictionary of arrays using keys
    new_npd = {key: value for key, value in npd.items() if key in allowed_keys}
    
    return new_npd

def main(npz_p1,npz_p2, varlist_file, nruns):
    """Main entry point into partition_integrator to combine p1 and p2 restart files

    Parameters
    ----------
    npz_p1 : str
        path to p1 npz file
    
    npz_p2 : str
        path to p2 npz file
    
    varlist_file : str
        path to csv file that lists variables from P2 to carry over
    
    nruns : int
        message to write in logfile
    
    Returns
    -------
    none
    
    """
    npd1 = dict(np.load(npz_p1,allow_pickle=True))  #load p1 .npz restart file
    npd2 = dict(np.load(npz_p2,allow_pickle=True))  #load p2 .npz restart file
    filtered_npd2 = filter_npz(varlist_file, npd2)
    
    # Dictionary Union Operator to combine 2 dictionaries
    new_npz = npd1 | filtered_npd2

    # if NRUN in scedes is 1 (1 cycle), then write out file into p3/input
    if nruns == '1':
       outfile = "p3/input/restarti"
       np.savez_compressed(outfile, **new_npz)
    
    # else NRUN is larger than 1, write out to p3 level
    else:
        outfile = 'p3/restart.in'
        np.savez_compressed(outfile, **new_npz)
        if os.path.exists(outfile):
            os.replace('p3/restart.in.npz', outfile)
        else:
            os.rename('p3/restart.in.npz', outfile)

    
if __name__=="__main__":
    
    print("Running the partition integrator independently")
    varlist = 'p2//input//varlistrec.txt'
    my_file = "p1//restart.npz"
    my_file2 = "p2//restart.npz"
    main(my_file, my_file2, varlist, 2)
    print("Partition integrator Test complete")


