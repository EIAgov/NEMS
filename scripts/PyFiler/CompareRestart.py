import os
import sys
import argparse as ap
import numpy as np
import pandas as pd
sys.path.append(r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler")
os.add_dll_directory(r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler")
import pyfiler1
import time
import csv

restart_folder = r'.\restart_folder'


def PyFiler(restart_file):
    """
    This function initializes Filer (FORTRAN PROGRAM) into python for PyFiler use by pyfiler.utils.(variablename). The file for
    this code part is pyfiler.f90 which houses all of the commonblocks as well as the Filer (FORTRAN) code. This function
    initializes Filer using the command pyfiler.utils.init_filer().
    Parameters
    ----------
    restart_file- restart.unf file from NEMS

    Returns
    -------
    loads in restart file into memory
    """
    t1 = time.time()
    try:
        # initializes pyfiler using subroutine init_filer() from pyfiler.f90.
        print('restart_file in PyFiler', restart_file)
        pyfiler1.utils.read_filer(restart_file)
        print(("FILER Init:", time.time() - t1, "seconds"))
    finally:
        pass
    return pyfiler1


def Restart1(restart_file1):
    """
    Restart1 builds out the dictionary and saves a deep copy of the restart file so when a second restart file is loaded
    in there are no issues
    Parameters
    ----------
    restart_file1- path to the first restart.unf file to be loaded in

    Returns
    -------
    nemspyd1- saving the pyd for restartfile1 to be directly interacted with
    dictpyd1- saving a deep copy of data into a dictionary for restartfile1
    """
    # Read in restart file
    nemspyd1 = PyFiler(restart_file1)
    # Need to save i and a together, that way it's a one to one when comparing.
    # use dictionary, set I as key. Set that equal to np.copy(a). Cycle through keys, subtract.
    commonblocks = ['QBLK', 'MPBLK', 'AMPBLK', 'MXQBLK', 'MXPBLK', 'QSBLK', 'NCNTRL', 'NCHAR', 'CYCLEINFO', 'LFMMOUT',
                    'PMMOUT', 'PMMRPT', 'PMMFTAB', 'OGSMOUT', 'NGTDMOUT', 'ANGTDM',
                    'NGTDMREP', 'NGRPT', 'UEFPOUT', 'EFPOUT', 'UEFDOUT', 'UDATOUT', 'UECPOUT', 'DSMTFEFP', 'UETTOUT',
                    'COALOUT', 'COALREP', 'INDREP', 'INDREP2', 'RSCON', 'RESDREP',
                    'COMPARM', 'TRANREP', 'MACOUT', 'INTOUT', 'EMISSION', 'EMABLK', 'COGEN', 'WRENEW', 'CONVFACT',
                    'COALEMM', 'COALPRC', 'ACOALPRC', 'EUSPRC', 'EMEBLK', 'USO2GRP',
                    'BldgLrn', 'RSEFF', 'CONVERGE', 'BIFURC', 'EPMBANK', 'GHGREP', 'QONROAD', 'PONROAD', 'APONROAD',
                    'QMORE', 'PMORE', 'APMORE', 'HMMBLK', 'REGCO2', 'AEUSPRC',
                    'CONTINEW', 'AB32', 'RGGI', 'CSAPR', 'E111D', 'TCS45Q']
    dictpyd1 = {}
    for h in range(0, len(commonblocks)):
        try:
            nems = getattr(nemspyd1, commonblocks[h].lower())
            for i in set(dir(nems)):
                if i[1] != '_':
                    a = getattr(nems, i)
                    a = np.copy(a)
                    dictpyd1[(commonblocks[h].upper() + '.' + i)] = a
                else:
                    continue
        except:
            nems = getattr(nemspyd1, 'ghg')
            for i in set(dir(nems)):
                if i[1] != '_':
                    a = getattr(nems, i)
                    a = np.copy(a)
                    dictpyd1[('GHGREP' + '.' + i)] = a
                else:
                    continue
    return nemspyd1, dictpyd1

def CompareRestart(restart_file1, restart_file2, tol, errtype):
    """
    Compare Restart loads in two different restart files, a tolerance, and an error type for comparison of variables and
    the selected error type given a tolerance.
    Parameters
    ----------
    restart_file1-  a first restart.unf to compare
    restart_file2- a second restart.unf to compare
    tol- tolerance between one variable in restart_file1 and restart_file2
    errtype- absolute or relative error

    Returns
    -------
    dictpyd1- dictionary of pyd file from restart_file1
    dictpyd2- dictionary of pyd file from restart_file2
    variables- list of variables
    results- list of results
    variablesTypErr- list of any variable containing a type error when calling
    resultsTypErr- list of any results of type error
    variablesunknown- list of variables that could not be found/unknown problem
    resultsunknown- results of the variables with unknown problem
    """
    # loading in restart file 1 and 2
    nemspyd1, dictpyd1 = Restart1(restart_file1)
    nemspyd2, dictpyd2 = Restart1(restart_file2)
    # building containers for lists
    results = []
    resultsTypErr = []
    resultsunknown = []
    variables = []
    variablesTypErr = []
    variablesunknown = []
    # starting a count
    count = 0
    # setting dictionaries to variables
    dict1 = set(dictpyd1)
    dict2 = set(dictpyd2)
    # if, elseif for error type
    if errtype == 'relative':
        for i in dict1.intersection(dict2):
            try:
                # solving for difference
                c = np.divide(abs(np.subtract(dictpyd1[i], dictpyd2[i])), abs(dictpyd1[i])) * 100 > tol
                c = np.nan_to_num(c, copy=False, nan=0.0)
                # if there is difference, append to results
                if c.any() == True:
                    results.append(i)
                # append variable to variables list
                variables.append(i)
            except TypeError:
                # in case of TypeError, check to see if dictionary values are equal
                c = np.array_equal(dictpyd1[i], dictpyd2[i])
                # if not equal, append to type error
                if c != True:
                    resultsTypErr.append(i)
                # append this variable to ones that have type error
                variablesTypErr.append(i)
            except:
                # If some other issue occurred, append results and variables
                resultsunknown.append(i)
                variablesunknown.append(i)
            # add to count for each variable
            count = count + 1
        # print statements regarding result
        print('Process completed. There were', len(results), 'variables with discernable differences of greater than',
              tol, ' % relative')
        # save results to text file
        textfile = open('results.txt', 'w')
        print('There were', len(resultsTypErr), 'variables with relative differences unknown')
        print('There were', len(resultsunknown), 'variables with unknown data')
        textfile.write('These are the variables which failed to meet the relative tolerance of ' + str(tol) + '%:\n')
        for element in results:
            textfile.write(element + "\n")
        textfile.close()
        print(i)
    # if error is absolute
    elif errtype == 'absolute':
        for i in set(dir(nemspyd1)):
            try:
                # solving for difference
                c = abs(np.subtract(dictpyd1[i], dictpyd2[i])) > tol
                c = np.nan_to_num(c, copy=False, nan=0.0)
                # if there is difference, append to results
                if c.any() == True:
                    results.append(i)
                # append variable to variables list
                variables.append(i)
            except TypeError:
                # in case of TypeError, check to see if dictionary values are equal
                c = np.array_equal(dictpyd1[i], dictpyd2[i])
                # if not equal, append to type error
                if c != True:
                    # append this variable to ones that have type error
                    resultsTypErr.append(i)
                # append this variable to ones that have type error
                variablesTypErr.append(i)
            except:
                # If some other issue occurred, append results and variables
                resultsunknown.append(i)
                variablesunknown.append(i)
            print(i)
            # add to count for each variable
            count = count + 1
        # print statements regarding result
        print('Process completed. There were', len(results),
              'variables with discernable absolute differences of greater than',
              tol, )
        textfile = open('results.txt', 'w')
        print('There were', len(resultsTypErr), 'variables with absolute differences unknown')
        print('There were', len(resultsunknown), 'variables with unknown data')
        textfile.write('These are the variables which failed to meet the absolute tolerance of ' + str(tol) + '\n')
        for element in results:
            textfile.write(element + "\n")
        textfile.close()
    else:
        sys.exit("Fatal Error- No/Wrong Error type detected, ending script. Please check")
    print(count)
    return dictpyd1, dictpyd2, variables, results, variablesTypErr, resultsTypErr, variablesunknown, resultsunknown


def RestartFileList(path):
    """
    RestartFileList walks through a given path to return a list of restart files in that path.
    Parameters
    ----------
    path- Path for folder holding restart files with .unf

    Returns
    -------
    restlist- list of restart files
    """
    restlist = []
    files = os.listdir(path)
    for root, directories, filenames in os.walk(path, topdown=False):
        for name in files:
            print(os.path.join(root, name))
            if name.endswith(".unf"):
                restlist.append(os.path.join(root, name))
        for name in directories:
            print(os.path.join(root, name))

    return restlist


if __name__ == "__main__":
    tstart = time.time()

    # create an empty restart.unf file if it doesn't exist:
    with open('restart.unf', 'w+') as fp:
        pass

    if not os.path.exists(restart_folder):
        os.mkdir(restart_folder)
        print(f"Please copy your two restart.unf in {restart_folder} and re-run this script")
        os.sys.exit()

    # Loading in a location of restart files, can be commented out if user only wants to compare two restart files they
    # know the location of
    restlist = RestartFileList(restart_folder)
    # tolerance comparison
    tol = 0.0001
    # errtype can be relative or absoute
    errtype = 'relative'
    # creating dataframe to hold differences
    tableoDiff = pd.DataFrame()
    # holding list of differences
    listoDiff = []

    # Users can point to specific restart file as a starting point here and loop through the restart files in restlist
    # Runs a comparison for files against each other
    for i in range(1, len(restlist)):
        dictpyd1, dictpyd2, variables, results, variablesTypErr, resultsTypErr, variablesunknown, resultsunknown = CompareRestart(
            restlist[0],
            restlist[i], tol, errtype)
        print(sorted(results, key=str))
        listoDiff.append(results)
    # print statements to console regarding the differences
    print('It took', time.time() - tstart, 'seconds to test')
    print('Process completed. There were', len(results),
          'variables with discernable differences of greater than ' + str(tol) + '% in ' + errtype + ' error.')
    print('There were', len(resultsTypErr), 'variables with differences unknown')
    print('There were', len(resultsunknown), 'variables with unknown data')