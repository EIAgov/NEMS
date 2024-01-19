'''
Created in Apr, 2022

@author: ARK
@author2: JMW

A set of utilities to read a fortan style restart file, send it to hdf, and send it back for NEMS.
'''
import warnings
#import tables
import sys
import os
import numpy as np
import math
import pandas as pd
import datetime
import itertools

warnings.filterwarnings('ignore', category=pd.io.pytables.PerformanceWarning)
pd.options.mode.chained_assignment = None
import string
import time
import shutil
import subprocess
from shutil import move, copy, rmtree
import os

def get_data_forhdf (nems, store, c):
    """Collects the data from PyFiler restart file to store into a newly written HDF file. Once the data is collected,
    function then rearranges the data into commonblock, variable, indices for HDF writing. Once the data is rearranged
    for the variable, it is saved into the HDF (store).

    Parameters
    ----------
    nems - the .pyd file which holds the shape and data from restart.unf
    vartabledf - variable table saved in HDF that holds the Fortran Variables, Common Block, Parameter Dimensions, and extra indeces
    store - HDF being saved
    c - option of whether we are using pyfiler.utils (0) or pyfiler.other (1)

    Returns
    -------
    VarIndex - Returns any variables which may have issues if debugging commented out in line 206 of NEMSHDFRestartIO.py
    in the function wrt_hdf
    """
    vartabledf = store['_variablelisting']
    if c == 0:
        nemsmod = nems.utils
    else:
        nemsmod = nems.other
    VarIssue=[]
    for i in set(dir(nemsmod)).intersection(vartabledf.index.str.lower()):
        t2 = time.time()
        #getting name, common block and data
        try:
            nam = vartabledf.loc[i.upper()]['Fortran Variable Name']
            comblock = vartabledf.loc[nam.upper()]['Common Block Name'].upper()
            comblocklow = comblock.lower()
            #("Length for "+ str(i) +" is " + str(len(vartabledf.loc[nam.upper()]['Dimension Params'])))
            nemsblock = getattr(nems, comblocklow)
            dat = getattr(nemsblock, nam)
        except AttributeError:
            #occasionally, there is an issue of a variable existing in two common blocks. This except AttributeError
            #works on finding the correct variable and common block to be placed into the HDF
            nam = vartabledf.loc[i.upper()]['Fortran Variable Name']
            comblock = vartabledf.loc[i.upper()]['Common Block Name']
            if isinstance(comblock, pd.Series):
                comblock = vartabledf.loc[i.upper()]['Common Block Name'][c]
            try:
                comblocklow = comblock.lower()
                nemsblock = getattr(nems, comblocklow)
                if isinstance(nam, pd.Series):
                    nam = nam[0]
                dat = getattr(nemsblock, nam)
                #print("Length for " + str(i) + " is " + str(len(vartabledf.loc[i.upper()]['Dimension Params'])))
            except:
                #on the rare occasion that pyfiler.commonblock.variable errors, this will return the variable from the
                #nems.utils or nems otherpiece of PyFiler, which has separated out the variables that repeat. Please refer to
                #Pyfiler.f90 for which include files/common blocks are in utils or other.
                try:
                    dat = getattr(nemsmod, nam)
                    #("Length for " + str(i) + " is " + str(len(vartabledf.loc[i.upper()]['Dimension Params'])))
                except:
                    #If for some reason there was a completely different issue- VarIssue will save off the variable into
                    #a list with commonblock and variable name for assistance in checking. This list may hold common blocks
                    #and variables that are currently in the pyfiler.other section.
                    #TODO: append VarIssue with variables from each section. If it ends up being placed, remove from VarIssue
                    print(nam + " has a problem!")
                    VarIssue.append(comblock + ' ' + nam)
                    pass

        #breakdown through loop of depending on dimensional parameters, how the variable will be loaded into the HDF. For
        #each additional layer of extra indeces, the loop adds a layer with / for the HDF.
        vardims = vartabledf.loc[i.upper()]
        #Quick check for if vardim holds the variable twice. If so, select the correct one from the common block we want
        if isinstance(vardims, pd.DataFrame):
            vardims = vardims.loc[vardims['Common Block Name'] == comblock]
        else:
            #making vardims a pandas dataframe for easier search and work with.
            vardims = pd.DataFrame(vardims)
            vardims = vardims.transpose(copy=True)
        if len(vardims.loc[i.upper()]['Dimension Params']) == 1:
            try:
                store[comblock + '/' + i] = pd.Series(dat)
            except:
                # VarIssue holds any variables that may have an issue
                print(nam + " has an issue!")
                VarIssue.append(comblock + ' ' + nam)
        elif len(vardims.loc[i.upper()]['Dimension Params']) == 2:
            try:
                store[comblock+'/'+i] = pd.DataFrame(dat)
            except:
                # VarIssue holds any variables that may have an issue
                print(nam + " has an issue!")
                VarIssue.append(comblock + ' ' + nam)
        elif len(vardims.loc[i.upper()]['Dimension Params']) == 3:
            row = vardims.loc[i.upper()]
            for k in range(0, len(dat)):
                try:
                    store[comblock+'/'+ i + "/" + str(k)] = pd.DataFrame(dat[k])
                except:
                    # VarIssue holds any variables that may have an issue
                    print(nam + " has an issue!")
                    VarIssue.append(comblock + ' ' + nam)
                    pass
        elif len(vardims.loc[i.upper()]['Dimension Params']) == 4:
            row = vardims.loc[i.upper()]
            for j in range(0, len(dat)):
                varname=comblock + '/' + i + "/" + str(j) + '/'
                shortdat=dat[j]
                for k in range(0, len(dat[1])):
                    try:
                        store[varname+ str(k)] = pd.DataFrame(shortdat[k])
                    except:
                        #VarIssue holds any variables that may have an issue
                        print(nam + " has an issue!")
                        VarIssue.append(comblock + ' ' + nam)
        elif len(vardims.loc[nam.upper()]['Dimension Params']) == 5:
            row = vardims.loc[i.upper()]
            for j in range(0, len(dat)):
                varname=comblock+'/'+ i + "/" + str(j)
                shortdat=dat[j]
                for k in range(0, dat.shape[1]):
                    store_var_name=varname+ '/'+ str(k)+'/'
                    shorterdat=shortdat[k]
                    for l in range(0, dat.shape[2]):
                        try:
                            store[store_var_name + str(l)] = pd.DataFrame(shorterdat[l])
                        except:
                            # VarIssue holds any variables that may have an issue
                            print(nam + " has an issue!")
                            VarIssue.append(comblock + ' ' + nam)
        #TODO: work on a generalized approach
        elif len(vardims.loc[i.upper()]['Dimension Params']) > 5: # Generalized 4D+ variables
            sys.exit('Variable Greater than 5D. Ending as NEMSHDFRestartIO.py does not currently handle this case')

        print('Takes '+ str(time.time() - t2) + " seconds to write variable " + str(i) + ' to HDF.')
    return VarIssue, store

def wrt_hdf (nems, user):
    """This section of code creates the pandas dataframes for hdfstore to output an hdf5 file which holds the NEMS restart
    variables. The code iterates through each variable from nems, shapes the corresponding data to match the HDF format
    using slashes to denote various levels, and then writes that dataframe to an hdf, creating a full hdf of the variables.

    Parameters
    ----------
    nems: the .pyd file which holds the shape and data from restart.unf
    user: class that holds file locations.

    Returns
    -------
    saves the hdf5 file for use by module.
    """
    import os
    #load in .hdf5 file to be written to and the dataframe containing fortran variable name, commonblock, dimensional parameters
    dbloc = user.file_map['RestartHDF']
    t1 = time.time()
    vartabledf = user.NEMSVardf

    #store the HDF in local memory
    store = pd.HDFStore(dbloc)
    #writes dataframe containing fortran variable name, common block, dimenisonal parameters to hdf for storage in case
    #the data is needed later or for going from HDF to .unf format.
    #vartabledf.to_hdf(store, '_variabletable')
    store['_variablelisting'] = vartabledf
    store['_varrange'] = user.NEMSVardf['Extra Indeces']
    #iterating through the intersection of variable names with the names stored in the .pyd file

    #sending to function get_data_hdf
    VarIssue1, store = get_data_forhdf(nems, store, 0)
    VarIssue2, store = get_data_forhdf(nems, store, 1)
    print("HDF restart updated:", time.time() - t1, "seconds")

    #For debugging of any missing variables noted in VarIssue. Generally commented out unless debugging
    #VarIssue = []
    #VarIssue = VarIssue1 + VarIssue2
    #unique_list_index = []
    # for x in (VarIssue):
    #     if x not in unique_list_index:
    #         unique_list_index.append(x)
    # unique_list_index.sort()
    store.close()

def strings_to_fortran(name, a, nems):
    '''
    in the weps schema, fortran strings are stored weirdly.
    This function does the column row switches that are necessary.
    '''
    import re
    var = getattr(nems, name)

    slist = []
    l2 = int(re.findall('\d+', var.dtype.str)[0])
    l1 = len(a)
    for i in a:
        b = '{0:<{width}}'.format(i, width=l2)
        slist.append(b)
    columns = l2
    rows = l1
    filler = []
    for i in range(rows):
        filler.append(list(np.zeros(columns)))
    letters = "".join(slist)
    for i in range(len(letters)):
        filler[i % rows][int(math.floor(i / rows))] = letters[i]
    return (np.array(filler))

def get_data_hdf2restart(nems, vartabledf, store, c):
    """"Collects the data from HDF restart file to store into a newly written restart file. Once the data is collected,
    function then rearranges the data into pyfiler's necessary style of data writing. Once the data is rearranged
    for the variable, it is passed for the unf to be written.

    Parameters
    ----------
    nems - the .pyd file which holds the shape and data from restart.unf
    vartabledf - variable table saved in HDF that holds the Fortran Variables, Common Block, Parameter Dimensions, and extra indeces
    store - HDF being parsed to be written to unf
    c - option of whether we are using pyfiler.utils (0) or pyfiler.other (1)

    Returns
    -------
    NONE. Saves data into
    """
    if c == 0:
        nemsmod = nems.utils
    else:
        nemsmod = nems.other
    for i in set(dir(nemsmod)).intersection(vartabledf.index.str.lower()):
        trow = vartabledf.loc[i.upper()]
        #if trow['Dimension Params'] == 'parameter':
        #    continue
        if isinstance(trow, pd.DataFrame):
            trow = trow.iloc[c]
            comblock = trow['Common Block Name'].upper()
            comblocklow = comblock.lower()
        else:
            comblock = trow['Common Block Name'].upper()
            comblocklow = comblock.lower()
        #Checking of common block and nems.utils. If in nems.utils, kick out.
        if (comblock == "AMPBLK" or comblock == "ANGTDM" or comblock == "RSCON" or comblock == "ACOALPRC" or comblock == "APONROAD" or comblock == "AEUSPRC") and (c == 0):
            continue
        #Equation that counts slashes.
        tableloc = len(trow['Dimension Params'])

        #Section of code loops through searching for what tableloc is equal to and then reshapes the HDF styled code back
        #to a dataframe of the same shape.
        if tableloc == 'singval':
            setattr(nems,i,np.array(store['_singlevalue']).loc[i][0])
        if tableloc == 1:
            try:
                setattr(nems, i, store[comblock+'/'+i].values.flatten())
            except AttributeError:
                setattr(nemsmod, i, store[comblock+'/'+i].values.flatten())
            except SystemError:
                print(("Can't update: ", i))
            except ValueError:
                print(("Can't update: ", i))
        elif tableloc == 2:
            #reading hdf for variable of interest using HDF style of Common Block name then Variable name
            temp = store[comblock+'/'+i]
            if temp.shape[0] > 16:
                #if the shape of this variable is greater than 16 in terms of the length of the extra indecess, this goes
                #through and works on reshaping the index.
                myindex = store[comblock+'/'+i].index.tolist()
                #print('WARNING: Table {} has strange index: {}'.format(i,myindex))
                q2 = [q for q in myindex if not str(q).isdigit()]
                temp = temp.loc[q2,:]
            v = temp.values
            nems.i = v
            try:
                #setting attribute to values taken from the HDF.
                setattr(nems, i, v)
            except SystemError:
                print(("Can't update: ", i))
        elif tableloc == 3:
            #This detremines the length of the leftmost demension
            try:
                nemsset = getattr(nems, comblocklow)
                nfram = len(getattr(nemsset, i))
            except:
                nfram = len(getattr(nemsmod,i))
            #This gets the information for the NEMSFortVarTable with Variable Name, Common Block, Dimension Params,
            # and Extra Indeces
            #creation of empy list to hold and loop through
            l = []
            for k in range(0, nfram):
                #first tries the index assuming a string
                s = trow['Extra Indeces'][0][k]
                #appending the list l with the data from reading the hdf with path commonblock name, variable name, s extra
                #index name.
                l.append(store[comblock+'/'+i + "/" + str(s)].values)
            try:
                #setting attribute to values taken from HDF
                setattr(nemsmod, i, np.array(l))
            except SystemError:
                print(("Can't update: ", i))
            except ValueError:
                print(("Broadcast error: ", i))
                # mc6: temp code
                print('Broadcast error in HDFRestartIO.py.')
                print('but keep going...')
                print(trow)
                print(i)
                print(np.array(l))
                #setattr(nems, i, np.array(l))  # mc6: added line to help debug
        elif tableloc == 4:
            #Gets length of leftmost dimension
            try:
                nemsset = getattr(nems, comblocklow)
                nfram = len(getattr(nemsset, i))
            except:
                nfram = len(getattr(nemsmod,i))
            # This gets the information for the NEMSFortVarTable with Variable Name, Common Block, Dimension Params,
            # and Extra Indeces
            #creation of empty array to hold and loop through
            holding = np.empty((len(trow['Extra Indeces'][0]),len(trow['Extra Indeces'][1]),
                                len(trow['Extra Indeces'][2]),len(trow['Extra Indeces'][3])),
                               dtype=float)
            for k in range(0, nfram):
                for l in range(0, len(trow['Extra Indeces'][1])):
                    s = str(trow['Extra Indeces'][0][k])
                    t = str(trow['Extra Indeces'][1][l])
                    # filling in entry with the data from reading the hdf with path commonblock name, variable name, s extra
                    # index name.
                    holding[k,l,:,:] = store[comblock + '/' + i + "/" + s + '/' + t].values
            try:
                nemsset = getattr(nems,comblocklow)
                setattr(nemsset, i, holding)
            except SystemError:
                print(("Can't update: ", i))
            except ValueError:
                try:
                    setattr(nemsmod, i, holding)
                except:
                    print(("Broadcast error: ", i))

                    # mc6: temp code
                    print('Broadcast error in HDFRestartIO.py.')
                    print('but keep going...')
                    print(trow)
                    print(i)
                    print(np.array(holding))
                    # setattr(nems, i, np.array(l))  # mc6: added line to help debug
        elif tableloc == 5:
            try:
            #Gets length of leftmost dimension
                nemsset = getattr(nems, comblocklow)
                nfram = len(getattr(nemsset, i))
                # This gets the information for the NEMSFortVarTable with Variable Name, Common Block, Dimension Params,
                # and Extra Indeces
                #creation of empty array to hold and loop through
                holding = np.empty((len(trow['Extra Indeces'][0]),len(trow['Extra Indeces'][1]),
                                    len(trow['Extra Indeces'][2]),len(trow['Extra Indeces'][3]),
                                    len(trow['Extra Indeces'][4])),dtype=float)
                for k in range(0, nfram):
                    for l in range(0, len(trow['Extra Indeces'][1])):
                        for jj in range(0, len(trow['Extra Indeces'][2])):
                            s = str(trow['Extra Indeces'][0][k])
                            t = str(trow['Extra Indeces'][1][l])
                            u = str(trow['Extra Indeces'][2][jj])
                            # filling in entry with the data from reading the hdf with path commonblock name, variable name, s extra
                            # index name.
                            holding[k,l,jj,:,:] = store[comblock + '/' + i + "/" + s + '/' + t + '/' + u].values
                try:
                    setattr(nems, i, holding)
                except SystemError:
                    print(("Can't update: ", i))
                except ValueError:
                    try:
                        nemsset = getattr(nems, comblocklow)
                        setattr(nemsset, i, holding)
                    except:
                        try:
                            setattr(nemsmod, i, holding)
                        except:
                            print(("Broadcast error: ", i))

                            # mc6: temp code
                            print('Broadcast error in HDFRestartIO.py.')
                            print('but keep going...')
                            print(trow)
                            print(i)
                            #print(np.array(holding))
                            # setattr(nems, i, np.array(l))  # mc6: added line to help debug
            except:
                print('Problem with variable ' + i + ". Need to investigate why")
        #TODO: Generalization of anything greater than 5D variables will need to be taken into consideration. The code below
        #TODO: exists for further investigation into the topic.
        elif tableloc > 5: # Generalized 4D+ variables
            sys.exit('Variable Greater than 5D. Ending as NEMSHDFRestartIO.py does not currently handle this case')

def update_restart_matrix(dbloc, nems, newloc):
    """This section of code creates a pandas dataframe of the fortran variable values from the HDF to store back to the
    restart.unf and write the restart.unf using PyFiler. Code counts the slashes to find out the dimension of the
    variable and then writes to dataframe of the correct size.

    Parameters
    ----------
    dbloc: HDF5 File to be converted to restart.unf
    nems: .pyd file
    newloc: new location for restart file

    Returns
    -------
    saves restart.unf file.
    """
    t1 = time.time()
    #read in HDF passed in dbloc
    store = pd.HDFStore(dbloc)
    vartabledf = store['_variablelisting']
    get_data_hdf2restart(nems, vartabledf, store, 0)
    get_data_hdf2restart(nems, vartabledf, store, 1)
    #now that the data is in the correct format, nems.write_filer() writes out the new restart file given in newloc
    nemswrite = nems.utils
    nemswrite.write_filer(newloc)
    #prints that the HDF is finished updating, the time it took to update, and then closes the HDF.
    print(("UNF restart updated:", time.time() - t1, "seconds"))
    #close the HDF
    store.close()