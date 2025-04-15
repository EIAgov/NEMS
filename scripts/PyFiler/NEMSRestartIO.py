'''
A set of utilities to read a fortan style restart file from NEMS, convert it to an npz file, and send it back for NEMS.
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

def to_npz(nems, vartable):
    """This function reads in pyfiler and outputs the numpy arrays to a compressed npz file

    Parameters
    ----------
    nems: module
        pyfiler fortran module
        
    vartable: dataframe
        dataframe of NEMS Variables after ParseDict and RetriveVarDim from PyFilerWrapper. In NEMS
        runs, this is NEMSVardf and is saved as NEMSVardf.csv

    Returns
    -------
    npz
        NEMS Restart File in npz format
    """
    npd = {"_variablelisting": np.array(vartable)}
    errors = {}

    for c, nemsmod in enumerate([nems.utils, nems.other]):
        for var in set(dir(nemsmod)).intersection(vartable.index.str.lower()):
            # Get name, common block and data
            name = vartable.loc[var.upper()]["Fortran Variable Name"]
            try:
                comblock = vartable.loc[name.upper()]["Common Block Name"].upper()
                nemsblock = getattr(nems, comblock.lower())
                arr = getattr(nemsblock, name)
            # Find the correct variable and common block if a variable is in two common blocks
            except AttributeError:
                comblock = vartable.loc[var.upper()]["Common Block Name"]
                if isinstance(comblock, pd.Series):
                    comblock = vartable.loc[var.upper()]["Common Block Name"][c]
                try:
                    nemsblock = getattr(nems, comblock.lower())
                    if isinstance(name, pd.Series):
                        name = name[0]
                    arr = getattr(nemsblock, name)

                # If pyfiler.commonblock.variable errors, this will return the variable from nems.utils or nems.other of PyFiler
                # Please refer to Pyfiler.f90 for which include files/common blocks are in utils or other.
                except:
                    try:
                        arr = getattr(nemsmod, name)

                    # For unexpected errors, VarIssue will save off the variable into a dict with commonblock and variable name
                    # This list may hold common blocks and variables that are currently in the pyfiler.other section.
                    # TODO: Append VarIssue with variables from each section. If it ends up being placed, remove from VarIssue
                    except Exception as e:
                        print(f"{name} has a problem!")
                        errors[f"{comblock} {name}"] = traceback.format_exception_only(e)  # fmt: skip
            key = f"{comblock}/{var}"
            npd[key] = arr
    npz = os.path.join(os.getcwd(), "restart.npz")
    np.savez_compressed(npz, **npd)
    err_df = pd.DataFrame([key, val] for key, val in errors.items())
    if not err_df.empty:
        err_df.to_csv("errors_npz.csv")
    return npz


def read_npz(npz, vartable):
    """Reads in restart.npz file into memory.

    Parameters
    ----------
    npz : path or str
        the .npz file with the numpy arrays from pyfiler
    vartable : dataframe
        NEMSVardf read in from NEMSVardf.csv that holds the Fortran Variables,
        Common Block, Parameter Dimensions, and extra indeces

    Returns
    -------
    dataframe
        Dictionary of numpy arrays converted to DataFrames
    """
    # Load npz file into dictionary of numpy arrays
    npd = {}
    with np.load(npz) as f:
        for key, val in f.items():
            npd[key] = val

    errors = {}  # Holds variables with errors
    dfd = {}  # DataFrame dictionary

    for key, arr in npd.items():
        comblock = key.split("/")[0]  # Get common block
        var = key.split("/")[1]  # Get variable name
        vardims = vartable.loc[var.upper()]

        # Quick check for if vardim holds the variable twice. If so, select the correct one from the common block we want
        if isinstance(vardims, pd.DataFrame):
            vardims = vardims.loc[vardims["Common Block Name"] == comblock]
        else:
            # Making vardims a pandas dataframe for easier search and work with.
            vardims = pd.DataFrame(vardims)
            vardims = vardims.transpose(copy=True)
        try:
            dparams = vardims.loc[var.upper()]["Dimension Params"]
            indices = [[_ for _ in range(1, int(s) + 1)] for s in arr.shape]
            dims = arr.ndim
            years = "MNUMYR"

            # Process column names
            buffer = []
            for i, param in enumerate(dparams):
                if param.isdigit():  # Prefix 'M' to integer column names
                    dparams[i] = f"M{param}"

                # If duplicate columns, rename to avoid errors
                if param in buffer:
                    dparams[i] = f"{dparams[i]}_{buffer.count(param)}"
                buffer.append(param)  # List of variable names used

                # Change years to 1990=index instead of 1-indexed
                if param == years:
                    start = 1990 - 1  # 1-indexed  # TODO: Remove hardcode
                    indices[i] = [str(ind + start) for ind in indices[i]]

            # 0D and 1D arrays convert to Series directly
            if dims < 2:
                dfd[key] = pd.Series(arr, indices, dtype=arr.dtype)

            # 2D arrays convert to DataFrames directly
            elif dims == 2:
                dfd[key] = pd.DataFrame(arr, index=indices[0], columns=indices[1])

            # 3D or greater arrays need to be processed.
            else:
                # Create index using cartesian product of list of ranges
                index = pd.MultiIndex.from_product(indices, names=dparams)

                # TODO: Confirm that correct order is used; C=C-style, F=Fortran-style
                flat = np.ravel(arr, order="F").copy()

                # Create DataFrame from flattened array with MultiIndex
                df = pd.DataFrame(flat, columns=["VALUES"])
                df.index = index  # Assign index

                df = df.reset_index()  # Reset index to columns
                if years in dparams:  # Pivot if data is by year
                    dparams.remove(years)
                    df = df.pivot(columns=years, index=dparams)
                dfd[key] = df
            if key in errors:
                errors.pop(key)
        except pd.io.pytables.PerformanceWarning as w:
            pass  # For debugging
        except Exception as e:
            # VarIssue holds any variables that may have an issue
            print(f"{var} error: {e}")
            errors[key] = traceback.format_exception_only(e)

    # Output errors to a csv file
    err_df = pd.DataFrame([key, val] for key, val in errors.items())
    if not err_df.empty:
        err_df.to_csv("npz_errors.csv")

    return dfd