# RestartResizeNPZ.py
"""
The purpose of this script is to adapt NEMS restart files (.npz) to changes in the model's input dictionary (dict.txt). 
It reads an existing restart file, updates variable sizes based on a new dictionary, 
transfers data from the old restart file to the new one where possible, and saves the result as a new restart file.
"""
import numpy as np
import pandas as pd
import os
from ast import literal_eval
import sys
import time
# Get the absolute path to the directory containing script.py
try:
    current_dir = os.path.dirname(os.path.abspath(__file__))
except NameError:
    current_dir = os.path.dirname(os.getcwd())
# Calculate the path to the directory one level up and one level over
module_dir = os.path.join(os.path.dirname(current_dir), 'PyFiler')
# Add the module directory to sys.path
sys.path.append(module_dir)
#sys.path.append(r'L:\main\ark\NEMS_base\scripts\PyFiler')
import PyFilerWrapper as pfw

def filter_dict_by_slash(input_dict):
  """Removes keys from a dictionary that do not contain a forward slash ('/').

  Parameters
  ----------
  input_dict : dict
    The dictionary to filter.

  Returns
  -------
  dict
    A new dictionary with only the keys containing a forward slash.
  """
  filtered_dict = {key: value for key, value in input_dict.items() if '/' in key}
  return filtered_dict

def replace_empty_with_nearest(arr):
    """Replaces empty strings in a NumPy array with the nearest non-empty value.

    Parameters
    ----------
    arr : numpy.ndarray
        The input NumPy array.

    Returns
    -------
    numpy.ndarray
        A new NumPy array with empty strings replaced.
        Returns the original array if no empty strings are found.
    """
    arr = np.copy(arr)  # Avoid modifying the original array
    empty_indices = np.where(arr == '')[0]

    if empty_indices.size == 0:
        return arr  # Return the original array if no empty strings are found

    for idx in empty_indices:
        # Find the nearest non-empty indices to the left and right
        left_idx = (np.where(arr[:idx] != '')[0])
        right_idx = (np.where(arr[idx+1:] != '')[0] + idx + 1) # Shift indices to match original array


        if left_idx.size > 0: #Check if there are any non-empty indices to the left
             left_idx = left_idx[-1] # Index of the rightmost non-empty element to the left of idx
        else:
             left_idx = None  #No non-empty elements to the left
        if right_idx.size > 0:
             right_idx = right_idx[0] # Index of the leftmost non-empty element to the right of idx
        else:
             right_idx = None  #No non-empty elements to the right

        #Determine which is closer (or if only one exists)
        if left_idx is not None and right_idx is not None:
            if (idx - left_idx) <= (right_idx - idx):
                nearest_value = arr[left_idx]
            else:
                nearest_value = arr[right_idx]
        elif left_idx is not None:
            nearest_value = arr[left_idx]
        elif right_idx is not None:
            nearest_value = arr[right_idx]
        else:
            nearest_value = 0.0  # or some other default if the entire array is empty

        arr[idx] = nearest_value
    return arr

def load_npz_data(npz_file):
    """Reads in the specified .npz file and return as an N-dimension array.

    Parameters
    ----------
    npz_file : string
        The .npz file path.

    Returns
    -------
    array
        A n-dimension Numpy array with npz data.
    """
    npz_data=dict(np.load(npz_file,allow_pickle=True))
    return npz_data

def capitalize_after_slash(input_dict):
    """Capitalizes the part of the string after the '/' in keys of a dictionary.

    Parameters
    ----------
    input_dict : dict
        The input dictionary with string keys potentially containing '/'.

    Returns
    -------
    dict
        A new dictionary with modified keys.
    """
    new_dict = {}
    for key, value in input_dict.items():
        if isinstance(key, str) and "/" in key:
            parts = key.split("/")
            if len(parts) > 1:
                new_key = parts[0] + "/" + parts[1].upper()
                new_dict[new_key] = value
            else:
                new_dict[key] = value  # If no part after '/', keep original key
        else:
            new_dict[key] = value  # If no '/', keep original key
    return new_dict

def find_different_keys(dict1, dict2):
    """Finds keys that are present in one dictionary but not in the other.

    Parameters
    ----------
    dict1 : dict
        The first dictionary.
    dict2 : dict
        The second dictionary.

    Returns
    -------
    set
        A set containing keys that are different between the two dictionaries.
    """

    keys1 = set(dict1.keys())
    keys2 = set(dict2.keys())

    different_keys = (keys1 - keys2) | (keys2 - keys1)  #symmetric difference

    return different_keys

import numpy as np

def transfer_data(dict1, dict2, key):
    """Transfers data from dict1 to dict2 for a given key, filling missing values with zeros.

    Parameters
    ----------
    dict1 : dict
        The source dictionary.
    dict2 : dict
        The destination dictionary.
    key : str
        The key to transfer data for.
    """

    # Get the shapes of the arrays
    shape1 = dict1[key].shape
    shape2 = dict2[key].shape

    # Determine the minimum dimensions to copy
    min_z = min(shape1[2], shape2[2])

    # Copy the data
    dict2[key][:, :, :min_z] = dict1[key][:, :, :min_z]

    # For the remaining slices in dict2, fill with zeros
    if shape2[2] > shape1[2]:
        dict2[key][:, :, shape1[2]:] = 0  # Fill remaining slices with zeros


def main(user):
    vartable = pd.read_csv(user.file_map["NEMSVardf"])
    vartable.set_index(vartable["Fortran Variable Name"], inplace=True)
    user.file_map["vartable"] = vartable
    user.newnpz = 0
    restartnpz = load_npz_data(user.file_map["RestartNPZ"])

    #Second, create new NEMSVardf from new dict.txt
    newNEMSFortTable, newNEMSAttributesTable = pfw.ParseDict(user.file_map["dictnew"])
    newNEMSVardf = pfw.RetrieveVarDim(newNEMSFortTable, newNEMSAttributesTable)
    user.newNEMSVardf = newNEMSVardf
    #Third, generate empty npz of zeros sized to NEMSVardf.csv
    #Need to be careful here. Need to make sure npz is same as NEMS one, just resized. Look at Create_new_restart.py in WEPS
    #Need to make sure we don't overwrite the data in memory either, as they'll be same names. Could save as dat and datnew
    #Get max of list in list of lists
    #Loop through each key in newNEMSVardf and generate np.zeros?
    
    npd = {}
    for i in range(0,len(newNEMSVardf)):
        comblock = newNEMSVardf.iloc[i]["Common Block Name"]
        var = newNEMSVardf.iloc[i]["Fortran Variable Name"][0]
        dimvar = newNEMSVardf.iloc[i]["Extra Indeces"]
        max_values = [max(sublist) for sublist in dimvar]
        dims4npz = tuple(max_values)
        hold = np.zeros(dims4npz)
        comblock = comblock.upper()
        var = var.lower()
        key = f"{comblock}/{var}"
        npd[key] = hold

    npz = os.path.join(os.getcwd(), "tempnpz.npz")
    np.savez_compressed(npz, **npd)
    user.file_map["RestartNPZnew"] = npz

    #Fourth, fill values of NewNPZ where oldNPZ is same size
    #If same size, copy straight over
    #Making Variable Larger

    user.newnpz = 1

    # Example usage:
    #restartnpz = capitalize_after_slash(restartnpz)

    newrestartnpz = load_npz_data(user.file_map["RestartNPZnew"])
    test = len(newrestartnpz)
    print(str(test) + " is the size of newrestartnpz")
    newrestartnpz = filter_dict_by_slash(newrestartnpz)
    test = len(newrestartnpz)
    print(str(test) + " is the size of filtered newrestartnpz")
    test = len(restartnpz)
    print(str(test) + " is the size of restartnpz")
    restartnpz = filter_dict_by_slash(restartnpz)
    test = len(restartnpz)
    print(str(test) + " is the size of filtered restartnpz")

    intersectnpz =set(newrestartnpz.keys()) & set(restartnpz.keys())
    different_keys_set = find_different_keys(newrestartnpz, restartnpz)
    test = len(intersectnpz)
    print(str(test) + " is the size of intersectnpz")
    
    for key in intersectnpz:
        try:
            if newrestartnpz[key].shape == restartnpz[key].shape:
            #if npd[key].shape == restartnpz[key].shape:
                #print(key + " same shape, use values")
                newrestartnpz[key] = restartnpz[key]
                #npd[key] = restartnpz[key]
            else:
                if newrestartnpz[key].shape > restartnpz[key].shape:
                #elif npd[key].shape > restartnpz[key].shape:
                    #print(key + " size has changed larger, making zeros")
                    #res = tuple(map(lambda j, k: j - k, newrestartnpz[key].shape, restartnpz[key].shape))
                    res = tuple(map(lambda j, k: j - k, newrestartnpz[key].shape, restartnpz[key].shape))
                    if len(res) > 0:
                        tup1 = restartnpz[key].shape
                        #tup1 = npd[key].shape
                        tup2 = newrestartnpz[key].shape
                        resulttup = tuple(t1 if t1==t2 else (t2 - t1) for t1, t2 in zip(tup1, tup2))
                        padding_array = np.zeros(resulttup,)
                        #print("Shape of newrestartnpz[key] prior to concatenating:", newrestartnpz[key].shape)
                        axchg = 0
                        for i, value in enumerate(res):
                            if value > 0:
                                axchg = i
                        hold = np.concatenate((restartnpz[key], padding_array), axis=axchg)
                        hold = replace_empty_with_nearest(hold)
                        newrestartnpz[key] = hold
                    else:
                        newrestartnpz[key] = restartnpz[key]
                else:
                    res = tuple(map(lambda j, k: j - k, newrestartnpz[key].shape, restartnpz[key].shape))
# newrestartnpz is smaller: Truncate restartnpz[key] by copying values
                    print(f"Key `{key}` has shape {restartnpz[key].shape} in restartnpz and shape {newrestartnpz[key].shape} in newrestartnpz.")
                    
                    changing_vars = ["RESDREP/rshtrs","RSCON/htrcon","RSEFF/shtconwt","RESDREP/rscoolcn","RSEFF/shtconin","RSCON/shtcon","RESDREP/rshtrcon"]
                    # Make sure the first dimension is compatible (both 1800)
                    if key in changing_vars:
                        slices = []
                        newrestart_shape = newrestartnpz[key].shape
                        for i in range(len(newrestart_shape)):
                            # For each dimension, take elements up to the size of the corresponding dimension in newrestart_shape
                            slices.append(slice(0, newrestart_shape[i]))

                        # Convert the list of slices to a tuple for advanced indexing
                        target_slices = tuple(slices)

                        # Copy the data from restartnpz[key] to newrestartnpz[key] using the generated slices
                        newrestartnpz[key] = restartnpz[key][target_slices]
                        print("")

                    else:
                        if restartnpz[key].shape[0] == newrestartnpz[key].shape[0]:
                            # Extract the first column from restartnpz[key]
                            try:
                                first_column = restartnpz[key][:, 0]
                            except:
                                print(f"Error, attempting conversion to numpy ndarray")
                                restartnpz[key] = np.array(restartnpz[key])
                                first_column = restartnpz[key][:, 0]

                            # Assign the first column to newrestartnpz[key]
                            print(f"Confirming after resize key `{key}` has shape {newrestartnpz[key].shape} in newrestartnpz.")

                        else:
                            print(f"{key} is a shrinking variable, truncating restartnpz[key]")
                            smaller_shape = newrestartnpz[key].shape

                            # Create an array with the same shape and type as newrestartnpz[key] to store the values
                            truncated_array = np.empty(smaller_shape)

                            # Iterate through the indices of newrestartnpz[key] and copy values from restartnpz[key]
                            it = np.nditer(newrestartnpz[key], flags=['multi_index'])
                            while not it.finished:
                                try:
                                    # Attempt to access using the multi_index as a tuple
                                    truncated_array[it.multi_index] = restartnpz[key][it.multi_index]
                                except KeyError as e:
                                # Handles the case where something is wrong with the logic
                                    print(f"KeyError: {e} not found in restartnpz[key]")
                                    truncated_array[it.multi_index] = 0  # Example: set to 0
                                except ValueError as e:
                                    # Handles the case where something is wrong with the logic
                                    print(f"Value Error: {e} not found in restartnpz[key]")
                                    truncated_array[it.multi_index] = 0

                                it.iternext()
                            newrestartnpz[key] = truncated_array
                            print(f"Shape of newrestartnpz[key] after truncation: {newrestartnpz[key].shape}")
        except KeyError:
            print("there is an error with: " + key)
            time.sleep(5)
            newrestartnpz[key] = np.zeros(newrestartnpz[key].shape)

    newrestartnpz['RSCON/shtcon'] = newrestartnpz['RSCON/shtcon'] * 0
    npz = os.path.join(os.getcwd(), "newrestartnpz.npz")
    np.savez_compressed(npz, **newrestartnpz)
    print("done")

if __name__ == "__main__":
    class user:
        pass

    user.file_map = {}
    user.file_map["RestartNPZ"] = r"T:\output\aeo2026\ref2026\d090925x\restart.npz"
    user.file_map["dictnew"] = r"L:\main\jmw\git\NEMSRDM\input\dict.txt"
    user.file_map["NEMSVardf"]=r"T:\output\aeo2026\ref2026\d090925x\NEMSvardf.csv"
    main(user)