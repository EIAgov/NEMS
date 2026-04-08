"""PyScedesAll
Created on April 4 2023
@author: jmw

PyScedesAll is a Python function for parsing the scedes.all file into a user class that can be passed between Python
programs for use in NEMS. This code sets a dictionary of scedes keys in the user class.
"""

import os
import sys

class User:
    def __init__(self, scedes_dict):
        self.scedes = scedes_dict


def find_keys_sed():
    current_dir = os.getcwd()
    if current_dir.endswith("PyFiler"):
        keys_path = os.path.abspath(os.path.join(current_dir, os.pardir, "scedes.all"))
    elif "p1" in os.listdir(current_dir):
        keys_path = os.path.abspath(os.path.join(current_dir, "p1", "scedes.all"))
    elif os.path.basename(current_dir) in ["p1", "p2", "p3"]:
        keys_path = os.path.abspath(os.path.join(current_dir, "scedes.all"))
    else:
        keys_path = os.path.abspath("scedes.all")

    if os.path.exists(keys_path):
        return keys_path
    else:
        print("Oh no! I cannot find scedes.all!")
        sys.exit("Fatal Error- No scedes.all file found. Please check in output directory")


def parse_scedes_file(filename):
    '''

    Parameters
    ----------
    filename- currently hardcoded to the scedes.all file inside of the local folder of NEMS

    Returns
    -------
    scedes_dict- dictionary of scedes keys

    '''
    scedes_dict = {}
    with open(filename, 'r') as file:
        for line in file:
            key, value = line.strip().split("=")
            scedes_dict[key] = value
    return scedes_dict


if __name__ == '__main__':
    filename = find_keys_sed()
    scedes_dict = parse_scedes_file(filename)
    user = User(scedes_dict)
    print(user.scedes["SCEN"])