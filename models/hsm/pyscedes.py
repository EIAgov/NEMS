"""PyScedesAll
Created on April 4 2023
@author: jmw
updated on May 21 2025: ads

PyScedesAll is a Python function for parsing the scedes.all file into a user class that can be passed between Python
programs for use in NEMS. This code sets a dictionary of scedes keys in the user class.
"""
import sys
import os
sys.path.append(os.path.dirname(__file__))

import os
import sys
import csv
import numpy as np

class User:
    def __init__(self, scedes_dict):
        self.scedes = scedes_dict


def find_scedes(scedes_name):
    """
    Locates the absolute path to a SCEDES file in a predefined directory.

    Constructs a path to the SCEDES file based on `scedes_name` and a relative
    path structure (two levels above the current script, under the 'scedes' folder).

    Args:
        scedes_name (str): The name of the SCEDES file or directory.

    Returns:
        str: The absolute path to the SCEDES file if found.

    Raises:
        SystemExit: If the SCEDES file is not found, the program exits with an error message.
    """
    current_dir = os.path.dirname(os.path.abspath(__file__))

    # Construct the relative path
    relative_path = f'..\\..\\scedes\\{scedes_name}'

    # Construct the absolute path
    absolute_scedes_path = os.path.join(current_dir, relative_path)

    if os.path.exists(absolute_scedes_path):
        return absolute_scedes_path
    else:
        print(f"Oh no! I cannot find {scedes_name}!")
        sys.exit("Fatal Error- No scedes file found. Please check scedes path")

def parse_scedes(filename):
    """
    Parses a scedes file into a dictionary.

    Args:
        filename (str): The path to the scedes file as defined in module_unf.py

    Returns:
        dict: A dictionary where keys are from the first column and values are from the second column in scedes CSV.
    """
    with open(filename, 'r', newline='') as file:
        csv_reader = csv.reader(file)
        header = next(csv_reader)  # Skip the header row
        data = np.array(list(csv_reader))
        scedes_dict = dict(zip(data[:, 0], data[:, 1]))
    return scedes_dict


if __name__ == '__main__':
    filename = find_scedes(scedes_name = 'scedes.ref2026.csv')
    scedes_dict = parse_scedes(filename)
    user = User(scedes_dict)