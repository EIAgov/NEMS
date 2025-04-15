"""
Created on Nov 16 2023

@author: ARK

Function to read in the scedes file into a dictionary
Function to create the initial user object passed in NEMS

"""


from types import SimpleNamespace
import os


def parse_scedes_file(f):
    """Reads in scedes.all into memory as dict
    
    Parse a NEMS scedes file into a dict. Keep only the first
    instance of a key, if there are duplicates.
    
    Parameters
    ----------
    f : str
        name of file to read in (scedes.all)
    
    Returns
    -------
    dict
        dictionary of scedes keys and value pair
    
    """
    with open(f) as scedesall:
        scedesall_lines = scedesall.readlines()
    scedes_dict = {}
    for line in scedesall_lines:
        a = line.split("=")
        k = a[0].strip()
        v = a[-1].strip("\n").strip()
        if k in scedes_dict.keys():
            print(f"Ignoring duplicate key: {k}")
        else:
            scedes_dict[k]= v

    return scedes_dict


def create_user_class(scedes):

    """Combines read in dictionaries into a user class

    Parameters
    ----------
    scedes : dict
        dictionary of scedes keys and value pairs parsed from SCEDES.ALL file

    Returns
    -------
    SimpleNamespace
        Contains dictionaries with NEMS runtime data
    
    """


    d = {"SCEDES" : scedes}
    user = SimpleNamespace(**d)

    return user


if __name__ == "__main__":
    print("Please load from the main program script. Exit now...")
    os.sys.exit()