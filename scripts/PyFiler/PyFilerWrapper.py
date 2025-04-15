"""
A collection of utility code for PyFiler to parse the dictionary of NEMS (dict.txt) and retrieve NEMS variable dimensions.
"""

# Importing neccessary components
import os
import sys
import numpy as np
import pandas as pd


def ParseDict(file_dict):
    """parses dict.txt (given as an input) and outputs the Fortran Variable Table and Attribute Table.

    Parameters
    ----------
    file_dict: dict
        dictionary file path from NEMS (dict.txt).

    Returns
    -------
    dataframe
        dataframe consisting of the first half of dict.txt comprised of attributes of dimensions of common block variables
        
    dataframe
        dataframe consisting of the second half of dict.txt comprised of commonblock, variable names, dimensions, dimension parameters
    """
    try:
        with open(file_dict) as resf:
            dictlines = resf.readlines()
    except:
        with open('input/dict.txt') as resf:
            dictlines = resf.readlines()
    dictlines = [xx for xx in dictlines if not xx.startswith('*')]
    dictlines = [yy for yy in dictlines if not yy.endswith('TYPE   UNITS  FUEL   SECTOR                   QUANTITIES')]

    index = [idx for idx, s in enumerate(dictlines) if 'QBLK' in s][0]
    index_ATT = [idx for idx, s in enumerate(dictlines) if 'ATT' in s][0]

    dictattrib = dictlines[:index - 1]
    comblock = dictlines[index:]
    dictattrib = dictattrib[:index_ATT]

    # Create DictAttributes in pandas dataframe
    n_rows = len(dictattrib)
    n_cols = 4
    NEMSFortranAttributesTable = pd.DataFrame(np.zeros((n_rows, n_cols)))
    NEMSFortranAttributesTable.columns = ['Dimensions', 'DimName', 'DimSize', 'DimDescrip']

    for jj in range(len(dictattrib)):
        dictparse = dictattrib[jj]
        DIM = str.rstrip(str.strip(dictparse[0:5]))
        FDIMNAM = str.rstrip(str.strip(dictparse[6:15]))
        FDIMSIZ = str.rstrip(str.strip(dictparse[16:23]))
        FDIMDESCRIP = str.rstrip(str.strip(dictparse[24:-1]))
        NEMSFortranAttributesTable.loc[jj] = DIM, FDIMNAM, FDIMSIZ, FDIMDESCRIP

    NEMSFortranAttributesTable = NEMSFortranAttributesTable.set_index('DimName', drop=False)

    # Create NEMS Fortran Variable Table in pandas dataframe vectorized

    dictparse = pd.Series(comblock)
    # Common Block
    ComBlockName = dictparse.str.slice(start=0, stop=17).str.replace(' ', '')
    # Fortran Variable
    NEMSFortVarName = dictparse.str.slice(start=18, stop=34).str.replace(' ', '')
    NEMSFortVarName = NEMSFortVarName.str.upper()
    # Real or Integer Variable
    NEMSReOrImagVal = dictparse.str.slice(start=35, stop=38).str.replace(' ', '')
    # Dimensions
    NEMSFortVarDim = dictparse.str.slice(start=38, stop=43)
    # Dimension Parameters
    NEMSFortVarParam = dictparse.str.slice(start=44, stop=79).str.replace(' ', '')
    NEMSFortVarParam = NEMSFortVarParam.str.lstrip('(')
    NEMSFortVarParam = NEMSFortVarParam.str.rstrip(')')
    NEMSFortVarType = dictparse.str.slice(start=80, stop=86).str.replace(' ', '')
    NEMSFortVarUnit = dictparse.str.slice(start=87, stop=93).str.replace(' ', '')
    NEMSFortVarFuel = dictparse.str.slice(start=94, stop=100).str.replace(' ', '')
    NEMSFortVarSect = dictparse.str.slice(start=101, stop=107).str.replace(' ', '')
    NEMSFortVarNote = dictparse.str.slice(start=108, stop=-1)

    NEMSFortVarTable = pd.concat([ComBlockName, NEMSFortVarName, NEMSReOrImagVal, NEMSFortVarDim, NEMSFortVarParam,
                                  NEMSFortVarType, NEMSFortVarUnit, NEMSFortVarFuel, NEMSFortVarSect, NEMSFortVarNote],
                                 axis=1)
    NEMSFortVarTable.columns = ["Common Block Name", "Fortran Variable Name", "Real or Imaginary",
                                "Dimensions", "Dimensions Parameters", "Parameters", "Units", "Fuel", "Sector", "Note"]

    NEMSFortVarTable = NEMSFortVarTable.set_index('Fortran Variable Name', drop=False)
    NEMSFortVarTable['Fortran Variable Name'].replace('', np.nan, inplace=True)
    NEMSFortVarTable.dropna(subset=['Fortran Variable Name'], inplace=True)

    return NEMSFortVarTable, NEMSFortranAttributesTable


def RetrieveVarDim(NEMSFortTable, NEMSAttributeTable):
    """Construct a dataframe that includes columns for restart variable name, common block name, dimensions, and extra indexes

    Parameters
    ----------
    NEMSFortTable: dataframe
        dataframe that is the return value of function ‘ParseDict’ of variable name from dict.txt
        
    NEMSAttributeTable: dataframe
        dataframe that is the return value of function ‘ParseDict’ of attributes of top half of dict.txt

    Returns
    -------
    dataframe 
        columns are NEMS Fortran Variable Name, Common Block Name, Dimensions, and Extra Indeces necessary for
        conversion of restart to HDF and vice versa.
    
    """
    # this loop goes through the dimension parameters, creates an empty list of lists, then goes through individual
    # dimension parameters to select from the refExtraIndexdf the indeces associated to the dimension parameter.
    # For example, for MNUMYR, it would find all the years from 1990 to 2050 (or other years when that is updated).
    # The loop then appends the list to list_of_list and at the end places it into the appropriate column.

    # Use NEMSFortTable to get out pieces from dict.txt
    dfs = NEMSFortTable
    dfs = dfs[['Fortran Variable Name', 'Common Block Name', 'Dimensions', 'Dimensions Parameters']]
    dfs['Fortran Variable Name'] = dfs['Fortran Variable Name'].str.lower()
    # Build dataframe of variables and commonblock
    vardf = dfs[['Fortran Variable Name', 'Common Block Name']]
    vardf.index = vardf['Fortran Variable Name'].str.upper()
    # Get Dimension Parameters from NEMSFortTable using Variable name (MUST BE UPPER CASE)
    vardf['Dimension Params'] = dfs['Dimensions Parameters']

    # Split any Dimension Parameters that have more than one by commas
    vardf['Dimension Params'] = vardf['Dimension Params'].str.split(',')
    # Create new column for extra indexes, which will hold a list of lists for each extra index
    vardf['Extra Indeces'] = ''
    NEMSDimTable = NEMSAttributeTable[NEMSAttributeTable["Dimensions"] == "DIM"]
    for jj in range(len(vardf['Dimension Params'])):
        list_of_list = []
        # Parse through vardf to get length of dimensional parameter indices
        for kk in range(len(vardf['Dimension Params'][jj])):
            try:
                c = str.strip(vardf['Dimension Params'][jj][kk])
                if str(c).isdigit():
                    c = int(c)
                else:
                    # If variable is string (such as MNUMYR), use the NEMSDimTable to locate size from NEMS dict.txt
                    hold = NEMSDimTable.loc[c]["DimSize"]
                    c = int(hold)
                NumRan = range(1, c + 1)
                list_of_list.append(list(NumRan))
            except:
                #In this instance, MX_UNT does not return correctly from dict.txt. This uses the static 1800 for now 
                if c == 'MX_UNT':
                    hold = 1800
                    NumRan = range(1, hold + 1)
                    list_of_list.append(list(NumRan))
                #In this instance, if the return string ends in 1), drop the trailing )
                elif c == "1)":
                    c = int(c[0])
                    NumRan = range(1, c + 1)
                    list_of_list.append(list(NumRan))
                #In this instance, there is a variable error and it prints out the problem
                else:
                    print(c)
                    print("Error, look again")
        vardf['Extra Indeces'][jj] = list_of_list
    print('End of ParseDict')
    return vardf