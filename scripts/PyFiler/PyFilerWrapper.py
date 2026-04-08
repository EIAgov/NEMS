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
    NEMSFortranAttributesTable = pd.DataFrame(np.zeros((n_rows, n_cols),dtype=object))
    NEMSFortranAttributesTable.columns = ['Dimensions', 'DimName', 'DimSize', 'DimDescrip']

    for jj in range(len(dictattrib)):
        dictparse = dictattrib[jj]
        DIM = str.rstrip(str.strip(dictparse[0:5]))
        FDIMNAM = str.rstrip(str.strip(dictparse[6:13]))
        FDIMSIZ = str.rstrip(str.strip(dictparse[14:23]))
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
    #NEMSFortVarTable['Fortran Variable Name'].replace('', np.nan, inplace=True)
    NEMSFortVarTable.loc[:, 'Fortran Variable Name'] = NEMSFortVarTable['Fortran Variable Name'].replace('', np.nan)
    NEMSFortVarTable.dropna(subset=['Fortran Variable Name'], inplace=True)

    return NEMSFortVarTable, NEMSFortranAttributesTable


def RetrieveVarDim(NEMSFortTable, NEMSAttributeTable):
    dfs = NEMSFortTable
    dfs = dfs[['Fortran Variable Name', 'Common Block Name', 'Dimensions', 'Dimensions Parameters']]
    #dfs['Fortran Variable Name'] = dfs['Fortran Variable Name'].str.lower() #Keep this lower case
    dfs.loc[:, 'Fortran Variable Name'] = dfs['Fortran Variable Name'].str.lower()  # Keep this lower case
    # Create a unique index for dfs
    #dfs['Fortran Variable Name Upper'] = dfs['Fortran Variable Name'].str.upper()
    dfs.loc[:, 'Fortran Variable Name Upper'] = dfs['Fortran Variable Name'].str.upper()
    #dfs['CombinedIndex'] = dfs['Fortran Variable Name Upper'] + '_' + dfs['Common Block Name']
    dfs.loc[:, 'CombinedIndex'] = dfs['Fortran Variable Name Upper'] + '_' + dfs['Common Block Name']

    # Check for duplicates in the combined index
    if dfs['CombinedIndex'].duplicated().any():
        print("Warning: Duplicate combined indices found.  Investigate data for potential issues.")
        #Optionally handle duplicates here, e.g., by removing them or modifying them

    dfs = dfs.set_index('CombinedIndex', drop=True)

    # Filter NEMSAttributeTable to exclude "DIMD" entries
    NEMSAttributeTable = NEMSAttributeTable[NEMSAttributeTable["Dimensions"] != "DIMD"]

    # Initialize Extra Indeces as an empty list
    extra_indeces = []

    # Iterate through rows to construct Extra Indeces
    for index, row in dfs.iterrows():
        list_of_list = []
        dimension_params = row['Dimensions Parameters'].split(',') if isinstance(row['Dimensions Parameters'], str) else []

        for param in dimension_params:
            try:
                c = str.strip(param)
                if str(c).isdigit():
                    c = int(c)
                else:
                    # If variable is string (such as MNUMYR), use the NEMSDimTable to locate size from NEMS dict.txt
                    if c in NEMSAttributeTable.index:  # Check if c exists in NEMSAttributeTable
                        hold = NEMSAttributeTable.loc[c, "DimSize"]
                        c = int(hold)
                    else:
                        print(c)
                        print("Error, look again")
                        c = 1  # Assign default value
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

        extra_indeces.append(list_of_list)

    # Create vardf using the unique index from dfs
    vardf = pd.DataFrame({
        'Fortran Variable Name': dfs['Fortran Variable Name'],  # Lowercase
        'Fortran Variable Name Upper': dfs['Fortran Variable Name Upper'],  # Capitalized
        'Common Block Name': dfs['Common Block Name'],
        'Dimension Params': dfs['Dimensions Parameters'],   #Dimension Parameters get pulled here
        'Extra Indeces': extra_indeces  # Assign prepared list here
    }, index=dfs.index)  # index from dfs to keep the indexing

    # Ensure the 'Extra Indeces' column is of type object
    vardf['Extra Indeces'] = vardf['Extra Indeces'].astype(object)

    # Split any Dimension Parameters that have more than one by commas
    vardf['Dimension Params'] = vardf['Dimension Params'].str.split(',')

    # Enforce the exact column order and reset index to avoid a column named "Fortran Variable Name Upper"
    #vardf = vardf.reset_index(drop=True)  # Drop the index,
    vardf = vardf[['Fortran Variable Name Upper', 'Fortran Variable Name', 'Common Block Name', 'Dimension Params', 'Extra Indeces']] # Enforce desired column order and switch order
    
    vardf = vardf.reset_index()
    vardf = vardf.set_index(vardf["Fortran Variable Name Upper"])
    vardf = vardf[['Fortran Variable Name Upper', 'Fortran Variable Name', 'Common Block Name', 'Dimension Params', 'Extra Indeces']] # Enforce desired column order and switch order
    vardf = vardf.rename(columns={"Fortran Variable Name Upper": "Fortran Variable Name"})
    print('End of ParseDict')
    return vardf

if __name__ == "__main__":
    a,b = ParseDict('/input/dict.txt')
    vardf = RetrieveVarDim(a,b)
    vardf.to_csv('NEWVardf.csv', index=False)
    print("done")
