import pandas as pd
import numpy as np

# TODO GNM: remove this import and fix warnings
import warnings
warnings.simplefilter('ignore', category=FutureWarning)

def ParseDict(file_dict):
    """parses dict.txt (given as an input) and outputs the Fortran Variable Table and Attribute Table.

    Parameters
    ----------
    file_dict : dict
        dictionary file path from NEMS (dict.txt).

    Returns
    -------
    dataframe
        consisting of the first half of dict.txt
        
    dataframe
        consisting of the second half of dict.txt comprised of commonblock, variable names, dimensions, dimension parameters, etc
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
    NEMSFortranAttributesTable = NEMSFortranAttributesTable.astype({
        'Dimensions': 'str',
        'DimName': 'str',
        'DimSize': 'int64',
        'DimDescrip': 'str'
    })

    for jj, dictparse in enumerate(dictattrib):
        DIM = str.rstrip(str.strip(dictparse[0:5]))
        if 'DIMD' == DIM:
            continue
        FDIMNAM = str.rstrip(str.strip(dictparse[6:15]))
        # try to cast to int64 if fail use original code
        # TODO: Josh + Greg -- What is the correct/expected value when FDIMSIZ = '0 1'
        # Set breakpoint for FDIMNAM = MNUMPR to recreate
        try:
            FDIMSIZ = np.int64(str.rstrip(str.strip(dictparse[16:23])))
        except TypeError:
            FDIMSIZ = str.rstrip((str.strip(dictparse[16:23])))
        except ValueError:
            FDIMSIZ = str.rstrip(str.strip(dictparse[16:23]))
        except Exception as e:
            print(e)
        FDIMDESCRIP = str.rstrip(str.strip(dictparse[24:-1]))

        # TODO GNM: fix FutureWarning here -- occurs when a NaN is in data and set as ''
        NEMSFortranAttributesTable.loc[jj] = DIM, FDIMNAM, FDIMSIZ, FDIMDESCRIP
    NEMSFortranAttributesTable = NEMSFortranAttributesTable.set_index('DimName', drop=False)

    # Create NEMS Fortran Variable Table in pandas dataframe vectorized

    dictparse = pd.Series(comblock)
    # Common Block
    ComBlockName = dictparse.str.slice(start=0, stop=17).str.replace(' ', '')
    # Fortran Variable
    NEMSFortVarName = dictparse.str.slice(start=18, stop=34).str.replace(' ', '')
    NEMSFortVarName = NEMSFortVarName.str.upper()
    # Real or Imaginary?
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
    NEMSFortVarTable['Fortran Variable Name'] = NEMSFortVarTable['Fortran Variable Name'].replace('', np.nan)
    NEMSFortVarTable.dropna(subset=['Fortran Variable Name'], inplace=True)

    return NEMSFortVarTable, NEMSFortranAttributesTable