"""Stand-ins replacing Fortran functionality for the new Python EPM.

Everything in this file should eventually be made obsolete and deleted. These
utilities exist only to simplify the process of translating Fortran code into
Python. As the new Python code matures and is refactored, uses of these
utilities should be replaced with more Pythonic solutions.
"""

from typing import BinaryIO, Literal, overload

import numpy as np
import numpy.typing as npt
import pandas as pd


def readrngxlsx(excel_file: BinaryIO, sheet_name: str) -> pd.DataFrame:
    """Provide a substitute for `subroutine ReadRngXLSX` defined in nemswk1.f.

    The Fortran subroutine of this name defined in $NEMS/source/nemswk1.f reads
    all "defined ranges" from one worksheet of an Excel spreadsheet into memory
    for later retrieval with the `getrngr` and `getrngi` subroutines.

    For this Python version, just read the entire Excel worksheet into a pandas
    DataFrame and return it. That DataFrame will then be passed into Python
    versions of `getrngr` and `getrngi` to extract the numeric data.

    Parameters
    ----------
    excel_file : BinaryIO
        A file object reading in binary mode from the Excel file.
    sheet_name : str
        The name of the worksheet to read.

    Returns
    -------
    pd.DataFrame
        Unprocessed contents of the Excel worksheet.

    See Also
    --------
    getrngr : Used to extract float arrays from this function's return value.
    getrngi : Used to extract int arrays from this function's return value.
    """
    return pd.read_excel(excel_file, sheet_name, header=None, dtype=object)


@overload
def getrngr(
    df: pd.DataFrame,
    name: str,
    n_rows: Literal[1],
    n_cols: Literal[1]
) -> np.float64:
    ...


@overload
def getrngr(
    df: pd.DataFrame,
    name: str,
    n_rows: int,
    n_cols: int
) -> npt.NDArray[np.float64]:
    ...


def getrngr(df: pd.DataFrame, name: str, n_rows, n_cols):
    """Provide a substitute for `subroutine getrngr` defined in nemswk1.f.

    The Fortran subroutine retrieves arrays of reals from an Excel worksheet
    after `readrngxlsx` has already been called to read the file.

    This Python version just searches through the DataFrame returned by
    `readrngxlsx` to find and process the requested range of data. It is
    returned as an array of floats with the appropriate number of dimensions.

    Parameters
    ----------
    df : pd.DataFrame
        An Excel worksheet DataFrame returned by `readrngxlsx`.
    name : str
        The name of the desired range of data.
    n_rows : int
        The number of rows the data spans in the spreadsheet.
    n_cols : int
        The number of columns the data spans in the spreadsheet.

    Returns
    -------
    np.float64 | npt.NDArray[np.float64]
        The requested scalar, 1D array, or 2D array of data.

    See Also
    --------
    readrngxlsx : Call to get the DataFrame for this function.
    getrngi : Same as this function, but returns ints.
    """
    i_row, i_col = find_xlsx_position(df, name, n_rows, n_cols)

    # If the data range is a single scalar, don't bother returning an array
    if n_rows == 1 and n_cols == 1:
        return np.float64(df.iat[i_row, i_col])

    # Slice the range of data out of the DataFrame
    j_row = i_row + n_rows
    j_col = i_col + n_cols
    data = df.iloc[i_row:j_row, i_col:j_col]

    # Convert to an array of floats and flatten if it doesn't need to be 2D
    array = data.to_numpy(np.float64)
    if n_rows == 1 or n_cols == 1:
        array = array.flatten()
    return array


@overload
def getrngi(
    df: pd.DataFrame,
    name: str,
    n_rows: Literal[1],
    n_cols: Literal[1]
) -> np.int64:
    ...


@overload
def getrngi(
    df: pd.DataFrame,
    name: str,
    n_rows: int,
    n_cols: int
) -> npt.NDArray[np.int64]:
    ...


def getrngi(df: pd.DataFrame, name: str, n_rows, n_cols):
    """Provide a substitute for `subroutine getrngi` defined in nemswk1.f.

    The Fortran subroutine retrieves arrays of integers from an Excel worksheet
    after `readrngxlsx` has already been called to read the file.

    This Python version just searches through the DataFrame returned by
    `readrngxlsx` to find and process the requested range of data. It is
    returned as an array of ints with the appropriate number of dimensions.

    Parameters
    ----------
    df : pd.DataFrame
        An Excel worksheet DataFrame returned by `readrngxlsx`.
    name : str
        The name of the desired range of data.
    n_rows : int
        The number of rows the data spans in the spreadsheet.
    n_cols : int
        The number of columns the data spans in the spreadsheet.

    Returns
    -------
    np.int64 | npt.NDArray[np.int64]
        The requested scalar, 1D array, or 2D array of data.

    See Also
    --------
    readrngxlsx : Call to get the DataFrame for this function.
    getrngr : Same as this function, but returns floats.
    """
    i_row, i_col = find_xlsx_position(df, name, n_rows, n_cols)

    # If the data range is a single scalar, don't bother returning an array
    if n_rows == 1 and n_cols == 1:
        return np.int64(df.iat[i_row, i_col])

    # Slice the range of data out of the DataFrame
    j_row = i_row + n_rows
    j_col = i_col + n_cols
    data = df.iloc[i_row:j_row, i_col:j_col]

    # Convert to an array of ints and flatten if it doesn't need to be 2D
    array = data.to_numpy(np.int64)
    if n_rows == 1 or n_cols == 1:
        array = array.flatten()
    return array


def find_xlsx_position(
    df: pd.DataFrame,
    name: str,
    n_rows: int,
    n_cols: int,
) -> tuple[int, int]:
    """Find the start of a data range in a DataFrame returned by `readrngxlsx`.

    The row and column indices returned point to the top left cell of the
    requested data range.

    Parameters
    ----------
    df : pd.DataFrame
        An Excel worksheet DataFrame returned by `readrngxlsx`.
    name : str
        The name of the data range to search for.
    n_rows : int
        The number of rows the data spans in the spreadsheet.
    n_cols : int
        The number of columns the data spans in the spreadsheet.

    Returns
    -------
    tuple[int, int]
        The row index `i_row` and column index `i_col` in the DataFrame where
        the specified data range begins.

    Raises
    ------
    ValueError
        If the specified data range name is not found or matches more than one
        position in the DataFrame.

    See Also
    --------
    readrngxlsx : Call to get the DataFrame for this function.
    getrngr, getrngi : Use this function to find data ranges.
    """
    # The second column of the Excel worksheet contains the data range names
    name_column = df.iloc[:, 1]
    matching_indices = name_column.index[name_column == name].to_list()

    # Make sure the requested data range name occurs in exactly one place
    if len(matching_indices) == 0:
        raise ValueError(f"name {name!r} was not found in find_xlsx_position")
    if len(matching_indices) > 1:
        raise ValueError(f"name {name!r} is non-unique in find_xlsx_position")

    # The data start on the same row as the name and one column to the right
    i_row = matching_indices[0]
    i_col = 2

    # Add an additional offset if the requested data range is 2D
    # This reflects the way the data are laid out in the Excel file
    if n_rows > 1 and n_cols > 1:
        i_row += 1
        i_col += 1

    return i_row, i_col
