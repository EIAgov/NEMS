"""Classes for storing restart file data in memory and doing restart file IO.

This module contains the `Restart` class, which wraps around PyFiler for
reading and writing EPM-relevant parts of the NEMS binary restart file. For
standalone runs, a Restart object is used to read the restart file once before
EPM runs, store copies of all relevant arrays in memory while EPM runs, and
then write the outputs to a new restart file after EPM runs. For integrated
runs, no file IO occurs; an existing PyFiler object is directly passed in when
the Restart object is created.
"""

import importlib
import os
from pathlib import Path
import sys
from types import ModuleType
from typing import Any, Callable, Final

import numpy as np
import numpy.typing as npt

from epm_common import get_input_path, get_output_path, print_it


# List of possible PyFiler import paths to check
PYFILER_IMPORT_PATHS: Final[list[Path]] = [
    Path.cwd() / "PyFiler",
    Path.cwd().parent / "PyFiler",
    Path.cwd().parent.parent / "PyFiler",
    Path.cwd().parent / "Scripts" / "PyFiler",
]

# Directory containing Intel compiler DLLs needed by PyFiler
PYFILER_DLL_DIR: Final[Path] = Path(
    "C:\\",
    "Program Files (x86)",
    "Intel",
    "oneAPI",
    "compiler",
    "2023.2.1",
    "windows",
    "redist",
    "intel64_win",
    "compiler",
)

# List of files that PyFiler needs to find for a successful standalone run
PYFILER_NEEDED_FILES: Final[list[Path]] = [
    Path.cwd() / "PyFiler" / "pyfiler1.cp311-win_amd64.pyd",
    Path.cwd() / "FILELIST",
    Path.cwd() / "restart.unf",
    Path.cwd() / "input" / "dict.txt",
    Path.cwd() / "input" / "varlist.txt",
]


# An internal PyFiler object used by EPM for standalone runs
_epm_pyfiler: ModuleType | None = None


def import_pyfiler() -> ModuleType:
    """Import and return the PyFiler module as needed for standalone runs.

    This function encapsulates a number of supporting operations that ensure
    PyFiler actually imports and runs correctly. The first time it is called,
    this function assigns the imported PyFiler object to an internal global
    variable. Subsequent calls just return that stored object.

    Returns
    -------
    ModuleType
        The imported PyFiler module object.
    """
    global _epm_pyfiler
    if _epm_pyfiler is not None:
        return _epm_pyfiler

    sys_paths = set(map(Path, sys.path))

    for path in PYFILER_IMPORT_PATHS:
        if path not in sys_paths:
            sys.path.append(str(path))

    if PYFILER_DLL_DIR not in sys_paths:
        sys.path.append(str(PYFILER_DLL_DIR))
    os.add_dll_directory(str(PYFILER_DLL_DIR))

    print_paths = [f"    {Path(path)!s}" for path in sys.path]
    print_it(0, "Print all system paths:\n" + "\n".join(print_paths))

    # Try to warn the user if any necessary PyFiler files are absent
    for needed_file in PYFILER_NEEDED_FILES:
        if not needed_file.is_file():
            warning_message = " ".join([
                "WARNING: PyFiler will likely crash due to missing file",
                str(needed_file),
            ])
            print_it(0, warning_message)

    # Import the PyFiler module dynamically so linters won't try to find it
    # Linters are not able to understand runtime sys.path.append() calls
    _epm_pyfiler = importlib.import_module("pyfiler1")
    return _epm_pyfiler


# Base file names to use for the input and output restart files
# These are used when EPM is running in standalone configuration
RESTART_IN_FILE_NAME: Final[str] = "EPM_restart_in.unf"
RESTART_OUT_FILE_NAME: Final[str] = "EPM_restart_out.unf"

# All numpy dtypes that could conceivably come in from Fortran
INT8: Final[np.dtype] = np.dtype(np.int8)
INT16: Final[np.dtype] = np.dtype(np.int16)
INT32: Final[np.dtype] = np.dtype(np.int32)
INT64: Final[np.dtype] = np.dtype(np.int64)
FLOAT32: Final[np.dtype] = np.dtype(np.float32)
FLOAT64: Final[np.dtype] = np.dtype(np.float64)
BYTES: Final[np.dtype] = np.dtype(np.bytes_)


def get_scalar(array: npt.NDArray) -> Any:
    """Extract the single scalar value from inside a zero-dimensional array.

    Parameters
    ----------
    array : npt.NDArray
        An array with shape equal to ().

    Returns
    -------
    Any
        The single element contained inside the array.

    Raises
    ------
    TypeError
        If the array's shape is not equal to ().
    """
    if array.shape != ():
        raise TypeError(
            f"get_scalar called on array of shape {array.shape!r}"
        )
    return array[()]


def get_array(
    array: npt.NDArray,
    *,
    dtype: npt.DTypeLike = None
) -> npt.NDArray:
    """Copy an array of values, optionally modifying the array's data type.

    Parameters
    ----------
    array : npt.NDArray
        An array of values, which must be at least one-dimensional.
    dtype : npt.DTypeLike, optional
        The desired data type of the returned array. If None, then NumPy will
        infer the data type from the contents of the input array. This is the
        default behavior.

    Returns
    -------
    npt.NDArray
        The copied array with the specified data type.

    Raises
    ------
    TypeError
        If the input array is a zero-dimensional scalar array.
    """
    if array.shape == ():
        raise TypeError("get_array called on a zero-dimensional array")
    return np.array(array, dtype, copy=True)


def unpack_int(pyfiler_variable: npt.NDArray) -> int:
    """Cautiously extract an integer value from a PyFiler variable.

    Parameters
    ----------
    pyfiler_variable : npt.NDArray
        A PyFiler variable containing a scalar integer value.

    Returns
    -------
    int
        The extracted integer value.

    Raises
    ------
    TypeError
        If the PyFiler variable has an incompatible data type.
    """
    dtype = pyfiler_variable.dtype
    if dtype not in (INT8, INT16, INT32, INT64):
        raise TypeError(
            f"unpack_int called with incompatible dtype {dtype!r}"
        )
    scalar = get_scalar(pyfiler_variable)
    return int(scalar)


def unpack_float(pyfiler_variable: npt.NDArray) -> float:
    """Cautiously extract a float value from a PyFiler variable.

    Parameters
    ----------
    pyfiler_variable : npt.NDArray
        A PyFiler variable containing a scalar float value.

    Returns
    -------
    float
        The extracted float value.

    Raises
    ------
    TypeError
        If the PyFiler variable has an incompatible data type.
    """
    dtype = pyfiler_variable.dtype
    if dtype not in (FLOAT32, FLOAT64):
        raise TypeError(
            f"unpack_float called with incompatible dtype {dtype!r}"
        )
    scalar = get_scalar(pyfiler_variable)
    return float(scalar)


def unpack_str(pyfiler_variable: npt.NDArray) -> str:
    """Cautiously extract a string value from a PyFiler variable.

    Parameters
    ----------
    pyfiler_variable : npt.NDArray
        A PyFiler variable containing a scalar string value.

    Returns
    -------
    str
        The extracted string value.

    Raises
    ------
    TypeError
        If the PyFiler variable has an incompatible data type.
    """
    dtype = pyfiler_variable.dtype
    if not np.issubdtype(dtype, BYTES):
        raise TypeError(
            f"unpack_str called with incompatible dtype {dtype!r}"
        )
    scalar = get_scalar(pyfiler_variable)
    return scalar.decode().rstrip()


def unpack_int_array(pyfiler_variable: npt.NDArray) -> npt.NDArray[np.int64]:
    """Cautiously copy an array of integers from a PyFiler variable.

    Parameters
    ----------
    pyfiler_variable : npt.NDArray
        A PyFiler variable containing an array of integer values.

    Returns
    -------
    npt.NDArray[np.int64]
        The copied array of integer values.

    Raises
    ------
    TypeError
        If the PyFiler variable has an incompatible data type.
    """
    dtype = pyfiler_variable.dtype
    if dtype not in (INT8, INT16, INT32, INT64):
        raise TypeError(
            f"unpack_int_array called with incompatible dtype {dtype!r}"
        )
    array_copy = get_array(pyfiler_variable, dtype=np.int64)
    return array_copy


def unpack_float_array(
    pyfiler_variable: npt.NDArray
) -> npt.NDArray[np.float64]:
    """Cautiously copy an array of floats from a PyFiler variable.

    Parameters
    ----------
    pyfiler_variable : npt.NDArray
        A PyFiler variable containing an array of float values.

    Returns
    -------
    npt.NDArray[np.float64]
        The copied array of float values.

    Raises
    ------
    TypeError
        If the PyFiler variable has an incompatible data type.
    """
    dtype = pyfiler_variable.dtype
    if dtype not in (FLOAT32, FLOAT64):
        raise TypeError(
            f"unpack_float_array called with incompatible dtype {dtype!r}"
        )
    array_copy = get_array(pyfiler_variable, dtype=np.float64)
    return array_copy


def identity(x: Any) -> Any:
    """A function that returns its input.

    Parameters
    ----------
    x : Any
        An input value.

    Returns
    -------
    Any
        The same input value.
    """
    return x


class Reference:
    """A reference to a specific variable in PyFiler with data converters."""

    def __init__(
        self,
        pyfiler_variable: npt.NDArray,
        input_converter: Callable[[Any], Any],
        output_converter: Callable[[Any], Any] | None = None
    ) -> None:
        """Create a new reference to a PyFiler variable with given converters.

        When the restart file data comes in via PyFiler, the `input_converter`
        will be called to transform the PyFiler variable into a Python format
        that will be used while EPM is running. When the data is put back into
        back to the PyFiler variable after EPM runs, the `output_converter` is
        called to transform the data from its EPM Python format back to a
        format that can be safely assigned to the original PyFiler variable.

        Parameters
        ----------
        pyfiler_variable : npt.NDArray
            The PyFiler variable that will hold the values of interest.
        input_converter : Callable[[Any], Any]
            Called on the PyFiler variable once the restart file data is
            available to convert the value into an EPM Python format.
        output_converter : Callable[[Any], Any] | None, optional
            Called on the EPM Python variable to convert the format back before
            the value is assigned back to the PyFiler variable. If None (the
            default), then this PyFiler variable is not used for output.
        """
        self.pyfiler_variable = pyfiler_variable
        self.input_converter = input_converter
        self.output_converter = output_converter

    def use_for_output(self) -> bool:
        """Whether this variable will be used for output back to PyFiler.

        Returns
        -------
        bool
            True if this variable will be used for output and False otherwise.
        """
        return self.output_converter is not None

    def clone_reference(self) -> 'Reference':
        """Make a new reference with the same PyFiler variable and converters.

        Returns
        -------
        Reference
            A new, distinct reference object that points to the same PyFiler
            variable and uses the same input and output converters.
        """
        return Reference(
            self.pyfiler_variable,
            self.input_converter,
            self.output_converter
        )

    def read_values(self) -> Any:
        """Read values from the PyFiler variable and apply the input converter.

        Returns
        -------
        Any
            Values from PyFiler after applying the input converter.
        """
        return self.input_converter(self.pyfiler_variable)

    def write_values(self, values: Any) -> None:
        """Apply the output converter and write values to the PyFiler variable.

        Parameters
        ----------
        values : Any
            New values that should be passed through the output converter and
            written to the PyFiler variable.

        Raises
        ------
        TypeError
            If this reference is not allowed to be used for output.
        """
        if self.output_converter is None:
            raise TypeError("this reference cannot be used for output")
        self.pyfiler_variable[...] = self.output_converter(values)


class Restart:
    """Handles restart file IO with PyFiler and stores values while EPM runs.

    This class manages the full list of restart file variables that are
    relevant to EPM for both input and output. It is capable of both restart
    file IO with the file system (for standalone runs) and reading directly
    from a pre-initialized PyFiler object (for integrated runs). While EPM is
    running, the values for each restart file variable are stored in instance
    attributes named for the include file and Fortran variable with an
    underscore separator, e.g., `self.ghgrep_em_resd`.
    """

    def __init__(self, pyfiler: ModuleType | None = None) -> None:
        """Create a new restart file object and set up its instance attributes.

        One instance attribute will be created for each EPM-relevant restart
        variable. These attributes will initially contain reference objects,
        which will then be dynamically replaced by values of the correct types
        when PyFiler reads the binary restart file. If instead a PyFiler object
        is provided, the actual reading of the restart file will be skipped.

        Restart file disk IO is used for standalone EPM runs and direct use of
        an existing PyFiler object is for integrated runs.

        Parameters
        ----------
        pyfiler : ModuleType | None, optional
            A pre-initialized PyFiler module object to use instead of reading
            the binary restart file from disk. The default value of None
            indicates that PyFiler should be internally initialized and the
            restart file data should be read from disk.
        """
        # Initialize the PyFiler module internally if not provided
        if pyfiler is None:
            pyfiler = import_pyfiler()
        self.pyfiler = pyfiler

        # Start with the two essential include files that define all the NEMS
        # constants and control variables
        self._include_parametr()
        self._include_ncntrl()

        # Then do the rest of the include files EPM needs alphabetically
        self._include_ab32()
        self._include_ampblk()
        self._include_bifurc()
        self._include_calshr()
        self._include_ccatsdat()
        self._include_coalemm()
        self._include_cogen()
        self._include_convfact()
        self._include_ecpcntl()
        self._include_emablk()
        self._include_emeblk()
        self._include_emission()
        self._include_emoblk()
        self._include_epmbank()
        self._include_ghgrep()
        self._include_hmmblk()
        self._include_indepm()
        self._include_indout()
        self._include_indrep()
        self._include_lfmmout()
        self._include_macout()
        self._include_mpblk()
        self._include_ngtdmrep()
        self._include_ogsmout()
        self._include_pmmftab()
        self._include_pmmout()
        self._include_pmmrpt()
        self._include_qblk()
        self._include_qsblk()
        self._include_tranrep()
        self._include_uefdout()
        self._include_wrenew()

        # Construct an internal dict of output references, copying each one
        self._output_references: dict[str, Reference] = {}
        for name in self._find_all_references():
            ref: Reference = getattr(self, name)
            if ref.use_for_output():
                self._output_references[name] = ref.clone_reference()

    def _find_all_references(self) -> list[str]:
        """List all instance attributes that are currently references.

        Returns
        -------
        list[str]
            List of strings corresponding to the names of every instance
            attribute that is currently an instance of the Reference class.
        """
        refs: list[str] = []
        for attribute_name, attribute_value in self.__dict__.items():
            if isinstance(attribute_value, Reference):
                refs.append(attribute_name)
        return refs

    def read_file(self) -> None:
        """Read the restart file with PyFiler and set instance attributes.

        If a PyFiler object was passed in when this restart file object was
        constructed, then the actual file IO operation is skipped. However,
        calling this method is still necessary for setting the instance
        attributes.
        """
        # Read data from the restart file into the PyFiler arrays if needed
        global _epm_pyfiler
        if self.pyfiler is _epm_pyfiler:
            file_path = get_input_path() / RESTART_IN_FILE_NAME
            self.pyfiler.utils.read_filer(str(file_path))

        # Dynamically convert references into values taken out of PyFiler
        for name in self._find_all_references():
            ref: Reference = getattr(self, name)
            values = ref.read_values()
            setattr(self, name, values)

    def write_file(self) -> None:
        """Write instance attributes out to the restart file with PyFiler.

        If a PyFiler object was passed in when this restart file object was
        constructed, then the actual file IO operation is skipped. However,
        calling this method is still necessary for writing the instance
        attribute values back to PyFiler.
        """
        # Write values from the instance attributes back to the PyFiler arrays
        for name, ref in self._output_references.items():
            values = getattr(self, name)
            ref.write_values(values)

        # Write data from the PyFiler arrays out to the restart file if needed
        global _epm_pyfiler
        if self.pyfiler is _epm_pyfiler:
            file_path = get_output_path() / RESTART_OUT_FILE_NAME
            self.pyfiler.utils.write_filer(str(file_path))

    # Methods for adding new instance attributes as PyFiler references

    def _add_int(
        self,
        pyfiler_variable: npt.NDArray,
        output: bool = False
    ) -> int:
        """Add a new Reference attribute that refers to a single integer.

        Parameters
        ----------
        pyfiler_variable : npt.NDArray
            The PyFiler variable to use for reading and writing the integer
            value.
        output : bool, optional
            Whether this integer should be written back to PyFiler as an EPM
            output. Defaults to False, which indicates input only.

        Returns
        -------
        int
            Reference that will be replaced with an integer when the restart
            file data is read from PyFiler.
        """
        ref = Reference(
            pyfiler_variable,
            input_converter=unpack_int,
            output_converter=(identity if output else None)
        )

        # The return type hint is intentionally wrong to trick the linter
        return ref  # type: ignore

    def _add_float(
        self,
        pyfiler_variable: npt.NDArray,
        output: bool = False
    ) -> float:
        """Add a new reference attribute that refers to a single float.

        Parameters
        ----------
        pyfiler_variable : npt.NDArray
            The PyFiler variable to use for reading and writing the float
            value.
        output : bool, optional
            Whether this float should be written back to PyFiler as an EPM
            output. Defaults to False, which indicates input only.

        Returns
        -------
        int
            Reference that will be replaced with a float when the restart file
            data is read from PyFiler.
        """
        ref = Reference(
            pyfiler_variable,
            input_converter=unpack_float,
            output_converter=(identity if output else None)
        )

        # The return type hint is intentionally wrong to trick the linter
        return ref  # type: ignore

    def _add_str(
        self,
        pyfiler_variable: npt.NDArray,
        output: bool = False
    ) -> str:
        """Add a new reference attribute that refers to a single string.

        Parameters
        ----------
        pyfiler_variable : npt.NDArray
            The PyFiler variable to use for reading and writing the string
            value.
        output : bool, optional
            Whether this string should be written back to PyFiler as an EPM
            output. Defaults to False, which indicates input only.

        Returns
        -------
        str
            Reference that will be replaced with a string when the restart file
            data is read from PyFiler.
        """
        ref = Reference(
            pyfiler_variable,
            input_converter=unpack_str,
            output_converter=(identity if output else None)
        )

        # The return type hint is intentionally wrong to trick the linter
        return ref  # type: ignore

    def _add_int_array(
        self,
        pyfiler_variable: npt.NDArray,
        output: bool = False
    ) -> npt.NDArray[np.int64]:
        """Add a new reference attribute that refers to an array of integers.

        Parameters
        ----------
        pyfiler_variable : npt.NDArray
            The PyFiler variable to use for reading and writing the array of
            integer values.
        output : bool, optional
            Whether this array should be written back to PyFiler as an EPM
            output. Defaults to False, which indicates input only.

        Returns
        -------
        npt.NDArray[np.int64]
            Reference that will be replaced with an array of integers when the
            restart file data is read from PyFiler.
        """
        ref = Reference(
            pyfiler_variable,
            input_converter=unpack_int_array,
            output_converter=(identity if output else None)
        )

        # The return type hint is intentionally wrong to trick the linter
        return ref  # type: ignore

    def _add_float_array(
        self,
        pyfiler_variable: npt.NDArray,
        output: bool = False
    ) -> npt.NDArray[np.float64]:
        """Add a new reference attribute that refers to an array of floats.

        Parameters
        ----------
        pyfiler_variable : Any
            The PyFiler variable to use for reading and writing the array of
            float values.
        output : bool, optional
            Whether this array should be written back to PyFiler as an EPM
            output. Defaults to False, which indicates input only.

        Returns
        -------
        npt.NDArray[np.float64]
            Reference that will be replaced with an array of floats when the
            restart file data is read from PyFiler.
        """
        ref = Reference(
            pyfiler_variable,
            input_converter=unpack_float_array,
            output_converter=(identity if output else None)
        )

        # The return type hint is intentionally wrong to trick the linter
        return ref  # type: ignore

    # Includes are defined at the end of this file because they are very long

    def _include_parametr(self) -> None:
        """Create instance variables for the parametr include file.

        This include file defines important global NEMS constants, or
        parameters, as Fortran calls them.
        """
        # Everything in this include file is a constant parameter
        self.parametr_mnumyr = self._add_int(self.pyfiler.utils.mnumyr)
        self.parametr_mnumcr = self._add_int(self.pyfiler.utils.mnumcr)
        self.parametr_mnumpr = self._add_int(self.pyfiler.utils.mnumpr)
        self.parametr_ndreg = self._add_int(self.pyfiler.utils.ndreg)
        self.parametr_mnumnr = self._add_int(self.pyfiler.utils.mnumnr)
        self.parametr_mnpollut = self._add_int(self.pyfiler.utils.mnpollut)
        self.parametr_msedyr = self._add_int(self.pyfiler.utils.msedyr)
        self.parametr_nngem = self._add_int(self.pyfiler.utils.nngem)
        self.parametr_baseyr = self._add_int(self.pyfiler.utils.baseyr)

    def _include_ncntrl(self) -> None:
        """Create instance variables for the ncntrl include file.

        This include file contains important global NEMS control variables,
        which are generally set by the Integrating Module.
        """
        # Model switches
        self.ncntrl_exi = self._add_int(self.pyfiler.ncntrl.exi)

        # Years
        self.ncntrl_firsyr = self._add_int(self.pyfiler.ncntrl.firsyr)
        self.ncntrl_lastyr = self._add_int(self.pyfiler.ncntrl.lastyr)
        self.ncntrl_lastcalyr = self._add_int(self.pyfiler.ncntrl.lastcalyr)
        self.ncntrl_maxitr = self._add_int(self.pyfiler.ncntrl.maxitr)
        self.ncntrl_ijumpyr = self._add_int(self.pyfiler.ncntrl.ijumpyr)
        self.ncntrl_ijumpcalyr = self._add_int(self.pyfiler.ncntrl.ijumpcalyr)

        # Control variables set in Integrating Module
        self.ncntrl_curitr = self._add_int(self.pyfiler.ncntrl.curitr)
        self.ncntrl_curiyr = self._add_int(self.pyfiler.ncntrl.curiyr)
        self.ncntrl_curcalyr = self._add_int(self.pyfiler.ncntrl.curcalyr)
        self.ncntrl_fcrl = self._add_int(self.pyfiler.ncntrl.fcrl)
        self.ncntrl_ncrl = self._add_int(self.pyfiler.ncntrl.ncrl)
        self.ncntrl_yearpr = self._add_int(self.pyfiler.ncntrl.yearpr)

        # Global character variables
        self.ncntrl_scen = self._add_str(self.pyfiler.nchar.scen)
        self.ncntrl_date = self._add_str(self.pyfiler.nchar.date)

        # Control variables for NEMS cycles
        self.ncntrl_curirun = self._add_int(self.pyfiler.cycleinfo.curirun)
        self.ncntrl_numiruns = self._add_int(self.pyfiler.cycleinfo.numiruns)

    # Includes besides parametr and ncntrl are listed alphabetically from here

    def _include_ab32(self) -> None:
        """Create instance variables for the ab32 include file.

        This include file contains cap and trade variables for California AB32.
        """
        self.ab32_ab_cap_tot = self._add_float_array(
            self.pyfiler.ab32.ab_cap_tot, output=True
        )
        self.ab32_ab_offset_frac = self._add_float_array(
            self.pyfiler.ab32.ab_offset_frac, output=True
        )
        self.ab32_ab_cstcont_frac = self._add_float_array(
            self.pyfiler.ab32.ab_cstcont_frac, output=True
        )
        self.ab32_ab_cstcont_avl = self._add_float_array(
            self.pyfiler.ab32.ab_cstcont_avl, output=True
        )
        self.ab32_ab_reserve_p = self._add_float_array(
            self.pyfiler.ab32.ab_reserve_p, output=True
        )
        self.ab32_ab_auction_p = self._add_float_array(
            self.pyfiler.ab32.ab_auction_p, output=True
        )

    def _include_ampblk(self) -> None:
        """Create instance variables for the ampblk include file.

        This include file contains adjustments for prices in mpblk.
        """
        self.ampblk_mprc = self._add_float_array(self.pyfiler.other.mprc)

    def _include_bifurc(self) -> None:
        """Create instance variables for the bifurc include file.

        This include file stores fossil energy use for entities covered (and
        not covered) by carbon allowance requirements.
        """
        # Industrial sector
        self.bifurc_qe2ngin = self._add_float_array(
            self.pyfiler.bifurc.qe2ngin
        )
        self.bifurc_qe2clin = self._add_float_array(
            self.pyfiler.bifurc.qe2clin
        )
        self.bifurc_qe2mcin = self._add_float_array(
            self.pyfiler.bifurc.qe2mcin
        )
        self.bifurc_qe2mgin = self._add_float_array(
            self.pyfiler.bifurc.qe2mgin
        )
        self.bifurc_qe2dsin = self._add_float_array(
            self.pyfiler.bifurc.qe2dsin
        )
        self.bifurc_qe2ksin = self._add_float_array(
            self.pyfiler.bifurc.qe2ksin
        )
        self.bifurc_qe2lgin = self._add_float_array(
            self.pyfiler.bifurc.qe2lgin
        )
        self.bifurc_qe2rsin = self._add_float_array(
            self.pyfiler.bifurc.qe2rsin
        )
        self.bifurc_qe2pfin = self._add_float_array(
            self.pyfiler.bifurc.qe2pfin
        )
        self.bifurc_qe2sgin = self._add_float_array(
            self.pyfiler.bifurc.qe2sgin
        )
        self.bifurc_qe2pcin = self._add_float_array(
            self.pyfiler.bifurc.qe2pcin
        )
        self.bifurc_qe2otin = self._add_float_array(
            self.pyfiler.bifurc.qe2otin
        )
        self.bifurc_qe2ciin = self._add_float_array(
            self.pyfiler.bifurc.qe2ciin
        )

    def _include_calshr(self) -> None:
        """Create instance variables for the calshr include file.

        This include file contains California shares of Pacific energy use for
        computing AB32 emissions.
        """
        # California share of transportaion ethanol
        self.calshr_ettr_shr = self._add_float_array(
            self.pyfiler.calshr.ettr_shr
        )

    def _include_ccatsdat(self) -> None:
        """Create instance variables for the ccatsdat include file.

        This include file contains CO2 data for the Carbon Capture, Allocation,
        Transportation, and Sequestration (CCATS) module.
        """
        # CO2 volumes from recycled CO2, split by 45Q eligibility
        self.ccatsdat_sup_rec_45q = self._add_float_array(
            self.pyfiler.ccatsdat.sup_rec_45q
        )
        self.ccatsdat_sup_rec_ntc = self._add_float_array(
            self.pyfiler.ccatsdat.sup_rec_ntc
        )

        # CO2 volumes from natural gas processing facilities, split by 45Q
        # eligibility
        self.ccatsdat_sup_ngp_45q = self._add_float_array(
            self.pyfiler.ccatsdat.sup_ngp_45q
        )
        self.ccatsdat_sup_ngp_ntc = self._add_float_array(
            self.pyfiler.ccatsdat.sup_ngp_ntc
        )

        # CO2 volumes from power plants, split by 45Q eligibility
        self.ccatsdat_sup_emm_45q = self._add_float_array(
            self.pyfiler.ccatsdat.sup_emm_45q
        )
        self.ccatsdat_sup_emm_ntc = self._add_float_array(
            self.pyfiler.ccatsdat.sup_emm_ntc
        )

        # CO2 volumes from cement plants, split by 45Q eligibility
        self.ccatsdat_sup_cmt_45q = self._add_float_array(
            self.pyfiler.ccatsdat.sup_cmt_45q
        )
        self.ccatsdat_sup_cmt_ntc = self._add_float_array(
            self.pyfiler.ccatsdat.sup_cmt_ntc
        )

        # CO2 volumes from ethanol production, split by 45Q eligibility
        self.ccatsdat_sup_eth_45q = self._add_float_array(
            self.pyfiler.ccatsdat.sup_eth_45q
        )
        self.ccatsdat_sup_eth_ntc = self._add_float_array(
            self.pyfiler.ccatsdat.sup_eth_ntc
        )

        # CO2 volumes from hydrogen production, split by 45Q eligibility
        self.ccatsdat_sup_h2_45q = self._add_float_array(
            self.pyfiler.ccatsdat.sup_h2_45q
        )
        self.ccatsdat_sup_h2_ntc = self._add_float_array(
            self.pyfiler.ccatsdat.sup_h2_ntc
        )

        # CO2 supply output after CCATS optimization
        self.ccatsdat_co2_sup_out = self._add_float_array(
            self.pyfiler.ccatsdat.co2_sup_out
        )

    def _include_coalemm(self) -> None:
        """Create instance variables for the coalemm include file.

        This include file contains data that transfers between EMM and CMM,
        meaning that it pertains to coal power plants.
        """
        # Constant parameter for number of coal demand regions
        self.coalemm_ndrgg = self._add_int(self.pyfiler.utils.ndrgg)

        # Constant parameter for number of utility demand sectors
        self.coalemm_nclut1 = self._add_int(self.pyfiler.utils.nclut1)

        # Equivalences for arrays
        self.coalemm_qclclnr = self._add_float_array(
            self.pyfiler.utils.qclclnr
        )
        self.coalemm_cclclnr = self._add_float_array(
            self.pyfiler.utils.cclclnr, output=True
        )
        self.coalemm_xclclnr = self._add_float_array(
            self.pyfiler.utils.xclclnr
        )

    def _include_cogen(self) -> None:
        """Create instance variables for the cogen include file.

        This include file contains variables related to cogeneration.
        """
        # Non-traditional existing and planned cogeneration generation
        self.cogen_cgntgen = self._add_float_array(self.pyfiler.cogen.cgntgen)

    def _include_convfact(self) -> None:
        """Create instance variables for the convfact include file.

        This include file contains conversion factors.
        """
        self.convfact_cfdsq = self._add_float(self.pyfiler.convfact.cfdsq)
        self.convfact_cfetq = self._add_float_array(
            self.pyfiler.convfact.cfetq
        )
        self.convfact_cfnpq = self._add_float(self.pyfiler.convfact.cfnpq)
        self.convfact_cfrbob = self._add_float_array(
            self.pyfiler.convfact.cfrbob
        )
        self.convfact_cfftliq = self._add_float_array(
            self.pyfiler.convfact.cfftliq
        )

        # Convert natural gas from Bcf to trills for consumption purposes
        self.convfact_cfngc = self._add_float_array(
            self.pyfiler.convfact.cfngc
        )

        self.convfact_cfbiod = self._add_float_array(
            self.pyfiler.convfact.cfbiod
        )
        self.convfact_cfbiobute = self._add_float_array(
            self.pyfiler.convfact.cfbiobute
        )

    def _include_ecpcntl(self) -> None:
        """Create instance variables for the ecpcntl include file.

        This include file contains initial input data for the ECP linear
        programming optimization model in EMM.

        Note that the ecpcntl include file is not part of the restart file, but
        these variable are still accessible via PyFiler. These are here because
        EPM fills these variables for EMM during integrated runs.
        """
        self.ecpcntl_usw_hg = self._add_int(
            self.pyfiler.ecp_d_cntl.usw_hg, output=True
        )
        self.ecpcntl_usw_mact = self._add_int(
            self.pyfiler.ecp_d_cntl.usw_mact, output=True
        )
        self.ecpcntl_umact_yr = self._add_int(
            self.pyfiler.ecp_d_cntl.umact_yr, output=True
        )
        self.ecpcntl_usw_bact = self._add_int(
            self.pyfiler.ecp_d_cntl.usw_bact, output=True
        )
        self.ecpcntl_ubact_yr = self._add_int(
            self.pyfiler.ecp_d_cntl.ubact_yr, output=True
        )
        self.ecpcntl_usw_dsi = self._add_int(
            self.pyfiler.ecp_d_cntl.usw_dsi, output=True
        )
        self.ecpcntl_udsi_yr = self._add_int(
            self.pyfiler.ecp_d_cntl.udsi_yr, output=True
        )

    def _include_emablk(self) -> None:
        """Create instance variables for the emablk include file.

        This include file contains price adjustments, which are used to add
        carbon fees for policies in EPM.
        """
        # Natural gas, core
        self.emablk_jgfrs = self._add_float_array(
            self.pyfiler.emablk.jgfrs, output=True
        )
        self.emablk_jgfcm = self._add_float_array(
            self.pyfiler.emablk.jgfcm, output=True
        )
        self.emablk_jgftr = self._add_float_array(
            self.pyfiler.emablk.jgftr, output=True
        )
        self.emablk_jgfin = self._add_float_array(
            self.pyfiler.emablk.jgfin, output=True
        )
        self.emablk_jgfel = self._add_float_array(
            self.pyfiler.emablk.jgfel, output=True
        )

        # Natural gas, noncore
        self.emablk_jgirs = self._add_float_array(
            self.pyfiler.emablk.jgirs, output=True
        )
        self.emablk_jgicm = self._add_float_array(
            self.pyfiler.emablk.jgicm, output=True
        )
        self.emablk_jgitr = self._add_float_array(
            self.pyfiler.emablk.jgitr, output=True
        )
        self.emablk_jgiin = self._add_float_array(
            self.pyfiler.emablk.jgiin, output=True
        )
        self.emablk_jgiel = self._add_float_array(
            self.pyfiler.emablk.jgiel, output=True
        )

        # Natural gas
        self.emablk_jngrs = self._add_float_array(
            self.pyfiler.emablk.jngrs, output=True
        )
        self.emablk_jngcm = self._add_float_array(
            self.pyfiler.emablk.jngcm, output=True
        )
        self.emablk_jngtr = self._add_float_array(
            self.pyfiler.emablk.jngtr, output=True
        )
        self.emablk_jngin = self._add_float_array(
            self.pyfiler.emablk.jngin, output=True
        )
        self.emablk_jngel = self._add_float_array(
            self.pyfiler.emablk.jngel, output=True
        )

        # Other natural gas
        self.emablk_jnqngpf = self. _add_float_array(
            self.pyfiler.emablk.jnqngpf, output=True
        )
        self.emablk_jgptr = self._add_float_array(
            self.pyfiler.emablk.jgptr, output=True
        )
        self.emablk_jlpin = self._add_float_array(
            self.pyfiler.emablk.jlpin, output=True
        )

        # Coal
        self.emablk_jclrs = self._add_float_array(
            self.pyfiler.emablk.jclrs, output=True
        )
        self.emablk_jclcm = self._add_float_array(
            self.pyfiler.emablk.jclcm, output=True
        )
        self.emablk_jclin = self._add_float_array(
            self.pyfiler.emablk.jclin, output=True
        )
        self.emablk_jclel = self._add_float_array(
            self.pyfiler.emablk.jclel, output=True
        )

        # Metallurgical coal
        self.emablk_jmcin = self._add_float_array(
            self.pyfiler.emablk.jmcin, output=True
        )

        # Motor gasoline
        self.emablk_jmgcm = self._add_float_array(
            self.pyfiler.emablk.jmgcm, output=True
        )
        self.emablk_jmgtr = self._add_float_array(
            self.pyfiler.emablk.jmgtr, output=True
        )
        self.emablk_jmgin = self._add_float_array(
            self.pyfiler.emablk.jmgin, output=True
        )

        # Jet fuel
        self.emablk_jjftr = self._add_float_array(
            self.pyfiler.emablk.jjftr, output=True
        )

        # Distillate
        self.emablk_jdsrs = self._add_float_array(
            self.pyfiler.emablk.jdsrs, output=True
        )
        self.emablk_jdscm = self._add_float_array(
            self.pyfiler.emablk.jdscm, output=True
        )
        self.emablk_jdstr = self._add_float_array(
            self.pyfiler.emablk.jdstr, output=True
        )
        self.emablk_jdsin = self._add_float_array(
            self.pyfiler.emablk.jdsin, output=True
        )
        self.emablk_jdsel = self._add_float_array(
            self.pyfiler.emablk.jdsel, output=True
        )

        # Kerosene
        self.emablk_jksrs = self._add_float_array(
            self.pyfiler.emablk.jksrs, output=True
        )
        self.emablk_jkscm = self._add_float_array(
            self.pyfiler.emablk.jkscm, output=True
        )
        self.emablk_jksin = self._add_float_array(
            self.pyfiler.emablk.jksin, output=True
        )

        # Liquid petroleum gases
        self.emablk_jlgrs = self._add_float_array(
            self.pyfiler.emablk.jlgrs, output=True
        )
        self.emablk_jlgcm = self._add_float_array(
            self.pyfiler.emablk.jlgcm, output=True
        )
        self.emablk_jlgtr = self._add_float_array(
            self.pyfiler.emablk.jlgtr, output=True
        )
        self.emablk_jlgin = self._add_float_array(
            self.pyfiler.emablk.jlgin, output=True
        )

        # Residual fuel, low sulfur
        self.emablk_jrlcm = self._add_float_array(
            self.pyfiler.emablk.jrlcm, output=True
        )
        self.emablk_jrltr = self._add_float_array(
            self.pyfiler.emablk.jrltr, output=True
        )
        self.emablk_jrlin = self._add_float_array(
            self.pyfiler.emablk.jrlin, output=True
        )
        self.emablk_jrlel = self._add_float_array(
            self.pyfiler.emablk.jrlel, output=True
        )

        # Residual fuel, high sulfur
        self.emablk_jrhtr = self._add_float_array(
            self.pyfiler.emablk.jrhtr, output=True
        )
        self.emablk_jrhel = self._add_float_array(
            self.pyfiler.emablk.jrhel, output=True
        )

        # Residual fuel
        self.emablk_jrscm = self._add_float_array(
            self.pyfiler.emablk.jrscm, output=True
        )
        self.emablk_jrstr = self._add_float_array(
            self.pyfiler.emablk.jrstr, output=True
        )
        self.emablk_jrsin = self._add_float_array(
            self.pyfiler.emablk.jrsin, output=True
        )
        self.emablk_jrsel = self._add_float_array(
            self.pyfiler.emablk.jrsel, output=True
        )

        # Other petroleum
        self.emablk_jottr = self._add_float_array(
            self.pyfiler.emablk.jottr, output=True
        )

        # Alcohols
        self.emablk_jmetr = self._add_float_array(
            self.pyfiler.emablk.jmetr, output=True
        )
        self.emablk_jettr = self._add_float_array(
            self.pyfiler.emablk.jettr, output=True
        )

        # Pet coke, petroleum feedstocks, etc.
        self.emablk_jpfin = self._add_float_array(
            self.pyfiler.emablk.jpfin, output=True
        )
        self.emablk_jpcin = self._add_float_array(
            self.pyfiler.emablk.jpcin, output=True
        )
        self.emablk_jnqlgpf = self._add_float_array(
            self.pyfiler.emablk.jnqlgpf, output=True
        )
        self.emablk_jsgin = self._add_float_array(
            self.pyfiler.emablk.jsgin, output=True
        )
        self.emablk_jotin = self._add_float_array(
            self.pyfiler.emablk.jotin, output=True
        )
        self.emablk_jpcel = self._add_float_array(
            self.pyfiler.emablk.jpcel, output=True
        )

        # JCLCLNR and JCLELCDR
        self.emablk_jclclnr = self._add_float_array(
            self.pyfiler.emablk.jclclnr, output=True
        )
        self.emablk_jclelcdr = self._add_float_array(
            self.pyfiler.emablk.jclelcdr, output=True
        )

        # E util firm ng emissions (87$/MMBTU)
        self.emablk_jgfelgr = self._add_float_array(
            self.pyfiler.emablk.jgfelgr, output=True
        )

        # E util inter ng emission (87$/MMBTU)
        self.emablk_jgielgr = self._add_float_array(
            self.pyfiler.emablk.jgielgr, output=True
        )

        # E util compet ng emission (87$/MMBTU)
        self.emablk_jgcelgr = self._add_float_array(
            self.pyfiler.emablk.jgcelgr, output=True
        )

        # Hydrocarbon gas liquids, fuel and feedstock
        self.emablk_jprrs = self._add_float_array(
            self.pyfiler.emablk.jprrs, output=True
        )
        self.emablk_jprcm = self._add_float_array(
            self.pyfiler.emablk.jprcm, output=True
        )
        self.emablk_jprtr = self._add_float_array(
            self.pyfiler.emablk.jprtr, output=True
        )
        self.emablk_jetin = self._add_float_array(
            self.pyfiler.emablk.jetin, output=True
        )
        self.emablk_jprin = self._add_float_array(
            self.pyfiler.emablk.jprin, output=True
        )
        self.emablk_jbuin = self._add_float_array(
            self.pyfiler.emablk.jbuin, output=True
        )
        self.emablk_jisin = self._add_float_array(
            self.pyfiler.emablk.jisin, output=True
        )
        self.emablk_jetinpf = self._add_float_array(
            self.pyfiler.emablk.jetinpf, output=True
        )
        self.emablk_jprinpf = self._add_float_array(
            self.pyfiler.emablk.jprinpf, output=True
        )
        self.emablk_jbuinpf = self._add_float_array(
            self.pyfiler.emablk.jbuinpf, output=True
        )
        self.emablk_jisinpf = self._add_float_array(
            self.pyfiler.emablk.jisinpf, output=True
        )
        self.emablk_jppin = self._add_float_array(
            self.pyfiler.emablk.jppin, output=True
        )
        self.emablk_jppinpf = self._add_float_array(
            self.pyfiler.emablk.jppinpf, output=True
        )

        # Lubricants
        self.emablk_jluin = self._add_float_array(
            self.pyfiler.emablk.jluin, output=True
        )

    def _include_emeblk(self) -> None:
        """Create instance variables for the emeblk include file.

        This include file contains carbon emissions factors.

        Note that the hgeblk common block at the bottom of this include file is
        not part of the restart file, but its variables are still accessible
        via PyFiler. They are here because EPM fills these variables for EMM
        during integrated runs.
        """
        # Natural gas, core
        self.emeblk_egfrs = self._add_float_array(
            self.pyfiler.emeblk.egfrs, output=True
        )
        self.emeblk_egfcm = self._add_float_array(
            self.pyfiler.emeblk.egfcm, output=True
        )
        self.emeblk_egftr = self._add_float_array(
            self.pyfiler.emeblk.egftr, output=True
        )
        self.emeblk_egfin = self._add_float_array(
            self.pyfiler.emeblk.egfin, output=True
        )
        self.emeblk_egfel = self._add_float_array(
            self.pyfiler.emeblk.egfel, output=True
        )

        # Natural gas, noncore
        self.emeblk_egirs = self._add_float_array(
            self.pyfiler.emeblk.egirs, output=True
        )
        self.emeblk_egicm = self._add_float_array(
            self.pyfiler.emeblk.egicm, output=True
        )
        self.emeblk_egitr = self._add_float_array(
            self.pyfiler.emeblk.egitr, output=True
        )
        self.emeblk_egiin = self._add_float_array(
            self.pyfiler.emeblk.egiin, output=True
        )
        self.emeblk_egiel = self._add_float_array(
            self.pyfiler.emeblk.egiel, output=True
        )

        # Natural gas
        self.emeblk_engrs = self._add_float_array(
            self.pyfiler.emeblk.engrs, output=True
        )
        self.emeblk_engcm = self._add_float_array(
            self.pyfiler.emeblk.engcm, output=True
        )
        self.emeblk_engtr = self._add_float_array(
            self.pyfiler.emeblk.engtr, output=True
        )
        self.emeblk_engin = self._add_float_array(
            self.pyfiler.emeblk.engin, output=True
        )
        self.emeblk_engel = self._add_float_array(
            self.pyfiler.emeblk.engel, output=True
        )
        self.emeblk_enghm = self._add_float_array(
            self.pyfiler.emeblk.enghm, output=True
        )

        # Other natural gas
        self.emeblk_enqngpf = self._add_float_array(
            self.pyfiler.emeblk.enqngpf, output=True
        )
        self.emeblk_egptr = self._add_float_array(
            self.pyfiler.emeblk.egptr, output=True
        )
        self.emeblk_elpin = self._add_float_array(
            self.pyfiler.emeblk.elpin, output=True
        )

        # Coal
        self.emeblk_eclrs = self._add_float_array(
            self.pyfiler.emeblk.eclrs, output=True
        )
        self.emeblk_eclcm = self._add_float_array(
            self.pyfiler.emeblk.eclcm, output=True
        )
        self.emeblk_eclin = self._add_float_array(
            self.pyfiler.emeblk.eclin, output=True
        )
        self.emeblk_eclel = self._add_float_array(
            self.pyfiler.emeblk.eclel, output=True
        )
        self.emeblk_eclhm = self._add_float_array(
            self.pyfiler.emeblk.eclhm, output=True
        )

        # Metallurgical coal
        self.emeblk_emcin = self._add_float_array(
            self.pyfiler.emeblk.emcin, output=True
        )

        # Motor gasoline
        self.emeblk_emgcm = self._add_float_array(
            self.pyfiler.emeblk.emgcm, output=True
        )
        self.emeblk_emgtr = self._add_float_array(
            self.pyfiler.emeblk.emgtr, output=True
        )
        self.emeblk_emgin = self._add_float_array(
            self.pyfiler.emeblk.emgin, output=True
        )

        # Jet fuel
        self.emeblk_ejftr = self._add_float_array(
            self.pyfiler.emeblk.ejftr, output=True
        )

        # Distillate
        self.emeblk_edsrs = self._add_float_array(
            self.pyfiler.emeblk.edsrs, output=True
        )
        self.emeblk_edscm = self._add_float_array(
            self.pyfiler.emeblk.edscm, output=True
        )
        self.emeblk_edstr = self._add_float_array(
            self.pyfiler.emeblk.edstr, output=True
        )
        self.emeblk_edsin = self._add_float_array(
            self.pyfiler.emeblk.edsin, output=True
        )
        self.emeblk_edsel = self._add_float_array(
            self.pyfiler.emeblk.edsel, output=True
        )

        # Kerosene
        self.emeblk_eksrs = self._add_float_array(
            self.pyfiler.emeblk.eksrs, output=True
        )
        self.emeblk_ekscm = self._add_float_array(
            self.pyfiler.emeblk.ekscm, output=True
        )
        self.emeblk_eksin = self._add_float_array(
            self.pyfiler.emeblk.eksin, output=True
        )

        # Liquid petroleum gases
        self.emeblk_elgrs = self._add_float_array(
            self.pyfiler.emeblk.elgrs, output=True
        )
        self.emeblk_elgcm = self._add_float_array(
            self.pyfiler.emeblk.elgcm, output=True
        )
        self.emeblk_elgtr = self._add_float_array(
            self.pyfiler.emeblk.elgtr, output=True
        )
        self.emeblk_elgin = self._add_float_array(
            self.pyfiler.emeblk.elgin, output=True
        )

        # Residual fuel, low sulfur
        self.emeblk_erlcm = self._add_float_array(
            self.pyfiler.emeblk.erlcm, output=True
        )
        self.emeblk_erltr = self._add_float_array(
            self.pyfiler.emeblk.erltr, output=True
        )
        self.emeblk_erlin = self._add_float_array(
            self.pyfiler.emeblk.erlin, output=True
        )
        self.emeblk_erlel = self._add_float_array(
            self.pyfiler.emeblk.erlel, output=True
        )

        # Residual fuel, high sulfur
        self.emeblk_erhtr = self._add_float_array(
            self.pyfiler.emeblk.erhtr, output=True
        )
        self.emeblk_erhel = self._add_float_array(
            self.pyfiler.emeblk.erhel, output=True
        )

        # Residual fuel
        self.emeblk_erscm = self._add_float_array(
            self.pyfiler.emeblk.erscm, output=True
        )
        self.emeblk_erstr = self._add_float_array(
            self.pyfiler.emeblk.erstr, output=True
        )
        self.emeblk_ersin = self._add_float_array(
            self.pyfiler.emeblk.ersin, output=True
        )
        self.emeblk_ersel = self._add_float_array(
            self.pyfiler.emeblk.ersel, output=True
        )

        # Other petroleum
        self.emeblk_eottr = self._add_float_array(
            self.pyfiler.emeblk.eottr, output=True
        )
        self.emeblk_epfin = self._add_float_array(
            self.pyfiler.emeblk.epfin, output=True
        )
        self.emeblk_epcin = self._add_float_array(
            self.pyfiler.emeblk.epcin, output=True
        )
        self.emeblk_enqlgpf = self._add_float_array(
            self.pyfiler.emeblk.enqlgpf, output=True
        )
        self.emeblk_esgin = self._add_float_array(
            self.pyfiler.emeblk.esgin, output=True
        )
        self.emeblk_eotin = self._add_float_array(
            self.pyfiler.emeblk.eotin, output=True
        )
        self.emeblk_epcel = self._add_float_array(
            self.pyfiler.emeblk.epcel, output=True
        )

        # Alcohols
        self.emeblk_emetr = self._add_float_array(
            self.pyfiler.emeblk.emetr, output=True
        )
        self.emeblk_eettr = self._add_float_array(
            self.pyfiler.emeblk.eettr, output=True
        )

        # Municipal solid waste
        self.emeblk_emsel = self._add_float_array(
            self.pyfiler.emeblk.emsel, output=True
        )

        # Other electric power natural gas
        self.emeblk_egfelgr = self._add_float_array(
            self.pyfiler.emeblk.egfelgr, output=True
        )
        self.emeblk_egielgr = self._add_float_array(
            self.pyfiler.emeblk.egielgr, output=True
        )
        self.emeblk_egcelgr = self._add_float_array(
            self.pyfiler.emeblk.egcelgr, output=True
        )

        # Coke (for net coal coke imports)
        self.emeblk_eciin = self._add_float_array(
            self.pyfiler.emeblk.eciin, output=True
        )

        # Biomass with sequestration for hydrogen
        self.emeblk_ebmhm = self._add_float_array(
            self.pyfiler.emeblk.ebmhm, output=True
        )

        # Hydrocarbon gas liquids, fuels and feedstocks
        self.emeblk_eprrs = self._add_float_array(
            self.pyfiler.emeblk.eprrs, output=True
        )
        self.emeblk_eprcm = self._add_float_array(
            self.pyfiler.emeblk.eprcm, output=True
        )
        self.emeblk_eprtr = self._add_float_array(
            self.pyfiler.emeblk.eprtr, output=True
        )
        self.emeblk_eetin = self._add_float_array(
            self.pyfiler.emeblk.eetin, output=True
        )
        self.emeblk_eprin = self._add_float_array(
            self.pyfiler.emeblk.eprin, output=True
        )
        self.emeblk_ebuin = self._add_float_array(
            self.pyfiler.emeblk.ebuin, output=True
        )
        self.emeblk_eisin = self._add_float_array(
            self.pyfiler.emeblk.eisin, output=True
        )
        self.emeblk_eetinpf = self._add_float_array(
            self.pyfiler.emeblk.eetinpf, output=True
        )
        self.emeblk_eprinpf = self._add_float_array(
            self.pyfiler.emeblk.eprinpf, output=True
        )
        self.emeblk_ebuinpf = self._add_float_array(
            self.pyfiler.emeblk.ebuinpf, output=True
        )
        self.emeblk_eisinpf = self._add_float_array(
            self.pyfiler.emeblk.eisinpf, output=True
        )
        self.emeblk_eppin = self._add_float_array(
            self.pyfiler.emeblk.eppin, output=True
        )
        self.emeblk_eppinpf = self._add_float_array(
            self.pyfiler.emeblk.eppinpf, output=True
        )

        # Lubricants
        self.emeblk_eluin = self._add_float_array(
            self.pyfiler.emeblk.eluin, output=True
        )

        # Mercury emission factors for oil and gas
        # NOTE: these four variables are not in the restart file
        self.emeblk_hgdsel = self._add_float(
            self.pyfiler.hgeblk.hgdsel, output=True
        )
        self.emeblk_hgrlel = self._add_float(
            self.pyfiler.hgeblk.hgrlel, output=True
        )
        self.emeblk_hgrhel = self._add_float(
            self.pyfiler.hgeblk.hgrhel, output=True
        )
        self.emeblk_hgngel = self._add_float(
            self.pyfiler.hgeblk.hgngel, output=True
        )

    def _include_emission(self) -> None:
        """Create instance variables for the emission include file.

        This include file contains a variety of emissions-related variables.
        """
        # Constant parameters
        self.emission_mx_hg_grp = self._add_int(self.pyfiler.utils.mx_hg_grp)
        self.emission_epm_hg_cls = self._add_int(self.pyfiler.utils.epm_hg_cls)
        self.emission_epm_rank = self._add_int(self.pyfiler.utils.epm_rank)

        # Mercury cap and trade groups
        self.emission_num_hg_grp = self._add_int(
            self.pyfiler.emission.num_hg_grp, output=True
        )
        self.emission_hg_grp = self._add_int_array(
            self.pyfiler.emission.hg_grp, output=True
        )
        self.emission_emel_qhg = self._add_float_array(
            self.pyfiler.emission.emel_qhg, output=True
        )

        # Emissions indexed by pollutant
        self.emission_emrs = self._add_float_array(
            self.pyfiler.emission.emrs, output=True
        )
        self.emission_emrsc = self._add_float_array(
            self.pyfiler.emission.emrsc, output=True
        )
        self.emission_emcm = self._add_float_array(
            self.pyfiler.emission.emcm, output=True
        )
        self.emission_emcmc = self._add_float_array(
            self.pyfiler.emission.emcmc, output=True
        )
        self.emission_eminc = self._add_float_array(
            self.pyfiler.emission.eminc, output=True
        )
        self.emission_emincc = self._add_float_array(
            self.pyfiler.emission.emincc, output=True
        )
        self.emission_emtr = self._add_float_array(
            self.pyfiler.emission.emtr, output=True
        )
        self.emission_emtrc = self._add_float_array(
            self.pyfiler.emission.emtrc, output=True
        )
        self.emission_emnt = self._add_float_array(
            self.pyfiler.emission.emnt, output=True
        )
        self.emission_emel = self._add_float_array(
            self.pyfiler.emission.emel, output=True
        )
        self.emission_emelc = self._add_float_array(
            self.pyfiler.emission.emelc, output=True
        )

        # National emissions
        self.emission_emcarbon = self._add_float_array(
            self.pyfiler.emission.emcarbon, output=True
        )

        # Excise (consumption) tax by fuel
        self.emission_emetax = self._add_float_array(
            self.pyfiler.emission.emetax, output=True
        )

        # Emissions constraints and revenues
        self.emission_emlim = self._add_float_array(
            self.pyfiler.emission.emlim, output=True
        )
        self.emission_emrev = self._add_float_array(
            self.pyfiler.emission.emrev, output=True
        )

        # Methane
        self.emission_emmethane = self._add_float_array(
            self.pyfiler.emission.emmethane, output=True
        )

        # Emission level solved for
        self.emission_emsol = self._add_float_array(
            self.pyfiler.emission.emsol, output=True
        )

        # Mercury
        self.emission_hg_grams_mwh = self._add_float_array(
            self.pyfiler.emission.hg_grams_mwh, output=True
        )
        self.emission_hg_output = self._add_float_array(
            self.pyfiler.emission.hg_output, output=True
        )
        self.emission_hg_input = self._add_float_array(
            self.pyfiler.emission.hg_input, output=True
        )
        self.emission_hg_mef = self._add_float_array(
            self.pyfiler.emission.hg_mef, output=True
        )
        self.emission_em_auction_sh = self._add_float_array(
            self.pyfiler.emission.em_auction_sh, output=True
        )

        # Hydrogen emissions by fuel and pollutant
        self.emission_emhm = self._add_float_array(
            self.pyfiler.emission.emhm, output=True
        )

        # Required Hg MEF if no CAMR
        self.emission_hg_mefnc = self._add_float_array(
            self.pyfiler.emission.hg_mefnc, output=True
        )

        # Refinery carbon capture
        self.emission_ccs_pmm = self._add_float_array(
            self.pyfiler.emission.ccs_pmm
        )

        # Cap on Hg credit price
        self.emission_ucap_hg = self._add_float(
            self.pyfiler.emission.ucap_hg, output=True
        )

        # Switch to turn off CAMR
        self.emission_usw_camr = self._add_int(
            self.pyfiler.emission.usw_camr, output=True
        )

    def _include_emoblk(self) -> None:
        """Create instance variables for the emoblk include file.

        This include file contains switches and policy variables for EPM.
        """
        # Debug switch for EPM
        self.emoblk_dbugepm = self._add_int(
            self.pyfiler.emoblk.dbugepm, output=True
        )

        # Nominal or 1987 dollars
        self.emoblk_nominal_flag = self._add_int(
            self.pyfiler.emoblk.nominal_flag, output=True
        )

        # Emissions policy switches
        self.emoblk_tax_flag = self._add_int(
            self.pyfiler.emoblk.tax_flag, output=True
        )
        self.emoblk_permit_flag = self._add_int(
            self.pyfiler.emoblk.permit_flag, output=True
        )
        self.emoblk_market_flag = self._add_int(
            self.pyfiler.emoblk.market_flag, output=True
        )
        self.emoblk_offset_flag = self._add_int(
            self.pyfiler.emoblk.offset_flag, output=True
        )
        self.emoblk_etax_flag = self._add_int(
            self.pyfiler.emoblk.etax_flag, output=True
        )
        self.emoblk_tran_flag = self._add_int(
            self.pyfiler.emoblk.tran_flag, output=True
        )
        self.emoblk_elec_flag = self._add_int(
            self.pyfiler.emoblk.elec_flag, output=True
        )
        self.emoblk_resd_flag = self._add_int(
            self.pyfiler.emoblk.resd_flag, output=True
        )
        self.emoblk_comm_flag = self._add_int(
            self.pyfiler.emoblk.comm_flag, output=True
        )
        self.emoblk_bank_flag = self._add_int(
            self.pyfiler.emoblk.bank_flag, output=True
        )

        # Emissions policy constraints by year
        self.emoblk_emtax = self._add_float_array(
            self.pyfiler.emoblk.emtax, output=True
        )
        self.emoblk_total_emissions = self._add_float_array(
            self.pyfiler.emoblk.total_emissions, output=True
        )
        self.emoblk_emissions_goal = self._add_float_array(
            self.pyfiler.emoblk.emissions_goal, output=True
        )
        self.emoblk_max_tax = self._add_float_array(
            self.pyfiler.emoblk.max_tax, output=True
        )
        self.emoblk_min_tax = self._add_float_array(
            self.pyfiler.emoblk.min_tax, output=True
        )
        self.emoblk_init_alloc = self._add_float_array(
            self.pyfiler.emoblk.init_alloc
        )

        # Coal regions and census divisions
        self.emoblk_cl_cdnum = self._add_int_array(
            self.pyfiler.emoblk.cl_cdnum, output=True
        )
        self.emoblk_cl_cdmap = self._add_int_array(
            self.pyfiler.emoblk.cl_cdmap, output=True
        )

    def _include_epmbank(self) -> None:
        """Create instance variables for the epmbank include file.

        This include file contains variables for carbon permit pricing.
        """
        # Sum of pollutants at bracket points
        self.epmbank_low_sum = self._add_float(
            self.pyfiler.epmbank.low_sum, output=True
        )
        self.epmbank_high_sum = self._add_float(
            self.pyfiler.epmbank.high_sum, output=True
        )

        # Tax rates at bracket points
        self.epmbank_low_tax = self._add_float(
            self.pyfiler.epmbank.low_tax, output=True
        )
        self.epmbank_high_tax = self._add_float(
            self.pyfiler.epmbank.high_tax, output=True
        )

        # Line slope for regula falsi calculation
        self.epmbank_slope = self._add_float(
            self.pyfiler.epmbank.slope, output=True
        )

        # Iteration count for regfalsibank function
        self.epmbank_iter = self._add_int(
            self.pyfiler.epmbank.iter, output=True
        )

        # Flag for whether the root is bracketed
        self.epmbank_bracket = self._add_int(
            self.pyfiler.epmbank.bracket, output=True
        )

        # Year parameters for banking policy
        self.epmbank_bank_priceyr = self._add_int(
            self.pyfiler.epmbank.bank_priceyr, output=True
        )
        self.epmbank_bank_startyr = self._add_int(
            self.pyfiler.epmbank.bank_startyr, output=True
        )
        self.epmbank_bank_endyr = self._add_int(
            self.pyfiler.epmbank.bank_endyr, output=True
        )

        # Saved values of new_sum and new_tax by iteration for regfalsibank
        # function
        self.epmbank_newsum = self._add_float_array(
            self.pyfiler.epmbank.newsum, output=True
        )
        self.epmbank_newtax = self._add_float_array(
            self.pyfiler.epmbank.newtax, output=True
        )

        # Banked balance and emissions offsets
        self.epmbank_balance = self._add_float_array(
            self.pyfiler.epmbank.balance, output=True
        )
        self.epmbank_offset = self._add_float_array(
            self.pyfiler.epmbank.offset, output=True
        )

    def _include_ghgrep(self) -> None:
        """Create instance variables for the ghgrep include file.

        This include file contains the primary emissions arrays and variables
        for other greenhouse gases.
        """
        # Constant parameters
        self.ghgrep_ghg_nmax = self._add_int(self.pyfiler.utils.ghg_nmax)
        self.ghgrep_ghg_stmax = self._add_int(self.pyfiler.utils.ghg_stmax)

        # Offsets and other greenhouse gases
        self.ghgrep_ghg_ncat = self._add_int(
            self.pyfiler.ghgrep.ghg_ncat, output=True
        )
        self.ghgrep_ghg_bl = self._add_float_array(
            self.pyfiler.ghgrep.ghg_bl, output=True
        )
        self.ghgrep_ghg_oghg = self._add_float_array(
            self.pyfiler.ghgrep.ghg_oghg, output=True
        )
        self.ghgrep_ghg_abate = self._add_float_array(
            self.pyfiler.ghgrep.ghg_abate, output=True
        )
        self.ghgrep_ghg_macq = self._add_float_array(
            self.pyfiler.ghgrep.ghg_macq, output=True
        )
        self.ghgrep_ghg_macp = self._add_float_array(
            self.pyfiler.ghgrep.ghg_macp, output=True
        )
        self.ghgrep_ghg_steps = self._add_int_array(
            self.pyfiler.ghgrep.ghg_steps, output=True
        )
        self.ghgrep_ghg_class = self._add_int_array(
            self.pyfiler.ghgrep.ghg_class, output=True
        )
        self.ghgrep_ghg_offsetp = self._add_float_array(
            self.pyfiler.ghgrep.ghg_offsetp, output=True
        )
        self.ghgrep_ghg_offsetpint = self._add_float_array(
            self.pyfiler.ghgrep.ghg_offsetpint, output=True
        )
        self.ghgrep_ghg_rev = self._add_float_array(
            self.pyfiler.ghgrep.ghg_rev, output=True
        )
        self.ghgrep_banking = self._add_float_array(
            self.pyfiler.ghgrep.banking, output=True
        )
        self.ghgrep_mac_priceyr = self._add_int(
            self.pyfiler.ghgrep.mac_priceyr, output=True
        )

        # Constant parameters
        self.ghgrep_iel_r = self._add_int(self.pyfiler.utils.iel_r)
        self.ghgrep_iel_c = self._add_int(self.pyfiler.utils.iel_c)
        self.ghgrep_iel_i = self._add_int(self.pyfiler.utils.iel_i)
        self.ghgrep_iel_t = self._add_int(self.pyfiler.utils.iel_t)
        self.ghgrep_etot = self._add_int(self.pyfiler.utils.etot)
        self.ghgrep_endcalyr = self._add_int(self.pyfiler.utils.endcalyr)

        # Carbon dioxide emissions by fuel, region, and year for each sector
        self.ghgrep_em_resd = self._add_float_array(
            self.pyfiler.regco2.em_resd, output=True
        )
        self.ghgrep_em_comm = self._add_float_array(
            self.pyfiler.regco2.em_comm, output=True
        )
        self.ghgrep_em_indy = self._add_float_array(
            self.pyfiler.regco2.em_indy, output=True
        )
        self.ghgrep_em_tran = self._add_float_array(
            self.pyfiler.regco2.em_tran, output=True
        )
        self.ghgrep_em_elec = self._add_float_array(
            self.pyfiler.regco2.em_elec, output=True
        )

        # Corresponding fuel consumption associated with carbon dioxide
        # emissions in em_* arrays above
        self.ghgrep_fl_resd = self._add_float_array(
            self.pyfiler.regco2.fl_resd, output=True
        )
        self.ghgrep_fl_comm = self._add_float_array(
            self.pyfiler.regco2.fl_comm, output=True
        )
        self.ghgrep_fl_indy = self._add_float_array(
            self.pyfiler.regco2.fl_indy, output=True
        )
        self.ghgrep_fl_tran = self._add_float_array(
            self.pyfiler.regco2.fl_tran, output=True
        )
        self.ghgrep_fl_elec = self._add_float_array(
            self.pyfiler.regco2.fl_elec, output=True
        )

        # Carbon capture arrays by fuel for some sectors
        self.ghgrep_cc_indy = self._add_float_array(
            self.pyfiler.regco2.cc_indy, output=True
        )
        self.ghgrep_cc_elec = self._add_float_array(
            self.pyfiler.regco2.cc_elec, output=True
        )

    def _include_hmmblk(self) -> None:
        """Create instance variables for the hmmblk include file.

        This include file contains variables pertaining to the Hydrogen Market
        Module (HMM).
        """
        # Natural gas consumption for heat and power
        self.hmmblk_qnghmhp = self._add_float_array(
            self.pyfiler.hmmblk.qnghmhp
        )

        # Natural gas consumption for petrochemical feedstocks
        self.hmmblk_qnghmpf = self._add_float_array(
            self.pyfiler.hmmblk.qnghmpf
        )

    def _include_indepm(self) -> None:
        """Create instance variables for the indepm include file.

        This include file contains CO2 process emissions that are passed from
        IDM to EPM.
        """
        # Clinker process CO2 emissions in thousand metric tons CO2, by year
        # and region
        self.indepm_co2_clink = self._add_float_array(
            self.pyfiler.indepm.co2_clink
        )

        # Lime production process CO2 emissions in thousand metric tons CO2, by
        # year and region
        self.indepm_co2_lime = self._add_float_array(
            self.pyfiler.indepm.co2_lime
        )

    def _include_indout(self) -> None:
        """Create instance variables for the indout include file.

        This include file contains outputs from IDM.
        """
        # Consumption of natural gas feedstocks
        self.indout_inqngpf = self._add_float_array(
            self.pyfiler.indout.inqngpf
        )

        # Consumption of liquid petroleum gas feedstocks
        self.indout_inqlgpf = self._add_float_array(
            self.pyfiler.indout.inqlgpf
        )

    def _include_indrep(self) -> None:
        """Create instance variables for the indrep include file.

        This include file contains reporting variables for IDM.
        """
        # Number of industrial census regions
        self.indrep_mindcr = self._add_int(self.pyfiler.utils.mindcr)

        # Cement industry energy consumption by fuel
        self.indrep_cementcon = self._add_float_array(
            self.pyfiler.indrep.cementcon
        )

        # Industrial process emissions by industry
        self.indrep_ghg_processin = self._add_float_array(
            self.pyfiler.indrep.ghg_processin
        )

    def _include_lfmmout(self) -> None:
        """Create instance variables for the lfmmout include file.

        This include file contains refinery outputs from LFMM.
        """
        # Renewable diesel to to distillate Mbbl per day by PADD level
        self.lfmmout_grd2dsqty = self._add_float_array(
            self.pyfiler.lfmmout.grd2dsqty
        )

        # Renewable diesel to gasoline Mbbl per day by PADD level
        self.lfmmout_grn2mgqty = self._add_float_array(
            self.pyfiler.lfmmout.grn2mgqty
        )

        # Production of biobutenol by census division
        self.lfmmout_rfbiobutecd = self._add_float_array(
            self.pyfiler.lfmmout.rfbiobutecd
        )

        # Portion of California jet fuel covered by AB32 (intrastate use)
        self.lfmmout_ab32jetcover = self._add_float_array(
            self.pyfiler.lfmmout.ab32jetcover
        )

    def _include_macout(self) -> None:
        """Create instance variables for the macout include file.

        This include file contains outputs from MAM.
        """
        # Chained price index--gross domestic product; BEA; index- 2012=100.0
        # NOTE: index range in Fortran for this array is (-2:MNUMYR)
        self.macout_mc_jpgdp = self._add_float_array(
            self.pyfiler.macout.mc_jpgdp
        )

    def _include_mpblk(self) -> None:
        """Create instance variables for the mpblk include file.

        This include file contains energy prices.
        """
        # Equivalence array
        self.mpblk_mprc = self._add_float_array(self.pyfiler.utils.mprc)

    def _include_ngtdmrep(self) -> None:
        """Create instance variables for the ngtdmrep include file.

        This include file contains NG T & D (NGTDM) report writer variables.
        """
        # Total renewable natural gas production
        self.ngtdmrep_ogprrng = self._add_float_array(
            self.pyfiler.ngtdmrep.ogprrng
        )

    def _include_ogsmout(self) -> None:
        """Create instance variables for the ogsmout include file.

        This include file contains output variables from the oil & gas supply
        module, now HSM.
        """
        # CO2 emitted by natural gas processing plants (metric tons CO2)
        self.ogsmout_ngpco2em = self._add_float_array(
            self.pyfiler.ogsmout.ngpco2em
        )

    def _include_pmmftab(self) -> None:
        """Create instance variables for the pmmftab include file.

        This include file contains Ftab variables from the refinery module.
        """
        # Pyrolysis liquid blended into diesel, Mbbl/cd
        self.pmmftab_ubavolds = self._add_float_array(
            self.pyfiler.pmmftab.ubavolds
        )

        # Pyrolysis liquid blended into mogas, Mbbl/cd
        self.pmmftab_ubavolmg = self._add_float_array(
            self.pyfiler.pmmftab.ubavolmg
        )

    def _include_pmmout(self) -> None:
        """Create instance variables for the pmmout include file.

        This include file contains output variables from the refinery module.
        """
        # Percent ethanol in E85 (as frac)
        self.pmmout_ethne85 = self._add_float(self.pyfiler.pmmout.ethne85)

        # Percent traditional motor gasoline (TRG) in E85 (as frac)
        self.pmmout_trgne85 = self._add_float(self.pyfiler.pmmout.trgne85)

        # Quantity BTL liquid component produced by type in mmbd
        self.pmmout_btlfrac = self._add_float_array(
            self.pyfiler.pmmout.btlfrac
        )

        # Liquids produced from coal/biomass combo plant
        self.pmmout_cbtlfrac = self._add_float_array(
            self.pyfiler.pmmout.cbtlfrac
        )

    def _include_pmmrpt(self) -> None:
        """Create instance variables for the pmmrpt include file.

        This include file contains output variables from the refinery module.
        """
        # Ethanol imports and exports
        self.pmmrpt_ethimp = self._add_float_array(self.pyfiler.pmmrpt.ethimp)
        self.pmmrpt_ethexp = self._add_float_array(self.pyfiler.pmmrpt.ethexp)

        # Biodiesel imports and exports
        self.pmmrpt_biodimp = self._add_float_array(
            self.pyfiler.pmmrpt.biodimp
        )
        self.pmmrpt_biodexp = self._add_float_array(
            self.pyfiler.pmmrpt.biodexp
        )

        # Ethanol production from various sources
        self.pmmrpt_crnethcd = self._add_float_array(
            self.pyfiler.pmmrpt.crnethcd
        )
        self.pmmrpt_cllethcd = self._add_float_array(
            self.pyfiler.pmmrpt.cllethcd
        )
        self.pmmrpt_othethcd = self._add_float_array(
            self.pyfiler.pmmrpt.othethcd
        )

        # Quantity biodiesel produced by type in mbcd
        self.pmmrpt_bimqtycd = self._add_float_array(
            self.pyfiler.pmmrpt.bimqtycd
        )

    def _include_qblk(self) -> None:
        """Create instance variables for the qblk include file.

        This include file contains total energy consumption quantities.
        """
        # Purchased electricity
        # NOTE: the fact that EPM outputs to qelas might be a bug
        self.qblk_qelrs = self._add_float_array(self.pyfiler.qblk.qelrs)
        self.qblk_qelcm = self._add_float_array(self.pyfiler.qblk.qelcm)
        self.qblk_qeltr = self._add_float_array(self.pyfiler.qblk.qeltr)
        self.qblk_qelin = self._add_float_array(self.pyfiler.qblk.qelin)
        self.qblk_qelas = self._add_float_array(
            self.pyfiler.qblk.qelas, output=True
        )

        # Natural gas, core
        self.qblk_qgfrs = self._add_float_array(self.pyfiler.qblk.qgfrs)
        self.qblk_qgfcm = self._add_float_array(self.pyfiler.qblk.qgfcm)
        self.qblk_qgftr = self._add_float_array(self.pyfiler.qblk.qgftr)

        # Natural gas, noncore
        self.qblk_qgirs = self._add_float_array(self.pyfiler.qblk.qgirs)
        self.qblk_qgicm = self._add_float_array(self.pyfiler.qblk.qgicm)
        self.qblk_qgitr = self._add_float_array(self.pyfiler.qblk.qgitr)

        # Natural gas
        self.qblk_qngrs = self._add_float_array(self.pyfiler.qblk.qngrs)
        self.qblk_qngcm = self._add_float_array(self.pyfiler.qblk.qngcm)
        self.qblk_qngtr = self._add_float_array(self.pyfiler.qblk.qngtr)
        self.qblk_qngin = self._add_float_array(self.pyfiler.qblk.qngin)
        self.qblk_qngel = self._add_float_array(self.pyfiler.qblk.qngel)
        self.qblk_qnghm = self._add_float_array(self.pyfiler.qblk.qnghm)

        # Other natural gas
        self.qblk_qgptr = self._add_float_array(self.pyfiler.qblk.qgptr)
        self.qblk_qlpin = self._add_float_array(self.pyfiler.qblk.qlpin)

        # Coal
        self.qblk_qclrs = self._add_float_array(self.pyfiler.qblk.qclrs)
        self.qblk_qclcm = self._add_float_array(self.pyfiler.qblk.qclcm)
        self.qblk_qclin = self._add_float_array(self.pyfiler.qblk.qclin)
        self.qblk_qclel = self._add_float_array(self.pyfiler.qblk.qclel)

        # Metallurgical coal
        self.qblk_qmcin = self._add_float_array(self.pyfiler.qblk.qmcin)

        # Motor gasoline
        self.qblk_qmgcm = self._add_float_array(self.pyfiler.qblk.qmgcm)
        self.qblk_qmgtr = self._add_float_array(self.pyfiler.qblk.qmgtr)
        self.qblk_qmgin = self._add_float_array(self.pyfiler.qblk.qmgin)

        # Jet fuel
        self.qblk_qjftr = self._add_float_array(self.pyfiler.qblk.qjftr)

        # Distillate
        self.qblk_qdsrs = self._add_float_array(self.pyfiler.qblk.qdsrs)
        self.qblk_qdscm = self._add_float_array(self.pyfiler.qblk.qdscm)
        self.qblk_qdstr = self._add_float_array(self.pyfiler.qblk.qdstr)
        self.qblk_qdsin = self._add_float_array(self.pyfiler.qblk.qdsin)
        self.qblk_qdsel = self._add_float_array(self.pyfiler.qblk.qdsel)

        # Kerosene
        self.qblk_qksrs = self._add_float_array(self.pyfiler.qblk.qksrs)
        self.qblk_qkscm = self._add_float_array(self.pyfiler.qblk.qkscm)
        self.qblk_qksin = self._add_float_array(self.pyfiler.qblk.qksin)

        # Liquid petroleum gases
        self.qblk_qlgrs = self._add_float_array(self.pyfiler.qblk.qlgrs)
        self.qblk_qlgcm = self._add_float_array(self.pyfiler.qblk.qlgcm)
        self.qblk_qlgtr = self._add_float_array(self.pyfiler.qblk.qlgtr)
        self.qblk_qlgin = self._add_float_array(self.pyfiler.qblk.qlgin)

        # Residual fuel, low sulfur
        self.qblk_qrlcm = self._add_float_array(self.pyfiler.qblk.qrlcm)
        self.qblk_qrltr = self._add_float_array(self.pyfiler.qblk.qrltr)
        self.qblk_qrlin = self._add_float_array(self.pyfiler.qblk.qrlin)
        self.qblk_qrlel = self._add_float_array(self.pyfiler.qblk.qrlel)

        # Residual fuel, high sulfur
        self.qblk_qrhtr = self._add_float_array(self.pyfiler.qblk.qrhtr)
        self.qblk_qrhel = self._add_float_array(self.pyfiler.qblk.qrhel)

        # Residual fuel
        self.qblk_qrscm = self._add_float_array(self.pyfiler.qblk.qrscm)
        self.qblk_qrstr = self._add_float_array(self.pyfiler.qblk.qrstr)
        self.qblk_qrsin = self._add_float_array(self.pyfiler.qblk.qrsin)
        self.qblk_qrsel = self._add_float_array(self.pyfiler.qblk.qrsel)

        # Petrochemical feedstocks
        self.qblk_qpfin = self._add_float_array(self.pyfiler.qblk.qpfin)

        # Still gas
        self.qblk_qsgin = self._add_float_array(self.pyfiler.qblk.qsgin)

        # Petroleum coke
        self.qblk_qpcin = self._add_float_array(self.pyfiler.qblk.qpcin)
        self.qblk_qpcel = self._add_float_array(self.pyfiler.qblk.qpcel)

        # Other petroleum
        self.qblk_qottr = self._add_float_array(self.pyfiler.qblk.qottr)
        self.qblk_qotin = self._add_float_array(self.pyfiler.qblk.qotin)

        # Alcohols
        self.qblk_qmetr = self._add_float_array(self.pyfiler.qblk.qmetr)
        self.qblk_qettr = self._add_float_array(self.pyfiler.qblk.qettr)

        # Geothermal
        self.qblk_qgeel = self._add_float_array(self.pyfiler.qblk.qgeel)

        # Biomass
        self.qblk_qbmel = self._add_float_array(self.pyfiler.qblk.qbmel)

        # Net coal coke imports
        self.qblk_qciin = self._add_float_array(self.pyfiler.qblk.qciin)

        # Hydrocarbon gas liquid fuel
        self.qblk_qprrs = self._add_float_array(self.pyfiler.qmore.qprrs)
        self.qblk_qprcm = self._add_float_array(self.pyfiler.qmore.qprcm)
        self.qblk_qppin = self._add_float_array(self.pyfiler.qmore.qppin)
        self.qblk_qetin = self._add_float_array(self.pyfiler.qmore.qetin)
        self.qblk_qbuin = self._add_float_array(self.pyfiler.qmore.qbuin)
        self.qblk_qprin = self._add_float_array(self.pyfiler.qmore.qprin)
        self.qblk_qisin = self._add_float_array(self.pyfiler.qmore.qisin)
        self.qblk_qprtr = self._add_float_array(self.pyfiler.qmore.qprtr)

        # Hydrocarbon gas liquid feedstock
        self.qblk_qprolenerf = self._add_float_array(
            self.pyfiler.qmore.qprolenerf
        )
        self.qblk_qppinpf = self._add_float_array(self.pyfiler.qmore.qppinpf)
        self.qblk_qetinpf = self._add_float_array(self.pyfiler.qmore.qetinpf)
        self.qblk_qbuinpf = self._add_float_array(self.pyfiler.qmore.qbuinpf)
        self.qblk_qprinpf = self._add_float_array(self.pyfiler.qmore.qprinpf)
        self.qblk_qisinpf = self._add_float_array(self.pyfiler.qmore.qisinpf)

        # Natural gas used for liquefaction
        self.qblk_qnglq = self._add_float_array(self.pyfiler.qmore.qnglq)

    def _include_qsblk(self) -> None:
        """Create instance variables for the qsblk include file.

        This include file contains energy consumption quantities from SEDS.
        """
        # Purchased electricity
        self.qsblk_qselrs = self._add_float_array(self.pyfiler.qsblk.qselrs)
        self.qsblk_qselcm = self._add_float_array(self.pyfiler.qsblk.qselcm)
        self.qsblk_qseltr = self._add_float_array(self.pyfiler.qsblk.qseltr)
        self.qsblk_qselin = self._add_float_array(self.pyfiler.qsblk.qselin)

        # Natural gas
        self.qsblk_qsngrs = self._add_float_array(self.pyfiler.qsblk.qsngrs)
        self.qsblk_qsngcm = self._add_float_array(self.pyfiler.qsblk.qsngcm)
        self.qsblk_qsngtr = self._add_float_array(self.pyfiler.qsblk.qsngtr)
        self.qsblk_qsngin = self._add_float_array(self.pyfiler.qsblk.qsngin)
        self.qsblk_qsngel = self._add_float_array(self.pyfiler.qsblk.qsngel)

        # Other natural gas
        self.qsblk_qsgptr = self._add_float_array(self.pyfiler.qsblk.qsgptr)
        self.qsblk_qslpin = self._add_float_array(self.pyfiler.qsblk.qslpin)

        # Coal
        self.qsblk_qsclrs = self._add_float_array(self.pyfiler.qsblk.qsclrs)
        self.qsblk_qsclcm = self._add_float_array(self.pyfiler.qsblk.qsclcm)
        self.qsblk_qsclin = self._add_float_array(self.pyfiler.qsblk.qsclin)
        self.qsblk_qsclel = self._add_float_array(self.pyfiler.qsblk.qsclel)

        # Metallurgical coal
        self.qsblk_qsmcin = self._add_float_array(self.pyfiler.qsblk.qsmcin)

        # Motor gasoline
        self.qsblk_qsmgcm = self._add_float_array(self.pyfiler.qsblk.qsmgcm)
        self.qsblk_qsmgtr = self._add_float_array(self.pyfiler.qsblk.qsmgtr)
        self.qsblk_qsmgin = self._add_float_array(self.pyfiler.qsblk.qsmgin)

        # Jet fuel
        self.qsblk_qsjftr = self._add_float_array(self.pyfiler.qsblk.qsjftr)

        # Distillate
        self.qsblk_qsdsrs = self._add_float_array(self.pyfiler.qsblk.qsdsrs)
        self.qsblk_qsdscm = self._add_float_array(self.pyfiler.qsblk.qsdscm)
        self.qsblk_qsdstr = self._add_float_array(self.pyfiler.qsblk.qsdstr)
        self.qsblk_qsdsin = self._add_float_array(self.pyfiler.qsblk.qsdsin)
        self.qsblk_qsdsel = self._add_float_array(self.pyfiler.qsblk.qsdsel)

        # Kerosene
        self.qsblk_qsksrs = self._add_float_array(self.pyfiler.qsblk.qsksrs)
        self.qsblk_qskscm = self._add_float_array(self.pyfiler.qsblk.qskscm)
        self.qsblk_qsksin = self._add_float_array(self.pyfiler.qsblk.qsksin)

        # Liquid petroleum gases
        self.qsblk_qslgrs = self._add_float_array(self.pyfiler.qsblk.qslgrs)
        self.qsblk_qslgcm = self._add_float_array(self.pyfiler.qsblk.qslgcm)
        self.qsblk_qslgtr = self._add_float_array(self.pyfiler.qsblk.qslgtr)
        self.qsblk_qslgin = self._add_float_array(self.pyfiler.qsblk.qslgin)

        # Residual fuel
        self.qsblk_qsrscm = self._add_float_array(self.pyfiler.qsblk.qsrscm)
        self.qsblk_qsrstr = self._add_float_array(self.pyfiler.qsblk.qsrstr)
        self.qsblk_qsrsin = self._add_float_array(self.pyfiler.qsblk.qsrsin)
        self.qsblk_qsrsel = self._add_float_array(self.pyfiler.qsblk.qsrsel)

        # Petrochemical feedstocks
        self.qsblk_qspfin = self._add_float_array(self.pyfiler.qsblk.qspfin)

        # Still gas
        self.qsblk_qssgin = self._add_float_array(self.pyfiler.qsblk.qssgin)

        # Petroleum coke
        self.qsblk_qspcin = self._add_float_array(self.pyfiler.qsblk.qspcin)
        self.qsblk_qspcel = self._add_float_array(self.pyfiler.qsblk.qspcel)

        # Other petroleum
        self.qsblk_qsottr = self._add_float_array(self.pyfiler.qsblk.qsottr)
        self.qsblk_qsotin = self._add_float_array(self.pyfiler.qsblk.qsotin)

        # Methanol
        self.qsblk_qsmetr = self._add_float_array(self.pyfiler.qsblk.qsmetr)

        # Geothermal
        self.qsblk_qsgeel = self._add_float_array(self.pyfiler.qsblk.qsgeel)

        # Biomass
        self.qsblk_qsbmel = self._add_float_array(self.pyfiler.qsblk.qsbmel)

        # Net coal coke imports
        self.qsblk_qsciin = self._add_float_array(self.pyfiler.qsblk.qsciin)

    def _include_tranrep(self) -> None:
        """Create instance variables for the tranrep include file.

        This include file contains reporting variables for TDM.
        """
        # Electricity consumption for EMM
        self.tranrep_trq_elec = self._add_float_array(
            self.pyfiler.tranrep.trq_elec
        )

    def _include_uefdout(self) -> None:
        """Create instance variables for the uefdout include file.

        This include file contains outputs from EFD in EMM.
        """
        # Geothermal generation by ownership type/NERC
        self.uefdout_ugngenr = self._add_float_array(
            self.pyfiler.uefdout.ugngenr
        )

        # Carbon removal rates for oil
        self.uefdout_xdsel = self._add_float_array(self.pyfiler.uefdout.xdsel)
        self.uefdout_xrlel = self._add_float_array(self.pyfiler.uefdout.xrlel)
        self.uefdout_xrhel = self._add_float_array(self.pyfiler.uefdout.xrhel)

        # Natural gas consumption
        self.uefdout_qgfelgr = self._add_float_array(
            self.pyfiler.uefdout.qgfelgr
        )
        self.uefdout_qgielgr = self._add_float_array(
            self.pyfiler.uefdout.qgielgr
        )
        self.uefdout_qgcelgr = self._add_float_array(
            self.pyfiler.uefdout.qgcelgr
        )

        # Carbon removal rates for natural gas
        self.uefdout_xqgfelgr = self._add_float_array(
            self.pyfiler.uefdout.xqgfelgr
        )
        self.uefdout_xqgielgr = self._add_float_array(
            self.pyfiler.uefdout.xqgielgr
        )
        self.uefdout_xqgcelgr = self._add_float_array(
            self.pyfiler.uefdout.xqgcelgr
        )

    def _include_wrenew(self) -> None:
        """Create instance variables for the wrenew include file.

        This include file contains renewables variables.
        """
        # Utility municipal solid waste (MSW) non-biogenic consumption to be
        # subtracted from MSW consumption, in quads, by census region
        self.wrenew_wncmsel = self._add_float_array(
            self.pyfiler.wrenew.wncmsel
        )
