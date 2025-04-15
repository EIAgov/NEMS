"""Constant values and general utility functions for EPM."""

from datetime import datetime
import inspect
from pathlib import Path
import sys
from typing import Any, Final


BASE_YR: Final[int] = 34
"""Legacy `base_yr` constant for use in the `sum_emissions`, `oghg`, and
`accntrev` functions.

It is the NEMS year index for the last year of history overwrites. When reading
history in EPMDATA, 2023 is currently the last data year.

See Also
--------
EPM_READ_HIST : Same value for the `epm_read` function.
"""

EPM_READ_HIST: Final[int] = 34
"""Legacy `hist` constant for use in the `epm_read` function.

It is the number of years of emissions factor data, and indicates that we
should read the historical CO2 data through NEMS year 34 (2023).

See Also
--------
BASE_YR : Same value for several other functions in EPM.
"""

REGFALSI_EPM_FACTOR: Final[float] = 3.0
"""Legacy `epm_factor` constant used by the `regfalsi` function.

This is a mutliplicative factor to branch outward by when trying to get the
bracketing points.

See Also
--------
REGFALSI_TOL : Tolerance for the `regfalsi` function.
"""

REGFALSI_TOL: Final[float] = 1.0
"""Legacy `tol` constant used by the `regfalsi` function.

This is our tolerance on reaching the pollution goal, so that the function
knows when it has found the root to the desired level of accuracy.

See Also
--------
REGFALSI_EPM_FACTOR : Bracketing factor for the `regfalsi` function.
"""

REGFALSIBANK_CTB: Final[float] = 0.0
"""Legacy `ctb` constant used by the `regfalsibank` function.

In the rebracketing request logic, if the last two emissions totals were bigger
than this value, adjust the allowance price by slope rather than brackets.

See Also
--------
REGFALSIBANK_EPM_FACTOR : Bracketing factor for the `regfalsibank` function.
REGFALSIBANK_TOL : Tolerance for the `regfalsibank` function.
"""

REGFALSIBANK_EPM_FACTOR: Final[float] = 0.01
"""Legacy `epm_factor` constant used by the `regfalsibank` function.

This is a mutliplicative factor to branch outward by when trying to get the
bracketing points.

See Also
--------
REGFALSIBANK_CTB : Adjustment threshold for the `regfalsibank` function.
REGFALSIBANK_TOL : Tolerance for the `regfalsibank` function.
"""

REGFALSIBANK_TOL: Final[float] = 5.0
"""Legacy `tol` constant used by the `regfalsibank` function.

This is our tolerance on reaching the long-term banking goal, so that the
function knows when it has found the root to the desired level of accuracy.

See Also
--------
REGFALSIBANK_CTB : Adjustment threshold for the `regfalsibank` function.
REGFALSIBANK_EPM_FACTOR : Bracketing factor for the `regfalsibank` function.
"""


# Private global variables to store the state of EPM's logger
_log_file_path: Path | None = None
_log_verbosity: int | None = None


def set_log_file_path(file_path: Path) -> None:
    """Set the path where EPM should save its EPMOUT log file.

    An empty file will be created with the specified path, overwriting the file
    if it already exists.

    Parameters
    ----------
    file_path : Path
        The log file path to use.

    See Also
    --------
    set_log_verbosity : Set the logging verbosity level.
    log_it : Write a message to the log file.
    """
    global _log_file_path

    # Write an empty log file. This both ensures the path is valid and prevents
    # a bug. When EPM runs in standalone configuration, the desired behavior is
    # to overwrite any existing log files from previous standalone runs instead
    # of appending the current run's messages to the end of those old files.
    with file_path.open("w", encoding="utf-8") as _:
        pass

    _log_file_path = file_path


def set_log_verbosity(verbosity: int) -> None:
    """Set the verbosity level EPM should use when logging to the EPMOUT file.

    The verbosity level corresponds to the `dbugepm` parameter in the EPM
    control file. It determines which messages (if any) get written to the
    EPMOUT log file.

    Parameters
    ----------
    verbosity : int
        The desired verbosity level. There are three settings: 0 turns logging
        off, 1 turns logging on, and 2 enables additional verbose logging.

    See Also
    --------
    set_log_file_path : Set the path to the log file.
    log_it : Write a message to the log file.
    """
    global _log_verbosity
    verbosity_options = [
        0,  # Logging switched off
        1,  # Logging switched on
        2,  # Lots of logging output
    ]
    min_verbosity = min(verbosity_options)
    max_verbosity = max(verbosity_options)
    verbosity = max(verbosity, min_verbosity)
    verbosity = min(verbosity, max_verbosity)
    _log_verbosity = verbosity


def log_it(
    *message_parts: Any,
    sep: str = "\n",
    verbose: bool = False,
) -> None:
    """Write a timestamped log message to the EPMOUT log file.

    The provided parts of the message are converted to strings and then joined
    together with a separator before being written to the log file. Messages
    will only be written if logging is switched on, and messages flagged as
    "verbose" will only be written if the verbosity level is set to maximum.

    Before ever calling this function, ensure that both the log file path and
    the logging verbosity have been set using their associated functions.

    Parameters
    ----------
    message_parts : Any
        Zero or more arguments that will be joined together to form the log
        message. These objects should be convertable to strings.
    sep : str, optional
        The joining string to use between the message parts. Newlines are used
        by default.
    verbose : bool, optional
        If set to True, this message will be flagged as "verbose" and not
        written unless the logging verbosity level is set to maximum. Defaults
        to False.

    Raises
    ------
    RuntimeError
        If this function is called before setting the log file path and
        verbosity level.

    See Also
    --------
    set_log_file_path : Set the path to the log file.
    set_log_verbosity : Set the logging verbosity level.
    """
    global _log_file_path, _log_verbosity
    if _log_verbosity is None:
        raise RuntimeError("tried to write log without setting log verbosity")
    if _log_verbosity == 0 or (_log_verbosity == 1 and verbose):
        return
    if _log_file_path is None:
        raise RuntimeError("tried to write log without setting log file path")

    message = sep.join(str(part) for part in message_parts)

    # Use Python's inspect module to peek at the call stack and determine the
    # name of the file that this function is being called from. Also pull the
    # name of the calling function.
    caller_frame = inspect.stack()[1]
    caller_file = Path(caller_frame.filename).name
    caller_function = caller_frame.function

    verbosity_label = "VERBOSE" if verbose else "NORMAL"
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    with _log_file_path.open("a", encoding="utf-8") as f:
        f.write(f"[{timestamp}]")
        f.write(f"[{caller_file}]")
        f.write(f"[{caller_function}]")
        f.write(f"[{verbosity_label}]")
        f.write("\n")
        f.write(message)
        f.write("\n\n")


def print_it(cycle: int, message: str) -> None:
    """A standardized way to print an important message to stdout.

    In integrated runs, this will be captured by the nohup.out file.

    Parameters
    ----------
    cycle : int
        The current NEMS cycle number, also known as `curirun`.
    message : str
        The message to be printed.
    """
    # Use Python's inspect module to peek at the call stack and determine the
    # name of the file that this function is being called from
    caller_frame = inspect.stack()[1]
    caller_file = Path(caller_frame.filename).name

    print(
        f"{datetime.now()} :: cycle {cycle} :: {caller_file} :: {message}",
        end="\n\n"
    )
    sys.stdout.flush()
    sys.stderr.flush()


def discover_run_configuration() -> tuple[Path, bool]:
    """Infer the location of the EPM code and whether it is running integrated.

    This implementation uses the expected directory structures for integrated
    and standalone EPM runs. It looks for a file named `run_epm.py` and returns
    the path to the directory containing that file. Depending on where the file
    was found, we can determine whether EPM is running integrated or
    standalone.

    Returns
    -------
    Path
        Full path to the directory containing the EPM code.
    bool
        Whether EPM is running integrated (True) or standalone (False).

    Raises
    ------
    RuntimeError
        If the function is unable to definitively classify the EPM run
        configuration.
    """
    # For an integrated run, Path.cwd() will be either the top-level output
    # folder (in a serial/jognems run) or one of the p1/p2/p3 parition folders
    # (in a parallel/parnems run). The EPM code will be in an `epm` subfolder.
    integrated_epm_path = Path.cwd() / "epm"
    integrated_epm_py_file = integrated_epm_path / "run_epm.py"
    integrated_check = integrated_epm_py_file.is_file()

    # For a standalone run, the EPM code will be executed from inside its own
    # folder, so Path.cwd() will be the folder containing the code.
    standalone_epm_path = Path.cwd()
    standalone_epm_py_file = standalone_epm_path / "run_epm.py"
    standalone_check = standalone_epm_py_file.is_file()

    if integrated_check and not standalone_check:
        return integrated_epm_path, True  # True ==> integrated
    if standalone_check and not integrated_check:
        return standalone_epm_path, False  # False ==> standalone

    # Neither check file was found or both of them were found
    raise RuntimeError(
        "cannot determine whether EPM is running integrated or standalone"
    )


def running_integrated() -> bool:
    """Determine whether EPM is running integrated with the rest of NEMS.

    This is equivalent to calling the `discover_run_configuration` function and
    discarding the path it returns.

    Returns
    -------
    bool
        True if EPM is running integrated with NEMS and False if EPM is running
        standalone.
    """
    _, integrated = discover_run_configuration()
    return integrated


def get_epm_path() -> Path:
    """Get the path to the main EPM code directory for this run.

    This is equivalent to calling the `discover_run_configuration` function and
    discarding the boolean flag it returns.

    Returns
    -------
    Path
        Full path to the main EPM code directory.
    """
    epm_path, _ = discover_run_configuration()
    return epm_path


def get_input_path() -> Path:
    """Get the path to the EPM input directory for this run.

    Returns
    -------
    Path
        Full path to the EPM input directory.
    """
    return get_epm_path() / "input"


def get_output_path() -> Path:
    """Get the path to the directory where EPM should save its output files.

    Returns
    -------
    Path
        Full path to the EPM output directory.
    """
    return get_epm_path() / "output"


def incorporate_cycle_number(file_path: Path, cycle: int) -> Path:
    """Change the given file path to include the current NEMS cycle number.

    The cycle number is inserted with a '.' immediately before the file
    extension, so a file path of `C:/folder/example.txt` would be returned as
    `C:/folder/example.3.txt` for the third cycle.

    Parameters
    ----------
    file_path : Path
        Original file path without cycle number incorporated.
    cycle : int
        The current NEMS cycle number, also known as `curirun`.

    Returns
    -------
    Path
        The new path including the cycle number.
    """
    new_base_name = f"{file_path.stem}.{cycle}{file_path.suffix}"
    return file_path.parent / new_base_name
