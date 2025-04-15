"""Primary interface for running EPM with its supporting Python framework.

The only required argument (`mode`) specifies whether to run the `epm` function
only ("main"), the `epm_read` function only ("read"), both the `epm_read` and
`epm` functions in sequence ("both"), or neither function ("none"). In every
mode, the restart file object, scedes file object, and intermediate variables
object will all be initialized to start and cleaned up afterward.
"""

import argparse
from enum import StrEnum
import time
from types import ModuleType

from epm_common import print_it, running_integrated
from epm_core import epm
from epm_read import epm_read
from epm_restart import Restart
from epm_scedes import Scedes
from epm_variables import Variables


class Mode(StrEnum):
    """Constants for each EPM mode that can be specified via command line.

    When running the Emissions Policy Module, this string enum communicates
    which code should be executed. The available options are to run only the
    core `epm` routine ("main"), only the `epm_read` routine ("read"), both
    routines in sequence ("both"), or neither routine ("none").

    The surrounding framework code for the scedes file, restart file, and EPM
    intermediate variables are always executed.
    """
    MAIN = "main"
    READ = "read"
    BOTH = "both"
    NONE = "none"


def get_args() -> argparse.Namespace:
    """Get the command line arguments and parse them with an arg parser.

    The only required command line argument is `mode`, which specifies whether
    we should run `epm` only, `epm_read` only, both functions, or neither. The
    optional arguments `cycles`, `years`, and `iters` allow for overriding the
    NEMS control variables `numiruns`, `lastyr` (as well as `lastcalyr`), and
    `maxitr` instead of using the values from the input restart file.

    Returns
    -------
    argparse.Namespace
        Namespace containing parsed command line arguments.
    """
    parser = argparse.ArgumentParser(
        description=(
            "Run the NEMS Emissions Policy Module in standalone configuration"
        ),
    )

    modes = [mode.value for mode in Mode]
    parser.add_argument(
        "mode", type=Mode,
        help=f"which part of EPM to run: {', '.join(modes)}",
    )

    parser.add_argument(
        "-c", "--cycles", default=None, type=int,
        help="overrides restart file value of numiruns",
    )
    parser.add_argument(
        "-y", "--years", default=None, type=int,
        help="overrides restart file value of lastyr",
    )
    parser.add_argument(
        "-i", "--iters", default=None, type=int,
        help="overrides restart file value of maxitr",
    )

    return parser.parse_args()


class Arguments:
    """A simple structure to store and validate the external arguments."""

    def __init__(
        self,
        mode: Mode,
        *,
        cycles: int | None = None,
        years: int | None = None,
        iters: int | None = None,
    ) -> None:
        """Create a new arguments object with the given argument values.

        Parameters
        ----------
        mode : Mode
            String enum specifying which of the primary EPM routines to run.
            See the Mode type for more details.
        cycles : int | None, optional
            If not None, this overrides the restart file value of `numiruns`.
        years : int | None, optional
            If not None, this overrides the restart file value of `lastyr`. The
            value of `lastcalyr` is also overridden appropriately.
        iters : int | None, optional
            If not None, this overrides the restart file value of `maxitr`.

        See Also
        --------
        Mode : StrEnum subtype for the `mode` argument to this function.
        """
        self.mode = Mode(mode)
        self.cycles = None if cycles is None else int(cycles)
        self.years = None if years is None else int(years)
        self.iters = None if iters is None else int(iters)

    def validate(self, restart: Restart) -> None:
        """Use the restart file data to validate the external arguments to EPM.

        Parameters
        ----------
        restart : Restart
            The restart file object, which should already be loaded with data
            from the input restart file.

        Raises
        ------
        ValueError
            If the `cycles` argument is less than 1.
        ValueError
            If the `years` argument is less than 1 or greater than `mnumyr`.
        ValueError
            If the `iters` argument is less than 1.
        """
        if self.cycles is not None and self.cycles < 1:
            raise ValueError(f"invalid value for cycles arg: {self.cycles!r}")

        if (
            self.years is not None
            and not (1 <= self.years <= restart.parametr_mnumyr)
        ):
            raise ValueError(f"invalid value for years arg: {self.years!r}")

        if self.iters is not None and self.iters < 1:
            raise ValueError(f"invalid value for iters arg: {self.iters!r}")


def print_run_message(
    args: Arguments,
    restart: Restart,
    *,
    integrated: bool
) -> None:
    """Print a run message including the mode and some restart variables.

    Parameters
    ----------
    args : Arguments
        Structure containing the external arguments sent to the EPM code.
    restart : Restart
        The loaded restart file data object.
    integrated : bool
        Whether EPM is running in integrated (True) or standalone (False)
        configuration.
    """
    epm_config_parts = [
        "epm",
        ("integrated" if integrated else "standalone"),
        "configuration",
    ]
    epm_config = " ".join(epm_config_parts).upper()

    epm_args_parts = [
        f"mode={args.mode.value}",
        f"curcalyr={restart.ncntrl_curcalyr}",
        f"curitr={restart.ncntrl_curitr}",
        f"fcrl={restart.ncntrl_fcrl}",
        f"ncrl={restart.ncntrl_ncrl}",
    ]
    epm_args = " ".join(epm_args_parts)

    print_it(restart.ncntrl_curirun, " ".join([epm_config, epm_args]))


def run_epm_integrated(
    args: Arguments,
    restart: Restart,
    scedes: Scedes,
    variables: Variables
) -> None:
    """Run the core EPM code once in integrated NEMS run configuration.

    Parameters
    ----------
    args : Arguments
        Structure containing the external arguments sent to the EPM code.
    restart : Restart
        The restart file object, which should already be loaded with data from
        the input restart file.
    scedes : Scedes
        The scedes dict object, which should already be loaded with key-value
        pairs from the keys.sed file.
    variables : Variables
        The intermediate variables for EPM, which should already be loaded from
        the pickle file if the file exists.
    """
    print_run_message(args, restart, integrated=True)

    # Warn if any of the override args for standalone configuration are set
    if args.cycles is not None:
        print_it(
            restart.ncntrl_curirun,
            "WARNING: cycles arg is ignored when running integrated"
        )
    if args.years is not None:
        print_it(
            restart.ncntrl_curirun,
            "WARNING: years arg is ignored when running integrated"
        )
    if args.iters is not None:
        print_it(
            restart.ncntrl_curirun,
            "WARNING: iters arg is ignored when running integrated"
        )

    # Run the core EPM code once
    if args.mode in (Mode.READ, Mode.BOTH):
        epm_read(restart, scedes, variables)
    if args.mode in (Mode.MAIN, Mode.BOTH):
        epm(restart, scedes, variables)


def run_epm_standalone(
    args: Arguments,
    restart: Restart,
    scedes: Scedes,
    variables: Variables
) -> None:
    """Run the core EPM code through all loops in standalone configuration.

    Parameters
    ----------
    args : Arguments
        Structure containing the external arguments sent to the EPM code.
    restart : Restart
        The restart file object, which should already be loaded with data from
        the input restart file.
    scedes : Scedes
        The scedes dict object, which should already be loaded with key-value
        pairs from the keys.sed file.
    variables : Variables
        The intermediate variables for EPM, which should already be loaded from
        the pickle file if the file exists.
    """
    if args.cycles is not None:
        restart.ncntrl_numiruns = args.cycles
    if args.years is not None:
        restart.ncntrl_lastyr = args.years
        restart.ncntrl_lastcalyr = restart.parametr_baseyr + args.years - 1
        restart.ncntrl_ijumpyr = restart.ncntrl_lastyr  # Just in case
        restart.ncntrl_ijumpcalyr = restart.ncntrl_lastcalyr  # Just in case
    if args.iters is not None:
        restart.ncntrl_maxitr = args.iters

    # Loop through cycles and set the restart variable each time
    for cycle_index in range(1, restart.ncntrl_numiruns + 1):
        restart.ncntrl_curirun = cycle_index

        # Make sure epm_read is called at most once per cycle
        epm_read_calls = 0

        # Loop through year indices and set the restart variables each time
        for year_index in range(1, restart.ncntrl_lastyr + 1):
            restart.ncntrl_curiyr = year_index
            restart.ncntrl_curcalyr = restart.parametr_baseyr + year_index - 1

            # Loop through iterations and set the restart variable each time.
            # We loop maxitr + 2 times to allow for the fcrl and ncrl loops.
            for iter_index in range(1, restart.ncntrl_maxitr + 3):
                restart.ncntrl_curitr = iter_index

                # Set fcrl and ncrl based on the current iteration
                restart.ncntrl_fcrl = 0
                restart.ncntrl_ncrl = 0
                if iter_index > restart.ncntrl_maxitr:
                    restart.ncntrl_fcrl = 1
                if iter_index > restart.ncntrl_maxitr + 1:
                    restart.ncntrl_ncrl = 1

                timer_before = time.perf_counter()
                print_run_message(args, restart, integrated=False)

                # Run the core EPM code with the current control values
                if args.mode in (Mode.READ, Mode.BOTH) and epm_read_calls < 1:
                    epm_read(restart, scedes, variables)
                    epm_read_calls += 1
                if args.mode in (Mode.MAIN, Mode.BOTH):
                    epm(restart, scedes, variables)

                timer_after = time.perf_counter()
                print_it(
                    restart.ncntrl_curirun,
                    f"Runtime: {timer_after - timer_before:.6f} sec"
                )


def run_epm(
    mode: Mode,
    pyfiler: ModuleType | None = None,
    *,
    cycles: int | None = None,
    years: int | None = None,
    iters: int | None = None,
) -> None:
    """Primary interface function for executing the NEMS EPM code as a whole.

    When this file is executed as a command line script, it calls this function
    and runs EPM standalone, relying on PyFiler internally for restart file IO.
    For integrated NEMS runs, this function can be imported and directly passed
    a PyFiler module object as input. When running integrated, the `cycles`,
    `years`, and `iters` arguments are ignored.

    Parameters
    ----------
    mode : Mode
        String enum specifying which of the primary EPM routines to run. See
        the Mode type for more details.
    pyfiler : ModuleType | None, optional
        A pre-initialized PyFiler module object to use for restart file data.
        This is used for integrated NEMS runs and should be set to the default
        value of None for standalone runs when EPM will need to manage PyFiler
        on its own.
    cycles : int | None, optional
        If not None, this overrides the restart file value of `numiruns`. The
        default is to use the restart file value.
    years : int | None, optional
        If not None, this overrides the restart file value of `lastyr`. The
        value of `lastcalyr` is also overridden appropriately. The default is
        to use the restart file values.
    iters : int | None, optional
        If not None, this overrides the restart file value of `maxitr`. The
        default is to use the restart file value.

    See Also
    --------
    Mode : StrEnum subtype for the first argument to this function.
    """
    timer_at_start = time.perf_counter()

    # Create an internal structure to store the external arguments to EPM
    args = Arguments(mode, cycles=cycles, years=years, iters=iters)

    # Set up framework data structures before any core EPM code runs
    restart = Restart(pyfiler)
    restart.read_file()
    scedes = Scedes()
    scedes.read_file()
    variables = Variables(restart)
    variables.load()

    # Make the external arguments pass some basic validation checks
    args.validate(restart)

    # For standalone runs, report the time it took to set up (almost entirely
    # due to loading the restart file)
    timer_before_call = time.perf_counter()
    if not running_integrated():
        print_it(
            restart.ncntrl_curirun,
            f"Time to set up: {timer_before_call - timer_at_start:.6f} sec"
        )

    # Run the core EPM code differently for integrated vs standalone runs
    if running_integrated():
        run_epm_integrated(args, restart, scedes, variables)
    else:
        run_epm_standalone(args, restart, scedes, variables)

    # For standalone runs, report the total time spent looping through the core
    # EPM routines
    timer_after_call = time.perf_counter()
    if not running_integrated():
        print_it(
            restart.ncntrl_curirun,
            f"Time for loops: {timer_after_call - timer_before_call:.6f} sec"
        )

    # Handle cleanup and save the data after the core EPM code runs
    restart.write_file()
    variables.save()

    # For standalone runs, report the time spent cleaning up (writing restart
    # and variables files to disk). For both run configurations, report the
    # total EPM runtime before returning from this function.
    timer_at_end = time.perf_counter()
    if not running_integrated():
        print_it(
            restart.ncntrl_curirun,
            f"Time to clean up: {timer_at_end - timer_after_call:.6f} sec"
        )
    print_it(
        restart.ncntrl_curirun,
        f"EPM runtime: {timer_at_end - timer_at_start:.6f} sec"
    )


# This file is the primary entry point for running EPM. If it is run directly
# from the command line, then start up a standalone module run.
if __name__ == "__main__":
    command_line_args = get_args()
    run_epm(
        command_line_args.mode,
        cycles=command_line_args.cycles,
        years=command_line_args.years,
        iters=command_line_args.iters,
    )
