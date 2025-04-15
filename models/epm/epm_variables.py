"""Handling of static variables that EPM expects will be saved between calls.

Before EPM runs, read in the variables from a pickle file or just initialize
them to sensible values if no pickle file exists. After EPM runs, write the
variables to a pickle file for the next run.
"""

from pathlib import Path
import pickle
from typing import Any, Final

import numpy as np
import numpy.typing as npt

from epm_common import BASE_YR, get_output_path
from epm_restart import Restart


# Base file name to use for the saved variables pickle file
VARIABLES_FILE_NAME: Final[str] = "EPM_saved_variables.pkl"


class Variables:
    """Stores intermediate variables that need to be saved between EPM runs.

    All stored EPM variables are accessible as instance attributes. When Python
    is not running, the variables are stored in a dictionary object written to
    a temporary pickle file.
    """

    def __init__(self, restart: Restart) -> None:
        """Create a new collection of initialized variables.

        Most variables are initialized to zero, None, or some other "null"
        value with the appropriate type.
        """
        # Contents of EPM control file
        self.control: dict[str, Any] = {}

        # Common block epm_out
        self.epm_out_bank_onyr: int = 0

        # Common block ccsblk
        # Electric power sector carbon captured and stored by 0=distillate,
        # 1=residual, 2=coal, 3=ngas, 4=beccs, and 5=total. Currently not
        # used in any routine besides sum_emissions, but could be used in
        # accntrev.
        self.ccsblk_ccs: npt.NDArray[np.float64] = np.zeros(
            (6, restart.parametr_mnumnr), np.float64
        )

        # Historical carbon emissions by 4 fuels, 5 sectors, year
        # Fuels: 0=ngas, 1=oil, 2=coal, 3=msw/geo
        # Sectors: 0=resd, 1=comm, 2=indy, 3=tran, 4=elec
        # MSW/geothermal fuel index is only used for electric power sector
        # Values are in millions of metric tons of carbon
        self.history: npt.NDArray[np.float64] = np.zeros(
            (4, 5, BASE_YR), np.float64
        )

        # Common block epmoth
        # Target allowance balance in bank_endyr with cap&trade banking
        self.epmoth_bank_end_balance: float = 0.0

        # Subroutine epm
        # Local variable with carbon penalty
        self.epm_new_tax: float = 0.0
        self.epm_e_goal: float = 0.0
        self.epm_bank: npt.NDArray[np.float64] = np.zeros(
            restart.parametr_mnumyr, np.float64
        )
        # Position 0 is beginning tax; 1 is ending tax; write this out at end
        self.epm_begin_and_end: npt.NDArray[np.float64] = np.zeros(
            (2, restart.parametr_mnumyr), np.float64
        )

        # Subroutine regfalsi
        self.regfalsi_low_sum: float = 0.0  # Lower sum of pollutants
        self.regfalsi_high_sum: float = 0.0  # Higher sum of pollutants
        # Greater tax rate for lower sum of pollutants
        self.regfalsi_low_tax: float = 0.0
        # Lesser tax rate for higher sum of pollutants
        self.regfalsi_high_tax: float = 0.0
        # Store new_sum, new_tax by iteration
        self.regfalsi_newsum: npt.NDArray[np.float64] = np.zeros(
            11, np.float64
        )
        self.regfalsi_newtax: npt.NDArray[np.float64] = np.zeros(
            11, np.float64
        )
        # Do we have two points bracketing zero (note we subtracted the goal)
        self.regfalsi_bracket: bool = False

        # Subroutine accntrev
        self.accntrev_bank_local: npt.NDArray[np.float64] = np.zeros(
            restart.parametr_mnumyr, np.float64
        )
        self.accntrev_balance_local: npt.NDArray[np.float64] = np.zeros(
            restart.parametr_mnumyr, np.float64
        )

        # Subroutine oghg
        # error count, by max # of categories.
        self.oghg_err_count: npt.NDArray[np.int64] = np.zeros(
            restart.ghgrep_ghg_nmax, np.int64
        )

    def _get_file_path(self) -> Path:
        """Construct the file path to use for the save and load methods.

        Returns
        -------
        Path
            The full path to the intermediate variables pickle file.
        """
        return get_output_path() / VARIABLES_FILE_NAME

    def load(self) -> None:
        """Load the intermediate variables from a pickle file if one exists."""
        try:
            with self._get_file_path().open("rb") as f:
                pickled_dict: dict[str, Any] = pickle.load(f)
        except FileNotFoundError:
            return
        self.__dict__.update(pickled_dict)

    def save(self) -> None:
        """Save the intermediate variables out to a pickle file."""
        with self._get_file_path().open("wb") as f:
            pickle.dump(self.__dict__, f)
