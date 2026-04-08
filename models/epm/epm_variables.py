"""Handling of static variables that EPM expects will be saved between calls.

The small amount of code in this module exists only to create the collection of
variables and initialize them to sensible values.
"""

from typing import Any

import numpy as np
import numpy.typing as npt

from epm_common import BASE_YR
from epm_restart import Restart


class Variables:
    """Stores intermediate variables that need to be saved between EPM runs.

    All stored EPM variables are directly accessible as instance attributes.
    """

    def __init__(self, restart: Restart) -> None:
        """Create a new collection of initialized variables.

        The individual variables are initialized to zero, False, or some other
        "null" value of the appropriate type.

        Parameters
        ----------
        restart : Restart
            The currently loaded restart file data.
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
