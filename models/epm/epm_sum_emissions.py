"""Routines for summing and reporting all United States emissions.

The contents of this file are the heart of EPM, implementing what was once the
single Fortran subroutine named `sum_emissions`. Here, we add up emissions
across all sectors of the economy, share emissions by region and electrical
power usage, handle historical benchmarking/overwrites, and report the totals.
We also read in the historical emissions from the EPMDATA input file.
"""

from pathlib import Path
from typing import TextIO

import numpy as np
import numpy.typing as npt
import pandas as pd
import tabulate

from epm_common import BASE_YR, log_it, print_it
from epm_other_ghg import oghg
from epm_read import get_file_path
from epm_restart import Restart
from epm_scedes import Scedes
from epm_variables import Variables


def sum_emissions(
    restart: Restart,
    scedes: Scedes,
    variables: Variables
) -> None:
    """Sum pollutants by fuel to give aggregate totals across the country.

    Emissions are determined by summing quantities of fuel use from qblk
    weighted by fuel-specific emissions factors from emeblk. For the current
    NEMS year, emissions are computed for each fuel, region, and sector and
    then stored in emission and ghgrep. The relevant list of fuels varies by
    sector, and biofuel emissions from the transportation sector are
    subtracted. Emissions from the electric power sector are shared out to each
    end-use sector based on sectors' consumption of purchased electricity.
    California is included as an extra region so that its emissions can be
    tracked for the NEMS implementation of AB32, but the California region
    requires some special treatment. Note: for total U.S. emissions, we add up
    ONLY the first 9 regions to avoid double counting (index 9 = California and
    10 = United States).

    The emissions are summed up following these steps in order:
        1. Read historical emissions data from input file
        2. Compute residential sector emissions
        3. Compute commercial sector emissions
        4. Compute industrial sector emissions
        5. Compute transportation sector emissions
        6. Compute electricity sector emissions
        7. Apportion regional emissions by fuel and sector
        8. Adjust covered CO2 emissions from California biofuels
        9. Benchmark national emissions to history in historical years
        10. Share electricity emissions to sectors and adjust totals
        11. Overwrite computed emissions with history in historical years
        12. Compute total emissions for use with cap-and-trade policies
        13. Compute any other GHG emissions by calling subroutine oghg
        14. Do reporting loop output with calls to subrotuine demand_co2

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    scedes : Scedes
        The current scedes file dict.
    variables : Variables
        The active intermediate variables for EPM.

    See Also
    --------
    epm : Core EPM routine that directly calls this function.
    get_file_path : Used to find the path to the emissions history file.
    read_history : Handles reading in the historical emissions data.
    sum_emissions_residential : Handles the residential sector emissions.
    sum_emissions_commercial : Handles the commercial sector emissions.
    sum_emissions_industrial : Handles the industrial sector emissions.
    sum_emissions_transportation : Handles the transportation sector emissions.
    sum_emissions_electricity : Handles the electric power sector emissions.
    sum_emissions_regional : Handles regional apportionment of emissions.
    sum_emissions_california_biofuels : Adjusts California biofuel emissions.
    sum_emissions_history_benchmark : Handles the historical benchmarking.
    sum_emissions_share_electricity : Shares electricity emissions to regions.
    sum_emissions_history_overwrite : Handles the historical overwrites.
    sum_emissions_policy_totals : Compute totals for cap-and-trade policies.
    oghg : Called by this function to handle other greenhouse gases.
    sum_emissions_reporting_loop : Handles the reporting loop output.
    """
    # Names of the 11 regions
    # (9 census divisions + California subregion + national total)
    regnam = [
        "New England",
        "Middle Atlantic",
        "East North Central",
        "West North Central",
        "South Atlantic",
        "East South Central",
        "West South Central",
        "Mountain",
        "Pacific",
        "California",
        "United States"
    ]

    j = restart.ncntrl_curiyr - 1  # Python arrays are zero-based
    jcalyr = restart.ncntrl_curcalyr

    # Read historical data from history input file
    if np.all(variables.history == 0.0):
        history_path = get_file_path(variables, "history")
        read_history(variables, history_path)

    # Coal - electric power emission factor
    # Values for eclel are not read in. Only used for tax calcs because
    # emissions factors for coal are specified on a coal rank/region basis
    # elsewhere.
    restart.emeblk_eclel[:] = 25.72  # GHG pub 2003 value 10/4/2004

    # Residential sector carbon emissions
    nm_resd = []
    qelrs_ev = sum_emissions_residential(restart, j, nm_resd)

    # Commercial sector carbon emissions
    nm_comm = []
    qelcm_ev = sum_emissions_commercial(restart, j, nm_comm)

    # Industrial sector carbon emissions
    nm_indy = []
    sum_emissions_industrial(restart, j, nm_indy)

    # Transportation sector carbon emissions
    nm_tran = []
    e85_ethanol_share, e85_gasoline_share = sum_emissions_transportation(
        restart, j, nm_tran, qelrs_ev, qelcm_ev
    )

    # Electricity sector carbon emissions
    nm_elec = []
    sum_emissions_electricity(restart, variables, j, jcalyr, nm_elec)

    # Calculate regional emissions for each fuel (for each sector)
    sum_emissions_regional(restart, j)

    # Adjust covered CO2 emissions from California biofuels
    sum_emissions_california_biofuels(
        restart, j, e85_ethanol_share, e85_gasoline_share
    )

    # In history, benchmark national modeled emissions to national history.
    if j < BASE_YR:
        sum_emissions_history_benchmark(restart, variables, j, iy=j)

    # Share electricity emissions to sectors and adjust totals
    sum_emissions_share_electricity(restart, j, qelrs_ev, qelcm_ev)

    # Overwrite computed emissions with history.
    if j < BASE_YR:
        sum_emissions_history_overwrite(restart, variables, j)

    # Work-around ngtdm overwrite of EMNT in its reporting loop.
    if 1 <= j <= BASE_YR:
        usa = restart.parametr_mnumcr - 1  # Zero-based region index
        restart.emission_emnt[usa, 0, j - 1] = (
            variables.history[0, 3, j - 1] * 1000.0
        )

    # Compute total_emissions[j] of CO2 for covered sectors for use in cap and
    # trade policy options (via regfalsi function).
    sum_emissions_policy_totals(restart, j)

    # Compute emissions from other GHGes in covered sectors, non covered
    # sectors based on marginal abatement and offsets.
    oghg(restart, scedes, variables)

    # Reporting loop output
    if restart.ncntrl_ncrl == 1 and j == restart.ncntrl_lastyr - 1:
        sum_emissions_reporting_loop(
            restart, variables,
            nm_resd, nm_comm, nm_indy, nm_tran, nm_elec, regnam
        )


def read_history(variables: Variables, file_path: Path) -> None:
    """Read in the historical emissions data for use by `sum_emissions`.

    The historical emissions data is stored in its own EPM input file. Not sure
    why this historical read happens as part of `sum_emissions` and not as part
    of `epm_read`... possibly because the history is stored in a local EPM
    variable instead of a NEMS include file common block. In the future, this
    should likely be moved to `epm_read`, possibly gated behind a
    `RUNEPM != 0` conditional statement.

    Parameters
    ----------
    variables : Variables
        The active intermediate variables for EPM.
    file_path : Path
        Path to the CSV file that this function should open.

    See Also
    --------
    sum_emissions : Calls this function to read historical emissions data.
    """
    variables.history[:, :, :] = 0.0

    data = pd.read_csv(file_path, index_col=["sector", "fuel"], comment='#')

    # The ordering of elements in the sectors and fuels lists here is used to
    # map to the indices in the variables.history array
    sectors = [
        "residential",
        "commercial",
        "industrial",
        "transportation",
        "electric power",
    ]
    fuels = ["natural gas", "petroleum", "coal", "msw/geothermal"]

    for sector_and_fuel in data.index:
        data_row = data.loc[sector_and_fuel]
        sector, fuel = sector_and_fuel
        i_sector, i_fuel = sectors.index(sector), fuels.index(fuel)
        variables.history[i_fuel, i_sector, :] = data_row


def sum_emissions_residential(
    restart: Restart,
    j: int,
    nm_resd: list[str]
) -> npt.NDArray[np.float64]:
    """Compute the initial residential sector emissions for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Compute emissions for this zero-based year index.
    nm_resd : list[str]
        List to store the names of the residential sector fuel indicies.

    Returns
    -------
    qelrs_ev : npt.NDArray[np.float64]
        Residential electricity consumption used to charge electric vehicles in
        year index `j` by region. This consumption is excluded from residential
        emissions and should be moved over to the transportation sector.

    See Also
    --------
    sum_emissions : Calls this function for residential sector emissions.
    sum_emissions_transportation : Requires the returned `qelrs_ev` value.
    sum_emissions_share_electricity : Requires the returned `qelrs_ev` value.
    """
    # Zero-based Python region indices
    usa = restart.parametr_mnumcr - 1
    cali = restart.parametr_mnumcr - 2
    pacific = restart.parametr_mnumcr - 3

    # Get the part of residential electricity consumption that's being used to
    # charge electric vehicles, since that needs to be subtracted out and moved
    # over to the transportation sector
    qelrs_ev = restart.tranrep_trq_elec[0, :, j]

    # Clear residential C emissions for all fuels
    # First index of emrs is fuel: 0=ngas, 1=oil, 2=coal, 3=?
    restart.emission_emrs[:, 0, j] = 0.0
    # Clear national residential C emissions
    restart.emission_emrsc[usa, 0, j] = 0.0

    # Sum residential C emissions across all regions
    mnumcr2 = restart.parametr_mnumcr - 2

    restart.emission_emrs[0, 0, j] += 0.001 * np.sum(
        # Residential C emissions from natural gas, core
        restart.qblk_qgfrs[:mnumcr2, j] * restart.emeblk_egfrs[j]
        # Residential C emissions from natural gas, noncore
        + restart.qblk_qgirs[:mnumcr2, j] * restart.emeblk_egirs[j]
    )

    restart.emission_emrs[2, 0, j] += 0.001 * np.sum(
        # Residential C emissions from coal
        restart.qblk_qclrs[:mnumcr2, j] * restart.emeblk_eclrs[j]
    )

    restart.emission_emrs[1, 0, j] += 0.001 * np.sum(
        # Residential C emissions from distillate
        restart.qblk_qdsrs[:mnumcr2, j] * restart.emeblk_edsrs[j]
        # Residential C emissions from kerosene
        + restart.qblk_qksrs[:mnumcr2, j] * restart.emeblk_eksrs[j]
        # Residential C emissions from propane
        + restart.qblk_qprrs[:mnumcr2, j] * restart.emeblk_eprrs[j]
    )

    # National residential C emissions summed across all fuels
    restart.emission_emrsc[usa, 0, j] = np.sum(restart.emission_emrs[:, 0, j])

    # National residential CO2 emissions from propane
    restart.ghgrep_em_resd[0, usa, j] = (
        restart.qblk_qprrs[usa, j] * restart.emeblk_eprrs[j]
    )

    # National residential CO2 emissions from distillate
    restart.ghgrep_em_resd[1, usa, j] = (
        restart.qblk_qdsrs[usa, j] * restart.emeblk_edsrs[j]
    )

    # National residential CO2 emissions from kerosene
    restart.ghgrep_em_resd[2, usa, j] = (
        restart.qblk_qksrs[usa, j] * restart.emeblk_eksrs[j]
    )

    # National residential CO2 emissions from coal
    restart.ghgrep_em_resd[3, usa, j] = (
        restart.qblk_qclrs[usa, j] * restart.emeblk_eclrs[j]
    )

    # National residential CO2 emissions from natural gas
    restart.ghgrep_em_resd[4, usa, j] = (
        restart.qblk_qngrs[usa, j] * restart.emeblk_engrs[j]
    )

    # Regional residential CO2 emissions from purchased electricity (initially
    # just the sector share) (exclude charging of electric vehicles)
    zero = restart.qblk_qelas[:, j] == 0
    nonzero = np.logical_not(zero)
    restart.ghgrep_em_resd[5, nonzero, j] = (
        (restart.qblk_qelrs[nonzero, j] - qelrs_ev[nonzero])
        / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_resd[5, zero, j] = 0.0

    # Regional residential propane consumption
    restart.ghgrep_fl_resd[0, :, j] = restart.qblk_qprrs[:, j]

    # Regional residential distillate consumption
    restart.ghgrep_fl_resd[1, :, j] = restart.qblk_qdsrs[:, j]

    # Regional residential kerosene consumption
    restart.ghgrep_fl_resd[2, :, j] = restart.qblk_qksrs[:, j]

    # Regional residential coal consumption
    restart.ghgrep_fl_resd[3, :, j] = restart.qblk_qclrs[:, j]

    # Regional residential natural gas consumption
    restart.ghgrep_fl_resd[4, :, j] = restart.qblk_qngrs[:, j]

    # Regional residential purchased electricity
    restart.ghgrep_fl_resd[5, :, j] = restart.qblk_qelrs[:, j] - qelrs_ev

    # There are usually no California numbers until after msedyr, so estimate
    # them using the Pacific numbers scaled by appropriate ratios from SEDS
    if restart.ncntrl_curiyr <= restart.parametr_msedyr:

        # California residential propane consumption
        # For propane, use the SEDS LPG array instead of the propane array,
        # since residential LPG is only propane and the LPG history goes back
        # farther
        if restart.qsblk_qslgrs[pacific, j] != 0.0:
            restart.ghgrep_fl_resd[0, cali, j] = (
                restart.qsblk_qslgrs[cali, j] * restart.qblk_qlgrs[pacific, j]
                / restart.qsblk_qslgrs[pacific, j]
            )

        # California residential distillate consumption
        if restart.qsblk_qsdsrs[pacific, j] != 0.0:
            restart.ghgrep_fl_resd[1, cali, j] = (
                restart.qsblk_qsdsrs[cali, j] * restart.qblk_qdsrs[pacific, j]
                / restart.qsblk_qsdsrs[pacific, j]
            )

        # California residential kerosene consumption
        if restart.qsblk_qsksrs[pacific, j] != 0.0:
            restart.ghgrep_fl_resd[2, cali, j] = (
                restart.qsblk_qsksrs[cali, j] * restart.qblk_qksrs[pacific, j]
                / restart.qsblk_qsksrs[pacific, j]
            )

        # California residential coal consumption
        if restart.qsblk_qsclrs[pacific, j] != 0.0:
            restart.ghgrep_fl_resd[3, cali, j] = (
                restart.qsblk_qsclrs[cali, j] * restart.qblk_qclrs[pacific, j]
                / restart.qsblk_qsclrs[pacific, j]
            )

        # California residential natural gas consumption
        if restart.qsblk_qsngrs[pacific, j] != 0.0:
            restart.ghgrep_fl_resd[4, cali, j] = (
                restart.qsblk_qsngrs[cali, j] * restart.qblk_qngrs[pacific, j]
                / restart.qsblk_qsngrs[pacific, j]
            )

        # California residential purchased electricity (excluding charging of
        # electric vehicles)
        if restart.qsblk_qselrs[pacific, j] != 0.0:
            restart.ghgrep_fl_resd[5, cali, j] = (
                restart.qsblk_qselrs[cali, j]
                * (restart.qblk_qelrs[pacific, j] - qelrs_ev[pacific])
                / restart.qsblk_qselrs[pacific, j]
            )

    # Names of the residential sector fuels (plus electricity)
    nm_resd[:] = [
        "Propane",
        "Distillate",
        "Kerosene",
        "Coal",
        "Natural Gas",
        "Electricity",
    ]

    return qelrs_ev


def sum_emissions_commercial(
    restart: Restart,
    j: int,
    nm_comm: list[str]
) -> npt.NDArray[np.float64]:
    """Compute the initial commercial sector emissions for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Compute emissions for this zero-based year index.
    nm_comm : list[str]
        List to store the names of the commercial sector fuel indicies.

    Returns
    -------
    qelcm_ev : npt.NDArray[np.float64]
        Commercial electricity consumption used to charge electric vehicles in
        year index `j` by region. This consumption is excluded from commercial
        emissions and should be moved over to the transportation sector.

    See Also
    --------
    sum_emissions : Calls this function for commercial sector emissions.
    sum_emissions_transportation : Requires the returned `qelcm_ev` value.
    sum_emissions_share_electricity : Requires the returned `qelcm_ev` value.
    """
    # Zero-based Python region indices
    usa = restart.parametr_mnumcr - 1
    cali = restart.parametr_mnumcr - 2
    pacific = restart.parametr_mnumcr - 3

    # Get the part of commercial electricity consumption that's being used to
    # charge electric vehicles, since that needs to be subtracted out and moved
    # over to the transportation sector
    qelcm_ev = np.sum(restart.tranrep_trq_elec[1:9, :, j], axis=0)

    # Clear commercial C emissions for all fuels
    # First index of emcm is fuel: 0=ngas, 1=oil, 2=coal, 3=?, 4=?
    restart.emission_emcm[:4, 0, j] = 0.0
    # Clear national commercial C emissions
    restart.emission_emcmc[usa, 0, j] = 0.0

    # Sum commercial C emissions across all regions
    mnumcr2 = restart.parametr_mnumcr - 2

    restart.emission_emcm[0, 0, j] += 0.001 * np.sum(
        # Commercial C emissions from natural gas, core
        restart.qblk_qgfcm[:mnumcr2, j] * restart.emeblk_egfcm[j]
        # Commercial C emissions from natural gas, noncore
        + restart.qblk_qgicm[:mnumcr2, j] * restart.emeblk_egicm[j]
    )

    restart.emission_emcm[2, 0, j] += 0.001 * np.sum(
        # Commercial C emissions from coal
        restart.qblk_qclcm[:mnumcr2, j] * restart.emeblk_eclcm[j]
    )

    restart.emission_emcm[1, 0, j] += 0.001 * np.sum(
        # Commercial C emissions from motor gasoline
        restart.qblk_qmgcm[:mnumcr2, j] * restart.emeblk_emgcm[j]
        # Commercial C emissions from distillate
        + restart.qblk_qdscm[:mnumcr2, j] * restart.emeblk_edscm[j]
        # Commercial C emissions from kerosene
        + restart.qblk_qkscm[:mnumcr2, j] * restart.emeblk_ekscm[j]
        # Commercial C emissions from propane
        + restart.qblk_qprcm[:mnumcr2, j] * restart.emeblk_eprcm[j]
        # Commercial C emissions from residual fuel
        + restart.qblk_qrscm[:mnumcr2, j] * restart.emeblk_erscm[j]
    )

    # National commercial C emissions summed across all fuels
    restart.emission_emcmc[usa, 0, j] = np.sum(restart.emission_emcm[:3, 0, j])

    # National commerical CO2 emissions from motor gasoline
    restart.ghgrep_em_comm[0, usa, j] = (
        restart.qblk_qmgcm[usa, j] * restart.emeblk_emgcm[j]
    )

    # National commerical CO2 emissions from propane
    restart.ghgrep_em_comm[1, usa, j] = (
        restart.qblk_qprcm[usa, j] * restart.emeblk_eprcm[j]
    )

    # National commerical CO2 emissions from distillate
    restart.ghgrep_em_comm[2, usa, j] = (
        restart.qblk_qdscm[usa, j] * restart.emeblk_edscm[j]
    )

    # National commerical CO2 emissions from residual fuel
    restart.ghgrep_em_comm[3, usa, j] = (
        restart.qblk_qrscm[usa, j] * restart.emeblk_erscm[j]
    )

    # National commerical CO2 emissions from kerosene
    restart.ghgrep_em_comm[4, usa, j] = (
        restart.qblk_qkscm[usa, j] * restart.emeblk_ekscm[j]
    )

    # National commerical CO2 emissions from coal
    restart.ghgrep_em_comm[5, usa, j] = (
        restart.qblk_qclcm[usa, j] * restart.emeblk_eclcm[j]
    )

    # National commerical CO2 emissions from natural gas
    restart.ghgrep_em_comm[6, usa, j] = (
        restart.qblk_qngcm[usa, j] * restart.emeblk_engcm[j]
    )

    # Regional commercial CO2 emissions from purchased electricity (initially
    # just the sector share) (exclude charging of electric vehicles)
    zero = restart.qblk_qelas[:, j] == 0
    nonzero = np.logical_not(zero)
    restart.ghgrep_em_comm[7, nonzero, j] = (
        (restart.qblk_qelcm[nonzero, j] - qelcm_ev[nonzero])
        / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_comm[7, zero, j] = 0.0

    # Regional commercial motor gasoline consumption
    restart.ghgrep_fl_comm[0, :, j] = restart.qblk_qmgcm[:, j]

    # Regional commercial propane consumption
    restart.ghgrep_fl_comm[1, :, j] = restart.qblk_qprcm[:, j]

    # Regional commercial distillate consumption
    restart.ghgrep_fl_comm[2, :, j] = restart.qblk_qdscm[:, j]

    # Regional commercial residual fuel consumption
    restart.ghgrep_fl_comm[3, :, j] = restart.qblk_qrscm[:, j]

    # Regional commercial kerosene consumption
    restart.ghgrep_fl_comm[4, :, j] = restart.qblk_qkscm[:, j]

    # Regional commercial coal consumption
    restart.ghgrep_fl_comm[5, :, j] = restart.qblk_qclcm[:, j]

    # Regional commercial natural gas consumption
    restart.ghgrep_fl_comm[6, :, j] = restart.qblk_qngcm[:, j]

    # Regional commercial purchased electricity
    restart.ghgrep_fl_comm[7, :, j] = restart.qblk_qelcm[:, j] - qelcm_ev

    # There are usually no California numbers until after msedyr, so estimate
    # them using the Pacific numbers scaled by appropriate ratios from SEDS
    if restart.ncntrl_curiyr <= restart.parametr_msedyr:

        # California commercial motor gasoline consumption
        if restart.qsblk_qsmgcm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[0, cali, j] = (
                restart.qsblk_qsmgcm[cali, j] * restart.qblk_qmgcm[pacific, j]
                / restart.qsblk_qsmgcm[pacific, j]
            )

        # California commercial propane consumption
        # For propane, use the SEDS LPG array instead of the propane array,
        # since commercial LPG is only propane and the LPG history goes back
        # farther
        if restart.qsblk_qslgcm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[1, cali, j] = (
                restart.qsblk_qslgcm[cali, j] * restart.qblk_qlgcm[pacific, j]
                / restart.qsblk_qslgcm[pacific, j]
            )

        # California commercial distillate consumption
        if restart.qsblk_qsdscm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[2, cali, j] = (
                restart.qsblk_qsdscm[cali, j] * restart.qblk_qdscm[pacific, j]
                / restart.qsblk_qsdscm[pacific, j]
            )

        # California commercial residual fuel consumption
        if restart.qsblk_qsrscm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[3, cali, j] = (
                restart.qsblk_qsrscm[cali, j] * restart.qblk_qrscm[pacific, j]
                / restart.qsblk_qsrscm[pacific, j]
            )

        # California commercial kerosene consumption
        if restart.qsblk_qskscm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[4, cali, j] = (
                restart.qsblk_qskscm[cali, j] * restart.qblk_qkscm[pacific, j]
                / restart.qsblk_qskscm[pacific, j]
            )

        # California commercial coal consumption
        if restart.qsblk_qsclcm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[5, cali, j] = (
                restart.qsblk_qsclcm[cali, j] * restart.qblk_qclcm[pacific, j]
                / restart.qsblk_qsclcm[pacific, j]
            )

        # California commercial natural gas consumption
        if restart.qsblk_qsngcm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[6, cali, j] = (
                restart.qsblk_qsngcm[cali, j] * restart.qblk_qngcm[pacific, j]
                / restart.qsblk_qsngcm[pacific, j]
            )

        # California commercial purchased electricity (excluding charging of
        # electric vehicles)
        if restart.qsblk_qselcm[pacific, j] != 0.0:
            restart.ghgrep_fl_comm[7, cali, j] = (
                restart.qsblk_qselcm[cali, j]
                * (restart.qblk_qelcm[pacific, j] - qelcm_ev[pacific])
                / restart.qsblk_qselcm[pacific, j]
            )

    # Names of the commercial sector fuels (plus electricity)
    nm_comm[:] = [
        "Gasoline",
        "Propane",
        "Distillate",
        "Residual",
        "Kerosene",
        "Coal",
        "Natural Gas",
        "Electricity",
    ]

    return qelcm_ev


def sum_emissions_industrial(
    restart: Restart,
    j: int,
    nm_indy: list[str]
) -> None:
    """Compute the initial industrial sector emissions for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Compute emissions for this zero-based year index.
    nm_indy : list[str]
        List to store the names of the industrial sector fuel indicies.

    See Also
    --------
    sum_emissions : Calls this function for industrial sector emissions.
    """
    # Zero-based Python region indices
    usa = restart.parametr_mnumcr - 1
    cali = restart.parametr_mnumcr - 2
    pacific = restart.parametr_mnumcr - 3
    usa_pr = restart.parametr_mnumpr - 1

    # Compute dictionary of captured CO2 from cement kilns by fuel, then debug
    # print the results so they can be externally checked for correctness
    cement_kiln_ccs = partition_cement_kiln_ccs(restart)
    log_table_title = (
        f"Cement kiln CCS partitioning, year={restart.ncntrl_curcalyr}"
    )
    log_it(
        log_table_title, "=" * len(log_table_title),
        tabulate.tabulate(cement_kiln_ccs.items(), tablefmt="plain"),
        verbose=True
    )

    # Clear industrial C emissions for all fuels
    # First index of eminc is fuel: 0=ngas, 1=oil, 2=coal, 3=vented
    restart.emission_eminc[:, 0, j] = 0.0
    # Clear national industrial C emissions
    restart.emission_emincc[usa, 0, j] = 0.0

    # NOTE: The second index of eminc and emincc is mnpollut, which is the
    # number of air pollutants tracked by NEMS (0=C, 1=CO, 2=CO2, etc). All
    # other sectors only use the 0=C index of their arrays, but the industrial
    # sector uses the 1=CO and 2=CO2 indices in addition to the 0=C index.
    # However, the calculations here don't appear to be using those indices to
    # denote different pollutants. Instead, it looks like we're using index 1
    # for covered C emissions, index 2 for uncovered C emissions, and index 0
    # for total C emissions.

    # Industrial sector pollution calculations: calculate covered emissions
    # first, then uncovered, then total

    # Industrial covered C emissions from natural gas
    # Need to split NG into feedstock and combustion
    restart.emission_eminc[0, 1, j] = (
        (
            # Covered natural gas fuel
            (
                restart.bifurc_qe2ngin[usa, j, 0]
                - restart.indout_inqngpf[usa, j]  # Feedstocks
                - restart.hmmblk_qnghmhp[usa, j]  # Hydrogen heat & power
                - restart.hmmblk_qnghmpf[usa, j]  # Hydrogen feedstocks
            ) * restart.emeblk_egfin[j]
            # Natural gas feedstock
            + restart.indout_inqngpf[usa, j] * restart.emeblk_enqngpf[j]
            # Lease & plant natural gas
            # qlpin from ngtdm--not bifurcated
            + restart.qblk_qlpin[usa, j] * restart.emeblk_elpin[j]
            # Natural gas for liquefaction
            # Treat qnglq like qlpin for starters
            + restart.qblk_qnglq[usa, j] * restart.emeblk_elpin[j]
            # Natural gas for hydrogen heat & power
            + restart.hmmblk_qnghmhp[usa, j] * restart.emeblk_engin[j]
            # Natural gas for hydrogen feedstocks
            + restart.hmmblk_qnghmpf[usa, j] * restart.emeblk_enghm[j]
            # Carbon capture from cement kilns heated by natural gas
            - cement_kiln_ccs["natural gas"]
        ) / 1000.0
        # Refinery carbon captured and storage, natural gas
        - restart.emission_ccs_pmm[2, usa_pr, j]
        # Captured CO2 from steam methane reforming (eligible for 45Q)
        - restart.ccatsdat_sup_h2_45q[usa, j] * (12.0 / 44.0) / 1e6
        # Captured CO2 from steam methane reforming (no tax credit)
        - restart.ccatsdat_sup_h2_ntc[usa, j] * (12.0 / 44.0) / 1e6
    )

    # Industrial covered C emissions from coal
    restart.emission_eminc[2, 1, j] = (
        (
            # Covered steam coal
            restart.bifurc_qe2clin[usa, j, 0] * restart.emeblk_eclin[j]
            # Covered metallurgical coal
            + restart.bifurc_qe2mcin[usa, j, 0] * restart.emeblk_emcin[j]
            # Covered net coal coke imports
            + restart.bifurc_qe2ciin[usa, j, 0] * restart.emeblk_eciin[j]
            # Carbon capture from cement kilns heated by steam coal
            - cement_kiln_ccs["steam coal"]
        ) / 1000.0
        # Refinery carbon captured and storage, coal
        - restart.emission_ccs_pmm[1, usa_pr, j]
    )

    # Industrial covered C emissions from oil
    restart.emission_eminc[1, 1, j] = (
        (
            # Covered motor gasoline
            restart.bifurc_qe2mgin[usa, j, 0] * restart.emeblk_emgin[j]
            # Covered distillate
            + restart.bifurc_qe2dsin[usa, j, 0] * restart.emeblk_edsin[j]
            # Covered kerosene
            + restart.bifurc_qe2ksin[usa, j, 0] * restart.emeblk_eksin[j]
            # Covered propane fuel
            + np.clip(
                (
                    restart.bifurc_qe2lgin[usa, j, 0]
                    - restart.indout_inqlgpf[usa, j]
                ) / (
                    restart.qblk_qlgin[usa, j]
                    - restart.indout_inqlgpf[usa, j]
                ),
                0.0, 1.0
            )
            * (restart.qblk_qprin[usa, j] - restart.qblk_qprinpf[usa, j])
            * restart.emeblk_eprin[j]
            # Pentanes plus fuel
            + (restart.qblk_qppin[usa, j] - restart.qblk_qppinpf[usa, j])
            * restart.emeblk_eppin[j]
            # Ethane fuel
            + (restart.qblk_qetin[usa, j] - restart.qblk_qetinpf[usa, j])
            * restart.emeblk_eetin[j]
            # Butane fuel
            + (restart.qblk_qbuin[usa, j] - restart.qblk_qbuinpf[usa, j])
            * restart.emeblk_ebuin[j]
            # Isobutane fuel
            + (restart.qblk_qisin[usa, j] - restart.qblk_qisinpf[usa, j])
            * restart.emeblk_eisin[j]
            # Propane & refinery propylene feedstock
            + (restart.qblk_qprinpf[usa, j] + restart.qblk_qprolenerf[usa, j])
            * restart.emeblk_eprinpf[j]
            # Pentanes plus feedstock
            + restart.qblk_qppinpf[usa, j] * restart.emeblk_eppinpf[j]
            # Ethane feedstock
            + restart.qblk_qetinpf[usa, j] * restart.emeblk_eetinpf[j]
            # Butane feedstock
            + restart.qblk_qbuinpf[usa, j] * restart.emeblk_ebuinpf[j]
            # Isobutane feedstock
            + restart.qblk_qisinpf[usa, j] * restart.emeblk_eisinpf[j]
            # Covered residual fuel
            + restart.bifurc_qe2rsin[usa, j, 0] * restart.emeblk_ersin[j]
            # Covered petrochemical feedstock
            + restart.bifurc_qe2pfin[usa, j, 0] * restart.emeblk_epfin[j]
            # Covered petroleum coke
            + restart.bifurc_qe2pcin[usa, j, 0] * restart.emeblk_epcin[j]
            # Not putting PPIN or LUIN in here as they are in OTIN
            # Covered still gas
            + restart.bifurc_qe2sgin[usa, j, 0] * restart.emeblk_esgin[j]
            # Covered other petroleum
            + restart.bifurc_qe2otin[usa, j, 0] * restart.emeblk_eotin[j]
            # Carbon capture from cement kilns heated by distillate
            - cement_kiln_ccs["distillate"]
            # Carbon capture from cement kilns heated by petroleum coke
            - cement_kiln_ccs["petroleum coke"]
            # Carbon capture from cement kilns heated by other petroleum
            - cement_kiln_ccs["other petroleum"]
        ) / 1000.0
        # Refinery carbon captured and storage, still gas
        - restart.emission_ccs_pmm[3, usa_pr, j]
    )

    # Industrial covered C emissions from venting
    restart.emission_eminc[3, 1, j] = (
        # CO2 filtered out of raw natural gas during natural gas processing
        restart.ogsmout_ngpco2em[usa, j] * (12.0 / 44.0) / 1e6
    )

    # National industrial covered C emissions summed across all fuels
    restart.emission_emincc[usa, 1, j] = np.sum(
        restart.emission_eminc[:, 1, j]
    )

    # Now calculate uncovered (normally 0 unless carbshr[] in ind.f is < 1.0)

    # Industrial uncovered C emissions from natural gas
    restart.emission_eminc[0, 2, j] = (
        # Uncovered natural gas fuel
        restart.bifurc_qe2ngin[usa, j, 1] * restart.emeblk_egfin[j] / 1000.0
    )

    # Industrial uncovered C emissions from coal
    restart.emission_eminc[2, 2, j] = (
        (
            # Uncovered steam coal
            restart.bifurc_qe2clin[usa, j, 1] * restart.emeblk_eclin[j]
            # Uncovered metallurgical coal
            + restart.bifurc_qe2mcin[usa, j, 1] * restart.emeblk_emcin[j]
            # Uncovered net coal coke imports
            + restart.bifurc_qe2ciin[usa, j, 1] * restart.emeblk_eciin[j]
        ) / 1000.0
    )

    # Industrial uncovered C emissions from oil
    restart.emission_eminc[1, 2, j] = (
        (
            # Uncovered motor gasoline
            restart.bifurc_qe2mgin[usa, j, 1] * restart.emeblk_emgin[j]
            # Uncovered distillate
            + restart.bifurc_qe2dsin[usa, j, 1] * restart.emeblk_edsin[j]
            # Uncovered kerosene
            + restart.bifurc_qe2ksin[usa, j, 1] * restart.emeblk_eksin[j]
            # Uncovered propane fuel
            + (1.0 - np.clip(
                (
                    restart.bifurc_qe2lgin[usa, j, 1]
                    - restart.indout_inqlgpf[usa, j]
                ) / (
                    restart.qblk_qlgin[usa, j]
                    - restart.indout_inqlgpf[usa, j]
                ),
                0.0, 1.0
            ))
            * (restart.qblk_qprin[usa, j] - restart.qblk_qprinpf[usa, j])
            * restart.emeblk_eprin[j]
            # Uncovered residual fuel
            + restart.bifurc_qe2rsin[usa, j, 1] * restart.emeblk_ersin[j]
            # Uncovered petrochemical feedstock
            + restart.bifurc_qe2pfin[usa, j, 1] * restart.emeblk_epfin[j]
            # Uncovered petroleum coke
            + restart.bifurc_qe2pcin[usa, j, 1] * restart.emeblk_epcin[j]
            # Not putting PPIN or LUIN in here as they are in LGIN (PP) and
            # OTIN (LU)
            # Uncovered still gas
            + restart.bifurc_qe2sgin[usa, j, 1] * restart.emeblk_esgin[j]
            # Uncovered other petroleum
            + restart.bifurc_qe2otin[usa, j, 1] * restart.emeblk_eotin[j]
        ) / 1000.0
    )

    # Industrial uncovered C emissions from venting
    # Since we assume vented natural gas processing CO2 is covered, there are
    # no terms to include here at this time.
    restart.emission_eminc[3, 2, j] = 0.0

    # National industrial uncovered C emissions summed across all fuels
    restart.emission_emincc[usa, 2, j] = np.sum(
        restart.emission_eminc[:, 2, j]
    )

    # Now calculate total

    # Industrial total C emissions from natural gas
    restart.emission_eminc[0, 0, j] = (
        (
            # Total natural gas fuel
            (
                restart.qblk_qngin[usa, j]
                - restart.indout_inqngpf[usa, j]  # Feedstocks
                - restart.hmmblk_qnghmhp[usa, j]  # Hydrogen heat & power
                - restart.hmmblk_qnghmpf[usa, j]  # Hydrogen feedstocks
            ) * restart.emeblk_egfin[j]
            # Natural gas feedstock
            + restart.indout_inqngpf[usa, j] * restart.emeblk_enqngpf[j]
            # Lease & plant natural gas
            # qlpin from ngtdm--not bifurcated
            + restart.qblk_qlpin[usa, j] * restart.emeblk_elpin[j]
            # Natural gas for liquefaction
            # Treat qnglq like qlpin for starters
            + restart.qblk_qnglq[usa, j] * restart.emeblk_elpin[j]
            # Natural gas for hydrogen heat & power
            + restart.hmmblk_qnghmhp[usa, j] * restart.emeblk_engin[j]
            # Natural gas for hydrogen feedstocks
            + restart.hmmblk_qnghmpf[usa, j] * restart.emeblk_enghm[j]
            # Carbon capture from cement kilns heated by natural gas
            - cement_kiln_ccs["natural gas"]
        ) / 1000.0
        # Refinery carbon captured and storage, natural gas
        - restart.emission_ccs_pmm[2, usa_pr, j]
        # Captured CO2 from steam methane reforming (eligible for 45Q)
        - restart.ccatsdat_sup_h2_45q[usa, j] * (12.0 / 44.0) / 1e6
        # Captured CO2 from steam methane reforming (no tax credit)
        - restart.ccatsdat_sup_h2_ntc[usa, j] * (12.0 / 44.0) / 1e6
    )

    # Industrial total C emissions from coal
    restart.emission_eminc[2, 0, j] = (
        (
            # Total steam coal
            restart.qblk_qclin[usa, j] * restart.emeblk_eclin[j]
            # Total metallurgical coal
            + restart.qblk_qmcin[usa, j] * restart.emeblk_emcin[j]
            # Total net coal coke imports
            + restart.qblk_qciin[usa, j] * restart.emeblk_eciin[j]
            # Carbon capture from cement kilns heated by steam coal
            - cement_kiln_ccs["steam coal"]
        ) / 1000.0
        # Refinery carbon captured and storage, coal
        - restart.emission_ccs_pmm[1, usa_pr, j]
    )

    # Industrial total C emissions from oil
    restart.emission_eminc[1, 0, j] = (
        (
            # Total motor gasoline
            restart.qblk_qmgin[usa, j] * restart.emeblk_emgin[j]
            # Total distillate
            + restart.qblk_qdsin[usa, j] * restart.emeblk_edsin[j]
            # Total kerosene
            + restart.qblk_qksin[usa, j] * restart.emeblk_eksin[j]
            # Ethane fuel
            + (restart.qblk_qetin[usa, j] - restart.qblk_qetinpf[usa, j])
            * restart.emeblk_eetin[j]
            # Total propane fuel
            + (restart.qblk_qprin[usa, j] - restart.qblk_qprinpf[usa, j])
            * restart.emeblk_eprin[j]
            # Butane fuel
            + (restart.qblk_qbuin[usa, j] - restart.qblk_qbuinpf[usa, j])
            * restart.emeblk_ebuin[j]
            # Isobutane fuel
            + (restart.qblk_qisin[usa, j] - restart.qblk_qisinpf[usa, j])
            * restart.emeblk_eisin[j]
            # Pentanes plus fuel
            + (restart.qblk_qppin[usa, j] - restart.qblk_qppinpf[usa, j])
            * restart.emeblk_eppin[j]
            # Ethane feedstock
            + restart.qblk_qetinpf[usa, j] * restart.emeblk_eetinpf[j]
            # Propane & refinery propylene feedstock
            + (restart.qblk_qprinpf[usa, j] + restart.qblk_qprolenerf[usa, j])
            * restart.emeblk_eprinpf[j]
            # Butane feedstock
            + restart.qblk_qbuinpf[usa, j] * restart.emeblk_ebuinpf[j]
            # Isobutane feedstock
            + restart.qblk_qisinpf[usa, j] * restart.emeblk_eisinpf[j]
            # Pentanes plus feedstock
            + restart.qblk_qppinpf[usa, j] * restart.emeblk_eppinpf[j]
            # Total residual fuel
            + restart.qblk_qrsin[usa, j] * restart.emeblk_ersin[j]
            # Total petrochemical feedstock
            + restart.qblk_qpfin[usa, j] * restart.emeblk_epfin[j]
            # Total petroleum coke
            + restart.qblk_qpcin[usa, j] * restart.emeblk_epcin[j]
            # Total still gas
            + restart.qblk_qsgin[usa, j] * restart.emeblk_esgin[j]
            # Total other petroleum (lubricants still included in qotin/eotin)
            + restart.qblk_qotin[usa, j] * restart.emeblk_eotin[j]
            # Carbon capture from cement kilns heated by distillate
            - cement_kiln_ccs["distillate"]
            # Carbon capture from cement kilns heated by petroleum coke
            - cement_kiln_ccs["petroleum coke"]
            # Carbon capture from cement kilns heated by other petroleum
            - cement_kiln_ccs["other petroleum"]
        ) / 1000.0
        # Refinery carbon captured and storage, still gas
        - restart.emission_ccs_pmm[3, usa_pr, j]
    )

    # Industrial total C emissions from venting
    restart.emission_eminc[3, 0, j] = (
        # CO2 filtered out of raw natural gas during natural gas processing
        restart.ogsmout_ngpco2em[usa, j] * (12.0 / 44.0) / 1e6
    )

    # National industrial total C emissions summed across all fuels
    restart.emission_emincc[usa, 0, j] = np.sum(
        restart.emission_eminc[:, 0, j]
    )

    # National industrial CO2 emissions from motor gasoline
    restart.ghgrep_em_indy[0, usa, j] = (
        restart.qblk_qmgin[usa, j] * restart.emeblk_emgin[j]
    )

    # National industrial CO2 emissions from hydocarbon gas liquid (HGL) fuel
    restart.ghgrep_em_indy[1, usa, j] = (
        # Ethane
        (restart.qblk_qetin[usa, j] - restart.qblk_qetinpf[usa, j])
        * restart.emeblk_eetin[j]
        # Propane
        + (restart.qblk_qprin[usa, j] - restart.qblk_qprinpf[usa, j])
        * restart.emeblk_eprin[j]
        # Butane
        + (restart.qblk_qbuin[usa, j] - restart.qblk_qbuinpf[usa, j])
        * restart.emeblk_ebuin[j]
        # Isobutane
        + (restart.qblk_qisin[usa, j] - restart.qblk_qisinpf[usa, j])
        * restart.emeblk_eisin[j]
        # Pentanes plus
        + (restart.qblk_qppin[usa, j] - restart.qblk_qppinpf[usa, j])
        * restart.emeblk_eppin[j]
    )

    # National industrial CO2 emissions from hydocarbon gas liquid (HGL)
    # feedstock
    restart.ghgrep_em_indy[2, usa, j] = (
        # Ethane
        restart.qblk_qetinpf[usa, j] * restart.emeblk_eetinpf[j]
        # Propane & refinery propylene
        + (restart.qblk_qprinpf[usa, j] + restart.qblk_qprolenerf[usa, j])
        * restart.emeblk_eprinpf[j]
        # Butane
        + restart.qblk_qbuinpf[usa, j] * restart.emeblk_ebuinpf[j]
        # Isobutane
        + restart.qblk_qisinpf[usa, j] * restart.emeblk_eisinpf[j]
        # Pentanes plus
        + restart.qblk_qppinpf[usa, j] * restart.emeblk_eppinpf[j]
    )

    # National industrial CO2 emissions from distillate
    restart.ghgrep_em_indy[3, usa, j] = (
        restart.qblk_qdsin[usa, j] * restart.emeblk_edsin[j]
        # Carbon capture from cement kilns heated by distillate
        - cement_kiln_ccs["distillate"]
    )

    # National industrial CO2 emissions from residual fuel
    restart.ghgrep_em_indy[4, usa, j] = (
        restart.qblk_qrsin[usa, j] * restart.emeblk_ersin[j]
    )

    # National industrial CO2 emissions from kerosene
    restart.ghgrep_em_indy[5, usa, j] = (
        restart.qblk_qksin[usa, j] * restart.emeblk_eksin[j]
    )

    # National industrial CO2 emissions from petrochemical feedstock
    restart.ghgrep_em_indy[6, usa, j] = (
        restart.qblk_qpfin[usa, j] * restart.emeblk_epfin[j]
    )

    # National industrial CO2 emissions from petroleum coke
    restart.ghgrep_em_indy[7, usa, j] = (
        restart.qblk_qpcin[usa, j] * restart.emeblk_epcin[j]
        # Carbon capture from cement kilns heated by petroleum coke
        - cement_kiln_ccs["petroleum coke"]
    )

    # National industrial CO2 emissions from still gas
    restart.ghgrep_em_indy[8, usa, j] = (
        restart.qblk_qsgin[usa, j] * restart.emeblk_esgin[j]
        # Refinery carbon captured and storage
        - restart.emission_ccs_pmm[3, usa_pr, j] * 1000.0
    )

    # National industrial CO2 emissions from other petroleum
    restart.ghgrep_em_indy[9, usa, j] = (
        restart.qblk_qotin[usa, j] * restart.emeblk_eotin[j]
        # Carbon capture from cement kilns heated by other petroleum
        - cement_kiln_ccs["other petroleum"]
    )

    # National industrial CO2 emissions from steam coal
    restart.ghgrep_em_indy[10, usa, j] = (
        restart.qblk_qclin[usa, j] * restart.emeblk_eclin[j]
        # Carbon capture from cement kilns heated by steam coal
        - cement_kiln_ccs["steam coal"]
    )

    # National industrial CO2 emissions from metallurgical coal
    restart.ghgrep_em_indy[11, usa, j] = (
        restart.qblk_qmcin[usa, j] * restart.emeblk_emcin[j]
    )

    # National industrial CO2 emissions from net coal coke imports
    restart.ghgrep_em_indy[12, usa, j] = (
        restart.qblk_qciin[usa, j] * restart.emeblk_eciin[j]
        # Refinery carbon captured and storage
        - restart.emission_ccs_pmm[1, usa_pr, j] * 1000.0
    )

    # National industrial CO2 emissions from natural gas fuel
    restart.ghgrep_em_indy[13, usa, j] = (
        (
            restart.qblk_qngin[usa, j]
            # Exclude feedstock natural gas (handled separately)
            - restart.indout_inqngpf[usa, j]
            # Exclude natural gas for hydrogen (handled separately)
            - restart.hmmblk_qnghmhp[usa, j] - restart.hmmblk_qnghmpf[usa, j]
        )
        * restart.emeblk_engin[j]
        # Refinery carbon captured and storage
        - restart.emission_ccs_pmm[2, usa_pr, j] * 1000.0
        # Carbon capture from cement kilns heated by natural gas
        - cement_kiln_ccs["natural gas"]
    )

    # National industrial CO2 emissions from natural gas feedstock
    restart.ghgrep_em_indy[14, usa, j] = (
        restart.indout_inqngpf[usa, j] * restart.emeblk_enqngpf[j]
    )

    # National industrial CO2 emissions from lease & plant natural gas
    restart.ghgrep_em_indy[15, usa, j] = (
        restart.qblk_qlpin[usa, j] * restart.emeblk_elpin[j]
        # Natural gas for liquefaction
        + restart.qblk_qnglq[usa, j] * restart.emeblk_elpin[j]
    )

    # National industrial CO2 emissions from hydrogen heat & power natural gas
    restart.ghgrep_em_indy[16, usa, j] = (
        restart.hmmblk_qnghmhp[usa, j] * restart.emeblk_engin[j]
    )

    # Regional industrial CO2 emissions from hydrogen feedstock natural gas
    restart.ghgrep_em_indy[17, usa, j] = (
        restart.hmmblk_qnghmpf[usa, j] * restart.emeblk_enghm[j]
        # Captured CO2 from steam methane reforming (eligible for 45Q) (convert
        # to 1000s of metric tons of carbon)
        - restart.ccatsdat_sup_h2_45q[usa, j] * (12.0 / 44.0) / 1000.0
        # Captured CO2 from steam methane reforming (no tax credit) (convert to
        # 1000s of metric tons of carbon)
        - restart.ccatsdat_sup_h2_ntc[usa, j] * (12.0 / 44.0) / 1000.0
    )

    # Regional industrial CO2 emissions from natural gas processing
    restart.ghgrep_em_indy[18, usa, j] = (
        # Convert to 1000s of metric tons of carbon
        restart.ogsmout_ngpco2em[usa, j] * (12.0 / 44.0) / 1000.0
    )

    # National industrial CO2 emissions from purchased electricity (initially
    # just the sector share)
    zero = restart.qblk_qelas[:, j] == 0
    nonzero = np.logical_not(zero)
    restart.ghgrep_em_indy[19, nonzero, j] = (
        restart.qblk_qelin[nonzero, j] / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_indy[19, zero, j] = 0.0

    # Regional industrial motor gasoline consumption
    restart.ghgrep_fl_indy[0, :, j] = restart.qblk_qmgin[:, j]

    # Regional industrial hydocarbon gas liquid (HGL) fuel consumption
    restart.ghgrep_fl_indy[1, :, j] = (
        # Ethane
        restart.qblk_qetin[:, j] - restart.qblk_qetinpf[:, j]
        # Propane
        + restart.qblk_qprin[:, j] - restart.qblk_qprinpf[:, j]
        # Butane
        + restart.qblk_qbuin[:, j] - restart.qblk_qbuinpf[:, j]
        # Isobutane
        + restart.qblk_qisin[:, j] - restart.qblk_qisinpf[:, j]
        # Pentanes plus
        + restart.qblk_qppin[:, j] - restart.qblk_qppinpf[:, j]
    )

    # Regional industrial hydocarbon gas liquid (HGL) feedstock consumption
    restart.ghgrep_fl_indy[2, :, j] = (
        # Ethane
        restart.qblk_qetinpf[:, j]
        # Propane & refinery propylene
        + restart.qblk_qprinpf[:, j] + restart.qblk_qprolenerf[:, j]
        # Butane
        + restart.qblk_qbuinpf[:, j]
        # Isobutane
        + restart.qblk_qisinpf[:, j]
        # Pentanes plus
        + restart.qblk_qppinpf[:, j]
    )

    # Regional industrial distillate consumption
    restart.ghgrep_fl_indy[3, :, j] = restart.qblk_qdsin[:, j]

    # Regional industrial residual fuel consumption
    restart.ghgrep_fl_indy[4, :, j] = restart.qblk_qrsin[:, j]

    # Regional industrial kerosene consumption
    restart.ghgrep_fl_indy[5, :, j] = restart.qblk_qksin[:, j]

    # Regional industrial petrochemical feedstock consumption
    restart.ghgrep_fl_indy[6, :, j] = restart.qblk_qpfin[:, j]

    # Regional industrial petroleum coke consumption
    restart.ghgrep_fl_indy[7, :, j] = restart.qblk_qpcin[:, j]

    # Regional industrial still gas consumption
    restart.ghgrep_fl_indy[8, :, j] = restart.qblk_qsgin[:, j]

    # Regional industrial other petroleum consumption
    restart.ghgrep_fl_indy[9, :, j] = restart.qblk_qotin[:, j]

    # Regional industrial steam coal consumption
    restart.ghgrep_fl_indy[10, :, j] = restart.qblk_qclin[:, j]

    # Regional industrial metallurgical coal consumption
    restart.ghgrep_fl_indy[11, :, j] = restart.qblk_qmcin[:, j]

    # Regional industrial net coal coke imports
    restart.ghgrep_fl_indy[12, :, j] = restart.qblk_qciin[:, j]

    # Regional industrial natural gas fuel consumption
    restart.ghgrep_fl_indy[13, :, j] = (
        restart.qblk_qngin[:, j]
        # Exclude feedstock natural gas (handled separately)
        - restart.indout_inqngpf[:, j]
        # Exclude natural gas for hydrogen (handled separately)
        - restart.hmmblk_qnghmhp[:, j] - restart.hmmblk_qnghmpf[:, j]
    )

    # Regional industrial natural gas feedstock consumption
    restart.ghgrep_fl_indy[14, :, j] = restart.indout_inqngpf[:, j]

    # Regional industrial lease & plant natural gas consumption
    restart.ghgrep_fl_indy[15, :, j] = (
        restart.qblk_qlpin[:, j]
        # Natural gas for liquefaction
        + restart.qblk_qnglq[:, j]
    )

    # Regional industrial hydrogen heat & power natural gas consumption
    restart.ghgrep_fl_indy[16, :, j] = restart.hmmblk_qnghmhp[:, j]

    # Regional industrial hydrogen feedstock natural gas consumption
    restart.ghgrep_fl_indy[17, :, j] = restart.hmmblk_qnghmpf[:, j]

    # Regional industrial proxy consumption to associate with vented natural
    # gas processing emissions
    restart.ghgrep_fl_indy[18, :, j] = (
        # Convert emissions to 1000s of metric tons of carbon
        restart.ogsmout_ngpco2em[:, j] * (12.0 / 44.0) / 1000.0
        # Go backwards to an equivalent amount of natural gas combustion
        / restart.emeblk_engin[j]
    )

    # Regional industrial purchased electricity
    restart.ghgrep_fl_indy[19, :, j] = restart.qblk_qelin[:, j]

    # There are usually no California numbers until after msedyr, so estimate
    # them using the Pacific numbers scaled by appropriate ratios from SEDS
    if restart.ncntrl_curiyr <= restart.parametr_msedyr:

        # California industrial motor gasoline consumption
        if restart.qsblk_qsmgin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[0, cali, j] = (
                restart.qsblk_qsmgin[cali, j] * restart.qblk_qmgin[pacific, j]
                / restart.qsblk_qsmgin[pacific, j]
            )

        # California industrial hydrocarbon gas liquid (HGL) fuel consumption
        # For HGL fuel, scale the Pacific number by California's share of total
        # Pacific LPG consumption in SEDS. The Pacific region is only propane
        # and the LPG history goes back farther than the propane history. Also,
        # there are no SEDS feedstock variables to use for a more detailed
        # estimate.
        if restart.qsblk_qslgin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[1, cali, j] = (
                restart.ghgrep_fl_indy[1, pacific, j]
                * restart.qsblk_qslgin[cali, j]
                / restart.qsblk_qslgin[pacific, j]
            )

        # California industrial hydrocarbon gas liquid (HGL) feedstock
        # consumption
        # For HGL feedstock, scale the Pacific number by California's share of
        # total Pacific LPG consumption in SEDS, since there are no SEDS
        # feedstock variables to use for a more detailed estimate
        if restart.qsblk_qslgin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[2, cali, j] = (
                restart.ghgrep_fl_indy[2, pacific, j]
                * restart.qsblk_qslgin[cali, j]
                / restart.qsblk_qslgin[pacific, j]
            )

        # California industrial distillate consumption
        if restart.qsblk_qsdsin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[3, cali, j] = (
                restart.qsblk_qsdsin[cali, j] * restart.qblk_qdsin[pacific, j]
                / restart.qsblk_qsdsin[pacific, j]
            )

        # California industrial residual fuel consumption
        if restart.qsblk_qsrsin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[4, cali, j] = (
                restart.qsblk_qsrsin[cali, j] * restart.qblk_qrsin[pacific, j]
                / restart.qsblk_qsrsin[pacific, j]
            )

        # California industrial kerosene consumption
        if restart.qsblk_qsksin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[5, cali, j] = (
                restart.qsblk_qsksin[cali, j] * restart.qblk_qksin[pacific, j]
                / restart.qsblk_qsksin[pacific, j]
            )

        # California industrial petrochemical feedstock consumption
        if restart.qsblk_qspfin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[6, cali, j] = (
                restart.qsblk_qspfin[cali, j] * restart.qblk_qpfin[pacific, j]
                / restart.qsblk_qspfin[pacific, j]
            )

        # California industrial petroleum coke consumption
        if restart.qsblk_qspcin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[7, cali, j] = (
                restart.qsblk_qspcin[cali, j] * restart.qblk_qpcin[pacific, j]
                / restart.qsblk_qspcin[pacific, j]
            )

        # California industrial still gas consumption
        if restart.qsblk_qssgin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[8, cali, j] = (
                restart.qsblk_qssgin[cali, j] * restart.qblk_qsgin[pacific, j]
                / restart.qsblk_qssgin[pacific, j]
            )

        # California industrial other petroleum consumption
        if restart.qsblk_qsotin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[9, cali, j] = (
                restart.qsblk_qsotin[cali, j] * restart.qblk_qotin[pacific, j]
                / restart.qsblk_qsotin[pacific, j]
            )

        # California industrial steam coal consumption
        if restart.qsblk_qsclin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[10, cali, j] = (
                restart.qsblk_qsclin[cali, j] * restart.qblk_qclin[pacific, j]
                / restart.qsblk_qsclin[pacific, j]
            )

        # California industrial metallurgical coal consumption
        if restart.qsblk_qsmcin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[11, cali, j] = (
                restart.qsblk_qsmcin[cali, j] * restart.qblk_qmcin[pacific, j]
                / restart.qsblk_qsmcin[pacific, j]
            )

        # California industrial net coal coke imports
        if restart.qsblk_qsciin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[12, cali, j] = (
                restart.qsblk_qsciin[cali, j] * restart.qblk_qciin[pacific, j]
                / restart.qsblk_qsciin[pacific, j]
            )

        # California industrial natural gas fuel consumption
        # For natural gas fuel, scale the Pacific number by California's share
        # of total Pacific natural gas consumption in SEDS, since there is no
        # SEDS feedstock variable to use for a more detailed estimate
        if restart.qsblk_qsngin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[13, cali, j] = (
                restart.ghgrep_fl_indy[13, pacific, j]
                * restart.qsblk_qsngin[cali, j]
                / restart.qsblk_qsngin[pacific, j]
            )

        # California industrial natural gas feedstock consumption
        # For natural gas feedstock, scale the Pacific number by California's
        # share of total Pacific natural gas consumption in SEDS, since there
        # is no SEDS feedstock variable to use for a more detailed estimate
        if restart.qsblk_qsngin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[14, cali, j] = (
                restart.ghgrep_fl_indy[14, pacific, j]
                * restart.qsblk_qsngin[cali, j]
                / restart.qsblk_qsngin[pacific, j]
            )

        # California industrial lease & plant natural gas consumption
        if restart.qsblk_qslpin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[15, cali, j] = (
                restart.qsblk_qslpin[cali, j] * restart.qblk_qlpin[pacific, j]
                / restart.qsblk_qslpin[pacific, j]
            )

        # California industrial hydrogen heat & power natural gas consumption
        # For hydrogen heat and power natural gas, scale the Pacific number by
        # California's share of total Pacific natural gas consumption in SEDS,
        # since there is likely no SEDS variable to use for a better estimate
        if restart.qsblk_qsngin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[16, cali, j] = (
                restart.ghgrep_fl_indy[16, pacific, j]
                * restart.qsblk_qsngin[cali, j]
                / restart.qsblk_qsngin[pacific, j]
            )

        # California industrial hydrogen feedstock natural gas consumption
        # For hydrogen feedstock natural gas, scale the Pacific number by
        # California's share of total Pacific natural gas consumption in SEDS,
        # since there is likely no SEDS variable to use for a better estimate
        if restart.qsblk_qsngin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[17, cali, j] = (
                restart.ghgrep_fl_indy[17, pacific, j]
                * restart.qsblk_qsngin[cali, j]
                / restart.qsblk_qsngin[pacific, j]
            )

        # California industrial proxy consumption to associate with vented
        # natural gas processing emissions
        restart.ghgrep_fl_indy[18, cali, j] = (
            # TODO: Insert something appropriate here if we need it... maybe
            # use historical natural gas production numbers for California?
            restart.ghgrep_fl_indy[18, cali, j]
        )

        # California industrial purchased electricity
        if restart.qsblk_qselin[pacific, j] != 0.0:
            restart.ghgrep_fl_indy[19, cali, j] = (
                restart.qsblk_qselin[cali, j] * restart.qblk_qelin[pacific, j]
                / restart.qsblk_qselin[pacific, j]
            )

    # Names of the industrial sector fuels (plus electricity)
    nm_indy[:] = [
        "Gasoline",
        "HGL-Fuel",
        "HGL-Feedstock",
        "Distillate",
        "Residual",
        "Kerosene",
        "Petrochemical Feedstock",
        "Petroleum Coke",
        "Still Gas",
        "Other Petroleum",
        "Steam Coal",
        "Metallurgical Coal",
        "Net Coal Coke Imports",
        "Natural Gas-Fuel",
        "Natural Gas-Feedstock",
        "Natural Gas-Lease & Plant",
        "Natural Gas-Hydrogen Heat & Power",
        "Natural Gas-Hydrogen Feedstock",
        "Natural Gas Processing",
        "Electricity",
    ]

    # Start by zeroing out the regional industrial captured carbon
    restart.ghgrep_cc_indy[:, :, j] = 0.0

    # Regional industrial carbon capture for distillate (cement kilns)
    # Zero-based index = 0
    if restart.ghgrep_fl_indy[3, usa, j] != 0.0:
        restart.ghgrep_cc_indy[0, :, j] += (
            cement_kiln_ccs["distillate"]
            * restart.ghgrep_fl_indy[3, :, j]
            / restart.ghgrep_fl_indy[3, usa, j]
        )

    # Regional industrial carbon capture for petroleum coke (cement kilns)
    # Zero-based index = 1
    if restart.ghgrep_fl_indy[7, usa, j] != 0.0:
        restart.ghgrep_cc_indy[1, :, j] += (
            cement_kiln_ccs["petroleum coke"]
            * restart.ghgrep_fl_indy[7, :, j]
            / restart.ghgrep_fl_indy[7, usa, j]
        )

    # Regional industrial carbon capture for still gas (refineries)
    # Zero-based index = 2
    if restart.ghgrep_fl_indy[8, usa, j] != 0.0:
        restart.ghgrep_cc_indy[2, :, j] += (
            restart.emission_ccs_pmm[3, usa_pr, j] * 1000.0
            * restart.ghgrep_fl_indy[8, :, j]
            / restart.ghgrep_fl_indy[8, usa, j]
        )

    # Regional industrial carbon capture for other petroleum (cement kilns)
    # Zero-based index = 3
    if restart.ghgrep_fl_indy[9, usa, j] != 0.0:
        restart.ghgrep_cc_indy[3, :, j] += (
            cement_kiln_ccs["other petroleum"]
            * restart.ghgrep_fl_indy[9, :, j]
            / restart.ghgrep_fl_indy[9, usa, j]
        )

    # Regional industrial carbon capture for steam coal (cement kilns)
    # Zero-based index = 4
    if restart.ghgrep_fl_indy[10, usa, j] != 0.0:
        restart.ghgrep_cc_indy[4, :, j] += (
            cement_kiln_ccs["steam coal"]
            * restart.ghgrep_fl_indy[10, :, j]
            / restart.ghgrep_fl_indy[10, usa, j]
        )

    # Regional industrial carbon capture for net coal coke imports (refineries)
    # Zero-based index = 5
    if restart.ghgrep_fl_indy[12, usa, j] != 0.0:
        restart.ghgrep_cc_indy[5, :, j] += (
            restart.emission_ccs_pmm[1, usa_pr, j] * 1000.0
            * restart.ghgrep_fl_indy[12, :, j]
            / restart.ghgrep_fl_indy[12, usa, j]
        )

    # Regional industrial carbon capture for natural gas fuel (refineries)
    # Zero-based index = 6
    if restart.ghgrep_fl_indy[13, usa, j] != 0.0:
        restart.ghgrep_cc_indy[6, :, j] += (
            restart.emission_ccs_pmm[2, usa_pr, j] * 1000.0
            * restart.ghgrep_fl_indy[13, :, j]
            / restart.ghgrep_fl_indy[13, usa, j]
        )

    # Regional industrial carbon capture for natural gas fuel (cement kilns)
    # Zero-based index = 7
    if restart.ghgrep_fl_indy[13, usa, j] != 0.0:
        restart.ghgrep_cc_indy[7, :, j] += (
            cement_kiln_ccs["natural gas"]
            * restart.ghgrep_fl_indy[13, :, j]
            / restart.ghgrep_fl_indy[13, usa, j]
        )

    # Regional industrial carbon capture for natural gas feedstocks (steam
    # methane reforming)
    # Zero-based index = 8
    if restart.ghgrep_fl_indy[17, usa, j] != 0.0:
        restart.ghgrep_cc_indy[8, :, j] += (
            (
                restart.ccatsdat_sup_h2_45q[usa, j]
                + restart.ccatsdat_sup_h2_ntc[usa, j]
            ) * (12.0 / 44.0) / 1000.0
            * restart.ghgrep_fl_indy[17, :, j]
            / restart.ghgrep_fl_indy[17, usa, j]
        )

    # Regional industrial carbon capture from natural gas processing
    # Zero-based index = 9
    restart.ghgrep_cc_indy[9, :, j] = (
        (
            restart.ccatsdat_sup_ngp_45q[:, j]
            + restart.ccatsdat_sup_ngp_ntc[:, j]
        ) * (12.0 / 44.0) / 1000.0
    )


def partition_cement_kiln_ccs(restart: Restart) -> dict[str, float]:
    """Split up captured cement kiln carbon from combustion and non-combustion.

    Captured CO2 from cement kilns in IDM is a mixture of process emissions
    from the calcium carbonate together with combustion emissions from all the
    different fossil fuels that are used to heat kilns.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.

    Returns
    -------
    dict[str, float]
        A mapping from fuel names (plus the special fuel "cement process") to
        corresponding national cement kiln carbon capture totals for this year.
        The values will sum to the total captured cement kiln carbon that IDM
        sent to CCATS.
    """
    i_year = restart.ncntrl_curiyr - 1
    usa_indcr = restart.indrep_mindcr - 1  # IDM national region
    i_cement = 10  # IDM industry index for the cement industry

    # Map of industrial fuels translated from the Ftab indcarb function. Only
    # this subset of fuels are relevant for cement industry CCS.
    fuel_map = [
        # Fuel name, IDM fuel index, EPM carbon factor
        ("natural gas", 1, restart.emeblk_engin),
        ("steam coal", 2, restart.emeblk_eclin),
        ("distillate", 6, restart.emeblk_edsin),
        ("petroleum coke", 10, restart.emeblk_epcin),
        ("other petroleum", 14, restart.emeblk_eotin),
    ]

    # For each relevant fuel, compute the national gross carbon emissions from
    # the cement industry for this year
    gross_emissions = {}
    for fuel_name, fuel_index, carbon_factor in fuel_map:
        consumption = restart.indrep_cementcon[fuel_index, usa_indcr, i_year]
        gross_emissions[fuel_name] = consumption * carbon_factor[i_year]

    # Now get the gross amount of *process* emissions from all cement plants,
    # convert that to 1000s of metric tons of carbon, and include it with the
    # rest of the cement industry gross emissions
    process_emissions = restart.indrep_ghg_processin[i_cement, i_year]
    process_emissions *= (12.0 / 44.0) * 1000.0
    gross_emissions["cement process"] = process_emissions

    # For each emissions source, compute its share of the gross cement industry
    # emissions
    gross_emissions_sum = sum(gross_emissions.values())
    if gross_emissions_sum != 0.0:
        emissions_share = {
            fuel_name: fuel_emissions / gross_emissions_sum
            for fuel_name, fuel_emissions in gross_emissions.items()
        }
    else:
        emissions_share = {fuel_name: 0.0 for fuel_name in gross_emissions}

    # Finally, sum *captured* CO2 from cement kilns across all IDM regions and
    # convert that to 1000s of metric tons of carbon
    all_ccs = (
        # Captured CO2 from cement plants (eligible for 45Q tax credit)
        np.sum(restart.ccatsdat_sup_cmt_45q[:, i_year])
        # Captured CO2 from cement plants (not eligible for tax credits)
        + np.sum(restart.ccatsdat_sup_cmt_ntc[:, i_year])
    )
    all_ccs *= (12.0 / 44.0) / 1000.0

    # Partion the captured CO2 among all possible sources by their proportion
    # of the gross cement industry emissions
    partitioned = {
        fuel_name: float(all_ccs * fuel_share)
        for fuel_name, fuel_share in emissions_share.items()
    }
    return partitioned


def sum_emissions_transportation(
    restart: Restart,
    j: int,
    nm_tran: list[str],
    qelrs_ev: npt.NDArray[np.float64],
    qelcm_ev: npt.NDArray[np.float64],
) -> tuple[float, float]:
    """Compute the initial transportation sector emissions for `sum_emissions`.

    Return `e85_ethanol_share` and `e85_gasoline_share` because they are needed
    for the California biofuels calculations later on in `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Compute emissions for this zero-based year index.
    nm_tran : list[str]
        List to store the names of the transportation sector fuel indicies.
    qelrs_ev : npt.NDArray[np.float64]
        Electricity consumption by region transferred from the residential
        sector because it was used to charge electric vehicles.
    qelcm_ev : npt.NDArray[np.float64]
        Electricity consumption by region transferred from the commercial
        sector because it was used to charge electric vehicles.

    Returns
    -------
    e85_ethanol_share : float
        Fraction of all E85 consumed in year index `j` that was ethanol.
    e85_gasoline_share : float
        Fraction of all E85 consumed in year index `j` that was gasoline.

    See Also
    --------
    sum_emissions : Calls this function for transportation sector emissions.
    sum_emissions_residential : Provides the value for the `qelrs_ev` argument.
    sum_emissions_commercial : Provides the value for the `qelcm_ev` argument.
    sum_emissions_california_biofuels : Needs the returned `e85` variables.
    """
    # Zero-based Python region indices
    usa = restart.parametr_mnumcr - 1
    cali = restart.parametr_mnumcr - 2
    pacific = restart.parametr_mnumcr - 3
    usa_pr = restart.parametr_mnumpr - 1

    # Clear transportation C emissions for all fuels
    # First index of emtr is fuel: 0=ngas, 1=oil, 2=methanol, 3=?, 4=?
    restart.emission_emtr[:4, 0, j] = 0.0
    # Clear national transportation C emissions
    restart.emission_emtrc[usa, 0, j] = 0.0
    # Clear national NGTDM C emissions
    restart.emission_emnt[usa, 0, j] = 0.0

    # Ethanol is used for E85 and blending with gasoline. For E85 (about 74
    # percent ethanol, 26 percent gasoline by volume), need to account for the
    # carbon in the gasoline component. The ethanol is assumed to be a zero net
    # carbon source.

    # Fraction of all E85 consumed year that was gasoline
    e85_gasoline_share = (
        (restart.pmmout_trgne85 * restart.convfact_cfrbob[j])
        / (
            restart.pmmout_trgne85 * restart.convfact_cfrbob[j]
            + restart.pmmout_ethne85 * restart.convfact_cfetq[j]
        )
    )

    # Fraction of all E85 consumed this year that was ethanol
    e85_ethanol_share = (
        (restart.pmmout_ethne85 * restart.convfact_cfetq[j])
        / (
            restart.pmmout_trgne85 * restart.convfact_cfrbob[j]
            + restart.pmmout_ethne85 * restart.convfact_cfetq[j]
        )
    )

    # Total amount of ethanol consumed this year
    ethanol_tot = (
        365.0 * restart.convfact_cfetq[j]
        * (
            restart.pmmrpt_crnethcd[usa, j]
            + restart.pmmrpt_cllethcd[usa, j]
            + restart.pmmrpt_othethcd[usa, j]
            + restart.pmmrpt_ethimp[usa, j]
            - restart.pmmrpt_ethexp[usa, j]
        ) / 1000.0
    )

    # Total amount of ethanol consumed as part of E85 this year
    ethanol_e85 = restart.qblk_qettr[usa, j] * e85_ethanol_share

    # Total amount of ethanol consumed outside of E85 this year
    ethanol_blended = ethanol_tot - ethanol_e85

    # Sum transportation C emissions across all regions
    mnumcr2 = restart.parametr_mnumcr - 2

    restart.emission_emtr[0, 0, j] += 0.001 * np.sum(
        # Transportation C emissions from natural gas, core
        restart.qblk_qgftr[:mnumcr2, j] * restart.emeblk_egftr[j]
        # Transportation C emissions from natural gas, noncore
        + restart.qblk_qgitr[:mnumcr2, j] * restart.emeblk_egitr[j]
    )

    # Transportation C emissions from pipeline natural gas
    # Pipeline transportation is now kept in EMNT. We keep it by region for
    # completeness. Don't divide by 1000 for consistency with ftab.
    restart.emission_emnt[:mnumcr2, 0, j] = (
        # Warning--also calculated in ngafm
        restart.qblk_qgptr[:mnumcr2, j] * restart.emeblk_egptr[j]
    )
    restart.emission_emnt[usa, 0, j] += np.sum(
        restart.emission_emnt[:mnumcr2, 0, j]
    )

    restart.emission_emtr[1, 0, j] += 0.001 * np.sum(
        # Transportation C emissions from motor gasoline
        restart.qblk_qmgtr[:mnumcr2, j] * restart.emeblk_emgtr[j]
        # Transportation C emissions from jet fuel
        + restart.qblk_qjftr[:mnumcr2, j] * restart.emeblk_ejftr[j]
        # Transportation C emissions from distillate
        + restart.qblk_qdstr[:mnumcr2, j] * restart.emeblk_edstr[j]
        # Transportation C emissions from propane
        + restart.qblk_qprtr[:mnumcr2, j] * restart.emeblk_eprtr[j]
        # Transportation C emissions from residual fuel, low sulfur
        + restart.qblk_qrltr[:mnumcr2, j] * restart.emeblk_erltr[j]
        # Transportation C emissions from residual fuel, high sulfur
        + restart.qblk_qrhtr[:mnumcr2, j] * restart.emeblk_erhtr[j]
        # Transportation C emissions from other petroleum
        + restart.qblk_qottr[:mnumcr2, j] * restart.emeblk_eottr[j]
        # Transportation C emissions from the gasoline part of E85
        + restart.qblk_qettr[:mnumcr2, j] * e85_gasoline_share
        * restart.emeblk_emgtr[j]
    )

    restart.emission_emtr[2, 0, j] += 0.001 * np.sum(
        # Transportation C emissions from methanol
        restart.qblk_qmetr[:mnumcr2, j] * restart.emeblk_emetr[j]
    )

    # Subtract out renewable components used in gasoline blends from the motor
    # gasoline carbon computed above. Total ethanol from refineries minus e85
    # = ethanol for splash blending and for ETBE. Ethanol is only reported in
    # SEDS from 1993 on, so we only subtract it out in 1993 on.

    # Add other sources of renewable gasoline
    allrenewablegasoline = (
        (
            # Non ester renewable gasoline
            restart.lfmmout_grn2mgqty[usa_pr, j] * restart.convfact_cfnpq
            # Stream 1
            + (
                restart.pmmout_btlfrac[0, usa_pr, j]
                + restart.pmmout_cbtlfrac[1, 0, usa_pr, j]
            )
            * restart.convfact_cfftliq[0, j]
            # Stream 2
            + (
                restart.pmmout_btlfrac[1, usa_pr, j]
                + restart.pmmout_cbtlfrac[1, 1, usa_pr, j]
            )
            * restart.convfact_cfftliq[1, j]
            # BPU gasoline (pyrolysys)
            + restart.pmmftab_ubavolmg[usa_pr, j] * 5.763
            # Biobutanol
            + restart.lfmmout_rfbiobutecd[usa, j]
            * restart.convfact_cfbiobute[j]
        )
        # Convert from barrels per day to barrels
        * 0.365

        + ethanol_blended
    )

    if restart.ncntrl_curcalyr >= 1993:
        # Remove ethanol from the computed transportation C emissions for oil
        restart.emission_emtr[1, 0, j] -= (
            allrenewablegasoline * restart.emeblk_emgtr[j] / 1000.0
        )

    # For biodiesel calcs
    # Subtract out biodiesel used as blending stock. Includes soy, yellow
    # grease, white grease
    biodiesel = (
        (
            np.sum(restart.pmmrpt_bimqtycd[:, usa, j])
            + restart.pmmrpt_biodimp[usa, j] - restart.pmmrpt_biodexp[usa, j]
        )
        # Converting mbcd to tril btu
        * restart.convfact_cfbiod[j] * 365.0 / 1000.0
    )

    # Add other zero carbon diesel blendstocks
    allrenewable_diesel = (
        (
            # Non ester renewable diesel (NERD)
            restart.lfmmout_grd2dsqty[usa_pr, j] * restart.convfact_cfdsq
            # Stream 1
            + (
                restart.pmmout_btlfrac[2, usa_pr, j]
                + restart.pmmout_cbtlfrac[1, 2, usa_pr, j]
            )
            * restart.convfact_cfftliq[2, j]
            # Stream 2
            + (
                restart.pmmout_btlfrac[3, usa_pr, j]
                + restart.pmmout_cbtlfrac[1, 3, usa_pr, j]
            )
            * restart.convfact_cfftliq[3, j]
            # BPU diesel (pyrolysys)
            + restart.pmmftab_ubavolds[usa_pr, j] * 5.763
        )
        # From barrels per day to barrels
        * 0.365

        + biodiesel
    )

    # Adjust (for EMSOL) the computed transportation C emissions for oil
    restart.emission_emtr[1, 0, j] -= (
        allrenewable_diesel * restart.emeblk_edstr[j] / 1000.0
    )

    # Re-sum national transportation C emissions across all fuels
    restart.emission_emtrc[usa, 0, j] = np.sum(restart.emission_emtr[:4, 0, j])

    # National transportation CO2 emissions from motor gasoline
    restart.ghgrep_em_tran[0, usa, j] = (
        restart.qblk_qmgtr[usa, j] * restart.emeblk_emgtr[j]
    )

    # National transportation CO2 emissions from the gasoline part of E85
    restart.ghgrep_em_tran[1, usa, j] = (
        restart.qblk_qettr[usa, j] * restart.emeblk_emgtr[j]
        * e85_gasoline_share
    )

    # National transportation CO2 emissions adjustment for ethanol blending in
    # motor gasoline
    # Use the motor gasoline emissions factor emgtr because we're correcting
    # for an overestimate in the motor gasoline CO2 number
    restart.ghgrep_em_tran[2, usa, j] = (
        -allrenewablegasoline * restart.emeblk_emgtr[j]
    )

    # National transportation CO2 emissions from propane
    restart.ghgrep_em_tran[3, usa, j] = (
        restart.qblk_qprtr[usa, j] * restart.emeblk_eprtr[j]
    )

    # National transportation CO2 emissions from jet fuel
    restart.ghgrep_em_tran[4, usa, j] = (
        restart.qblk_qjftr[usa, j] * restart.emeblk_ejftr[j]
    )

    # National transportation CO2 emissions from distillate
    restart.ghgrep_em_tran[5, usa, j] = (
        restart.qblk_qdstr[usa, j] * restart.emeblk_edstr[j]
    )

    # National transportation CO2 emissions adjustment for biomass based diesel
    # blending in distillate
    # Use the distillate emissions factor edstr because we're correcting for an
    # overestimate in the distillate CO2 number
    restart.ghgrep_em_tran[6, usa, j] = (
        -allrenewable_diesel * restart.emeblk_edstr[j]
    )

    # National transportation CO2 emissions from residual fuel
    restart.ghgrep_em_tran[7, usa, j] = (
        restart.qblk_qrhtr[usa, j] * restart.emeblk_erhtr[j]  # High sulfur
        + restart.qblk_qrltr[usa, j] * restart.emeblk_erltr[j]  # Low sulfur
    )

    # National transportation CO2 emissions from other petroleum
    restart.ghgrep_em_tran[8, usa, j] = (
        restart.qblk_qottr[usa, j] * restart.emeblk_eottr[j]
    )

    # National transportation CO2 emissions from natural gas
    restart.ghgrep_em_tran[9, usa, j] = (
        (
            restart.qblk_qngtr[usa, j]
            # Subtract out renewable natural gas (biogenic)
            - restart.ngtdmrep_ogprrng[j] * restart.convfact_cfngc[j]
        ) * restart.emeblk_engtr[j]
        # Pipeline natural gas
        + restart.qblk_qgptr[usa, j] * restart.emeblk_egptr[j]
    )

    # National transportation CO2 emissions from methanol
    restart.ghgrep_em_tran[10, usa, j] = (
        restart.qblk_qmetr[usa, j] * restart.emeblk_emetr[j]
    )

    # National transportation CO2 emissions from purchased electricity
    # (initially just the sector share) (include charging of electric vehicles
    # in the residential and commercial sectors)
    zero = restart.qblk_qelas[:, j] == 0
    nonzero = np.logical_not(zero)
    restart.ghgrep_em_tran[11, nonzero, j] = (
        (
            restart.qblk_qeltr[nonzero, j]
            + qelrs_ev[nonzero] + qelcm_ev[nonzero]
        )
        / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_tran[11, zero, j] = 0.0

    # Regional transportation motor gasoline consumption
    restart.ghgrep_fl_tran[0, :, j] = restart.qblk_qmgtr[:, j]

    # Regional transportation gasoline consumption from the gasoline part of
    # E85
    restart.ghgrep_fl_tran[1, :, j] = (
        restart.qblk_qettr[:, j] * e85_gasoline_share
    )

    # Regional transportation adjustment to consumption for ethanol blending in
    # motor gasoline
    # Subtract blended ethanol (which is included in qmgtr) and use motor
    # gasoline consumption ratio to share to regions
    restart.ghgrep_fl_tran[2, :, j] = (
        -allrenewablegasoline
        * (restart.qblk_qmgtr[:, j] / restart.qblk_qmgtr[usa, j])
    )

    # Regional transportation propane consumption
    restart.ghgrep_fl_tran[3, :, j] = restart.qblk_qprtr[:, j]

    # Regional transportation jet fuel consumption
    restart.ghgrep_fl_tran[4, :, j] = restart.qblk_qjftr[:, j]

    # Regional transportation distillate consumption
    restart.ghgrep_fl_tran[5, :, j] = restart.qblk_qdstr[:, j]

    # Regional transportation adjustment to consumption due to biomass based
    # diesel blending in distillate
    # Use distillate consumption to share to regions
    restart.ghgrep_fl_tran[6, :, j] = (
        -allrenewable_diesel
        * (restart.qblk_qdstr[:, j] / restart.qblk_qdstr[usa, j])
    )

    # Regional transportation residual fuel consumption
    restart.ghgrep_fl_tran[7, :, j] = restart.qblk_qrstr[:, j]

    # Regional transportation other petroleum consumption
    restart.ghgrep_fl_tran[8, :, j] = restart.qblk_qottr[:, j]

    # Regional transportation natural gas consumption
    # Renewable natural gas can't be subtracted here as it's not regional
    restart.ghgrep_fl_tran[9, :, j] = (
        restart.qblk_qngtr[:, j]
        + restart.qblk_qgptr[:, j]  # Pipeline natural gas
    )

    # Regional transportation methanol consumption
    restart.ghgrep_fl_tran[10, :, j] = restart.qblk_qmetr[:, j]

    # Regional transportation purchased electricity
    restart.ghgrep_fl_tran[11, :, j] = (
        restart.qblk_qeltr[:, j] + qelrs_ev + qelcm_ev
    )

    # There are usually no California numbers until after msedyr, so estimate
    # them using the Pacific numbers scaled by appropriate ratios from SEDS
    if restart.ncntrl_curiyr <= restart.parametr_msedyr:

        # California transportation motor gasoline consumption
        if restart.qsblk_qsmgtr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[0, cali, j] = (
                restart.qsblk_qsmgtr[cali, j] * restart.qblk_qmgtr[pacific, j]
                / restart.qsblk_qsmgtr[pacific, j]
            )

        # California transportation gasoline consumption due to gasoline part
        # of E85
        # For gasoline part of E85, scale the Pacific number by California's
        # share of total Pacific motor gas consumption in SEDS, since SEDS does
        # not have consumption of E85 (qsettr == 0 at every index)
        if restart.qsblk_qsmgtr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[1, cali, j] = (
                restart.ghgrep_fl_tran[1, pacific, j]
                * restart.qsblk_qsmgtr[cali, j]
                / restart.qsblk_qsmgtr[pacific, j]
            )

        # California transportation adjustment to consumption for ethanol
        # blending in motor gasoline
        # For ethanol blending adjustment, scale the Pacific number by
        # California's share of total Pacific motor gas consumption in SEDS
        if restart.qsblk_qsmgtr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[2, cali, j] = (
                restart.ghgrep_fl_tran[2, pacific, j]
                * restart.qsblk_qsmgtr[cali, j]
                / restart.qsblk_qsmgtr[pacific, j]
            )

        # California transportation propane consumption
        # For propane, use the SEDS LPG array instead of the propane array,
        # since transportation LPG is only propane and the LPG history goes
        # back farther
        if restart.qsblk_qslgtr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[3, cali, j] = (
                restart.qsblk_qslgtr[cali, j] * restart.qblk_qlgtr[pacific, j]
                / restart.qsblk_qslgtr[pacific, j]
            )

        # California transportation jet fuel consumption
        if restart.qsblk_qsjftr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[4, cali, j] = (
                restart.qsblk_qsjftr[cali, j] * restart.qblk_qjftr[pacific, j]
                / restart.qsblk_qsjftr[pacific, j]
            )

        # California transportation distillate consumption
        if restart.qsblk_qsdstr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[5, cali, j] = (
                restart.qsblk_qsdstr[cali, j] * restart.qblk_qdstr[pacific, j]
                / restart.qsblk_qsdstr[pacific, j]
            )

        # California transportation adjustment to consumption for biomass based
        # diesel blending in distillate
        # For the bio-diesel blending adjustment, scale the Pacific number by
        # California's share of total Pacific distillate consumption in SEDS
        if restart.qsblk_qsdstr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[6, cali, j] = (
                restart.ghgrep_fl_tran[6, pacific, j]
                * restart.qsblk_qsdstr[cali, j]
                / restart.qsblk_qsdstr[pacific, j]
            )

        # California transportation residual fuel consumption
        if restart.qsblk_qsrstr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[7, cali, j] = (
                restart.qsblk_qsrstr[cali, j] * restart.qblk_qrstr[pacific, j]
                / restart.qsblk_qsrstr[pacific, j]
            )

        # California transportation other petroleum consumption
        if restart.qsblk_qsottr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[8, cali, j] = (
                restart.qsblk_qsottr[cali, j] * restart.qblk_qottr[pacific, j]
                / restart.qsblk_qsottr[pacific, j]
            )

        # California transportation natural gas consumption
        if (
            restart.qsblk_qsngtr[pacific, j] + restart.qsblk_qsgptr[pacific, j]
            != 0.0
        ):
            restart.ghgrep_fl_tran[9, cali, j] = (
                (restart.qsblk_qsngtr[cali, j] + restart.qsblk_qsgptr[cali, j])
                * (
                    restart.qblk_qngtr[pacific, j]
                    + restart.qblk_qgptr[pacific, j]
                ) / (
                    restart.qsblk_qsngtr[pacific, j]
                    + restart.qsblk_qsgptr[pacific, j]
                )
            )

        # California transportation methanol consumption
        if restart.qsblk_qsmetr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[10, cali, j] = (
                restart.qsblk_qsmetr[cali, j] * restart.qblk_qmetr[pacific, j]
                / restart.qsblk_qsmetr[pacific, j]
            )

        # California transportation purchased electricity (including charging
        # of electric vehicles in the residential and commercial sectors)
        if restart.qsblk_qseltr[pacific, j] != 0.0:
            restart.ghgrep_fl_tran[11, cali, j] = (
                restart.qsblk_qseltr[cali, j]
                * (
                    restart.qblk_qeltr[pacific, j]
                    + qelrs_ev[pacific] + qelcm_ev[pacific]
                )
                / restart.qsblk_qseltr[pacific, j]
            )

    # Names of the transportation sector fuels (plus electricity)
    nm_tran[:] = [
        "Motor Gasoline",
        "E85-Gasoline Part",
        "Ethanol Blending Adj.",
        "Propane",
        "Jet Fuel",
        "Distillate",
        "Biomass based diesel Blending Adj.",
        "Residual",
        "Other Petroleum",
        "Natural Gas",
        "Methanol",
        "Electricity",
    ]

    return float(e85_ethanol_share), float(e85_gasoline_share)


def sum_emissions_electricity(
    restart: Restart,
    variables: Variables,
    j: int,
    jcalyr: int,
    nm_elec: list[str]
) -> None:
    """Compute the initial electric power sector emissions for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    variables : Variables
        The active intermediate variables for EPM.
    j : int
        Compute emissions for this zero-based year index.
    jcalyr : int
        Four-digit calendar year corresponding to year index `j`.
    nm_elec : list[str]
        List to store the names of the electric power sector fuel indicies.

    See Also
    --------
    sum_emissions : Calls this function for electric power sector emissions.
    """
    # Zero-based Python indices
    usa = restart.parametr_mnumcr - 1
    cali = restart.parametr_mnumcr - 2
    pacific = restart.parametr_mnumcr - 3
    usa_nr = restart.parametr_mnumnr - 1
    base_yr = BASE_YR - 1
    usa_pr = restart.parametr_mnumpr - 1
    i_year = restart.ncntrl_curiyr - 1

    # Clear electricity C emissions for all fuels
    # First index of emel is fuel: 0=ngas, 1=oil, 2=coal, 3=geo+msw+beccs
    restart.emission_emel[:, 0, j] = 0.0
    # Clear national electricity C emissions
    restart.emission_emelc[usa, 0, j] = 0.0

    # Electricity emissions are adjusted for any carbon sequestration--usually
    # occurs in carbon-constrained cases.
    # Sum electricity C emissions from oil across all regions
    mnumcr2 = restart.parametr_mnumcr - 2

    restart.emission_emel[1, 0, j] += 0.001 * np.sum(
        # Electricity C emissions from distillate
        restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
        * (1.0 - restart.uefdout_xdsel[:mnumcr2, j])
        # Electricity C emissions from residual fuel, low sulfur
        + restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
        * (1.0 - restart.uefdout_xrlel[:mnumcr2, j])
        # Electricity C emissions from residual fuel, high sulfur
        + restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
        * (1.0 - restart.uefdout_xrhel[:mnumcr2, j])
        # Electricity C emissions from petroleum coke
        + restart.qblk_qpcel[:mnumcr2, j] * restart.emeblk_epcel[j]
    )

    # Sum electricity C emissions from coal across coal demand regions
    restart.emission_emel[2, 0, j] += 0.001 * np.sum(
        restart.coalemm_qclclnr[:, j, :] * restart.coalemm_cclclnr[:, j, :]
        * (1.0 - restart.coalemm_xclclnr[:, j, :])
    )

    log_parts = []
    if restart.ncntrl_curitr == 1:
        log_parts.append("curitr = 1\n")
    log_parts.append(
        f"Average electric power coal CO2 emission factor in {jcalyr}: "
    )
    log_parts.append(
        1000.0 * restart.emission_emel[2, 0, j] / restart.qblk_qclel[usa, j]
    )
    log_it(*log_parts, sep="")

    # Sum electricity C emissions from natural gas across natural gas
    # electricity regions
    restart.emission_emel[0, 0, j] += 0.001 * np.sum(
        # Electricity C emissions from firm natural gas consumption by NGTDM
        restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
        * (1.0 - restart.uefdout_xqgfelgr[:, j])
        # Electricity C emissions from interruptible natural gas consumption by
        # NGTDM
        + restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
        * (1.0 - restart.uefdout_xqgielgr[:, j])
        # Electricity C emissions from competitive natural gas consumption by
        # NGTDM
        + restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
        * (1.0 - restart.uefdout_xqgcelgr[:, j])
    )

    # Geothermal
    # 7.5296 from ghg pub xls file param08.xls, range EF_GEO, times 12/44.
    # based on btu production (generation)
    egeel = 3.412 * 2.0535  # Temp. variable for geothermal emissions factor
    geogen_bkwh = (
        np.sum(restart.uefdout_ugngenr[:, usa_nr, j])
        + np.sum(restart.cogen_cgntgen[usa_nr, j, 4, :]) * 0.001  # bkWh
    )
    geogen_bkwh_hist = (
        np.sum(restart.uefdout_ugngenr[:, usa_nr, base_yr])
        + np.sum(restart.cogen_cgntgen[usa_nr, base_yr, 4, :]) * 0.001  # bkWh
    )

    # Other: geothermal and non-biogenic waste energy
    # Set up emission factor for non-biogenic waste energy using GHG history
    # and bio waste-energy consumption.
    restart.emeblk_emsel[j] = (
        (
            variables.history[3, 4, base_yr] * 1000.0
            - geogen_bkwh_hist * egeel
        )
        / (restart.wrenew_wncmsel[base_yr, usa] * 1000.0)
    )

    # Electricity C emissions from geothermal, non-biogenic waste, and
    # bioenergy carbon capture and storage (BECCS)
    restart.emission_emel[3, 0, j] = (
        # Geothermal
        geogen_bkwh * egeel / 1000.0
        # Municipal solid waste (MSW)
        + restart.wrenew_wncmsel[max(base_yr, j), usa]
        * restart.emeblk_emsel[j]
        # BECCS (eligible for 45Q, index 4 indicates BECCS)
        - restart.ccatsdat_sup_emm_45q[4, usa, j] * (12.0 / 44.0) / 1e6
        # BECCS (no tax credit, index 4 indicates BECCS)
        - restart.ccatsdat_sup_emm_ntc[4, usa, j] * (12.0 / 44.0) / 1e6
    )
    restart.emission_emelc[usa, 0, j] = np.sum(restart.emission_emel[:, 0, j])

    # Compute carbon capture and storage numbers as we go along
    variables.ccsblk_ccs[:, :] = 0.0

    restart.ghgrep_em_elec[:5, usa, j] = 0.0
    mnumcr2 = restart.parametr_mnumcr - 2

    temp_reg = (
        restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
        * (1.0 - restart.uefdout_xdsel[:mnumcr2, j])
    )

    # National electricity CO2 emissions from distillate
    restart.ghgrep_em_elec[0, usa, j] += np.sum(
        restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
        * (1.0 - restart.uefdout_xdsel[:mnumcr2, j])
    )

    variables.ccsblk_ccs[0, :mnumcr2] += (
        restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
    )
    variables.ccsblk_ccs[0, usa_nr] += np.sum(
        restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
    )
    variables.ccsblk_ccs[0, :mnumcr2] -= temp_reg
    variables.ccsblk_ccs[0, usa_nr] -= restart.ghgrep_em_elec[0, usa, j]

    temp_reg = (
        restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
        * (1.0 - restart.uefdout_xrlel[:mnumcr2, j])

        + restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
        * (1.0 - restart.uefdout_xrhel[:mnumcr2, j])
    )

    # National electricity CO2 emissions from residual fuel
    restart.ghgrep_em_elec[1, usa, j] += np.sum(
        # Low sulfur
        restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
        * (1.0 - restart.uefdout_xrlel[:mnumcr2, j])
        # High sulfur
        + restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
        * (1.0 - restart.uefdout_xrhel[:mnumcr2, j])
    )

    variables.ccsblk_ccs[1, :mnumcr2] = (
        restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
        + restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
    )
    variables.ccsblk_ccs[1, usa_nr] = np.sum(
        restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
        + restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
    )
    variables.ccsblk_ccs[1, :mnumcr2] -= temp_reg
    variables.ccsblk_ccs[1, usa_nr] -= restart.ghgrep_em_elec[1, usa, j]

    # National electricity CO2 emissions from petroleum coke
    restart.ghgrep_em_elec[2, usa, j] += np.sum(
        restart.qblk_qpcel[:mnumcr2, j] * restart.emeblk_epcel[j]
    )

    # When qclclnr[] is positive and cclclnr[] is zero, set cclclnr[] to 25.5.
    # This should be removed when cclclnr[] has been properly populated by the
    # coal model.
    coal_oops_format = "EPM_COAL_CARBON_OOPS" + 6 * ":{:4d}" + 3 * ":{:21.6f}"
    for i in range(restart.coalemm_ndrgg):
        for k in range(restart.coalemm_nclut1):
            if (
                restart.coalemm_qclclnr[i, j, k] > 0.00001
                and restart.coalemm_cclclnr[i, j, k] < 0.001
            ):
                coal_oops_message = coal_oops_format.format(
                    restart.ncntrl_curirun,
                    restart.ncntrl_curiyr + restart.parametr_baseyr - 1,
                    restart.ncntrl_curitr,
                    i + 1, j + 1, k + 1,
                    restart.coalemm_qclclnr[i, j, k],
                    restart.coalemm_xclclnr[i, j, k],
                    restart.coalemm_cclclnr[i, j, k],
                )
                print_it(restart.ncntrl_curirun, coal_oops_message)
                restart.coalemm_cclclnr[i, j, k] = 25.5

    ndrgg = restart.coalemm_ndrgg
    temp_reg = np.sum(
        (
            restart.coalemm_qclclnr[:, j, :] * restart.coalemm_cclclnr[:, j, :]
            * (1.0 - restart.coalemm_xclclnr[:, j, :])
        ),
        axis=-1
    )

    # National electricity CO2 emissions from steam coal
    restart.ghgrep_em_elec[3, usa, j] += np.sum(
        restart.coalemm_qclclnr[:, j, :] * restart.coalemm_cclclnr[:, j, :]
        * (1.0 - restart.coalemm_xclclnr[:, j, :])
    )

    variables.ccsblk_ccs[2, :ndrgg] += np.sum(
        restart.coalemm_qclclnr[:, j, :] * restart.coalemm_cclclnr[:, j, :],
        axis=-1
    )
    variables.ccsblk_ccs[2, usa_nr] += np.sum(
        restart.coalemm_qclclnr[:, j, :] * restart.coalemm_cclclnr[:, j, :]
    )
    variables.ccsblk_ccs[2, :ndrgg] -= temp_reg

    # Since qclclnr is now 0 before 2009, we do this to compensate
    # Though we use eclel, which is hard-wired in the sum_emissions() function
    # and not read in from EPMDATA
    if restart.ncntrl_curiyr <= restart.parametr_msedyr:
        variables.ccsblk_ccs[2, usa_nr] = 0.0

        # Regional electricity CO2 emissions from steam coal
        restart.ghgrep_em_elec[3, :, j] = (
            restart.qblk_qclel[:, j] * restart.emeblk_eclel[j]
        )

        variables.ccsblk_ccs[2, usa_nr] += np.sum(
            restart.qblk_qclel[:, j] * restart.emeblk_eclel[j]
        )
        variables.ccsblk_ccs[2, usa_nr] = restart.ghgrep_em_elec[3, usa, j]
    variables.ccsblk_ccs[2, usa_nr] -= restart.ghgrep_em_elec[3, usa, j]

    nngem = restart.parametr_nngem
    temp_reg = (
        restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
        * (1.0 - restart.uefdout_xqgfelgr[:, j])

        + restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
        * (1.0 - restart.uefdout_xqgielgr[:, j])

        + restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
        * (1.0 - restart.uefdout_xqgcelgr[:, j])
    )

    # National electricity CO2 emissions from natural gas
    restart.ghgrep_em_elec[4, usa, j] += np.sum(
        # Firm
        restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
        * (1.0 - restart.uefdout_xqgfelgr[:, j])
        # Interruptible
        + restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
        * (1.0 - restart.uefdout_xqgielgr[:, j])
        # Competitive
        + restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
        * (1.0 - restart.uefdout_xqgcelgr[:, j])
    )

    variables.ccsblk_ccs[3, :nngem] = (
        restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
        + restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
        + restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
    )
    variables.ccsblk_ccs[3, usa_nr] += np.sum(
        restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
        + restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
        + restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
    )
    variables.ccsblk_ccs[3, :nngem] -= temp_reg
    variables.ccsblk_ccs[3, usa_nr] -= restart.ghgrep_em_elec[4, usa, j]

    # National electricity CO2 emissions from municipal solid waste (MSW)
    restart.ghgrep_em_elec[5, usa, j] = (
        1000.0 * restart.wrenew_wncmsel[max(base_yr, j), usa]
        * restart.emeblk_emsel[j]
    )

    # National electricity CO2 emissions from geothermal
    restart.ghgrep_em_elec[6, usa, j] = geogen_bkwh * egeel

    # National electricity CO2 emissions from bioenergy carbon capture and
    # storage (net negative emissions by assumption)
    restart.ghgrep_em_elec[7, usa, j] = (
        # BECCS (eligible for 45Q, index 4 indicates BECCS) (convert to 1000s
        # of metric tons of carbon)
        - restart.ccatsdat_sup_emm_45q[4, usa, j] * (12.0 / 44.0) / 1000.0
        # BECCS (no tax credit, index 4 indicates BECCS) (convert to 1000s of
        # metric tons of carbon)
        - restart.ccatsdat_sup_emm_ntc[4, usa, j] * (12.0 / 44.0) / 1000.0
    )
    variables.ccsblk_ccs[4, :mnumcr2] = (
        (
            restart.ccatsdat_sup_emm_45q[4, :mnumcr2, j]
            + restart.ccatsdat_sup_emm_ntc[4, :mnumcr2, j]
        ) * (12.0 / 44.0) / 1000.0
    )
    variables.ccsblk_ccs[4, usa_nr] = np.sum(variables.ccsblk_ccs[4, :mnumcr2])

    # National electricity CO2 emissions from electric power total
    restart.ghgrep_em_elec[8, usa, j] = np.sum(
        restart.ghgrep_em_elec[:8, usa, j]
    )

    # Eliminate small values due to rounding error
    small = np.abs(variables.ccsblk_ccs) < 0.05
    small[5, :] = False  # Exclude the final row
    variables.ccsblk_ccs[small] = 0.0

    # Total
    variables.ccsblk_ccs[5, :] = np.sum(variables.ccsblk_ccs[:5, :], axis=0)
    variables.ccsblk_ccs *= 0.001

    # emcarbon not previously used, but is convenient for holding this new
    # variable for ftab.
    restart.emission_emcarbon[:, :, j] = variables.ccsblk_ccs[:, :]

    if restart.ncntrl_ncrl == 1:
        log_table_title = f"jcalyr = {jcalyr}"
        log_table_headers = ["i_fuel", "ccs_pmm", "ccs", "em_elec"]
        log_table_n_rows = max(
            restart.emission_ccs_pmm.shape[0],
            variables.ccsblk_ccs.shape[0],
            restart.ghgrep_em_elec.shape[0],
        )
        log_table_data = []
        for i_fuel in range(log_table_n_rows):
            ccs_pmm, ccs, em_elec = None, None, None
            if i_fuel < restart.emission_ccs_pmm.shape[0]:
                ccs_pmm = restart.emission_ccs_pmm[i_fuel, usa_pr, i_year]
            if i_fuel < variables.ccsblk_ccs.shape[0]:
                ccs = variables.ccsblk_ccs[i_fuel, usa_nr]
            if i_fuel < restart.ghgrep_em_elec.shape[0]:
                em_elec = restart.ghgrep_em_elec[i_fuel, usa, j] * 1e-3
            log_table_data.append([i_fuel, ccs_pmm, ccs, em_elec])
        log_it(
            log_table_title, "=" * len(log_table_title),
            tabulate.tabulate(log_table_data, headers=log_table_headers),
        )

    # Regional electricity distillate consumption
    restart.ghgrep_fl_elec[0, :, j] = restart.qblk_qdsel[:, j]

    # Regional electricity residual fuel consumption
    restart.ghgrep_fl_elec[1, :, j] = (
        restart.qblk_qrlel[:, j] + restart.qblk_qrhel[:, j]
    )

    # Regional electricity petroleum coke consumption
    restart.ghgrep_fl_elec[2, :, j] = restart.qblk_qpcel[:, j]

    # Regional electricity steam coal consumption
    restart.ghgrep_fl_elec[3, :, j] = restart.qblk_qclel[:, j]

    # Regional electricity natural gas consumption
    restart.ghgrep_fl_elec[4, :, j] = restart.qblk_qngel[:, j]

    # Regional electricity municipal solid waste (MSW) consumption
    restart.ghgrep_fl_elec[5, :, j] = (
        1000.0 * restart.wrenew_wncmsel[max(base_yr, j), :]
    )

    # Regional electricity geothermal consumption
    restart.ghgrep_fl_elec[6, :, j] = restart.qblk_qgeel[:, j]

    # Regional electricity biomass consumption to associate with bioenergy
    # carbon capture and storage
    restart.ghgrep_fl_elec[7, :, j] = restart.qblk_qbmel[:, j]

    # There are usually no California numbers until after msedyr, so estimate
    # them using the Pacific numbers scaled by appropriate ratios from SEDS
    if restart.ncntrl_curiyr <= restart.parametr_msedyr:

        # California electricity distillate consumption
        if restart.qsblk_qsdsel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[0, cali, j] = (
                restart.qsblk_qsdsel[cali, j] * restart.qblk_qdsel[pacific, j]
                / restart.qsblk_qsdsel[pacific, j]
            )

        # California electricity residual fuel consumption
        if restart.qsblk_qsrsel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[1, cali, j] = (
                restart.qsblk_qsrsel[cali, j] * restart.qblk_qrsel[pacific, j]
                / restart.qsblk_qsrsel[pacific, j]
            )

        # California electricity petroleum coke consumption
        if restart.qsblk_qspcel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[2, cali, j] = (
                restart.qsblk_qspcel[cali, j] * restart.qblk_qpcel[pacific, j]
                / restart.qsblk_qspcel[pacific, j]
            )

        # California electricity steam coal consumption
        if restart.qsblk_qsclel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[3, cali, j] = (
                restart.qsblk_qsclel[cali, j] * restart.qblk_qclel[pacific, j]
                / restart.qsblk_qsclel[pacific, j]
            )

        # California electricity natural gas consumption
        if restart.qsblk_qsngel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[4, cali, j] = (
                restart.qsblk_qsngel[cali, j] * restart.qblk_qngel[pacific, j]
                / restart.qsblk_qsngel[pacific, j]
            )

        # California electricity municipal solid waste (MSW) consumption
        # For MSW, scale the Pacific non-bio MSW consumption (converted from
        # quads to trills) by California's share of total biomass consumption
        # in SEDS
        if restart.qsblk_qsbmel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[5, cali, j] = (
                (restart.wrenew_wncmsel[j, pacific] * 1000.0)
                * restart.qsblk_qsbmel[cali, j]
                / restart.qsblk_qsbmel[pacific, j]
            )

        # California electricity geothermal consumption
        if restart.qsblk_qsgeel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[6, cali, j] = (
                restart.qsblk_qsgeel[cali, j] * restart.qblk_qgeel[pacific, j]
                / restart.qsblk_qsgeel[pacific, j]
            )

        # California electricity biomass consumption to associate with
        # bioenergy carbon capture and storage
        if restart.qsblk_qsbmel[pacific, j] != 0.0:
            restart.ghgrep_fl_elec[7, cali, j] = (
                restart.qsblk_qsbmel[cali, j] * restart.qblk_qbmel[pacific, j]
                / restart.qsblk_qsbmel[pacific, j]
            )

    # Regional electric power totals
    restart.ghgrep_fl_elec[8, :, j] = np.sum(
        restart.ghgrep_fl_elec[:8, :, j], axis=0
    )

    # Names of the electricity sector fuels (plus the total)
    nm_elec[:] = [
        "Distillate",
        "Residual",
        "Petroleum Coke",
        "Steam Coal",
        "Natural Gas",
        "Municipal Solid Waste",
        "Geothermal",
        "Bioenergy Carbon Capture and Storage",
        "  Total Electric Power",
    ]

    # Start by zeroing out the regional electricity captured carbon
    restart.ghgrep_cc_elec[:, :, j] = 0.0

    # Regional electricity carbon capture from distillate
    # Zero-based index = 0
    if restart.ghgrep_fl_elec[0, usa, j] != 0.0:
        restart.ghgrep_cc_elec[0, :, j] += (
            np.sum(
                restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
                * restart.uefdout_xdsel[:mnumcr2, j]
            )
            * restart.ghgrep_fl_elec[0, :, j]
            / restart.ghgrep_fl_elec[0, usa, j]
        )

    # Regional electricity carbon capture from residual fuel
    # Zero-based index = 1
    if restart.ghgrep_fl_elec[1, usa, j] != 0.0:
        # Low sulfur residual fuel
        restart.ghgrep_cc_elec[1, :, j] += (
            np.sum(
                restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
                * restart.uefdout_xrlel[:mnumcr2, j]
            )
            * restart.ghgrep_fl_elec[1, :, j]
            / restart.ghgrep_fl_elec[1, usa, j]
        )
        # High sulfur residual fuel
        restart.ghgrep_cc_elec[1, :, j] += (
            np.sum(
                restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
                * restart.uefdout_xrhel[:mnumcr2, j]
            )
            * restart.ghgrep_fl_elec[1, :, j]
            / restart.ghgrep_fl_elec[1, usa, j]
        )

    # Regional electricity carbon capture from steam coal
    # Zero-based index = 2
    if restart.ghgrep_fl_elec[3, usa, j] != 0.0:
        restart.ghgrep_cc_elec[2, :, j] += (
            np.sum(
                restart.coalemm_qclclnr[:, j, :]
                * restart.coalemm_cclclnr[:, j, :]
                * restart.coalemm_xclclnr[:, j, :]
            )
            * restart.ghgrep_fl_elec[3, :, j]
            / restart.ghgrep_fl_elec[3, usa, j]
        )

    # Regional electricity carbon capture from natural gas
    # Zero-based index = 3
    if restart.ghgrep_fl_elec[4, usa, j] != 0.0:
        # Natural gas (firm)
        restart.ghgrep_cc_elec[3, :, j] += (
            np.sum(
                restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
                * restart.uefdout_xqgfelgr[:, j]
            )
            * restart.ghgrep_fl_elec[4, :, j]
            / restart.ghgrep_fl_elec[4, usa, j]
        )
        # Natural gas (interruptible)
        restart.ghgrep_cc_elec[3, :, j] += (
            np.sum(
                restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
                * restart.uefdout_xqgielgr[:, j]
            )
            * restart.ghgrep_fl_elec[4, :, j]
            / restart.ghgrep_fl_elec[4, usa, j]
        )
        # Natural gas (competitive)
        restart.ghgrep_cc_elec[3, :, j] += (
            np.sum(
                restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
                * restart.uefdout_xqgcelgr[:, j]
            )
            * restart.ghgrep_fl_elec[4, :, j]
            / restart.ghgrep_fl_elec[4, usa, j]
        )

    # Regional electricity carbon capture from bioenergy (BECCS)
    # Zero-based index = 4
    if restart.ghgrep_fl_elec[7, usa, j] != 0.0:
        restart.ghgrep_cc_elec[4, :, j] += (
            (
                restart.ccatsdat_sup_emm_45q[4, :, j]
                + restart.ccatsdat_sup_emm_ntc[4, :, j]
            ) * (12.0 / 44.0) / 1000.0
            * restart.ghgrep_fl_elec[7, :, j]
            / restart.ghgrep_fl_elec[7, usa, j]
        )


def sum_emissions_regional(
    restart: Restart,
    j: int,
) -> None:
    """Do the regional apportionment of emissions for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Apportion regional emissions for this zero-based year index.

    See Also
    --------
    sum_emissions : Calls this function for regional emissions apportionment.
    """
    # Zero-based Python region index
    usa = restart.parametr_mnumcr - 1

    # Clear residential sector CO2 numbers except for purchased electricity and
    # the national totals
    restart.ghgrep_em_resd[:-1, :-1, j] = 0.0

    # Compute regional emissions for electricity sector
    for ifl in range(restart.ghgrep_etot - 1):
        if restart.ghgrep_fl_elec[ifl, usa, j] != 0.0:
            restart.ghgrep_em_elec[ifl, :, j] = (
                restart.ghgrep_em_elec[ifl, usa, j]
                * restart.ghgrep_fl_elec[ifl, :, j]
                / restart.ghgrep_fl_elec[ifl, usa, j]
            )
        else:
            restart.ghgrep_em_elec[ifl, :, j] = 0.0

    # Compute regional emissions for residential sector
    for ifl in range(restart.ghgrep_iel_r - 1):
        if restart.ghgrep_fl_resd[ifl, usa, j] != 0.0:
            restart.ghgrep_em_resd[ifl, :, j] = (
                restart.ghgrep_em_resd[ifl, usa, j]
                * restart.ghgrep_fl_resd[ifl, :, j]
                / restart.ghgrep_fl_resd[ifl, usa, j]
            )
        else:
            restart.ghgrep_em_resd[ifl, :, j] = 0.0

    # Compute regional emissions for commerical sector
    for ifl in range(restart.ghgrep_iel_c - 1):
        if restart.ghgrep_fl_comm[ifl, usa, j] != 0.0:
            restart.ghgrep_em_comm[ifl, :, j] = (
                restart.ghgrep_em_comm[ifl, usa, j]
                * restart.ghgrep_fl_comm[ifl, :, j]
                / restart.ghgrep_fl_comm[ifl, usa, j]
            )
        else:
            restart.ghgrep_em_comm[ifl, :, j] = 0.0

    # Compute regional emissions for industrial sector
    for ifl in range(restart.ghgrep_iel_i - 1):
        if restart.ghgrep_fl_indy[ifl, usa, j] != 0.0:
            restart.ghgrep_em_indy[ifl, :, j] = (
                restart.ghgrep_em_indy[ifl, usa, j]
                * restart.ghgrep_fl_indy[ifl, :, j]
                / restart.ghgrep_fl_indy[ifl, usa, j]
            )
        else:
            restart.ghgrep_em_indy[ifl, :, j] = 0.0

    # Compute regional emissions for transportation sector
    for ifl in range(restart.ghgrep_iel_t - 1):
        if restart.ghgrep_fl_tran[ifl, usa, j] != 0.0:
            restart.ghgrep_em_tran[ifl, :, j] = (
                restart.ghgrep_em_tran[ifl, usa, j]
                * restart.ghgrep_fl_tran[ifl, :, j]
                / restart.ghgrep_fl_tran[ifl, usa, j]
            )
        else:
            restart.ghgrep_em_tran[ifl, :, j] = 0.0


def sum_emissions_california_biofuels(
    restart: Restart,
    j: int,
    e85_ethanol_share: float,
    e85_gasoline_share: float
) -> None:
    """Adjust the California biofuels emissions numbers for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Adjust California biofuels emissions for this zero-based year index.
    e85_ethanol_share : float
        Fraction of all E85 consumed in year index `j` that was ethanol. This
        value is computed and returned by the `sum_emissions_transportation`
        function.
    e85_gasoline_share : float
        Fraction of all E85 consumed in year index `j` that was gasoline. This
        value is computed and returned by the `sum_emissions_transportation`
        function.

    See Also
    --------
    sum_emissions : Uses this function to adjust California biofuels emissions.
    sum_emissions_transportation : Returns `e85` values used by this function.
    """
    # Zero-based Python indices
    cali = restart.parametr_mnumcr - 2
    pacific = restart.parametr_mnumcr - 3
    i_year = restart.ncntrl_curiyr - 1
    cali_pr = 7 - 1  # restart.parametr_mnumpr - 4

    # Calculate California biofuels use and its CO2 emissions so it can adjust
    # the state's covered emissions:
    # Ethanol is used for E85 and blending with gasoline. For E85 (about 74
    # percent ethanol, 26 percent gasoline by volume), need to account for the
    # carbon in the gasoline component. The ethanol is assumed to be a zero net
    # carbon source. Subtract out renewable components used in gasoline blends
    # from the motor gasoline emissions. Total ethanol from refineries minus
    # e85 = ethanol for splash blending and for ETBE.

    # Total amount of ethanol consumed this year in the Pacific region
    ethanol_tot = (
        365.0 * restart.convfact_cfetq[j] * restart.calshr_ettr_shr[i_year]
        * (
            restart.pmmrpt_crnethcd[pacific, j]
            + restart.pmmrpt_cllethcd[pacific, j]
            + restart.pmmrpt_othethcd[pacific, j]
            + restart.pmmrpt_ethimp[pacific, j]
            - restart.pmmrpt_ethexp[pacific, j]
        ) / 1000.0
    )

    # Total amount of ethanol consumed as part of E85 this year in the Pacific
    # region
    ethanol_e85 = (
        restart.qblk_qettr[pacific, j] * restart.calshr_ettr_shr[i_year]
        * e85_ethanol_share
    )

    # Total amount of ethanol consumed outside of E85 this year in the Pacific
    # region
    ethanol_blended = ethanol_tot - ethanol_e85

    # Add other sources of renewable gasoline.
    # Refinery region 6 (cali_pr) is California.
    allrenewablegasoline = (
        (
            # Non ester renewable gasoline
            restart.lfmmout_grn2mgqty[cali_pr, j]
            # BTL gasoline
            + np.sum(restart.pmmout_btlfrac[:2, cali_pr, j])
            # BPU gasoline
            + restart.pmmftab_ubavolmg[cali_pr, j]
            # B part of CBTL gasoline
            + np.sum(restart.pmmout_cbtlfrac[1, :2, cali_pr, j])
        )
        # From million barrels per day to trill btu per year. Napthas
        * 0.365 * restart.convfact_cfnpq

        + ethanol_blended
    )

    # For biodiesel calcs
    # Subtract out biodiesel used as blending stock. Includes soy, yellow
    # grease, white grease. Assume all renewable diesel in pacific used in
    # California.
    biodiesel = (
        (
            np.sum(restart.pmmrpt_bimqtycd[:, pacific, j])
            + restart.pmmrpt_biodimp[pacific, j]
            - restart.pmmrpt_biodexp[pacific, j]
        )
        # Converting mbcd to tril btu
        * restart.convfact_cfbiod[j] * 365.0 / 1000.0
    )

    # Add other zero carbon diesel blendstocks
    allrenewable_diesel = (
        (
            # Non ester renewable diesel (NERD)
            restart.lfmmout_grd2dsqty[cali_pr, j]
            # BTL diesel
            + np.sum(restart.pmmout_btlfrac[2:, cali_pr, j])
            # BPU diesel
            + restart.pmmftab_ubavolds[cali_pr, j]
            # B part of CBTL diesel
            + np.sum(restart.pmmout_cbtlfrac[1, 2:, cali_pr, j])
        )
        # From million barrels per day to trill btu per year. Diesel
        * 0.365 * restart.convfact_cfdsq

        + biodiesel
    )

    # California transportation CO2 emissions adjustment for ethanol blending
    # in motor gasoline
    # Subtract blended ethanol (which is included in qmgtr) and use motor
    # gasoline consumption ratio to share to regions
    restart.ghgrep_em_tran[2, cali, i_year] = (
        -allrenewablegasoline * restart.emeblk_emgtr[j]
    )

    # California transportation CO2 emissions from propane
    if restart.ncntrl_curiyr > restart.parametr_msedyr:
        # Use qlgtr as no value for California in qprtr now
        restart.ghgrep_em_tran[3, cali, i_year] = (
            restart.qblk_qlgtr[cali, j] * restart.emeblk_eprtr[j]
        )

    # California transportation CO2 emissions from jet fuel
    # CA 2014 GHG inventory query tool says intrastate jet fuel CO2 emissions
    # were 3.554 and the total including int'l and interstate was 42.99. So
    # 8.25% is intrastate jet fuel, but it's not what LFMM uses, so we'll pass
    # back what they use and use it.
    restart.ghgrep_em_tran[4, cali, i_year] = (
        restart.ghgrep_em_tran[4, cali, i_year]
        * restart.lfmmout_ab32jetcover[j]
    )

    # California transportation CO2 emissions from adjustment for biomass based
    # diesel blending in distillate
    restart.ghgrep_em_tran[6, cali, i_year] = (
        -allrenewable_diesel * restart.emeblk_edstr[j]
    )


def sum_emissions_history_benchmark(
    restart: Restart,
    variables: Variables,
    j: int,
    iy: int
) -> None:
    """Benchmark the computed emissions values to history for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    variables : Variables
        The active intermediate variables for EPM.
    j : int
        Benchmark the emissions for this zero-based year index.
    iy : int
        Year index -- should be set to the same value as `j`.

    See Also
    --------
    sum_emissions : Uses this function for benchmarking in historical years.
    """
    # Zero-based Python region index
    usa = restart.parametr_mnumcr - 1

    # Residential sector: ngas
    restart.ghgrep_em_resd[4, usa, iy] += (
        1000.0 * variables.history[0, 0, j]
        - np.sum(restart.ghgrep_em_resd[4:5, usa, iy])
    )

    # Residential sector: oil (dist)
    restart.ghgrep_em_resd[1, usa, iy] += (
        1000.0 * variables.history[1, 0, j]
        - np.sum(restart.ghgrep_em_resd[0:3, usa, iy])
    )

    # Residential sector: coal
    restart.ghgrep_em_resd[3, usa, iy] += (
        1000.0 * variables.history[2, 0, j]
        - np.sum(restart.ghgrep_em_resd[3:4, usa, iy])
    )

    # Commercial sector: ngas
    restart.ghgrep_em_comm[6, usa, iy] += (
        1000.0 * variables.history[0, 1, j]
        - np.sum(restart.ghgrep_em_comm[6:7, usa, iy])
    )

    # Commercial sector: oil (dist)
    restart.ghgrep_em_comm[2, usa, iy] += (
        1000.0 * variables.history[1, 1, j]
        - np.sum(restart.ghgrep_em_comm[0:5, usa, iy])
    )

    # Commercial sector: coal
    restart.ghgrep_em_comm[5, usa, iy] += (
        1000.0 * variables.history[2, 1, j]
        - np.sum(restart.ghgrep_em_comm[5:6, usa, iy])
    )

    # Industrial sector: ngas
    restart.ghgrep_em_indy[13, usa, iy] += (
        1000.0 * variables.history[0, 2, j]
        - np.sum(restart.ghgrep_em_indy[13:18, usa, iy])
    )

    # Industrial sector: oil (other pet)
    restart.ghgrep_em_indy[9, usa, iy] += (
        1000.0 * variables.history[1, 2, j]
        - np.sum(restart.ghgrep_em_indy[0:10, usa, iy])
    )

    # Industrial sector: coal (stm coal)
    restart.ghgrep_em_indy[10, usa, iy] += (
        1000.0 * variables.history[2, 2, j]
        - np.sum(restart.ghgrep_em_indy[10:13, usa, iy])
    )

    # Transportation sector: ngas
    restart.ghgrep_em_tran[9, usa, iy] += (
        1000.0 * variables.history[0, 3, j]
        - np.sum(restart.ghgrep_em_tran[9:10, usa, iy])
    )

    # Transportation sector: oil (mogas)
    restart.ghgrep_em_tran[0, usa, iy] += (
        1000.0 * variables.history[1, 3, j]
        - np.sum(restart.ghgrep_em_tran[0:9, usa, iy])
    )

    # Electricity sector: ngas
    restart.ghgrep_em_elec[4, usa, iy] += (
        1000.0 * variables.history[0, 4, j]
        - np.sum(restart.ghgrep_em_elec[4:5, usa, iy])
    )

    # Electricity sector: oil (resid)
    restart.ghgrep_em_elec[1, usa, iy] += (
        1000.0 * variables.history[1, 4, j]
        - np.sum(restart.ghgrep_em_elec[0:3, usa, iy])
    )

    # Electricity sector: coal
    restart.ghgrep_em_elec[3, usa, iy] += (
        1000.0 * variables.history[2, 4, j]
        - np.sum(restart.ghgrep_em_elec[3:4, usa, iy])
    )

    # Electricity sector: geo+msw+beccs
    restart.ghgrep_em_elec[5, usa, iy] += (
        1000.0 * variables.history[3, 4, j]
        - np.sum(restart.ghgrep_em_elec[5:8, usa, iy])
    )

    # Scale regional values so they add up to the new national number.

    # Residential sector: ngas
    sumc = np.sum(restart.ghgrep_em_resd[4, :9, iy])
    restart.ghgrep_em_resd[4, :9, iy] *= (
        restart.ghgrep_em_resd[4, usa, iy] / sumc
    )

    # Residential sector: oil (dist)
    sumc = np.sum(restart.ghgrep_em_resd[1, :9, iy])
    restart.ghgrep_em_resd[1, :9, iy] *= (
        restart.ghgrep_em_resd[1, usa, iy] / sumc
    )

    # Residential sector: coal
    sumc = np.sum(restart.ghgrep_em_resd[3, :9, iy])
    restart.ghgrep_em_resd[3, :9, iy] = 0.0
    if sumc != 0.0:
        restart.ghgrep_em_resd[3, :9, iy] *= (
            restart.ghgrep_em_resd[3, usa, iy] / sumc
        )

    # Commercial sector: ngas
    sumc = np.sum(restart.ghgrep_em_comm[6, :9, iy])
    restart.ghgrep_em_comm[6, :9, iy] *= (
        restart.ghgrep_em_comm[6, usa, iy] / sumc
    )

    # Commercial sector: oil (dist)
    sumc = np.sum(restart.ghgrep_em_comm[2, :9, iy])
    restart.ghgrep_em_comm[2, :9, iy] *= (
        restart.ghgrep_em_comm[2, usa, iy] / sumc
    )

    # Commercial sector: coal
    sumc = np.sum(restart.ghgrep_em_comm[5, :9, iy])
    restart.ghgrep_em_comm[5, :9, iy] *= (
        restart.ghgrep_em_comm[5, usa, iy] / sumc
    )

    # Industrial sector: ngas
    sumc = np.sum(restart.ghgrep_em_indy[13, :9, iy])
    restart.ghgrep_em_indy[13, :9, iy] *= (
        restart.ghgrep_em_indy[13, usa, iy] / sumc
    )

    # Industrial sector: oil (other pet)
    sumc = np.sum(restart.ghgrep_em_indy[9, :9, iy])
    restart.ghgrep_em_indy[9, :9, iy] *= (
        restart.ghgrep_em_indy[9, usa, iy] / sumc
    )

    # Industrial sector: coal (stm coal)
    sumc = np.sum(restart.ghgrep_em_indy[10, :9, iy])
    restart.ghgrep_em_indy[10, :9, iy] *= (
        restart.ghgrep_em_indy[10, usa, iy] / sumc
    )

    # Transportation sector: ngas
    sumc = np.sum(restart.ghgrep_em_tran[9, :9, iy])
    restart.ghgrep_em_tran[9, :9, iy] *= (
        restart.ghgrep_em_tran[9, usa, iy] / sumc
    )

    # Transportation sector: oil (mogas)
    sumc = np.sum(restart.ghgrep_em_tran[0, :9, iy])
    restart.ghgrep_em_tran[0, :9, iy] *= (
        restart.ghgrep_em_tran[0, usa, iy] / sumc
    )

    # Electricity sector: ngas
    sumc = np.sum(restart.ghgrep_em_elec[4, :9, iy])
    if sumc != 0.0:
        restart.ghgrep_em_elec[4, :9, iy] *= (
            restart.ghgrep_em_elec[4, usa, iy] / sumc
        )

    # Electricity sector: oil (resid)
    sumc = np.sum(restart.ghgrep_em_elec[1, :9, iy])
    if sumc != 0.0:
        restart.ghgrep_em_elec[1, :9, iy] *= (
            restart.ghgrep_em_elec[1, usa, iy] / sumc
        )

    # Electricity sector: coal
    sumc = np.sum(restart.ghgrep_em_elec[3, :9, iy])
    if sumc != 0.0:
        restart.ghgrep_em_elec[3, :9, iy] *= (
            restart.ghgrep_em_elec[3, usa, iy] / sumc
        )

    # Electricity sector: geo+msw+beccs
    sumc = np.sum(restart.ghgrep_em_elec[5, :9, iy])
    if sumc != 0.0:
        restart.ghgrep_em_elec[5, :9, iy] *= (
            restart.ghgrep_em_elec[5, usa, iy] / sumc
        )


def sum_emissions_share_electricity(
    restart: Restart,
    j: int,
    qelrs_ev: npt.NDArray[np.float64],
    qelcm_ev: npt.NDArray[np.float64],
) -> None:
    """Share electric power sector emissions to sectors for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Share out emissions for this zero-based year index.
    qelrs_ev : npt.NDArray[np.float64]
        Electricity consumption by region transferred from the residential
        sector to the transportation sector because it was used to charge
        electric vehicles.
    qelcm_ev : npt.NDArray[np.float64]
        Electricity consumption by region transferred from the commercial
        sector to the transportation sector because it was used to charge
        electric vehicles.

    See Also
    --------
    sum_emissions : Calls this function to share out electricity emissions.
    sum_emissions_residential : Provides the value for the `qelrs_ev` argument.
    sum_emissions_commercial : Provides the value for the `qelcm_ev` argument.
    """
    # Zero-based Python region index
    usa = restart.parametr_mnumcr - 1

    # Share electricity emissions to sectors based on sales shares.
    # NOTE: Should EPM actually be writing values out to qblk here?
    restart.qblk_qelas[:, j] = (
        restart.qblk_qelrs[:, j] + restart.qblk_qelcm[:, j]
        + restart.qblk_qelin[:, j] + restart.qblk_qeltr[:, j]
    )

    # Total electricity sector emissions
    restart.ghgrep_em_elec[8, :, j] = np.sum(
        restart.ghgrep_em_elec[:8, :, j], axis=0
    )

    zero = restart.qblk_qelas[:, j] == 0
    nonzero = np.logical_not(zero)

    restart.ghgrep_em_resd[5, nonzero, j] = (
        restart.ghgrep_em_elec[8, nonzero, j]
        * (restart.qblk_qelrs[nonzero, j] - qelrs_ev[nonzero])
        / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_comm[7, nonzero, j] = (
        restart.ghgrep_em_elec[8, nonzero, j]
        * (restart.qblk_qelcm[nonzero, j] - qelcm_ev[nonzero])
        / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_indy[19, nonzero, j] = (
        restart.ghgrep_em_elec[8, nonzero, j]
        * restart.qblk_qelin[nonzero, j] / restart.qblk_qelas[nonzero, j]
    )
    restart.ghgrep_em_tran[11, nonzero, j] = (
        restart.ghgrep_em_elec[8, nonzero, j]
        * (
            restart.qblk_qeltr[nonzero, j]
            + qelrs_ev[nonzero] + qelcm_ev[nonzero]
        )
        / restart.qblk_qelas[nonzero, j]
    )

    restart.ghgrep_em_resd[5, zero, j] = 0.0
    restart.ghgrep_em_comm[7, zero, j] = 0.0
    restart.ghgrep_em_indy[19, zero, j] = 0.0
    restart.ghgrep_em_tran[11, zero, j] = 0.0

    restart.ghgrep_em_resd[5, usa, j] = np.sum(
        restart.ghgrep_em_resd[5, :9, j]
    )
    restart.ghgrep_em_comm[7, usa, j] = np.sum(
        restart.ghgrep_em_comm[7, :9, j]
    )
    restart.ghgrep_em_indy[19, usa, j] = np.sum(
        restart.ghgrep_em_indy[19, :9, j]
    )
    restart.ghgrep_em_tran[11, usa, j] = np.sum(
        restart.ghgrep_em_tran[11, :9, j]
    )

    # Adjust regional distribution of electricity emissions to sectors so that
    # that total across regions matches the US total for each sector.
    # See example in l:/main/dsa/epmreg/weighting_ex.xls
    adj_resd = (
        restart.ghgrep_em_resd[-1, usa, j]
        - np.sum(restart.ghgrep_em_resd[-1, :9, j])
    )
    adj_comm = (
        restart.ghgrep_em_comm[-1, usa, j]
        - np.sum(restart.ghgrep_em_comm[-1, :9, j])
    )
    adj_indy = (
        restart.ghgrep_em_indy[-1, usa, j]
        - np.sum(restart.ghgrep_em_indy[-1, :9, j])
    )
    adj_tran = (
        restart.ghgrep_em_tran[-1, usa, j]
        - np.sum(restart.ghgrep_em_tran[-1, :9, j])
    )

    mnumcr2 = restart.parametr_mnumcr - 2
    reg_shr = restart.qblk_qelas[:mnumcr2, j] / restart.qblk_qelas[usa, j]

    restart.ghgrep_em_resd[-1, :mnumcr2, j] += adj_resd * reg_shr
    restart.ghgrep_em_comm[-1, :mnumcr2, j] += adj_comm * reg_shr
    restart.ghgrep_em_indy[-1, :mnumcr2, j] += adj_indy * reg_shr
    restart.ghgrep_em_tran[-1, :mnumcr2, j] += adj_tran * reg_shr


def sum_emissions_history_overwrite(
    restart: Restart,
    variables: Variables,
    j: int
) -> None:
    """Overwrite computed emissions with history as needed by `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    variables : Variables
        The active intermediate variables for EPM.
    j : int
        Overwrite with history for this zero-based year index.

    See Also
    --------
    sum_emissions : Calls this function to overwrite emissions with history.
    """
    # Zero-based Python region index
    usa = restart.parametr_mnumcr - 1

    # Overwrite C emissions for all five sectors
    # Note that only emel has a nonzero values in history for the 4th fuel type
    restart.emission_emrs[:4, 0, j] = variables.history[:, 0, j]
    restart.emission_emcm[:4, 0, j] = variables.history[:, 1, j]
    restart.emission_eminc[:4, 0, j] = variables.history[:, 2, j]
    restart.emission_emtr[:4, 0, j] = variables.history[:, 3, j]
    restart.emission_emel[:4, 0, j] = variables.history[:, 4, j]

    # For history reporting, keep all NG use in the pipeline transportation
    # array.

    # Transportation C emissions from natural gas
    restart.emission_emtr[0, 0, j] = 0.0

    # National NGTDM C emissions
    restart.emission_emnt[usa, 0, j] = variables.history[0, 3, j] * 1000.0

    # National residential C emissions
    restart.emission_emrsc[usa, 0, j] = np.sum(
        restart.emission_emrs[0:3, 0, j]
    )

    # National commercial C emissions
    restart.emission_emcmc[usa, 0, j] = np.sum(
        restart.emission_emcm[0:3, 0, j]
    )

    # National industrial C emissions
    # TODO ADR. is `emincc` a bug?
    restart.emission_emincc[usa, 0, j] = np.sum(
        restart.emission_eminc[:3, 0, j]
    )

    # National transportation C emissions
    restart.emission_emtrc[usa, 0, j] = np.sum(restart.emission_emtr[:3, 0, j])

    # National electricity C emissions
    restart.emission_emelc[usa, 0, j] = np.sum(restart.emission_emel[:4, 0, j])


def sum_emissions_policy_totals(restart: Restart, j: int) -> None:
    """Add up the cap-and-trade policy CO2 totals for `sum_emissions`.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    j : int
        Compute emissions policy totals for this zero-based year index.

    See Also
    --------
    sum_emissions : Calls this function to get totals for emissions policies.
    """
    restart.emoblk_total_emissions[j] = 0.0

    # Zero-based Python region index
    usa = restart.parametr_mnumcr - 1

    # Use C emissions numbers from each sector

    if restart.emoblk_elec_flag:  # elec only
        restart.emoblk_total_emissions[j] = np.sum(
            restart.emission_emel[:, 0, j]  # elec
        )

    elif restart.emoblk_tran_flag:  # tran only
        restart.emoblk_total_emissions[j] = (
            np.sum(restart.emission_emtr[:2, 0, j])  # tran
            + restart.emission_emnt[usa, 0, j] / 1000.0  # tran
        )

    elif restart.emoblk_resd_flag:  # exclude resd
        restart.emoblk_total_emissions[j] = (
            np.sum(restart.emission_emcm[:3, 0, j])  # comm
            + np.sum(restart.emission_emtr[:2, 0, j])  # tran
            + restart.emission_emnt[usa, 0, j] / 1000.0  # tran
            + np.sum(restart.emission_eminc[:3, 0, j])  # indus
            + np.sum(restart.emission_emel[:, 0, j])  # elec
        )

    elif restart.emoblk_comm_flag:  # exclude comm
        restart.emoblk_total_emissions[j] = (
            np.sum(restart.emission_emrs[:3, 0, j])  # resd
            + np.sum(restart.emission_emtr[:2, 0, j])  # tran
            + restart.emission_emnt[usa, 0, j] / 1000.0  # tran
            + np.sum(restart.emission_eminc[:3, 0, j])  # indus
            + np.sum(restart.emission_emel[:, 0, j])  # elec
        )

    else:  # everything
        restart.emoblk_total_emissions[j] = (
            np.sum(restart.emission_emrs[:3, 0, j])  # resd
            + np.sum(restart.emission_emcm[:3, 0, j])  # comm
            + np.sum(restart.emission_emtr[:2, 0, j])  # tran
            + restart.emission_emnt[usa, 0, j] / 1000.0  # tran
            + np.sum(restart.emission_eminc[:3, 0, j])  # indus
            + np.sum(restart.emission_emel[:, 0, j])  # elec
        )


def sum_emissions_reporting_loop(
    restart: Restart,
    variables: Variables,
    nm_resd: list[str],
    nm_comm: list[str],
    nm_indy: list[str],
    nm_tran: list[str],
    nm_elec: list[str],
    regnam: list[str]
) -> None:
    """Handle the NEMS reporting loop output for `sum_emissions`.

    This part of the `sum_emissions` process uses repeated calls to the
    `demand_co2` function to write the final emissions values to a file in CSV
    format.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    variables : Variables
        The active intermediate variables for EPM.
    nm_resd : list[str]
        The names of the residential sector fuel indicies.
    nm_comm : list[str]
        The names of the commercial sector fuel indicies.
    nm_indy : list[str]
        The names of the industrial sector fuel indicies.
    nm_tran : list[str]
        The names of the transportation sector fuel indicies.
    nm_elec : list[str]
        The names of the electric power sector fuel indicies.
    regnam : list[str]
        The names of the NEMS regions by region index.

    See Also
    --------
    sum_emissions : Calls this function for output during the reporting loop.
    get_file_path : Used to find the path to the DEMAND_CO2 output file.
    demand_co2 : Used by this function to write out CO2 emissions values.
    """
    # Zero-based Python region index
    usa = restart.parametr_mnumcr - 1

    # Compute range of years to output, starting with 2004 for some reason
    n_years = (
        min(restart.ncntrl_ijumpcalyr, restart.ncntrl_lastcalyr) - 2004 + 1
    )
    y1 = 2004 - restart.parametr_baseyr
    y2 = y1 + n_years

    format_sum = '"{:s}"' + 2 * "," + n_years * ",{:.3f}" + ",\n"

    # Overwrite all zeros with +0.0 to avoid entries of -0.0 in output CSV
    restart.ghgrep_em_resd[restart.ghgrep_em_resd == 0.0] = +0.0
    restart.ghgrep_fl_resd[restart.ghgrep_fl_resd == 0.0] = +0.0
    restart.ghgrep_em_comm[restart.ghgrep_em_comm == 0.0] = +0.0
    restart.ghgrep_fl_comm[restart.ghgrep_fl_comm == 0.0] = +0.0
    restart.ghgrep_em_indy[restart.ghgrep_em_indy == 0.0] = +0.0
    restart.ghgrep_fl_indy[restart.ghgrep_fl_indy == 0.0] = +0.0
    restart.ghgrep_em_tran[restart.ghgrep_em_tran == 0.0] = +0.0
    restart.ghgrep_fl_tran[restart.ghgrep_fl_tran == 0.0] = +0.0
    restart.ghgrep_em_elec[restart.ghgrep_em_elec == 0.0] = +0.0
    restart.ghgrep_fl_elec[restart.ghgrep_fl_elec == 0.0] = +0.0

    demand_co2_path = get_file_path(variables, "demand_co2", output_file=True)
    iunit = demand_co2_path.open("w", encoding="utf-8")

    iunit.write(
        f'"{restart.ncntrl_scen.strip()}.{restart.ncntrl_date.strip()}"\n'
    )

    # Loop over all regions, but make sure to do the national region first
    regions = [usa]
    regions.extend(i for i in range(restart.parametr_mnumcr) if i != usa)

    for i in regions:

        if i == usa:
            iunit.write("National Totals\n")
        else:
            iunit.write(f"\n\n\n\nRegion {i + 1}, {regnam[i].strip()}\n")

        demand_co2(
            restart, iunit,
            restart.ghgrep_em_resd, restart.ghgrep_fl_resd,
            nm_resd, "Residential",
            n_pet=3, n_coal=1, n_gas=1, n_other=0, n_elec=1, i_reg=i
        )
        demand_co2(
            restart, iunit,
            restart.ghgrep_em_comm, restart.ghgrep_fl_comm,
            nm_comm, "Commercial",
            n_pet=5, n_coal=1, n_gas=1, n_other=0, n_elec=1, i_reg=i
        )
        demand_co2(
            restart, iunit,
            restart.ghgrep_em_indy, restart.ghgrep_fl_indy,
            nm_indy, "Industrial",
            n_pet=10, n_coal=3, n_gas=5, n_other=1, n_elec=1, i_reg=i
        )
        demand_co2(
            restart, iunit,
            restart.ghgrep_em_tran, restart.ghgrep_fl_tran,
            nm_tran, "Transportation",
            n_pet=9, n_coal=0, n_gas=1, n_other=0, n_elec=1, i_reg=i
        )
        demand_co2(
            restart, iunit,
            restart.ghgrep_em_elec, restart.ghgrep_fl_elec,
            nm_elec, "Electric Power",
            n_pet=3, n_coal=1, n_gas=1, n_other=3, n_elec=0, i_reg=i
        )

        iunit.write("\n\n\nCO2 Emissions Summary\n\n")

        iunit.write(format_sum.format(
            "Petroleum",
            *(
                (
                    np.sum(restart.ghgrep_em_resd[0:3, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_comm[0:5, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_indy[0:10, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_tran[0:9, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_elec[0:3, i, y1:y2], axis=0)
                ) * 0.001 * (44.0 / 12.0)
            ),
        ))
        iunit.write(format_sum.format(
            "Coal",
            *(
                (
                    restart.ghgrep_em_resd[3, i, y1:y2]
                    + restart.ghgrep_em_comm[5, i, y1:y2]
                    + np.sum(restart.ghgrep_em_indy[10:13, i, y1:y2], axis=0)
                    + restart.ghgrep_em_elec[3, i, y1:y2]
                ) * 0.001 * (44.0 / 12.0)
            ),
        ))
        iunit.write(format_sum.format(
            "Natural Gas",
            *(
                (
                    restart.ghgrep_em_resd[4, i, y1:y2]
                    + restart.ghgrep_em_comm[6, i, y1:y2]
                    + np.sum(restart.ghgrep_em_indy[13:18, i, y1:y2], axis=0)
                    + restart.ghgrep_em_tran[9, i, y1:y2]
                    + restart.ghgrep_em_elec[4, i, y1:y2]
                ) * 0.001 * (44.0 / 12.0)
            ),
        ))
        iunit.write(format_sum.format(
            "Other",
            *(
                (
                    restart.ghgrep_em_indy[18, i, y1:y2]
                    + np.sum(restart.ghgrep_em_elec[5:8, i, y1:y2], axis=0)
                ) * 0.001 * (44.0 / 12.0)
            ),
        ))
        iunit.write(format_sum.format(
            "  Total",
            *(
                (
                    np.sum(restart.ghgrep_em_resd[0:5, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_comm[0:7, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_indy[0:19, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_tran[0:11, i, y1:y2], axis=0)
                    + np.sum(restart.ghgrep_em_elec[0:8, i, y1:y2], axis=0)
                ) * 0.001 * (44.0 / 12.0)
            ),
        ))
        iunit.write(format_sum.format(
            "Electricity",
            *(
                (
                    restart.ghgrep_em_resd[5, i, y1:y2]
                    + restart.ghgrep_em_comm[7, i, y1:y2]
                    + restart.ghgrep_em_indy[19, i, y1:y2]
                    + restart.ghgrep_em_tran[11, i, y1:y2]
                ) * 0.001 * (44.0 / 12.0)
            ),
        ))

    iunit.close()


def demand_co2(
    restart: Restart,
    iunit: TextIO,
    emis: npt.NDArray[np.float64],
    fuel: npt.NDArray[np.float64],
    name: list[str],
    sector: str,
    n_pet: int,
    n_coal: int,
    n_gas: int,
    n_other: int,
    n_elec: int,
    i_reg: int,
) -> None:
    """Report CO2 emissions and energy consumption for one sector and region.

    Called by the `sum_emissions_reporting_loop` function to output CO2
    emissions and the corresponding energy use from which they were computed.
    One call to this function handles the reporting for all fuels across one
    sector in one region. The CSV-formatted report is written to the output
    file whose file handle is passed in.

    NOTE: This function assumes that fuels are indexed in five clusters: first
    come all of the petroleum products, then the coal, then the natural gas,
    then the others, and finally the purchased electricity. Make sure that the
    input arrays adhere to this pattern or the reported subtotals will be
    incorrect.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    iunit : TextIO
        File handle of the DEMAND_CO2 output file.
    emis : npt.NDArray[np.float64]
        Sector CO2 emissions array.
    fuel : npt.NDArray[np.float64]
        Sector fuel consumption array with same dimensions as `emis`.
    name : list[str]
        List of fuel index names with length matching the first dimension of
        `emis` and `fuel`.
    sector : str
        Name of the sector, e.g., 'Residential'.
    n_pet : int
        Number of fuel indices that are petroleum products.
    n_coal : int
        Number of fuel indices that are coal.
    n_gas : int
        Number of fuel indices that are natural gas.
    n_other : int
        Number of fuel indices that are "other" fuels.
    n_elec : int
        Number of fuel indices that are purchased electricity. This will be 0
        for the electric power sector and 1 for all other sectors.
    i_reg : int
        Report results for the region at this index.

    Raises
    ------
    TypeError
        If any of the input arrays do not have the expected dimensions.

    See Also
    --------
    sum_emissions_reporting_loop : Calls this function.
    """
    # Zero-based Python index
    lastyr = restart.ncntrl_lastyr - 1

    if emis.shape != fuel.shape:
        raise TypeError(f"shapes {emis.shape} and {fuel.shape} do not match")
    n_fuel, _n_reg, _n_yr = emis.shape
    if len(name) != n_fuel:
        raise TypeError(f"{len(name)=} instead of {n_fuel}")

    # Compute range of years to output, starting with 2004 for some reason
    n_years = (
        min(restart.ncntrl_ijumpcalyr, restart.ncntrl_lastcalyr) - 2004 + 1
    )
    y1 = 2004 - restart.parametr_baseyr
    y2 = y1 + n_years

    # Set up the clusters for each fuel type, saving a name and a fuel index
    # slice for each one. For fuel types that have 0 or 1 fuel indices, set the
    # slice to None to indicate we don't want to write a subtotal row.
    fuel_clusters = {}
    i_fuel = 0
    fuel_clusters["Petroleum"] = (
        slice(i_fuel, i_fuel + n_pet) if n_pet > 1 else None
    )
    i_fuel += n_pet
    fuel_clusters["Coal"] = (
        slice(i_fuel, i_fuel + n_coal) if n_coal > 1 else None
    )
    i_fuel += n_coal
    fuel_clusters["Natural Gas"] = (
        slice(i_fuel, i_fuel + n_gas) if n_gas > 1 else None
    )
    i_fuel += n_gas
    fuel_clusters["Other"] = (
        slice(i_fuel, i_fuel + n_other) if n_other > 1 else None
    )

    # Set up format strings for the header row, rows with emissions factors,
    # and rows without emissions factors
    format_header = (
        '\n\n\n"{:s} Sector"' + 2 * ',"Emis Fac"' + n_years * ",{:d}" + ",\n"
    )
    format_w_fac = '"{:s}"' + 2 * ",{:.3f}" + n_years * ",{:.3f}" + ",\n"
    format_wo_fac = '"{:s}"' + 2 * "," + n_years * ",{:.3f}" + ",\n"

    iunit.write(format_header.format(
        sector.strip(), *range(2004, 2004 + n_years)
    ))

    iunit.write('\n"CO2 Emissions (mmtCO2eq)"\n')

    # Write out a CO2 emissions row for each fuel index
    for i_fuel in range(n_fuel):
        co2 = emis[i_fuel, i_reg, lastyr]
        btu = fuel[i_fuel, i_reg, lastyr]
        fac = co2 / btu if btu != 0.0 else 0.0
        iunit.write(format_w_fac.format(
            name[i_fuel].rstrip(),
            fac, fac * 44.0 / 12.0,
            *(emis[i_fuel, i_reg, y1:y2] * 0.001 * 44.0 / 12.0)
        ))
        # When we reach the end of a fuel type cluster, write an extra subtotal
        # row if the associated slice exists
        for fuel_type, fuel_slice in fuel_clusters.items():
            if fuel_slice is not None and i_fuel == fuel_slice.stop - 1:
                tot_co2 = np.sum(emis[fuel_slice, i_reg, lastyr])
                tot_btu = np.sum(fuel[fuel_slice, i_reg, lastyr])
                avg_fac = tot_co2 / tot_btu if tot_btu != 0.0 else 0.0
                subtot = np.sum(emis[fuel_slice, i_reg, y1:y2], axis=0)
                subtot *= 0.001 * (44.0 / 12.0)
                iunit.write(format_w_fac.format(
                    f"  {fuel_type} Subtotal",
                    avg_fac, avg_fac * (44.0 / 12.0), *subtot,
                ))

    # Close out the CO2 emissions section with totals excluding and including
    # indirect emissions from electricity. This block executes for every sector
    # except the electric power sector, which has its own total line as a fuel
    # index within the array.
    if n_elec > 0:
        total = np.sum(emis[:-n_elec, i_reg, y1:y2], axis=0)
        total *= 0.001 * (44.0 / 12.0)
        iunit.write(format_wo_fac.format("  Total w/o Electricity", *total))
        total = np.sum(emis[:, i_reg, y1:y2], axis=0)
        total *= 0.001 * (44.0 / 12.0)
        iunit.write(format_wo_fac.format(f"  Total {sector.strip()}", *total))

    iunit.write('\n"Energy Use (trill Btu)"\n')

    # Write out an associated energy use row for each fuel index
    for i_fuel in range(n_fuel):
        iunit.write(format_wo_fac.format(
            name[i_fuel].rstrip(), *fuel[i_fuel, i_reg, y1:y2]
        ))
        # Just like with CO2 emissions above, when we reach the end of a fuel
        # type cluster, write an extra subtotal line if the slice exists
        for fuel_type, fuel_slice in fuel_clusters.items():
            if fuel_slice is not None and i_fuel == fuel_slice.stop - 1:
                iunit.write(format_wo_fac.format(
                    f"  {fuel_type} Subtotal",
                    *np.sum(fuel[fuel_slice, i_reg, y1:y2], axis=0),
                ))

    # For all sectors except the electric power sector, write out an energy use
    # total. The electric power sector has its own total line as a fuel index
    # within the array.
    if n_elec > 0:
        iunit.write(format_wo_fac.format(
            f"  Total {sector.strip()}", *np.sum(fuel[:, i_reg, y1:y2], axis=0)
        ))
