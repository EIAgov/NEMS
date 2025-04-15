"""Code to account for "other" greenhouse gases and offsets.

Account for CO2 from cement and lime, read the offsets data from the GHGOFFX
input file, and determine the amount of other greenhouse gas abatement using
the marginal abatement curves (MACs).
"""

from pathlib import Path

import numpy as np
import tabulate

from epm_common import BASE_YR, log_it, print_it
from epm_fortran import getrngi, getrngr, readrngxlsx
from epm_read import get_file_path
from epm_restart import Restart
from epm_scedes import Scedes
from epm_variables import Variables


def oghg(restart: Restart, scedes: Scedes, variables: Variables) -> None:
    """Compute other greenhouse gas emissions as a function of carbon price.

    Calculations include lime production and clinker process CO2 emissions from
    the industrial module. We also handle accounting for offsets, including
    incentives, domestic and international offsets, and bio sequestration (if
    allowed). The subroutine sums abatement from covered sources at the current
    price, relying on the greenhouse gas marginal abatement curves (GHG MACs)
    as necessary.

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
    sum_emissions : Calls this function after summing primary CO2 emissions.
    ghg_macs : Used by this function to compute GHG MACs.
    get_file_path : Used by this function to find the GHGOFFX file path.
    readoffsets : Used by this function to load offsets data.
    """
    # Set to 1 allowance credit per international offset
    allow_per_offset = 1.0

    # 1: percentage total offset limits applied pro-rata based on formula:
    #     2000 / (2000 + cap)
    # 2: total offsets given covered emission
    # 3: domestic offset limit
    # 4: international after adjustment if domestic < 900 mmtCO2eq

    # Zero-based Python indices
    base_yr = BASE_YR - 1
    curiyr = restart.ncntrl_curiyr - 1

    # Option to calculate int'l offsets and price independently of domestic
    # sources based on distinct limit (offsetint). If 1, calc int'l offsets and
    # price independently of domestic based on offsetint limit.
    offseti = int(scedes.get("OFFSETI", "0"))

    # With offsets, bioseqok = 1 means that bio sequestration offsets count
    # towards the goal. On the other hand, bioseqok = 0 means incentive only.
    # Incentives are given but don't count toward the goal.
    bioseqok = int(scedes.get("BIOSEQOK", "1"))

    if (
        restart.ncntrl_curitr == 1
        and restart.ncntrl_curiyr == restart.ncntrl_firsyr
    ):
        ghgoffx_path = get_file_path(variables, "ghgoffx")
        readoffsets(restart, ghgoffx_path)

    # (exi is IDM switch)
    if restart.ncntrl_curiyr >= BASE_YR and restart.ncntrl_exi == 1:

        # Assume non-energy CO2, other than lime and cement process emissions,
        # grows at 1%. Take total nonenergy CO2 (which includes adjustments to
        # exclude bunker and military fuel) and subtract base year estimated
        # lime and cement process emissions (converted to mmtC). Grow that
        # small residual at 1% per year.
        noncementco2 = (
            restart.ghgrep_ghg_bl[base_yr, 5]
            - 0.001 * (12.0 / 44.0) * (
                restart.indepm_co2_clink[base_yr, 4]
                + restart.indepm_co2_lime[base_yr, 4]
            )
        )
        noncementco2 *= 1.01 ** (restart.ncntrl_curiyr - BASE_YR)

        # Add in lime and cement process emissions (converted to mmtC)
        restart.ghgrep_ghg_bl[curiyr, 5] = (
            noncementco2
            + 0.001 * (12.0 / 44.0) * (
                restart.indepm_co2_lime[curiyr, 4]
                + restart.indepm_co2_clink[curiyr, 4]
            )
        )

        if (
            restart.ncntrl_curiyr == restart.ncntrl_lastyr
            and restart.ncntrl_ncrl == 1
        ):
            log_table_headers = [
                "Year", "Non-cement\nNon-energy CO2\n(mmtC)",
                "Lime\nProcess CO2\n(mmtC)", "Cement\nProcess CO2\n(mmtC)",
                "Total\nNon-energy CO2\n(mmtC)",
            ]
            log_table_data = []
            for i in range(base_yr, restart.ncntrl_lastyr):
                # Convert all to mmtC for output
                year = restart.parametr_baseyr + i
                total = restart.ghgrep_ghg_bl[i, 5]
                lime = restart.indepm_co2_lime[i, 4] * 0.001 * (12.0 / 44.0)
                clink = restart.indepm_co2_clink[i, 4] * 0.001 * (12.0 / 44.0)
                log_table_data.append([
                    year, total - lime - clink, lime, clink, total
                ])
            log_it(
                tabulate.tabulate(log_table_data, headers=log_table_headers),
            )

    restart.ghgrep_ghg_abate[curiyr, :] = 0.0
    restart.epmbank_offset[:, curiyr] = 0.0
    if restart.ncntrl_ncrl == 0:
        restart.ghgrep_ghg_rev[:, curiyr] = 0.0
    restart.ghgrep_ghg_offsetp[curiyr] = 0.0

    # For S2191 estimate the amount of biogenic sequestration that could result
    # from the USDA incentive program funded by a 5% allocation of allowances.
    # Any seq used in this program cannot also be used as an offset. Since the
    # per-ton value of this incentive is the value of an allowance, and the
    # price of offsets may be less than or equal to the price of allowance,
    # assume the USDA program is fully subscribed before using bio seq as an
    # offset. Subtract this quantity from the biosequestration offset supply
    # curve when calculating offsets.
    bioseq = 0.0  # TODO: long description here for a variable that's not used

    # Sum abatement from covered sources at current price emtax[curiyr] and
    # compute covered sector emissions as: Baseline - Abatement
    # Compute domestic and international offsets at current price
    qoffdom, qoffint = 0.0, 0.0  # dom = domestic, int = international

    # Zero-based current year index for Python
    i_year = restart.ncntrl_curiyr - 1

    restart.ghgrep_ghg_ncat = max(restart.ghgrep_ghg_ncat, 0)

    for icat in range(restart.ghgrep_ghg_ncat):
        icl = restart.ghgrep_ghg_class[icat] - 1
        if icl == 0:
            q = 0.0
            if restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr:
                # With covered emissions, full allowance price is used
                q = ghg_macs(
                    restart, variables,
                    icat, restart.emoblk_emtax[i_year], restart.ncntrl_curcalyr
                )
                restart.ghgrep_ghg_abate[i_year, icat] = min(
                    q, restart.ghgrep_ghg_bl[i_year, icat]
                )
            restart.ghgrep_ghg_oghg[i_year, icat] = (
                restart.ghgrep_ghg_bl[i_year, icat]
                - restart.ghgrep_ghg_abate[i_year, icat]
            )
            if not (restart.emoblk_elec_flag or restart.emoblk_tran_flag):
                restart.emoblk_total_emissions[i_year] += (
                    restart.ghgrep_ghg_oghg[i_year, icat]
                )
            if (
                restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr
                and restart.ncntrl_ncrl == 0
                and (icl != 2 or bioseqok == 1)
            ):
                restart.ghgrep_ghg_rev[icl, i_year] += (
                    restart.emoblk_emtax[i_year]
                    * restart.ghgrep_ghg_oghg[i_year, icat]
                )

        elif icl == 4:
            # Comparable international allowances, unlimited
            q = ghg_macs(
                restart, variables,
                icat, restart.emoblk_emtax[i_year], restart.ncntrl_curcalyr
            )
            restart.ghgrep_ghg_abate[i_year, icat] = q
            restart.epmbank_offset[icl - 1, i_year] += (
                restart.ghgrep_ghg_abate[i_year, icat]
            )

        elif icl >= 0:
            # A zero makes it exclude the abatement curve from affecting the
            # baseline
            if offseti == 0 or (offseti == 1 and icl != 3):
                # Calc int'l offsets separately, so don't include the abatement
                # curve
                # Sum uncovered MAC and offsets
                if (
                    restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr
                    and restart.emoblk_offset_flag
                ):
                    q = ghg_macs(
                        restart, variables,
                        icat,
                        restart.emoblk_emtax[i_year], restart.ncntrl_curcalyr
                    )
                    if icl != 2 or (icl == 2 and bioseqok == 1):
                        qoffdom += q
            elif offseti == 1 and icl == 3:
                if (
                    restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr
                    and restart.emoblk_offset_flag
                ):
                    # Set to 1.0 now
                    q = ghg_macs(
                        restart, variables,
                        icat,
                        allow_per_offset * restart.emoblk_emtax[i_year],
                        restart.ncntrl_curcalyr
                    )
                    qoffint += q

    if (
        restart.ncntrl_curcalyr < restart.epmbank_bank_startyr
        or not restart.emoblk_offset_flag
    ):
        restart.epmbank_offset[:, i_year] = 0.0
        # If prior to compliance period, zero any abatement, since the offset
        # market won't be activated until then.
        for icat in range(restart.ghgrep_ghg_ncat):
            icl = restart.ghgrep_ghg_class[icat] - 1
            if icl != 0:
                restart.ghgrep_ghg_abate[i_year, icat] = 0.0
                restart.ghgrep_ghg_oghg[i_year, icat] = (
                    restart.ghgrep_ghg_bl[i_year, icat]
                )
        return

    # Find the amount of offsets available at the current price. If more
    # than the maximum offsets allowed, find minimum price at which the
    # maximum occurs.

    # Normal assumption
    qmax = restart.emoblk_emissions_goal[i_year]

    # Max domestic offset quantity (or total of domestic and int'l, if
    # offseti = 0) (dom = domestic)
    offsetdom = float(scedes.get("OFFSETDO", "15")) * 0.01

    # Max international offset quantity (if offseti = 1) (int =
    # international)
    offsetint = float(scedes.get("OFFSETIN", "15")) * 0.01

    # Find domestic offset quantity
    qmax = offsetdom * restart.emoblk_emissions_goal[i_year]
    p = restart.emoblk_emtax[i_year]
    if qoffdom > qmax and p > 0.0:

        # Find p that results in qsum = qmax
        phigh, plow, ncount = p, 0.0, 0

        # Binary search
        while abs(qmax - qoffdom) > 0.1 and ncount <= 200:

            # 0.1 is a tolerance--tenth of a ton
            ncount += 1
            p = (phigh + plow) * 0.5
            qoffdom = 0.0

            # Compute emissions at trial price
            for icat in range(restart.ghgrep_ghg_ncat):
                icl = restart.ghgrep_ghg_class[icat] - 1
                if 0 < icl < 4:

                    # Changed to leave int'l out when offseti = 1
                    if offseti == 0 or (offseti == 1 and icl != 3):

                        # Sum uncovered MAC and offsets
                        q = ghg_macs(
                            restart, variables,
                            icat, p, restart.ncntrl_curcalyr
                        )
                        if icl != 2 or (icl == 2 and bioseqok == 1):
                            qoffdom += q

            if qoffdom < qmax:
                plow = p
            elif qoffdom > qmax:
                phigh = p
            else:
                plow, phigh = p, p

    restart.ghgrep_ghg_offsetp[i_year] = p

    if offseti == 1:

        # If limit on international offsets is independent of domestic
        # limit, do int'l price separately. Find international offset price
        # subject to maximum offset. Find domestic offset price
        p = restart.emoblk_emtax[i_year]
        qmax = offsetint * restart.emoblk_emissions_goal[i_year]

        if qoffint > qmax and p > 0.0:

            # Find p that results in qsum = qmax
            phigh, plow, ncount = p, 0.0, 0

            # Binary search
            while abs(qmax - qoffint) > 0.1 and ncount <= 200:

                # 0.1 is a tolerance--tenth of a ton
                ncount += 1
                p = (phigh + plow) * 0.5
                qoffint = 0.0

                # Compute emissions at trial price
                for icat in range(restart.ghgrep_ghg_ncat):
                    icl = restart.ghgrep_ghg_class[icat] - 1
                    if icl == 3:

                        # Sum any international offsets
                        q = ghg_macs(
                            restart, variables,
                            icat, p, restart.ncntrl_curcalyr
                        )
                        qoffint += q

                if qoffint < qmax:
                    plow = p
                elif qoffint > qmax:
                    phigh = p
                else:
                    plow, phigh = p, p

        restart.ghgrep_ghg_offsetpint[i_year] = p
    else:
        restart.ghgrep_ghg_offsetpint[i_year] = (
            restart.ghgrep_ghg_offsetp[i_year]
        )

    # Retally emissions from non-covered sources
    restart.epmbank_offset[:4, i_year] = 0.0
    qoffdom, qoffint = 0.0, 0.0  # dom = domestic, int = international
    for icat in range(restart.ghgrep_ghg_ncat):
        icl = restart.ghgrep_ghg_class[icat] - 1
        if icl == 4:  # Comparable international allowances, unlimited
            q = ghg_macs(
                restart, variables,
                icat, restart.emoblk_emtax[i_year], restart.ncntrl_curcalyr
            )
            restart.ghgrep_ghg_abate[i_year, icat] = q
            restart.epmbank_offset[icl - 1, i_year] += (
                restart.ghgrep_ghg_abate[i_year, icat]
            )
        elif icl > 0:  # Domestic and international offsets
            # Leave int'l out if offseti = 1
            if offseti == 0 or (offseti == 1 and icl != 3):
                # Sum uncovered MAC and offsets
                q = ghg_macs(
                    restart, variables,
                    icat,
                    restart.ghgrep_ghg_offsetp[i_year],
                    restart.ncntrl_curcalyr
                )
                restart.ghgrep_ghg_abate[i_year, icat] = q
                if icl == 1:
                    restart.ghgrep_ghg_abate[i_year, icat] = min(
                        q, restart.ghgrep_ghg_bl[i_year, icat]
                    )
                restart.epmbank_offset[icl - 1, i_year] += (
                    restart.ghgrep_ghg_abate[i_year, icat]
                )
                restart.ghgrep_ghg_oghg[i_year, icat] = (
                    restart.ghgrep_ghg_bl[i_year, icat]
                    - restart.ghgrep_ghg_abate[i_year, icat]
                )
                if icl != 2 or (icl == 2 and bioseqok == 1):
                    qoffdom += restart.ghgrep_ghg_abate[i_year, icat]
            # Compute int'l at international offset price if separate limit
            # on int'l offsets
            elif offseti == 1 and icl == 3:
                q = ghg_macs(
                    restart, variables,
                    icat,
                    restart.ghgrep_ghg_offsetpint[i_year],
                    restart.ncntrl_curcalyr
                )
                restart.ghgrep_ghg_abate[i_year, icat] = q
                restart.epmbank_offset[icl - 1, i_year] += (
                    restart.ghgrep_ghg_abate[i_year, icat]
                )
                qoffint += restart.ghgrep_ghg_abate[i_year, icat]
        # not eligible for offsets and not counting abatement
        elif icl == -1:
            restart.ghgrep_ghg_abate[i_year, icat] = 0.0
            restart.ghgrep_ghg_oghg[i_year, icat] = (
                restart.ghgrep_ghg_bl[i_year, icat]
            )

    for ioff in range(4):
        icl = min(ioff + 1, 3)
        # Domestic offset price
        p = restart.ghgrep_ghg_offsetp[i_year]
        # International offset price
        if ioff == 2:
            p = restart.ghgrep_ghg_offsetpint[i_year]
        # International allowance price same as domestic in HR2454
        if ioff == 3:
            p = restart.emoblk_emtax[i_year]
        if restart.ncntrl_ncrl == 0:
            restart.ghgrep_ghg_rev[icl, i_year] += (
                p * restart.epmbank_offset[ioff, i_year]
            )


def readoffsets(restart: Restart, file_path: Path) -> None:
    """Read the greenhouse gas offsets data from the input spreadsheet.

    Called by `oghg` on the first iteration of the first year to read the
    greenhouse gas offsets data from the "offsets" sheet of the GHGOFFX input
    file. The data gets stored in the appropriate arrays that are defined in
    the ghg common block within the ghgrep include file. The input file is an
    Excel spreadsheet, so this function reads it using the three functions
    `readrngxlsx`, `getrngr`, and `getrngi` from `epm_fortran`. They mirror the
    functions of the same names that are availabe in Fortran NEMS code.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    file_path : Path
        Path to the XLSX file that this function should open.

    See Also
    --------
    oghg : Calls this function on the first iteration of the first year.
    """
    with file_path.open("rb") as f:
        # Call function to read in data from worksheet named "offsets"
        # This stores all the Excel data in df for the time being
        df = readrngxlsx(f, "offsets")

    # Copy each range from worksheet data area to variables
    # getrngi: Copies an integer variable from the worksheet data frame into
    #          the variable. The variable dimensions are passed as the 3rd and
    #          4th arguments, e.g., (df, name, 1, 1). A variable with dimesions
    #          of (1, 1) is a scalar. A variable with dimensions of (26, 1) is
    #          a one-dimensional array with 26 elements.
    # getrngr: Copies a float variable from the worksheet data frame into the
    #          variable.

    # based on spreadsheet number of CL_ ranges.
    # last two Cats don't have Baselines
    restart.ghgrep_ghg_ncat = 17
    restart.ghgrep_ghg_bl[:, :5] = 0.0
    restart.ghgrep_ghg_bl[:, 6:] = 0.0

    if restart.ncntrl_exi == 1:  # exi is the IDM switch
        restart.ghgrep_ghg_bl[:, 5] = 0.0

    restart.ghgrep_ghg_bl[:, 0] = getrngr(
        df, "BL_CH4LAND", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 1] = getrngr(
        df, "BL_CH4COAL", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 2] = getrngr(
        df, "BL_CH4NG", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 3] = getrngr(
        df, "BL_CH4COMB", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 4] = getrngr(
        df, "BL_CH4OTH", 1, restart.parametr_mnumyr
    )

    # Take from input restart file if industrial not on
    if restart.ncntrl_exi == 1:
        restart.ghgrep_ghg_bl[:, 5] = getrngr(
            df, "BL_NECO2", 1, restart.parametr_mnumyr
        )

    restart.ghgrep_ghg_bl[:, 6] = getrngr(
        df, "BL_N2OAG", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 7] = getrngr(
        df, "BL_N2OMOB", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 8] = getrngr(
        df, "BL_N2OACID", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 9] = getrngr(
        df, "BL_N2OOTH", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 10] = getrngr(
        df, "BL_FGASHFC23", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 11] = getrngr(
        df, "BL_FGASHFC", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 12] = getrngr(
        df, "BL_FGASPFC", 1, restart.parametr_mnumyr
    )
    restart.ghgrep_ghg_bl[:, 13] = getrngr(
        df, "BL_FGASSF6", 1, restart.parametr_mnumyr
    )

    # Convert from mmtCO2eq to mmtCe
    restart.ghgrep_ghg_bl[:, :5] *= 12.0 / 44.0
    if restart.ncntrl_exi == 1:
        restart.ghgrep_ghg_bl[:, 5] *= 12.0 / 44.0
    restart.ghgrep_ghg_bl[:, 6:] *= 12.0 / 44.0

    restart.ghgrep_ghg_class[0] = getrngi(df, "CL_CH4LAND", 1, 1)
    restart.ghgrep_ghg_class[1] = getrngi(df, "CL_CH4COAL", 1, 1)
    restart.ghgrep_ghg_class[2] = getrngi(df, "CL_CH4NG", 1, 1)
    restart.ghgrep_ghg_class[3] = getrngi(df, "CL_CH4COMB", 1, 1)
    restart.ghgrep_ghg_class[4] = getrngi(df, "CL_CH4OTH", 1, 1)
    restart.ghgrep_ghg_class[5] = getrngi(df, "CL_NECO2", 1, 1)
    restart.ghgrep_ghg_class[6] = getrngi(df, "CL_N2OAG", 1, 1)
    restart.ghgrep_ghg_class[7] = getrngi(df, "CL_N2OMOB", 1, 1)
    restart.ghgrep_ghg_class[8] = getrngi(df, "CL_N2OACID", 1, 1)
    restart.ghgrep_ghg_class[9] = getrngi(df, "CL_N2OOTH", 1, 1)
    restart.ghgrep_ghg_class[10] = getrngi(df, "CL_FGASHFC23", 1, 1)
    restart.ghgrep_ghg_class[11] = getrngi(df, "CL_FGASHFC", 1, 1)
    restart.ghgrep_ghg_class[12] = getrngi(df, "CL_FGASPFC", 1, 1)
    restart.ghgrep_ghg_class[13] = getrngi(df, "CL_FGASSF6", 1, 1)
    restart.ghgrep_ghg_class[14] = getrngi(df, "CL_SEQUS", 1, 1)
    restart.ghgrep_ghg_class[15] = getrngi(df, "CL_INTOFF", 1, 1)
    restart.ghgrep_ghg_class[16] = getrngi(df, "CL_INTCO2", 1, 1)

    restart.ghgrep_mac_priceyr = int(getrngi(df, "MAC_PRICEYR", 1, 1))

    restart.ghgrep_ghg_macp[:, :] = 0.0

    # Landfill
    restart.ghgrep_ghg_macp[:, 0] = getrngr(
        df, "MACP_CH4LAND", restart.ghgrep_ghg_stmax, 1
    )
    # Coal mining
    restart.ghgrep_ghg_macp[:, 1] = getrngr(
        df, "MACP_CH4COAL", restart.ghgrep_ghg_stmax, 1
    )
    # Natural gas/oil systems
    restart.ghgrep_ghg_macp[:, 2] = getrngr(
        df, "MACP_CH4NG", restart.ghgrep_ghg_stmax, 1
    )
    # Combustion placeholder
    restart.ghgrep_ghg_macp[:, 3] = getrngr(
        df, "MACP_CH4COMB", restart.ghgrep_ghg_stmax, 1
    )
    # Agriculture
    restart.ghgrep_ghg_macp[:, 4] = getrngr(
        df, "MACP_CH4OTH", restart.ghgrep_ghg_stmax, 1
    )

    # Why is index 5 missing here? The sixth index is also missing from the
    # original Fortran code.

    # Agriculture
    restart.ghgrep_ghg_macp[:, 6] = getrngr(
        df, "MACP_N2OAG", restart.ghgrep_ghg_stmax, 1
    )
    # Combustion placeholder
    restart.ghgrep_ghg_macp[:, 7] = getrngr(
        df, "MACP_N2OCOMB", restart.ghgrep_ghg_stmax, 1
    )
    # Acids
    restart.ghgrep_ghg_macp[:, 8] = getrngr(
        df, "MACP_N2OACID", restart.ghgrep_ghg_stmax, 1
    )
    # Other placeholder
    restart.ghgrep_ghg_macp[:, 9] = getrngr(
        df, "MACP_N2OOTH", restart.ghgrep_ghg_stmax, 1
    )

    restart.ghgrep_ghg_macp[:, 10] = getrngr(
        df, "MACP_FGASHFC23", restart.ghgrep_ghg_stmax, 1
    )
    restart.ghgrep_ghg_macp[:, 11] = getrngr(
        df, "MACP_FGASHFC", restart.ghgrep_ghg_stmax, 1
    )
    restart.ghgrep_ghg_macp[:, 12] = getrngr(
        df, "MACP_FGASPFC", restart.ghgrep_ghg_stmax, 1
    )
    restart.ghgrep_ghg_macp[:, 13] = getrngr(
        df, "MACP_FGASSF6", restart.ghgrep_ghg_stmax, 1
    )
    restart.ghgrep_ghg_macp[:, 14] = getrngr(
        df, "MACP_SEQUS", restart.ghgrep_ghg_stmax, 1
    )

    # International offsets
    restart.ghgrep_ghg_macp[:, 15] = getrngr(
        df, "MACP_INTOFF", restart.ghgrep_ghg_stmax, 1
    )
    # International allowances
    restart.ghgrep_ghg_macp[:, 16] = getrngr(
        df, "MACP_INTCO2", restart.ghgrep_ghg_stmax, 1
    )

    # Count the steps and convert to EPM price units (1987$/kg)
    restart.ghgrep_ghg_steps[:] = 0
    print_it(
        restart.ncntrl_curirun,
        "\n".join([
            "mc_jpgdp is:", str(restart.macout_mc_jpgdp),
            "mac_priceyr is:", str(restart.ghgrep_mac_priceyr),
        ])
    )
    dollarmac = restart.macout_mc_jpgdp[restart.ghgrep_mac_priceyr - 1987]
    restart.ghgrep_ghg_macp *= 0.001 / dollarmac
    restart.ghgrep_ghg_steps[:] = (
        np.argmax(restart.ghgrep_ghg_macp, axis=0) + 1
    )
    # For places with no positive values, set the step number to zero
    no_positives = np.all(restart.ghgrep_ghg_macp <= 0.0, axis=0)
    restart.ghgrep_ghg_steps[no_positives] = 0

    # Read the quantities available at each price--specified by five 5-year
    # intervals, 2005-2050
    restart.ghgrep_ghg_macq[:, :, :] = 0.0
    restart.ghgrep_ghg_macq[:, :, 0] = getrngr(
        df, "MACQ_CH4LAND", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 1] = getrngr(
        df, "MACQ_CH4COAL", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 2] = getrngr(
        df, "MACQ_CH4NG", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 3] = getrngr(
        df, "MACQ_CH4COMB", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 4] = getrngr(
        df, "MACQ_CH4OTH", restart.ghgrep_ghg_stmax, 10
    )

    # Index 5 is missing again...

    restart.ghgrep_ghg_macq[:, :, 6] = getrngr(
        df, "MACQ_N2OAG", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 7] = getrngr(
        df, "MACQ_N2OCOMB", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 8] = getrngr(
        df, "MACQ_N2OACID", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 9] = getrngr(
        df, "MACQ_N2OOTH", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 10] = getrngr(
        df, "MACQ_FGASHFC23", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 11] = getrngr(
        df, "MACQ_FGASHFC", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 12] = getrngr(
        df, "MACQ_FGASPFC", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 13] = getrngr(
        df, "MACQ_FGASSF6", restart.ghgrep_ghg_stmax, 10
    )
    restart.ghgrep_ghg_macq[:, :, 14] = getrngr(
        df, "MACQ_SEQUS", restart.ghgrep_ghg_stmax, 10
    )
    # International offsets
    restart.ghgrep_ghg_macq[:, :, 15] = getrngr(
        df, "MACQ_INTOFF", restart.ghgrep_ghg_stmax, 10
    )
    # International allowances
    restart.ghgrep_ghg_macq[:, :, 16] = getrngr(
        df, "MACQ_INTCO2", restart.ghgrep_ghg_stmax, 10
    )


def ghg_macs(
    restart: Restart,
    variables: Variables,
    icat: int,
    p: float,
    year: int
) -> float:
    """Return the amount of other greenhouse gas abatement by using the MACs.

    Use the other greenhouse gas marginal abatement curves (GHG MACs) to
    determine the amount of abatement in a given category at a given price in a
    given year. This subroutine assumes that prices are organized in ascending
    order, and it interpolates between adjacent steps and adjacent intervals.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    variables : Variables
        The active intermediate variables for EPM.
    icat : int
        Index of other greenhouse gas category.
    p : float
        Price level for abatement.
    year : int
        Four-digit calendar year.

    Returns
    -------
    q : float
        The abatement quantity.

    See Also
    --------
    oghg : Calls this function for MAC calculations.
    iyear5 : Used by this function to get interval numbers.
    """
    if not (0 <= icat < restart.ghgrep_ghg_nmax) or p <= 0.0:
        return 0.0

    # Get the lower and upper curve number or index based on year, along with
    # the interpolation fraction for going between the 5 year intervals.
    # iyl and iyh are year interval, 0-9
    iyl, iyh, fraciyl = iyear5(restart, year)
    if iyl == -1:
        return 0.0

    nsteps = restart.ghgrep_ghg_steps[icat]  # Number of steps
    if nsteps == 0:
        return 0.0

    # Initialize q just in case nothing else sets it value before return
    q = 0.0

    # See if we max-out on the last step. If price is higher, we can quit early
    if p >= restart.ghgrep_ghg_macp[nsteps - 1, icat]:
        # fraciyl is interpolation fraction for going between 5 year intervals.
        ql = restart.ghgrep_ghg_macq[nsteps - 1, iyl, icat]
        qh = restart.ghgrep_ghg_macq[nsteps - 1, iyh, icat]
        q = np.average([ql, qh], weights=[fraciyl, 1.0 - fraciyl])
        return float(q)

    # Find interval surrounding the input price.
    istep = 0  # Step number
    found = False  # True when interval found

    if p < restart.ghgrep_ghg_macp[istep, icat]:
        # Price is less than first step price
        found = True
        # Interpolate between curves (for q) and between 0 and first price
        q1 = np.float64(0.0)
        q2l = restart.ghgrep_ghg_macq[istep, iyl, icat]
        q2h = restart.ghgrep_ghg_macq[istep, iyh, icat]
        q2 = np.average([q2l, q2h], weights=[fraciyl, 1.0 - fraciyl])
        p1 = 0.0
        p2 = restart.ghgrep_ghg_macp[istep, icat]
        frac = 0.0
        if p2 != p1:
            frac = (p - p1) / (p2 - p1)
        q = np.average([q1, q2], weights=[1.0 - frac, frac])

    while not found and istep < nsteps - 1:
        if (
            restart.ghgrep_ghg_macp[istep, icat]
            <= p < restart.ghgrep_ghg_macp[istep + 1, icat]
        ):
            found = True
            # Now interpolate between curves (for q) and between price steps
            q1l = restart.ghgrep_ghg_macq[istep, iyl, icat]
            q1h = restart.ghgrep_ghg_macq[istep, iyh, icat]
            q1 = np.average([q1l, q1h], weights=[fraciyl, 1.0 - fraciyl])
            q2l = restart.ghgrep_ghg_macq[istep + 1, iyl, icat]
            q2h = restart.ghgrep_ghg_macq[istep + 1, iyh, icat]
            q2 = np.average([q2l, q2h], weights=[fraciyl, 1.0 - fraciyl])
            p1 = restart.ghgrep_ghg_macp[istep, icat]
            p2 = restart.ghgrep_ghg_macp[istep + 1, icat]
            frac = 0.0
            if p2 != p1:
                frac = (p - p1) / (p2 - p1)
            q = np.average([q1, q2], weights=[1.0 - frac, frac])
        istep += 1

    if not found:
        # The following error log message is written the first time an error
        # occurs for each value of icat, but not on subsequent errors for the
        # same value of icat
        variables.oghg_err_count[icat] += 1
        if variables.oghg_err_count[icat] < 2:
            log_lines = []
            log_lines.append(
                f"Error in abatement curve #{icat} "
                + f"{year=} intervals={iyl},{iyh}"
            )
            log_lines.append(f"Abatement curve for category {icat}")
            log_lines.append(f"{fraciyl=}")
            log_lines.append(f"Looking for price {p=}")
            log_table_data = []
            for istep in range(nsteps):
                macp = restart.ghgrep_ghg_macp[istep, icat]
                macq_l = restart.ghgrep_ghg_macq[istep, iyl, icat]
                macq_h = restart.ghgrep_ghg_macq[istep, iyh, icat]
                log_table_data.append([istep, macp, macq_l, macq_h])
            log_lines.append(
                tabulate.tabulate(log_table_data, tablefmt="plain")
            )
            log_it(*log_lines)

    return max(float(q), 0.0)


def iyear5(restart: Restart, year: int) -> tuple[int, int, float]:
    """Return the interval numbers for the MACs based on the calendar year.

    This lookup utility is called by subroutine ghg_macs to retrieve the five-
    year interval numbers for the greenhouse gas marginal abatement curves
    (MACs). The interval numbers are based on the year, with lower and higher
    indices for each interval, as well as an interpolation fraction between
    them.

    For example, consider the interval from 2010 to 2015; the lower index is 1,
    the higher index is 2, and the interpolation fraction (which is weighted on
    the lower side) ranges from 1.0 in 2010 to 0.2 in 2014, stepping by 0.2
    each year.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    year : int
        Calendar year for which to look up the interval numbers.

    Returns
    -------
    iyl : int
        Receives the lower interval number.
    iyh : int
        Receives the higher interval number.
    fraciyl : float
        Receives the interpolation fraction (weighted on the lower side) for
        interpolation within the five-year intervals.

    See Also
    --------
    ghg_macs : Uses this function to get interval numbers.
    """
    if year < 2004 or year > restart.ncntrl_ijumpcalyr:
        return -1, -1, 1.0
    if year == 2004:
        return 0, 0, 1.0

    # The lower interval number is 0 in 2005, 1 in 2010, 2 in 2015, etc. For
    # years that are not a multiple of 5, round down to the year at the start
    # of the interval.
    iyl = (year - 2005) // 5  # Division with round down

    # The upper interval number is also 0 in 2005, 1 in 2010, 2 in 2015 etc.
    # However, for years that are not a multiple of 5, round *up* to the year
    # at the end of the interval.
    iyh = -(-(year - 2005) // 5)  # Division with round up

    # The interpolation fraction is 1.0 in years that are a multiple of 5.
    # Years that are 1 greater than a multiple of 5 step down to 0.8, years
    # that are 2 greater step down to 0.6, etc.
    fraciyl = (5 - year % 5) / 5.0

    return iyl, iyh, fraciyl
