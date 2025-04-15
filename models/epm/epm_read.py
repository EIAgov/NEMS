"""Routines for reading the majority of the EPM input files.

Code in this file sets the EPM output file, reads the policy switches in the
EPM control file, and parses emissions-related data from a variety of other
input files. This data includes carbon taxes or caps, coal region mappings,
carbon factors, mercury inputs, and California AB32 information.
"""

from pathlib import Path
import tomllib

import numpy as np
import pandas as pd
import tabulate

from epm_common import (
    get_epm_path,
    get_input_path,
    get_output_path,
    incorporate_cycle_number,
    log_it,
    print_it,
    set_log_file_path,
    set_log_verbosity,
)
from epm_restart import Restart
from epm_scedes import Scedes
from epm_variables import Variables


def epm_read(restart: Restart, scedes: Scedes, variables: Variables) -> None:
    """Read the EPM input files and designate the EPM output file.

    This function is called from NEMS main to read the contents of the EPM
    control file and several other EPM input files, as well as to set the
    EPMOUT file path for Emissions Policy Module output.

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
    find_control_file : Used to find the EPM control file.
    read_control_file : Used to read the contents of the EPM control file.
    get_file_path : Used to find all other EPM input and output files.
    read_tax_or_cap : Used read the carbon tax/cap data file.
    read_coal_regions : Used to read the coal regions mapping file.
    read_carbon_factors : Used to read the carbon factors file.
    read_mercury_files : Used to read the mercury input files.
    read_ab32_files : Used to read the California AB32 files.
    """
    control_path = find_control_file(scedes, restart.ncntrl_curirun)
    read_control_file(restart, scedes, variables, control_path)

    # Designate the EPMOUT file that this module can write to
    epmout_path = get_file_path(variables, "epmout", output_file=True)
    set_log_file_path(
        incorporate_cycle_number(epmout_path, restart.ncntrl_curirun)
    )
    set_log_verbosity(restart.emoblk_dbugepm)
    log_it("Verbose logging output is turned on", verbose=True)

    # Log important settings read from the EPM control file
    log_table_data = [
        ["tax_flag", restart.emoblk_tax_flag],
        ["bank_flag", restart.emoblk_bank_flag],
        ["bank_priceyr", restart.epmbank_bank_priceyr],
        ["bank_startyr", restart.epmbank_bank_startyr],
        ["bank_onyr", variables.epm_out_bank_onyr],
        ["bank_endyr", restart.epmbank_bank_endyr],
        ["bank_end_balance", variables.epmoth_bank_end_balance],
        ["BANKSAFE option", int(scedes.get("BANKSAFE", "0"))],
        ["permit_flag", restart.emoblk_permit_flag],
        ["market_flag", restart.emoblk_market_flag],
        ["offset_flag", restart.emoblk_offset_flag],
        ["etax_flag", restart.emoblk_etax_flag],
        ["elec_flag", restart.emoblk_elec_flag],
        ["tran_flag", restart.emoblk_tran_flag],
        ["resd_flag", restart.emoblk_resd_flag],
        ["comm_flag", restart.emoblk_comm_flag],
    ]
    log_it(tabulate.tabulate(
        log_table_data, tablefmt="plain", numalign="right"
    ))

    log_it("NOW IN EPM_READ")

    tax_or_cap_path = get_file_path(variables, "tax_or_cap")
    read_tax_or_cap(restart, tax_or_cap_path)

    coal_regions_path = get_file_path(variables, "coal_regions")
    read_coal_regions(restart, coal_regions_path)

    carbon_factors_path = get_file_path(variables, "carbon_factors")
    read_carbon_factors(restart, carbon_factors_path)

    mercury_parameters_path = get_file_path(variables, "mercury_parameters")
    mercury_classes_path = get_file_path(variables, "mercury_classes")
    mercury_caps_path = get_file_path(variables, "mercury_caps")
    read_mercury_files(
        restart, scedes,
        mercury_parameters_path, mercury_classes_path, mercury_caps_path
    )

    ab32_parameters_path = get_file_path(variables, "ab32_parameters")
    ab32_data_path = get_file_path(variables, "ab32_data")
    read_ab32_files(restart, scedes, ab32_parameters_path, ab32_data_path)

    # Emission allowance auction allocation share. This is the pass-through
    # percentage for regulated electricity pricing. It is supposedly used, but
    # largely obsolete, so just keep it set to zero. Once upon a time, these
    # values were read in from the EPMDATA input file---they could be moved to
    # a new input file if ever actually needed again.
    restart.emission_em_auction_sh[:] = 0.0


def find_control_file(scedes: Scedes, cycle: int) -> Path:
    """Find the path to the the EPM control file using the EPMCNTLN scedes key.

    The scedes value should either refer to a file inside the EPM model's input
    directory or be an absolute path to a file located elsewhere.

    Parameters
    ----------
    scedes : Scedes
        The current scedes file dict.
    cycle : int
        The current NEMS cycle number, also known as `curirun`.

    Returns
    -------
    Path
        The full path to the EPM control file that the module should read.

    Raises
    ------
    ValueError
        If the scedes path refers to an invalid control file location as
        described above.

    See Also
    --------
    epm_read : Calls this function.
    get_file_path : Related function to find other EPM input and output files.
    """
    # Start by pulling the EPMCNTLN key from the scedes. In the rare case that
    # it is not a $NEMS path, deal with that.
    scedes_path = Path(scedes["EPMCNTLN"])
    if not scedes_path.is_relative_to("$NEMS"):
        if scedes_path.is_absolute():
            return scedes_path  # Try to read from arbitrary path
        raise ValueError(
            "EPMCNTLN path is invalid -- must start with $NEMS or be absolute"
        )

    # Paths that start with $NEMS should always point to a file within the
    # EPM model input folder. Figure out the path to that input folder.
    model_path = Path(scedes["EPMPY"])
    model_input_path = (
        model_path / get_input_path().relative_to(get_epm_path())
    )

    # Make sure the path points to the expected location, then discard that
    # part of the path so we don't read from the original file in the repo
    if scedes_path.parent != model_input_path:
        raise ValueError(
            "EPMCNTLN path starts with $NEMS but is not in EPM input folder"
        )
    base_name = scedes_path.name

    # Legacy: redirect outdated paths like epmcntl*.txt to epm_control*.toml
    if base_name.startswith("epmcntl") and base_name.endswith(".txt"):
        print_it(cycle, "WARNING: found outdated EPMCNTLN path in scedes file")
        base_name = "".join([
            "epm_control",
            base_name.removeprefix("epmcntl").removesuffix(".txt"),
            ".toml"
        ])

    return get_input_path() / base_name


def get_file_path(
    variables: Variables,
    file_key: str,
    output_file: bool = False,
) -> Path:
    """Find the path to an input or output file other than the control file.

    This function uses the input_files and output_files lists from the EPM
    control data stored in the intermediate variables object. Be sure to load
    the EPM control file before calling this function.

    Parameters
    ----------
    variables : Variables
        The active intermediate variables for EPM.
    file_key : str
        The key for the specific input or output file whose path should be
        returned. This is the TOML key used inside the EPM control file.
    output_file : bool
        Set to True if the file path being sought is for an output file. The
        default is False, which indicates an input file.

    Returns
    -------
    Path
        The full path to the specified input or output file.

    Raises
    ------
    LookupError
        If the function cannot find the necessary file list from the EPM
        control file.

    See Also
    --------
    epm_read : Calls this function.
    find_control_file: Similar function to find the EPM control file.
    """
    files_list = "input_files" if not output_file else "output_files"
    if files_list not in variables.control:
        raise LookupError(
            f"unable to find {files_list} list from the EPM control file"
        )

    file_path = Path(variables.control[files_list][file_key])
    if file_path.is_absolute():
        return file_path  # Try and use this arbitrary path

    if output_file:
        return get_output_path() / file_path
    return get_input_path() / file_path


def read_control_file(
    restart: Restart,
    scedes: Scedes,
    variables: Variables,
    file_path: Path,
) -> None:
    """Read the EPM control file.

    This function is called as part of `epm_read` to handle the reading of the
    EPM control input file. The file contains the module debug switch, names of
    all other input and output files, and flags and parameters that directly
    control the NEMS emissions policy functionality.

    In addition to loading restart file variables, this function also saves the
    complete contents of the control file to an attribute on the intermediate
    variables object.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    scedes : Scedes
        The current scedes file dict.
    variables : Variables
        The active intermediate variables for EPM.
    file_path : Path
        Path to the TOML file that this function should open.

    See Also
    --------
    epm_read : Calls this function.
    """
    with file_path.open("rb") as f:
        control = tomllib.load(f)

    restart.emoblk_dbugepm = int(control["dbugepm"])

    restart.emoblk_nominal_flag = int(control["nominal_flag"])

    restart.emoblk_tax_flag = int(control["tax_flag"])
    restart.emoblk_permit_flag = int(control["permit_flag"])
    restart.emoblk_market_flag = int(control["market_flag"])
    restart.emoblk_offset_flag = int(control["offset_flag"])
    restart.emoblk_etax_flag = int(control["etax_flag"])

    # Flags to make a carbon tax or cap apply to the electric power sector only
    # or to the transportation sector only
    restart.emoblk_elec_flag = int(control["elec_flag"])
    restart.emoblk_tran_flag = int(control["tran_flag"])

    # Flags to exclude the residential sector or commercial sector from a
    # carbon tax or cap
    restart.emoblk_resd_flag = int(control["resd_flag"])
    restart.emoblk_comm_flag = int(control["comm_flag"])

    # Flag escalated fee with bank to meet cap over long period.
    # Used with tax_flag != 0
    restart.emoblk_bank_flag = int(control["bank_flag"])

    # Start year for bank, initial fee (if bank_flag != 0)
    restart.epmbank_bank_priceyr = int(control["bank_priceyr"])

    # Start year for bank or offsets if bank_flag != 0 or offset_flag != 0
    restart.epmbank_bank_startyr = int(control["bank_startyr"])

    # End year for bank, initial fee (if bank_flag != 0)
    restart.epmbank_bank_endyr = int(control["bank_endyr"])

    # Target balance in end-year for bank
    variables.epmoth_bank_end_balance = float(control["bank_end_balance"])

    # Year between bank_startyr and bank_endyr in which the banking and price
    # path apply
    variables.epm_out_bank_onyr = int(scedes.get("BANKONYR", "2051"))
    if variables.epm_out_bank_onyr < restart.epmbank_bank_startyr:
        # When bank_startyr < bank_onyr, price determined without banking
        # initially
        variables.epm_out_bank_onyr = restart.epmbank_bank_startyr

    if variables.epm_out_bank_onyr > restart.epmbank_bank_endyr:
        variables.epm_out_bank_onyr = restart.epmbank_bank_startyr

    # Save contents of EPM control file
    variables.control = control


def read_tax_or_cap(restart: Restart, file_path: Path) -> None:
    """Read the carbon tax or carbon cap input file.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    file_path : Path
        Path to the CSV file that this function should open.

    See Also
    --------
    epm_read : Calls this function.
    """
    data = pd.read_csv(file_path, index_col="year", comment='#')

    restart.emoblk_emtax[:] = data["carbon tax"]
    restart.emoblk_emissions_goal[:] = data["carbon cap"]
    restart.emoblk_max_tax[:] = data["maximum fee"]
    restart.emoblk_min_tax[:] = data["minimum fee"]

    # Carbon goal -- ftab
    restart.emission_emlim[0, :] = restart.emoblk_emissions_goal


def read_coal_regions(restart: Restart, file_path: Path) -> None:
    """Read the input file mapping coal regions into census divisions.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    file_path : Path
        Path to the TOML file that this function should open.

    See Also
    --------
    epm_read : Calls this function.
    """
    with file_path.open("rb") as f:
        coal_regions = tomllib.load(f)

    restart.emoblk_cl_cdmap[:, :] = 0
    restart.emoblk_cl_cdnum[:] = 0

    num_census_divisions = len(restart.emoblk_cl_cdnum)

    for i in range(num_census_divisions):
        regions_array = coal_regions[f"census_{i + 1}"]
        num_regions = len(regions_array)
        restart.emoblk_cl_cdmap[i, :num_regions] = regions_array
        restart.emoblk_cl_cdnum[i] = num_regions


def read_carbon_factors(restart: Restart, file_path: Path) -> None:
    """Read the carbon factors input file.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    file_path : Path
        Path to the TSV (tab-separated values) file that this function should
        open.

    See Also
    --------
    epm_read : Calls this function.
    """
    data = pd.read_csv(file_path, sep='\t', index_col="variable", comment='#')

    # Dynamically assign carbon factor values to the matching restart file
    # variables in emeblk. The factors are the same for all years.
    for variable_name, factor_value in data["factor"].items():
        full_variable_name = f"emeblk_{variable_name}".lower()
        emeblk_array = getattr(restart, full_variable_name)
        emeblk_array[:] = float(factor_value)

    # Hardwire for now as requested by PKC
    restart.emeblk_eclhm[:] = restart.emeblk_eclin[:]
    restart.emeblk_ebmhm[:] = -25.05

    # All electric utility emissions factors are the same
    restart.emeblk_egfelgr[:] = restart.emeblk_egfel[:]
    restart.emeblk_egielgr[:] = restart.emeblk_egiel[:]
    restart.emeblk_egcelgr[:] = restart.emeblk_egiel[:]


def read_mercury_files(
    restart: Restart,
    scedes: Scedes,
    parameters_file_path: Path,
    classes_file_path: Path,
    caps_file_path: Path,
) -> None:
    """Read the mercury input files that EPM maintains for EMM.

    Several NEMS "restart variables" that are filled as part of this function
    are not actually part of the restart file, so those are debug printed to
    the EPMOUT file to make them easier to track.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    scedes : Scedes
        The current scedes file dict.
    parameters_file_path : Path
        Path to the TOML file that this function should open to find the
        mercury parameters.
    classes_file_path : Path
        Path to the CSV file that this function should open to find the mercury
        compliance classes.
    caps_file_path : Path
        Path to the CSV file that this function should open to find the mercury
        caps.

    See Also
    --------
    epm_read : Calls this function.
    """
    with parameters_file_path.open("rb") as f:
        parameters = tomllib.load(f)

    # Mercury Maximum Avaliable Control Technology (MACT) Info
    restart.ecpcntl_usw_mact = int(parameters["usw_mact"])
    restart.ecpcntl_umact_yr = int(parameters["umact_yr"])
    restart.ecpcntl_usw_bact = int(parameters["usw_bact"])
    restart.ecpcntl_ubact_yr = int(parameters["ubact_yr"])
    restart.ecpcntl_usw_dsi = int(parameters["usw_dsi"])
    restart.ecpcntl_udsi_yr = int(parameters["udsi_yr"])

    restart.emission_ucap_hg = float(parameters["ucap_hg"])

    # Mercury Emissions Group (max = MX_HG_GRP) - establishes mercury emission
    # regions by coal demand region
    restart.emission_hg_grp[:] = parameters["hg_grp"]
    restart.emission_num_hg_grp = int(np.max(restart.emission_hg_grp))

    # Mercury compliance classes / mercury emission capacity classes
    # Mercury emissions in grams per megawatthour
    classes = pd.read_csv(
        classes_file_path, index_col=["class", "year"], comment='#'
    )
    num_classes = classes.index.get_level_values("class").unique().size
    for i_class in range(num_classes):
        data = classes.loc[(i_class + 1, slice(None))]
        restart.emission_hg_grams_mwh[i_class, :] = data["hg_grams_mwh"]
        for r in range(3):
            hg_output_column = f"hg_output({r + 1})"
            hg_input_column = f"hg_input({r + 1})"
            hg_mef_column = f"hg_mef({r + 1})"
            restart.emission_hg_output[i_class, r, :] = data[hg_output_column]
            restart.emission_hg_input[i_class, r, :] = data[hg_input_column]
            restart.emission_hg_mef[i_class, r, :] = data[hg_mef_column]
            # NOTE: The hard-coded constant here is exactly the number of
            # pounds in one kilogram, so we're apparently converting grams per
            # MWh into thousandths of a pound per MWh. Not sure why...
            restart.emission_hg_output[i_class, r, :] = (
                restart.emission_hg_grams_mwh[i_class, :] * 2.2046
            )
        restart.emission_hg_grams_mwh[i_class, :] = 0.0

    # Mercury Emissions Requirements Switch (Without CAMR) by Coal Demand
    # Region
    restart.emission_usw_camr = int(parameters["usw_camr"])

    # Mercury Emissions Requirements MEF (Without CAMR) by Coal Demand Region
    # There are a number of years with different mercury standards
    most_recent_standards = None
    for i_year in range(restart.parametr_mnumyr):
        year = restart.parametr_baseyr + i_year
        # First check whether this year is the start of a new mercury standard
        if str(year) in parameters["hg_mefnc"]:
            most_recent_standards = parameters["hg_mefnc"][str(year)]
        # Apply latest standard if there is one, but otherwise set no standard
        if most_recent_standards is not None:
            restart.emission_hg_mefnc[:, i_year] = most_recent_standards
        else:
            restart.emission_hg_mefnc[:, i_year] = 1.0  # No standard

    # Check for switch to ignore mercury limits and skip structure
    skip_hg = int(scedes.get("HGSTRUCT", "0"))

    caps = pd.read_csv(caps_file_path, index_col="year", comment='#')

    restart.emission_emel_qhg[:, :] = 0.0  # Zero whole array before filling
    for i in range(restart.emission_num_hg_grp):
        restart.emission_emel_qhg[i, :] = caps[f"region {i + 1}"]
    i_end = restart.emission_num_hg_grp
    if skip_hg == 0 or restart.emission_usw_camr == 1:
        restart.emission_emel_qhg[:i_end, :] = 999.9
    restart.emission_emlim[3, :] = np.sum(
        restart.emission_emel_qhg[:i_end, :], axis=0
    )

    # Set the mercury switch (usw_hg)
    i_class = 0
    i_year_end = restart.ncntrl_ijumpyr
    emlim_check = np.any(restart.emission_emlim[3, :i_year_end] < 100.0)
    hg_checks = (
        np.any(restart.emission_hg_grams_mwh[i_class, :i_year_end] > 0.0)
        or np.any(restart.emission_hg_output[i_class, :, :i_year_end] > 0.0)
        or np.any(restart.emission_hg_input[i_class, :, :i_year_end] > 0.0)
        or np.any(restart.emission_hg_mef[i_class, :, :i_year_end] > 0.0)
    )
    usw_checks = restart.ecpcntl_usw_mact > 0 or restart.emission_usw_camr > 0
    if emlim_check or hg_checks or usw_checks:
        restart.ecpcntl_usw_hg = 1
    else:
        restart.ecpcntl_usw_hg = 0  # No limit

    # Mercury content for oil and gas in lbs/mmmmtbu
    restart.emeblk_hgdsel = float(parameters["hgdsel"])
    restart.emeblk_hgrlel = float(parameters["hgrlel"])
    restart.emeblk_hgrhel = float(parameters["hgrhel"])
    restart.emeblk_hgngel = float(parameters["hgngel"])

    log_table_title = "Loaded values of non-restart-file mercury inputs:"
    log_table_headers = ["Include\nFile", "Variable\nName", "Variable\nValue"]
    log_table_data = [
        ["ecpcntl", "usw_hg", restart.ecpcntl_usw_hg],
        ["ecpcntl", "usw_mact", restart.ecpcntl_usw_mact],
        ["ecpcntl", "umact_yr", restart.ecpcntl_umact_yr],
        ["ecpcntl", "usw_bact", restart.ecpcntl_usw_bact],
        ["ecpcntl", "ubact_yr", restart.ecpcntl_ubact_yr],
        ["ecpcntl", "usw_dsi", restart.ecpcntl_usw_dsi],
        ["ecpcntl", "udsi_yr", restart.ecpcntl_udsi_yr],
        ["emeblk", "hgdsel", restart.emeblk_hgdsel],
        ["emeblk", "hgrlel", restart.emeblk_hgrlel],
        ["emeblk", "hgrhel", restart.emeblk_hgrhel],
        ["emeblk", "hgngel", restart.emeblk_hgngel],
    ]
    log_it(
        log_table_title, "=" * len(log_table_title),
        tabulate.tabulate(
            log_table_data, headers=log_table_headers, numalign="right"
        ),
    )


def read_ab32_files(
    restart: Restart,
    scedes: Scedes,
    parameters_file_path: Path,
    data_file_path: Path,
) -> None:
    """Read the input files pertaining to California AB32.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    scedes : Scedes
        The current scedes file dict.
    parameters_file_path : Path
        Path to the TOML file that this function should open to find the AB32
        parameters.
    data_file_path : Path
        Path to the CSV file that this function should open to find the AB32
        data.

    See Also
    --------
    epm_read : Calls this function.
    """
    with parameters_file_path.open("rb") as f:
        parameters = tomllib.load(f)

    reserve_price_dollar_year = int(parameters["reserve_price_dollar_year"])
    reserve_price_start_year = int(parameters["reserve_price_start_year"])
    reserve_price_end_year = int(parameters["reserve_price_end_year"])
    reserve_price_tiers = int(parameters["reserve_price_tiers"])
    reserve_price_rate = float(parameters["reserve_price_rate"])
    auction_price_start_year = int(parameters["auction_price_start_year"])
    auction_price_end_year = int(parameters["auction_price_end_year"])
    auction_price_rate = float(parameters["auction_price_rate"])

    data = pd.read_csv(data_file_path, index_col="year", comment='#')

    restart.ab32_ab_cap_tot[:] = data["ab_cap_tot"]
    restart.ab32_ab_offset_frac[:] = data["ab_offset_frac"]
    restart.ab32_ab_cstcont_frac[:] = data["ab_cstcont_frac"]
    for i_tier in range(reserve_price_tiers):
        ab_reserve_p_column = f"ab_reserve_p({i_tier + 1})"
        restart.ab32_ab_reserve_p[i_tier, :] = data[ab_reserve_p_column]
    restart.ab32_ab_auction_p[:] = data["ab_auction_p"]

    # If no AB32, set inputs to unlimited or zero
    ab32sw = int(scedes.get("AB32SW", "1"))
    if ab32sw <= 0:
        restart.ab32_ab_cap_tot[:] = 272.727
        restart.ab32_ab_offset_frac[:] = 0.0
        restart.ab32_ab_cstcont_frac[:] = 0.0
        restart.ab32_ab_reserve_p[:reserve_price_tiers, :] = 0.0
        restart.ab32_ab_auction_p[:] = 0.0
        return

    # Reserve prices and auction prices come from input file in dollars per
    # metric ton of CO2. Convert them to dollars per kilogram of carbon.
    restart.ab32_ab_reserve_p[:reserve_price_tiers, :] *= (44.0 / 12.0) * 0.001
    restart.ab32_ab_auction_p *= (44.0 / 12.0) * 0.001

    # The reserve prices and auction prices also need to be converted to 1987$
    restart.ab32_ab_reserve_p[:reserve_price_tiers, :] /= (
        restart.macout_mc_jpgdp[reserve_price_dollar_year - 1987]
    )
    restart.ab32_ab_auction_p /= (
        restart.macout_mc_jpgdp[auction_price_start_year - 1987]
    )

    # Also convert caps from millions of metric tons of CO2 to millions of
    # metric tons of carbon
    restart.ab32_ab_cap_tot *= 12.0 / 44.0

    # Determine 1st year for containment reserve and accumulate total reserve
    # allowances in 1st year
    for i_year in range(restart.parametr_mnumyr):
        if restart.ab32_ab_cstcont_frac[i_year] > 0.0:
            restart.ab32_ab_cstcont_avl[i_year] = np.sum(
                restart.ab32_ab_cap_tot[i_year:]
                * restart.ab32_ab_cstcont_frac[i_year:]
            )
            break  # Because we found the 1st year

    for i_year in range(restart.parametr_mnumyr):
        year = restart.parametr_baseyr + i_year
        # Escalate the reserve price from its start year until its end year
        if year > reserve_price_start_year:
            reserve_price_escalation_years = (
                min(reserve_price_end_year, year) - reserve_price_start_year
            )
            restart.ab32_ab_reserve_p[:reserve_price_tiers, i_year] *= (
                (1.0 + reserve_price_rate) ** reserve_price_escalation_years
            )
        # Escalate the auction price from its start year until its end year
        if year > auction_price_start_year:
            auction_price_escalation_years = (
                min(auction_price_end_year, year) - auction_price_start_year
            )
            restart.ab32_ab_auction_p[i_year] *= (
                (1.0 + auction_price_rate) ** auction_price_escalation_years
            )
