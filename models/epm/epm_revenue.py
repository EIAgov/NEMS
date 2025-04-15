"""Functions for calculating the revenue effects of emissions policies.

This file implements the `initrev`, `accntrev`, and `epm_addoff` functions,
which compute the revenue effects from any active emissions policies. This
includes the revenue produced by initial allocation of permits. In a related
process, we also add up the qualifying emissions offsets where needed.
"""

from dataclasses import dataclass

import numpy as np
import numpy.typing as npt
import tabulate

from epm_common import BASE_YR, log_it
from epm_restart import Restart
from epm_scedes import Scedes
from epm_variables import Variables


def accntrev(restart: Restart, scedes: Scedes, variables: Variables) -> None:
    """Calculate the revenue effects of the selected emissions policy.

    For a tax, this amounts to the revenue returned to the government. For an
    auction, it amounts to the same thing. For a market, we need to subtract
    out the value of the initial allocation of permits, which is done by the
    `initrev` function. We sum the revenue by sector:
        0. Residential
        1. Commercial
        2. Industrial
        3. Transportation
        4. Utility
    The revenue from each sector is summed by fuel and is directly proportional
    to total emissions. Emissions associated with ethanol and biodiesel are
    assumed to be exempt, but emissions from geothermal and MSW are included.
    This function also handles the case of carbon offsets (with or without bio
    sequestration) and the case of an allowance price maximum (safety valve).

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
    epm_addoff : Called by this function to account for carbon offsets.
    initrev : Related function for computing the value of permit allocation.
    """
    allow_per_offset = 1.0  # Set to 1 allowance credit per offset

    allow_alloc = np.zeros((1, restart.parametr_mnumyr), np.float64)

    j = restart.ncntrl_curiyr - 1  # Zero-based index for Python
    i = restart.parametr_mnumcr - 1  # Last census region
    usa_pr = restart.parametr_mnumpr - 1  # Last mnumpr index
    usa_nr = restart.parametr_mnumnr - 1  # Last mnumnr index
    base_yr = BASE_YR - 1  # Zero-based Python index

    etax = EtaxLogic(restart, j)

    restart.emission_emrev[:, j] = 0.0

    if (
        restart.emoblk_bank_flag
        and restart.ncntrl_curcalyr < restart.epmbank_bank_startyr
    ):
        return
    if (
        (restart.emoblk_market_flag or restart.emoblk_permit_flag)
        and etax.etax == 0.0
    ):
        return

    # These emrev[0:5,] calculations were designed for a carbon or energy tax
    # case. They do not apply to a cap and trade policy. The emrev[0:5,] values
    # are no longer reported in the default version of ftab table 118. Instead,
    # auction revenues, allowance distribution value, and offset value
    # associated with a cap and trade policy are calculated further below
    # (emrev[5:12,]). These results are used by the macro module and reported
    # in ftab table 118.
    restart.emission_emrev[0, j] += etax.resd * (
        # Residential natural gas, core
        restart.qblk_qgfrs[i, j] * restart.emeblk_egfrs[j]
        # Residential natural gas, noncore
        + restart.qblk_qgirs[i, j] * restart.emeblk_egirs[j]
        # Residential coal
        + restart.qblk_qclrs[i, j] * restart.emeblk_eclrs[j]
        # Residential distillate
        + restart.qblk_qdsrs[i, j] * restart.emeblk_edsrs[j]
        # Residential kerosene
        + restart.qblk_qksrs[i, j] * restart.emeblk_eksrs[j]
        # Residential propane
        + restart.qblk_qprrs[i, j] * restart.emeblk_eprrs[j]
    )

    log_table_data = [
        ["emrev", restart.emission_emrev[0, j]],
        ["qgfrs", restart.qblk_qgfrs[i, j]],
        ["etax", etax.resd],
        ["egfrs", restart.emeblk_egfrs[j]],
    ]
    log_it(
        tabulate.tabulate(log_table_data, tablefmt="plain", numalign="right"),
        verbose=True
    )

    restart.emission_emrev[1, j] += etax.comm * (
        # Commercial natural gas, core
        restart.qblk_qgfcm[i, j] * restart.emeblk_egfcm[j]
        # Commercial natural gas, noncore
        + restart.qblk_qgicm[i, j] * restart.emeblk_egicm[j]
        # Commercial coal
        + restart.qblk_qclcm[i, j] * restart.emeblk_eclcm[j]
        # Commercial motor gasoline
        + restart.qblk_qmgcm[i, j] * restart.emeblk_emgcm[j]
        # Commercial distillate
        + restart.qblk_qdscm[i, j] * restart.emeblk_edscm[j]
        # Commercial kerosene
        + restart.qblk_qkscm[i, j] * restart.emeblk_ekscm[j]
        # Commercial propane
        + restart.qblk_qprcm[i, j] * restart.emeblk_eprcm[j]
        # Commercial residual fuel
        + restart.qblk_qrscm[i, j] * restart.emeblk_erscm[j]
    )

    # TODO: There's no industrial or refinery carbon capture accounted for in
    # here. Is that an oversight in need of fixing, or was it intentional?
    restart.emission_emrev[2, j] += etax.endu * (
        # Industrial natural gas fuel
        (
            restart.qblk_qngin[i, j]
            - restart.indout_inqngpf[i, j]  # Feedstocks
            - restart.hmmblk_qnghmhp[i, j]  # Hydrogen heat & power
            - restart.hmmblk_qnghmpf[i, j]  # Hydrogen feedstocks
        ) * restart.emeblk_engin[j]
        # Industrial natural gas feedstocks
        + restart.indout_inqngpf[i, j] * restart.emeblk_enqngpf[j]
        # Industrial lease and plant fuel
        + restart.qblk_qlpin[i, j] * restart.emeblk_elpin[j]
        # Industrial natural gas for liquefaction
        + restart.qblk_qnglq[i, j] * restart.emeblk_elpin[j]
        # Industrial natural gas for hydrogen heat & power
        + restart.hmmblk_qnghmhp[i, j] * restart.emeblk_engin[j]
        # Industrial natural gas for hydrogen feedstocks
        + restart.hmmblk_qnghmpf[i, j] * restart.emeblk_enghm[j]
        # Industrial steam coal
        + restart.qblk_qclin[i, j] * restart.emeblk_eclin[j]
        # Industrial metallurgical coal
        + restart.qblk_qmcin[i, j] * restart.emeblk_emcin[j]
        # Industrial net coal coke imports
        + restart.qblk_qciin[i, j] * restart.emeblk_eciin[j]
        # Industrial motor gasoline
        + restart.qblk_qmgin[i, j] * restart.emeblk_emgin[j]
        # Industrial distillate
        + restart.qblk_qdsin[i, j] * restart.emeblk_edsin[j]
        # Industrial kerosene
        + restart.qblk_qksin[i, j] * restart.emeblk_eksin[j]
        # Industrial ethane fuel
        + (restart.qblk_qetin[i, j] - restart.qblk_qetinpf[i, j])
        * restart.emeblk_eetin[j]
        # Industrial propane fuel
        + (restart.qblk_qprin[i, j] - restart.qblk_qprinpf[i, j])
        * restart.emeblk_eprin[j]
        # Industrial butane fuel
        + (restart.qblk_qbuin[i, j] - restart.qblk_qbuinpf[i, j])
        * restart.emeblk_ebuin[j]
        # Industrial isobutane fuel
        + (restart.qblk_qisin[i, j] - restart.qblk_qisinpf[i, j])
        * restart.emeblk_eisin[j]
        # Industrial pentanes plus fuel
        + (restart.qblk_qppin[i, j] - restart.qblk_qppinpf[i, j])
        * restart.emeblk_eppin[j]
        # Industrial ethane feedstocks
        + restart.qblk_qetinpf[i, j] * restart.emeblk_eetinpf[j]
        # Industrial propane feedstocks
        + (restart.qblk_qprinpf[i, j] + restart.qblk_qprolenerf[i, j])
        * restart.emeblk_eprinpf[j]
        # Industrial butane feedstocks
        + restart.qblk_qbuinpf[i, j] * restart.emeblk_ebuinpf[j]
        # Industrial isobutane feedstocks
        + restart.qblk_qisinpf[i, j] * restart.emeblk_eisinpf[j]
        # Industrial pentanes plus feedstocks
        + restart.qblk_qppinpf[i, j] * restart.emeblk_eppinpf[j]
        # Industrial residual fuel
        + restart.qblk_qrsin[i, j] * restart.emeblk_ersin[j]
        # Industrial petrochemical feedstocks
        + restart.qblk_qpfin[i, j] * restart.emeblk_epfin[j]
        # Industrial petroleum coke
        + restart.qblk_qpcin[i, j] * restart.emeblk_epcin[j]
        # Industrial still gas
        + restart.qblk_qsgin[i, j] * restart.emeblk_esgin[j]
        # Industrial other petroleum
        # Lubricants still contained in qotin/eotin
        + restart.qblk_qotin[i, j] * restart.emeblk_eotin[j]
    )

    restart.emission_emrev[3, j] += etax.tran * (
        # Transportation natural gas, core
        restart.qblk_qgftr[i, j] * restart.emeblk_egftr[j]
        # Transportation natural gas, noncore
        + restart.qblk_qgitr[i, j] * restart.emeblk_egitr[j]
        # Transportation pipeline natural gas
        + restart.qblk_qgptr[i, j] * restart.emeblk_egptr[j]
        # Transportation motor gasoline
        + restart.qblk_qmgtr[i, j] * restart.emeblk_emgtr[j]
        # Transportation distillate
        + restart.qblk_qdstr[i, j] * restart.emeblk_edstr[j]
    )

    # Take out revenue associated with ethanol and biodiesel emissions, assumed
    # exempt. Add revenue associated with gasoline portion of E85.
    e85_gasoline_share = (
        (restart.pmmout_trgne85 * restart.convfact_cfrbob[j])
        / (
            restart.pmmout_trgne85 * restart.convfact_cfrbob[j]
            + restart.pmmout_ethne85 * restart.convfact_cfetq[j]
        )
    )
    e85_ethanol_share = (
        (restart.pmmout_ethne85 * restart.convfact_cfetq[j])
        / (
            restart.pmmout_trgne85 * restart.convfact_cfrbob[j]
            + restart.pmmout_ethne85 * restart.convfact_cfetq[j]
        )
    )
    ethanol_tot = (
        365.0 * restart.convfact_cfetq[j]
        * (
            restart.pmmrpt_crnethcd[i, j]
            + restart.pmmrpt_cllethcd[i, j]
            + restart.pmmrpt_othethcd[i, j]
            + restart.pmmrpt_ethimp[i, j]
            - restart.pmmrpt_ethexp[i, j]
        ) / 1000.0
    )
    ethanol_e85 = restart.qblk_qettr[i, j] * e85_ethanol_share
    ethanol_blended = ethanol_tot - ethanol_e85

    # Add other sources of renewable gasoline
    allrenewablegasoline = (
        # NER gasoline
        365.0 * restart.lfmmout_grn2mgqty[usa_pr, j]
        / 1000.0 * restart.convfact_cfnpq
        # BTL gasoline
        + 365.0 * (
            restart.pmmout_btlfrac[0, usa_pr, j]
            + restart.pmmout_btlfrac[1, usa_pr, j]
        ) / 1000.0 * restart.convfact_cfnpq
        # BPU gasoline
        + 365.0 * restart.pmmftab_ubavolmg[usa_pr, j]
        / 1000.0 * restart.convfact_cfnpq
        # B part of CBTL gasoline
        + 365.0 * (
            restart.pmmout_cbtlfrac[1, 0, usa_pr, j]
            + restart.pmmout_cbtlfrac[1, 1, usa_pr, j]
        ) / 1000.0 * restart.convfact_cfnpq

        + ethanol_blended
    )

    # Subtract out biodiesel used as blending stock. Includes soy, yellow
    # grease, white grease
    biodiesel = (
        (
            np.sum(restart.pmmrpt_bimqtycd[:4, i, j])
            + restart.pmmrpt_biodimp[i, j] - restart.pmmrpt_biodexp[i, j]
        )
        # Converting mbcd to tril btu
        * restart.convfact_cfbiod[j] * 365.0 / 1000.0
    )

    # Add other zero carbon diesel blendstocks
    allrenewable_diesel = (
        # NERD
        365.0 * restart.lfmmout_grd2dsqty[usa_pr, j]
        / 1000.0 * restart.convfact_cfdsq
        # BTL Diesel
        + 365.0 * (
            restart.pmmout_btlfrac[2, usa_pr, j]
            + restart.pmmout_btlfrac[3, usa_pr, j]
        ) / 1000.0 * restart.convfact_cfdsq
        # BPU Diesel
        + 365.0 * restart.pmmftab_ubavolds[usa_pr, j]
        / 1000.0 * restart.convfact_cfdsq
        # B part of CBTL Diesel
        + 365.0 * (
            restart.pmmout_cbtlfrac[1, 2, usa_pr, j]
            + restart.pmmout_cbtlfrac[1, 3, usa_pr, j]
        ) / 1000.0 * restart.convfact_cfdsq

        + biodiesel
    )

    restart.emission_emrev[3, j] += etax.tran * (
        # Transportation adjustment for ethanol blending in motor gasoline
        -allrenewablegasoline * restart.emeblk_emgtr[j]
        # Transportation adjustment for biomass based diesel blending in
        # distillate
        - allrenewable_diesel * restart.emeblk_edstr[j]
        # Transportation gasoline part of E85
        + restart.qblk_qettr[i, j] * e85_gasoline_share
        * restart.emeblk_emgtr[j]
        # Transportation jet fuel
        + restart.qblk_qjftr[i, j] * restart.emeblk_ejftr[j]
        # Transportation propane
        + restart.qblk_qprtr[i, j] * restart.emeblk_eprtr[j]
        # Transportation residual fuel, low sulfur
        + restart.qblk_qrltr[i, j] * restart.emeblk_erltr[j]
        # Transportation residual fuel, high sulfur
        + restart.qblk_qrhtr[i, j] * restart.emeblk_erhtr[j]
        # Transportation other petroleum
        + restart.qblk_qottr[i, j] * restart.emeblk_eottr[j]
        # Transportation methanol
        + restart.qblk_qmetr[i, j] * restart.emeblk_emetr[j]
    )

    mnumcr2 = restart.parametr_mnumcr - 2
    restart.emission_emrev[4, j] += etax.elec * np.sum(
        # Electricity distillate
        restart.qblk_qdsel[:mnumcr2, j] * restart.emeblk_edsel[j]
        * (1.0 - restart.uefdout_xdsel[:mnumcr2, j])
        # Eletricity residual fuel, low sulfur
        + restart.qblk_qrlel[:mnumcr2, j] * restart.emeblk_erlel[j]
        * (1.0 - restart.uefdout_xrlel[:mnumcr2, j])
        # Electricity residual fuel, high sulfur
        + restart.qblk_qrhel[:mnumcr2, j] * restart.emeblk_erhel[j]
        * (1.0 - restart.uefdout_xrhel[:mnumcr2, j])
    )

    # Sum electricity coal over coal/utility regions
    restart.emission_emrev[4, j] += etax.elec * np.sum(
        restart.coalemm_qclclnr[:, j, :] * restart.coalemm_cclclnr[:, j, :]
        * (1.0 - restart.coalemm_xclclnr[:, j, :])
    )

    # Geothermal
    # 2.0536 from GHG pub xls file param04.xls, range EF_GEO. based on btu
    # production (generation)
    egeel = 3.412 * 2.0536  # Temporary for geothermal emissions factor
    geogen_bkwh = (
        np.sum(restart.uefdout_ugngenr[:, usa_nr, j])
        + np.sum(restart.cogen_cgntgen[usa_nr, j, 4, :]) * 0.001  # bkwh
    )
    geogen_bkwh_hist = (
        np.sum(restart.uefdout_ugngenr[:, usa_nr, base_yr])
        + np.sum(restart.cogen_cgntgen[usa_nr, base_yr, 4, :]) * 0.001  # bkwh
    )

    # Other: geothermal and non-biogenic waste energy
    # Set up emission factor for non-biogenic waste energy using GHG history
    # and bio waste-energy consumption
    restart.emeblk_emsel[j] = (
        (
            variables.history[3, 4, base_yr] * 1000.0
            - geogen_bkwh_hist * egeel
        ) / (restart.wrenew_wncmsel[base_yr, i] * 1000.0)
    )
    restart.emission_emrev[4, j] += etax.elec * (
        # Electricity geothermal
        geogen_bkwh * egeel
        # Electricity municipal solid waste (MSW)
        + 1000.0 * restart.wrenew_wncmsel[max(base_yr, j), i]
        * restart.emeblk_emsel[j]
    )

    # Sum electricity natural gas over gas/utility regions
    restart.emission_emrev[4, j] += etax.elec * np.sum(
        # Electricity natural gas, firm
        restart.uefdout_qgfelgr[:, j] * restart.emeblk_egfelgr[j]
        * (1.0 - restart.uefdout_xqgfelgr[:, j])
        # Electricity natural gas, interruptible
        + restart.uefdout_qgielgr[:, j] * restart.emeblk_egielgr[j]
        * (1.0 - restart.uefdout_xqgielgr[:, j])
        # Electricity natural gas, competitive
        + restart.uefdout_qgcelgr[:, j] * restart.emeblk_egcelgr[j]
        * (1.0 - restart.uefdout_xqgcelgr[:, j])
    )

    # We total revenue in billions of dollars, requiring division by 1000
    restart.emission_emrev[:5, j] /= 1000.0

    # Eliminate sharing to sector--misleading and won't get used in this
    # version
    if not (
        restart.emoblk_bank_flag or restart.emoblk_permit_flag
        or restart.emoblk_market_flag
    ):
        restart.emission_emrev[5, j] = np.sum(restart.emission_emrev[:5, j])
        return

    # Runtime option. with offsets, bioseqok = 1, bio sequestration offsets
    # count towards goal. 0==> incentives given but don't count toward goal
    # If 1, bio sequestration counts as an offsets. If 0, incentive only
    bioseqok = int(scedes.get("BIOSEQOK", "1"))
    qoff = epm_addoff(
        restart.epmbank_offset, restart.parametr_baseyr, j,
        bioseqok, allow_per_offset, 2012
    )

    # Recalc potential revenue.
    # The potential revenue collected at auction (at 100% auction) would be
    # the number of allowances times the allowance price. This assumes no
    # safety valve and no increase in the number allowances issued in
    # exchange for offsets. Safety-valve sales complicate matters, as would
    # banking with a safety sales program. Usually this section has to be
    # customized for programs with safety-valves
    variables.accntrev_bank_local[j] = (
        restart.emoblk_emissions_goal[j]
        + qoff - restart.emission_emsol[0, j]
    )
    if j == 0:
        variables.accntrev_balance_local[j] = (
            variables.accntrev_bank_local[j]
        )
    else:
        variables.accntrev_balance_local[j] = (
            restart.epmbank_balance[j - 1] + variables.accntrev_bank_local[j]
        )

    rev_before = (
        np.sum(restart.emission_emrev[:5, j])
        + restart.ghgrep_ghg_rev[0, j] * restart.macout_mc_jpgdp[j + 3]
    )

    # emrev[8, j] filled in electricity module so don't initialize it here
    restart.emission_emrev[5:8, j] = 0.0
    restart.emission_emrev[9:, j] = 0.0

    # For banking cases with allowance price maximum (safety valve)
    # acccount for the use of previously banked allowances to reduce the
    # need for safety value allowance purchases.
    restart.emission_emrev[5, j] = (
        # 100% auction revenue = allowances issued times price
        restart.emission_emlim[0, j] * etax.etax
    )
    safety_sales = 0.0
    if restart.emoblk_emtax[j] >= restart.emoblk_max_tax[j] - 0.00001:
        safety_sales = max(
            0.0,
            (
                restart.emoblk_emissions_goal[j]
                + qoff - restart.emission_emsol[0, j]
            )
        )

    # Safety valve revenue
    restart.emission_emrev[6, j] = safety_sales * etax.etax

    if restart.emoblk_bank_flag:
        safety_sales = 0.0
        # If in withdrawal period, you may cover your excess emissions with
        # bank withdrawals until you are tapped out. You are tapped out
        # when the balance goes below zero. So, if the emissions greater
        # than the # allowances (bank_local[j] < 0) and you are tapped out
        # (balance[j] < 0), then you can buy safety_allowances
        if (
            variables.accntrev_bank_local[j] < 0.0
            and variables.accntrev_balance_local[j] <= 0.0
        ):
            # So safety sales have to cover the remainder of the shortfall
            # (balance_local), or the entire shortfall assuming you were
            # short last year (bank_local).
            safety_sales = -max(
                variables.accntrev_balance_local[j],
                variables.accntrev_bank_local[j]
            )
            # Safety valve revenue
        restart.emission_emrev[6, j] = safety_sales * etax.etax

    # Total potential revenue, including safety sales
    rev = np.sum(restart.emission_emrev[5:7, j])

    # Under s2191, the auction pool for 2012-2014 includes some allowances
    # reserved for early auctioning. Calculation the potential pool
    # assuming the allowances are sold in the issue year.
    pot_auction_pool = (
        restart.emission_emlim[0, j] * allow_alloc[0, j] / 100.0
    )

    # Assign auction revenue based on year allowance is created for
    restart.emission_emrev[11, j] = pot_auction_pool * etax.etax

    # emrev[7,] was used once to hold auction revenue given an
    # early-auction from the allocation for the first three compliance
    # years. no longer used.
    restart.emission_emrev[7, j] = (
        # etax.etax is in nominal dollars here
        restart.emission_emlim[0, j] * etax.etax * allow_alloc[0, j] / 100.0
    )

    # Calculate the value of the incremental sequestration incentive
    # (s2191).
    restart.emission_emrev[9, j] = restart.epmbank_offset[3, j] * etax.etax

    # Spending on int'l offsets
    restart.emission_emrev[10, j] = (
        restart.ghgrep_ghg_rev[3, j] * restart.macout_mc_jpgdp[j + 3]
    )

    # Share out to sectors based on emissions
    adjust = 1.0
    if rev > 0.0 and rev_before != 0.0:
        adjust = rev / rev_before
        restart.emission_emrev[:5, j] *= adjust
    restart.ghgrep_ghg_rev[0, j] *= adjust


def initrev(restart: Restart) -> None:
    """Calculate revenue produced by holding the initial allocation of permits.

    The total revenue is the number of permits multiplied by the value of each
    permit. We sum this revenue by sector:
        0. Residential
        1. Commercial
        2. Industrial
        3. Transportation
        4. Utility
    Note that calling this subroutine only has an effect when the bank flag is
    set in EPMCNTL and the GHG banking and compliance period has begun -- it
    otherwise updates none of the common block variables.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.

    See Also
    --------
    epm : Core EPM routine that directly calls this function.
    accntrev : Related function for computing all other revenue effects.
    """
    j = restart.ncntrl_curiyr - 1  # Zero-based Python index

    etax = EtaxLogic(restart, j)

    if (
        restart.emoblk_bank_flag
        and restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr
    ):
        restart.emission_emrev[:5, j] -= (
            restart.emoblk_init_alloc[:5, j]
            * np.array([etax.resd, etax.comm, etax.endu, etax.tran, etax.elec])
        )


def epm_addoff(
    offset: npt.NDArray[np.float64],
    baseyr: int,
    iy: int,
    bioseqok: int,
    allow_per_offset: float,
    offyear: int
) -> float:
    """Add up qualifying emissions offsets.

    Called by `epm` to sum up qualifying emissions offsets for one year in some
    policy scenarios. The result depends on whether bio sequestration counts
    towards the goal and on the exchange rate of allowance credits per
    international offset (after a specified calendar year).

    Parameters
    ----------
    offset : npt.NDArray[np.float64]
        The emissions offsets array--usually the array named `offset` that is
        defined in the `epmbank` includes file.
    baseyr : int
        Base NEMS calendar year corresponding to `curiyr` = 1.
    iy : int
        Zero-based year index for adding up offsets.
    bioseqok : int
        Set to 1 if bio sequestration offsets count towards the goal or to 0 if
        incentives are given but do not count towards the goal (incentive
        only).
    allow_per_offset : float
        Exchange rate in allowance credits per international offset.
    offyear : int
        Calendar year after which `allow_per_offset` starts being applied.

    Returns
    -------
    float
        Sum of qualifying emissions offsets.

    See Also
    --------
    epm : Calls this function in several places to add up emissions offsets.
    accntrev : Calls this function to add up emissions offsets.
    """
    calyr = baseyr + iy
    offset_weights = np.ones(4, np.float64)

    # Omit offset[1, iy] carbon sequestration if incentive only. Otherwise,
    # carbon sequestration counts as an offset.
    if bioseqok != 1:
        offset_weights[1] = 0.0

    # The offset_weights[2] value defaults to 1.0 when calyr <= offyear
    if calyr > offyear:
        offset_weights[2] = allow_per_offset

    qoff = np.sum(offset[:4, iy] * offset_weights)
    return float(qoff)


@dataclass(init=False, eq=False)
class EtaxLogic:
    """Handle standard etax logic and store the etax values for each sector."""
    etax: float  # Base etax value
    elec: float  # For electricity sector
    tran: float  # For transportation sector
    endu: float  # For end use (industrial) sector
    resd: float  # For residential sector
    comm: float  # For commercial sector

    def __init__(
        self,
        restart: Restart,
        j: int,
        *,
        use_1987_dollars: bool = False
    ) -> None:
        """Create an etax object with pre-computed etax values for each sector.

        Parameters
        ----------
        restart : Restart
            The currently loaded restart file data.
        j : int
            Zero-based index of the current NEMS year.
        use_1987_dollars : bool, optional
            Whether the computed etax values should be given in 1987$ or not.
            The default is False, which yields nominal dollars.
        """
        self.etax = restart.emoblk_emtax[j]
        is_1987_dollars = not bool(restart.emoblk_nominal_flag)
        deflator = restart.macout_mc_jpgdp[j + 3]

        if not use_1987_dollars and is_1987_dollars:
            self.etax *= deflator
        elif use_1987_dollars and not is_1987_dollars:
            self.etax /= deflator

        self.elec = self.etax
        self.tran = self.etax
        self.endu = self.etax
        self.resd = self.etax
        self.comm = self.etax

        elec_only = bool(restart.emoblk_elec_flag)
        tran_only = bool(restart.emoblk_tran_flag)
        resd_excluded = bool(restart.emoblk_resd_flag)
        comm_excluded = bool(restart.emoblk_comm_flag)

        if elec_only:
            self.tran, self.endu, self.resd, self.comm = 0.0, 0.0, 0.0, 0.0
        if tran_only:
            self.elec, self.endu, self.resd, self.comm = 0.0, 0.0, 0.0, 0.0
        if resd_excluded:
            self.resd = 0.0
        if comm_excluded:
            self.comm = 0.0
