"""Policy functions for applying price adjustments to the end-use fuels.

This file implements the `etax_adjust` and `price_adjust` functions, which
calculate price adjustments for energy taxes and carbon prices, respectively.
"""

import numpy as np
import tabulate

from epm_common import log_it
from epm_restart import Restart
from epm_revenue import EtaxLogic
from epm_scedes import Scedes


def etax_adjust(restart: Restart) -> None:
    """Calculate price adjustments (energy taxes) to apply to end-use fuels.

    This function is called by the `epm` function if the energy tag flag is set
    in the EPMCNTL file. The bulk of the function assigns energy taxes to the
    appropriate price adjustment variables defined in the EMABLK common block.
    In each case, we are finding the price that the demand sectors respond to
    and adjusting that price based on an energy tax. This is implemented by
    comparing prices from the previous two NEMS cycles.

    This is closely related to the `price_adjust` function, which carries out
    similar calculations for the case of carbon prices.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.

    See Also
    --------
    epm : Core EPM routine that calls this function to compute adjustments.
    price_adjust : Similar function for the case of a carbon price.
    """
    # Zero-based Python indices
    jstart = restart.ncntrl_curiyr - 1
    jend = jstart

    # run this routine for all years the first time through.
    # this is so that EMM will have some initial tax values to work with in
    # setting price expectations (for environmental adders).
    if (
        restart.ncntrl_curiyr == restart.ncntrl_firsyr
        and restart.ncntrl_curitr == 1
    ):
        firsyr = restart.ncntrl_firsyr - 1
        lastyr = restart.ncntrl_lastyr - 1
        # Sum carbon tax using ftab variable
        sumchk = np.sum(np.abs(restart.emission_emetax[0, firsyr:lastyr+1]))
        if sumchk == 0.0:
            restart.emission_emetax[:2, :lastyr+1] = (
                restart.emoblk_emtax[:lastyr+1]
            )
            if restart.emoblk_nominal_flag:
                restart.emission_emetax[:2, :lastyr+1] /= (
                    restart.macout_mc_jpgdp[3:lastyr+3+1]
                )
        # For electricity expectations, average taxes of prior run (Dad) with
        # those of Dad's Dad (Granddad) for expectations
        if restart.emoblk_permit_flag or restart.emoblk_market_flag:
            # Must be a cap case (permit or auction)
            log_table_headers = ["Year", "Guess", "Dad", "His-Guess"]
            log_table_data = []
            for j in range(restart.ncntrl_lastyr):
                # Average Dad results and Dad's guess for expectations
                restart.emoblk_emtax[j] = np.average(
                    restart.emission_emetax[:2, j]
                )
                log_table_data.append([
                    restart.parametr_baseyr + j,
                    restart.emoblk_emtax[j],
                    restart.emission_emetax[0, j],
                    restart.emission_emetax[1, j],
                ])
                # Store the guess
                restart.emission_emetax[1, j] = restart.emoblk_emtax[j]
            log_it(tabulate.tabulate(
                log_table_data, headers=log_table_headers
            ))
        jend = restart.ncntrl_lastyr - 1

    for j in range(jstart, jend + 1):

        etax = EtaxLogic(restart, j, use_1987_dollars=True)

        # Btu tax -- for ftab rpt
        restart.emission_emetax[0, j] = etax.etax
        # Carbon goal -- ftab
        restart.emission_emlim[0, j] = restart.emoblk_emissions_goal[j]

        # Natural gas, core (resd, comm, tran, indy, elec)
        restart.emablk_jgfrs[j] = etax.resd
        restart.emablk_jgfcm[j] = etax.comm
        restart.emablk_jgftr[j] = etax.tran
        restart.emablk_jgfin[j] = etax.endu
        restart.emablk_jgfel[j] = etax.elec

        # Natural gas, noncore (resd, comm, tran, indy, elec)
        restart.emablk_jgirs[j] = etax.resd
        restart.emablk_jgicm[j] = etax.comm
        restart.emablk_jgitr[j] = etax.tran
        restart.emablk_jgiin[j] = etax.endu
        restart.emablk_jgiel[j] = etax.elec

        # Natural gas, total (resd, comm, tran, indy, elec)
        restart.emablk_jngrs[j] = etax.resd
        restart.emablk_jngcm[j] = etax.comm
        restart.emablk_jngtr[j] = etax.tran
        restart.emablk_jngin[j] = etax.endu
        restart.emablk_jnqngpf[j] = etax.endu  # Natural gas feedstock
        restart.emablk_jngel[j] = etax.elec

        # Petroleum products
        restart.emablk_jgptr[j] = etax.tran
        restart.emablk_jlpin[j] = etax.endu
        restart.emablk_jclrs[j] = etax.resd
        restart.emablk_jclcm[j] = etax.comm
        restart.emablk_jclin[j] = etax.endu
        restart.emablk_jclel[j] = etax.elec
        restart.emablk_jmcin[j] = etax.endu
        restart.emablk_jmgcm[j] = etax.comm
        restart.emablk_jmgtr[j] = etax.tran
        restart.emablk_jmgin[j] = etax.endu
        restart.emablk_jjftr[j] = etax.tran
        restart.emablk_jdsrs[j] = etax.resd
        restart.emablk_jdscm[j] = etax.comm
        restart.emablk_jdstr[j] = etax.tran
        restart.emablk_jdsin[j] = etax.endu
        restart.emablk_jdsel[j] = etax.elec
        restart.emablk_jksrs[j] = etax.resd
        restart.emablk_jkscm[j] = etax.comm
        restart.emablk_jksin[j] = etax.endu
        restart.emablk_jlgrs[j] = etax.resd
        restart.emablk_jprrs[j] = etax.resd
        restart.emablk_jlgcm[j] = etax.comm
        restart.emablk_jprcm[j] = etax.comm
        restart.emablk_jlgtr[j] = etax.tran
        restart.emablk_jprtr[j] = etax.tran
        restart.emablk_jlgin[j] = etax.endu
        restart.emablk_jetin[j] = etax.endu
        restart.emablk_jprin[j] = etax.endu
        restart.emablk_jbuin[j] = etax.endu
        restart.emablk_jisin[j] = etax.endu
        restart.emablk_jnqlgpf[j] = etax.endu
        restart.emablk_jetinpf[j] = etax.endu
        restart.emablk_jprinpf[j] = etax.endu
        restart.emablk_jbuinpf[j] = etax.endu
        restart.emablk_jisinpf[j] = etax.endu
        restart.emablk_jrlcm[j] = etax.comm
        restart.emablk_jrltr[j] = etax.tran
        restart.emablk_jrlin[j] = etax.endu
        restart.emablk_jrlel[j] = etax.elec
        restart.emablk_jrhtr[j] = etax.tran
        restart.emablk_jrhel[j] = etax.elec
        restart.emablk_jrscm[j] = etax.comm
        restart.emablk_jrstr[j] = etax.tran
        restart.emablk_jrsin[j] = etax.endu
        restart.emablk_jrsel[j] = etax.elec
        restart.emablk_jpfin[j] = etax.endu
        restart.emablk_jpcin[j] = etax.endu
        restart.emablk_jpcel[j] = etax.elec
        restart.emablk_jsgin[j] = etax.endu
        restart.emablk_jotin[j] = etax.endu
        restart.emablk_jottr[j] = etax.tran
        restart.emablk_jmetr[j] = etax.tran
        restart.emablk_jettr[j] = etax.tran

        restart.emablk_jclclnr[j, :restart.coalemm_nclut1] = etax.elec
        restart.emablk_jclelcdr[j, :] = etax.elec

        # Adjusts by gas/utility regions
        restart.emablk_jgfelgr[j] = etax.elec
        restart.emablk_jgielgr[j] = etax.elec
        restart.emablk_jgcelgr[j] = etax.elec

        log_table_data = [
            ["j", j],
            ["jmetr[j]", restart.emablk_jmetr[j]],
            ["jettr[j]", restart.emablk_jettr[j]],
            ["jngel[j]", restart.emablk_jngel[j]]
        ]
        log_it(
            tabulate.tabulate(log_table_data, numalign="right"), verbose=True
        )

        log_it(
            "ADJUSTED FUEL PRICES --- REGION 1, YEAR 1",
            "VARIABLES FROM MPRC --- PRICES",
            restart.mpblk_mprc[0, j, :],
            "VARIABLES FROM AMPRC",
            restart.ampblk_mprc[0, j, :],
            verbose=True
        )

        log_it(
            "NATURAL GAS UTILITY VARIABLES (NOT COMPLETED)",
            "REGION 1, YEAR 1",
            f"jgfelgr[0] = {restart.emablk_jgfelgr[0]}",
            f"jgielgr[0] = {restart.emablk_jgielgr[0]}",
            f"jgcelgr[0] = {restart.emablk_jgcelgr[0]}",
            verbose=True
        )


def price_adjust(restart: Restart, scedes: Scedes) -> None:
    """Calculate price adjustments (carbon content) to apply to end-use fuels.

    Called by the `epm` function if the tag flag, market flag, or permit flag
    is set in the EPMCNTL file. The bulk of this function assigns carbon prices
    to the appropriate price adjustment variables defined in the EMABLK common
    block. In each case, we are finding the price that the demand sectors
    respond to and adjusting the price based on the carbon content of the fuel
    and the tax. This is implemented by comparing prices from the previous two
    NEMS cycles.

    This is closely related to the `etax_adjust` function, which carries out
    similar calculations for the case of an energy tax.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    scedes : Scedes
        The current scedes file dict.

    See Also
    --------
    epm : Core EPM routine that calls this function to compute adjustments.
    etax_adjust : Similar function for the case of an energy tax.
    """
    # Zero-based python indices
    usa = restart.parametr_mnumcr - 1
    jstart = restart.ncntrl_curiyr - 1
    jend = jstart

    # Run this routine for all years the first time through. This is so that
    # EMM will have some initial tax values to work with in setting price
    # expectations (for environmental adders).
    if (
        restart.ncntrl_curiyr == restart.ncntrl_firsyr
        and restart.ncntrl_curitr == 1
    ):
        firsyr = restart.ncntrl_firsyr - 1
        lastyr = restart.ncntrl_lastyr - 1
        # Sum carbon tax using ftab variable
        sumchk = np.sum(np.abs(restart.emission_emetax[0, firsyr:lastyr+1]))
        # For carbon fee case, set values for taxes from EPMDATA. If it is a
        # cap case (permit_flag != 0 or market_flag != 0 or bank_flag != 0),
        # then carbon fees will be reset based on what is in the restart file,
        # so we don't want to overwrite those numbers with the EPMDATA values.
        # Since banking cases start out with tax_flag != 0, then switch to a
        # permit_flag or market_flag cap case at the end of the banking
        # interval, we want to avoid overwriting the restart file prices for
        # the bank_flag case too.
        if (
            (sumchk == 0.0 or restart.emoblk_tax_flag)
            and not restart.emoblk_bank_flag
        ):
            restart.emission_emetax[:2, :lastyr+1] = (
                restart.emoblk_emtax[:lastyr+1]
            )
            if restart.emoblk_nominal_flag:
                restart.emission_emetax[:2, :lastyr+1] /= (
                    restart.macout_mc_jpgdp[3:lastyr+3+1]
                )
        # For electricity expectations, average taxes of prior run (Dad) with
        # those of Dad's Dad (Granddad) for expectations
        if not restart.emoblk_tax_flag:
            # Must be a cap case (permit or auction)
            wgtguess = float(scedes.get("WGTGUESS", "50")) / 100.0
            log_table_headers = ["Year", "Guess", "Dad", "His-Guess"]
            log_table_data = []
            for j in range(restart.ncntrl_lastyr):
                # Average Dad's results and Dad's guess for expectations based
                # on runtime option wgtguess
                restart.emoblk_emtax[j] = np.average(
                    restart.emission_emetax[:2, j],
                    weights=[1.0 - wgtguess, wgtguess]
                )
                log_table_data.append([
                    restart.parametr_baseyr + j,
                    restart.emoblk_emtax[j],
                    restart.emission_emetax[0, j],
                    restart.emission_emetax[1, j],
                ])
                # Store the guess
                restart.emission_emetax[1, j] = restart.emoblk_emtax[j]
            log_it(tabulate.tabulate(
                log_table_data, headers=log_table_headers
            ))
        jend = restart.ncntrl_lastyr - 1

    for j in range(jstart, jend + 1):

        # Max tax or TAP or Safety-Value
        restart.emission_emetax[2, j] = restart.emoblk_max_tax[j]

        etax = EtaxLogic(restart, j, use_1987_dollars=True)

        # Carbon penalty -- for ftab rpt
        restart.emission_emetax[0, j] = etax.etax
        # Carbon goal -- ftab
        restart.emission_emlim[0, j] = restart.emoblk_emissions_goal[j]

        # Natural gas, core (resd, comm, tran, indy, elec)
        restart.emablk_jgfrs[j] = etax.resd * restart.emeblk_egfrs[j]
        restart.emablk_jgfcm[j] = etax.comm * restart.emeblk_egfcm[j]
        restart.emablk_jgftr[j] = etax.tran * restart.emeblk_egftr[j]
        restart.emablk_jgfin[j] = etax.endu * restart.emeblk_egfin[j]
        restart.emablk_jgfel[j] = etax.elec * restart.emeblk_egfel[j]

        # Natural gas, noncore (resd, comm, tran, indy, elec)
        restart.emablk_jgirs[j] = etax.resd * restart.emeblk_egirs[j]
        restart.emablk_jgicm[j] = etax.comm * restart.emeblk_egicm[j]
        restart.emablk_jgitr[j] = etax.tran * restart.emeblk_egitr[j]
        restart.emablk_jgiin[j] = etax.endu * restart.emeblk_egiin[j]
        restart.emablk_jgiel[j] = etax.elec * restart.emeblk_egiel[j]

        # Natural gas, total (resd, comm, tran, indy, elec)
        restart.emablk_jngrs[j] = etax.resd * restart.emeblk_engrs[j]
        restart.emablk_jngcm[j] = etax.comm * restart.emeblk_engcm[j]
        restart.emablk_jngtr[j] = etax.tran * restart.emeblk_engtr[j]
        restart.emablk_jngin[j] = etax.endu * restart.emeblk_engin[j]
        restart.emablk_jnqngpf[j] = (
            # Natural gas feedstock
            etax.endu * restart.emeblk_enqngpf[j]
        )
        restart.emablk_jngel[j] = etax.elec * restart.emeblk_engel[j]

        # Coal
        restart.emablk_jclrs[j] = etax.resd * restart.emeblk_eclrs[j]
        restart.emablk_jclcm[j] = etax.comm * restart.emeblk_eclcm[j]
        restart.emablk_jclin[j] = etax.endu * restart.emeblk_eclin[j]
        restart.emablk_jclel[j] = etax.elec * restart.emeblk_eclel[j]
        restart.emablk_jmcin[j] = etax.endu * restart.emeblk_emcin[j]

        # Petroleum products
        restart.emablk_jgptr[j] = etax.tran * restart.emeblk_egptr[j]
        restart.emablk_jlpin[j] = etax.endu * restart.emeblk_elpin[j]
        restart.emablk_jmgcm[j] = etax.comm * restart.emeblk_emgcm[j]
        restart.emablk_jmgtr[j] = etax.tran * restart.emeblk_emgtr[j]
        restart.emablk_jmgin[j] = etax.endu * restart.emeblk_emgin[j]
        restart.emablk_jjftr[j] = etax.tran * restart.emeblk_ejftr[j]
        restart.emablk_jdsrs[j] = etax.resd * restart.emeblk_edsrs[j]
        restart.emablk_jdscm[j] = etax.comm * restart.emeblk_edscm[j]
        restart.emablk_jdstr[j] = etax.tran * restart.emeblk_edstr[j]
        restart.emablk_jdsin[j] = etax.endu * restart.emeblk_edsin[j]
        restart.emablk_jdsel[j] = etax.elec * restart.emeblk_edsel[j]
        restart.emablk_jksrs[j] = etax.resd * restart.emeblk_eksrs[j]
        restart.emablk_jkscm[j] = etax.comm * restart.emeblk_ekscm[j]
        restart.emablk_jksin[j] = etax.endu * restart.emeblk_eksin[j]
        restart.emablk_jprrs[j] = etax.resd * restart.emeblk_eprrs[j]
        restart.emablk_jlgrs[j] = restart.emablk_jprrs[j]
        restart.emablk_jprcm[j] = etax.comm * restart.emeblk_eprcm[j]
        restart.emablk_jlgcm[j] = restart.emablk_jprcm[j]
        restart.emablk_jprtr[j] = etax.tran * restart.emeblk_eprtr[j]
        restart.emablk_jlgtr[j] = restart.emablk_jprtr[j]
        restart.emablk_jetin[j] = etax.endu * restart.emeblk_eetin[j]
        restart.emablk_jprin[j] = etax.endu * restart.emeblk_eprin[j]
        restart.emablk_jbuin[j] = etax.endu * restart.emeblk_ebuin[j]
        restart.emablk_jisin[j] = etax.endu * restart.emeblk_eisin[j]

        jlgin_divisor = (
            restart.qblk_qetin[usa, j] + restart.qblk_qprin[usa, j]
            + restart.qblk_qbuin[usa, j] + restart.qblk_qisin[usa, j]
        )
        if jlgin_divisor != 0.0:
            restart.emablk_jlgin[j] = (
                (
                    restart.qblk_qetin[usa, j] * restart.emablk_jetin[j]
                    + restart.qblk_qprin[usa, j] * restart.emablk_jprin[j]
                    + restart.qblk_qbuin[usa, j] * restart.emablk_jbuin[j]
                    + restart.qblk_qisin[usa, j] * restart.emablk_jisin[j]
                ) / jlgin_divisor
            )
        else:
            restart.emablk_jlgin[j] = etax.endu * restart.emeblk_elgin[j]

        restart.emablk_jetinpf[j] = etax.endu * restart.emeblk_eetinpf[j]
        restart.emablk_jprinpf[j] = etax.endu * restart.emeblk_eprinpf[j]
        restart.emablk_jbuinpf[j] = etax.endu * restart.emeblk_ebuinpf[j]
        restart.emablk_jisinpf[j] = etax.endu * restart.emeblk_eisinpf[j]

        jnqlgpf_divisor = (
            restart.qblk_qetinpf[usa, j] + restart.qblk_qprinpf[usa, j]
            + restart.qblk_qbuinpf[usa, j] + restart.qblk_qisinpf[usa, j]
        )
        if jnqlgpf_divisor != 0.0:
            restart.emablk_jnqlgpf[j] = (
                (
                    restart.qblk_qetinpf[usa, j] * restart.emablk_jetinpf[j]
                    + restart.qblk_qprinpf[usa, j] * restart.emablk_jprinpf[j]
                    + restart.qblk_qbuinpf[usa, j] * restart.emablk_jbuinpf[j]
                    + restart.qblk_qisinpf[usa, j] * restart.emablk_jisinpf[j]
                ) / jnqlgpf_divisor
            )
        else:
            restart.emablk_jnqlgpf[j] = etax.endu * restart.emeblk_enqlgpf[j]

        restart.emablk_jrlcm[j] = etax.comm * restart.emeblk_erlcm[j]
        restart.emablk_jrltr[j] = etax.tran * restart.emeblk_erltr[j]
        restart.emablk_jrlin[j] = etax.endu * restart.emeblk_erlin[j]
        restart.emablk_jrlel[j] = etax.elec * restart.emeblk_erlel[j]
        restart.emablk_jrhtr[j] = etax.tran * restart.emeblk_erhtr[j]
        restart.emablk_jrhel[j] = etax.elec * restart.emeblk_erhel[j]
        restart.emablk_jrscm[j] = etax.comm * restart.emeblk_erscm[j]
        restart.emablk_jrstr[j] = etax.tran * restart.emeblk_erstr[j]
        restart.emablk_jrsin[j] = etax.endu * restart.emeblk_ersin[j]
        restart.emablk_jrsel[j] = etax.elec * restart.emeblk_ersel[j]
        restart.emablk_jpfin[j] = etax.endu * restart.emeblk_epfin[j]
        restart.emablk_jpcin[j] = etax.endu * restart.emeblk_epcin[j]
        restart.emablk_jppin[j] = etax.endu * restart.emeblk_eppin[j]
        restart.emablk_jppinpf[j] = etax.endu * restart.emeblk_eppinpf[j]
        restart.emablk_jluin[j] = etax.endu * restart.emeblk_eluin[j]
        restart.emablk_jpcel[j] = etax.endu * restart.emeblk_epcel[j]
        restart.emablk_jsgin[j] = etax.endu * restart.emeblk_esgin[j]
        restart.emablk_jotin[j] = etax.endu * restart.emeblk_eotin[j]
        restart.emablk_jottr[j] = etax.tran * restart.emeblk_eottr[j]
        restart.emablk_jmetr[j] = etax.tran * restart.emeblk_emetr[j]

        e85_gasoline_share = (
            (restart.pmmout_trgne85 * restart.convfact_cfrbob[j])
            / (
                restart.pmmout_trgne85 * restart.convfact_cfrbob[j]
                + restart.pmmout_ethne85 * restart.convfact_cfetq[j]
            )
        )
        restart.emablk_jettr[j] = (
            etax.tran * restart.emeblk_emgtr[j] * e85_gasoline_share
        )

        # Initialize coal adjustment variables for quantity weighting
        restart.emablk_jclelcdr[j, :] = 0.0
        totclclnr = np.empty(restart.coalemm_nclut1, np.float64)

        # Select portions of qclclnr and cclclnr for year index j
        qclclnr_j = restart.coalemm_qclclnr[:, j, :]
        cclclnr_j = restart.coalemm_cclclnr[:, j, :]

        # Sum etax * (demand * carbon content) over coal demand regions
        restart.emablk_jclclnr[j, :] = etax.elec * np.sum(
            qclclnr_j * cclclnr_j, axis=0
        )

        # Then compute total cost and total demand by summing over utility
        # demand sectors. The costemis and qtyemis vars are temp vars. They
        # help to calculate aclel var
        costemis = np.sum(restart.emablk_jclclnr[j, :])
        restart.emablk_jclelcdr[j, 0] = costemis
        totclclnr[:] = np.sum(qclclnr_j, axis=0)  # Total trills per sector
        qtyemis = np.sum(totclclnr)

        # Divide total dollars by total trills in each sector
        tot_positive = totclclnr > 0.0
        restart.emablk_jclclnr[j, tot_positive] /= totclclnr[tot_positive]
        tot_nonpositive = np.logical_not(tot_positive)
        restart.emablk_jclclnr[j, tot_nonpositive] = etax.elec * 25.0

        if qtyemis > 0.0:
            # Assign same value to both
            # TODO: is this a bug? it does not actually seem to be assigning
            #       the same value to both
            restart.emablk_jclelcdr[j, 0] = (
                restart.emablk_jclelcdr[j, 0] / qtyemis
            )
            restart.emablk_jclelcdr[j, 1] = (
                restart.emablk_jclelcdr[j, 0] / qtyemis
            )
        else:
            restart.emablk_jclelcdr[j, :] = etax.elec * 25.0

        if qtyemis > 0.0:
            restart.emablk_jclel[j] = costemis / qtyemis
        else:
            restart.emablk_jclel[j] = 0.0

        # Adjusts by gas/utility regions
        restart.emablk_jgfelgr[j] = etax.elec * restart.emeblk_egfelgr[j]
        restart.emablk_jgielgr[j] = etax.elec * restart.emeblk_egielgr[j]
        restart.emablk_jgcelgr[j] = etax.elec * restart.emeblk_egcelgr[j]

        log_table_data = [
            ["j", j],
            ["jmetr[j]", restart.emablk_jmetr[j]],
            ["jettr[j]", restart.emablk_jettr[j]],
            ["jngel[j]", restart.emablk_jngel[j]]
        ]
        log_it(
            tabulate.tabulate(log_table_data, numalign="right"), verbose=True
        )

        log_it(
            "ADJUSTED FUEL PRICES --- REGION 1, YEAR 1",
            "VARIABLES FROM MPRC --- PRICES",
            restart.mpblk_mprc[0, j, :],
            "VARIABLES FROM AMPRC",
            restart.ampblk_mprc[0, j, :],
            verbose=True
        )

        log_it(
            "NATURAL GAS UTILITY VARIABLES (NOT COMPLETED)",
            "REGION 1, YEAR 1",
            f"jgfelgr[0] = {restart.emablk_jgfelgr[0]}",
            f"jgielgr[0] = {restart.emablk_jgielgr[0]}",
            f"jgcelgr[0] = {restart.emablk_jgcelgr[0]}",
            verbose=True
        )
