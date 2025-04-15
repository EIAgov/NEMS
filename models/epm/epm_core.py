"""Core routines for the NEMS Emissions Policy Module (EPM).

The module is called from NEMS main as long as RUNEPM=1 is set in the scedes.

The primary role of this module is to add up carbon emissions from each sector
of the U.S. economy in proportion to fuel consumption. That task is handled by
`sum_emissions`, which is called from within the main `epm` function.

When required, EPM also implements carbon policy options -- the first three
policy options listed below are mutually exclusive (one at a time), the fourth
option is a variation on the market policy, and the fifth option is a modifier
that can be used with either the carbon cap or market policy:

1. Carbon Tax:

    A nominal or real $ tax per kilogram of carbon for fossil fuels. It is
    converted to a $/btu tax for each fuel/sector based on its carbon content.
    Revenue from the tax is passed to the macroeconomic module. There,
    treatment of such revenue (e.g., reducing the deficit or reducing other
    taxes, etc.) depends on options in effect. Generally, large changes in
    gov't revenue would require additional offline analysis to asses
    macroeconomic feedbacks.

    This policy is triggered by setting `tax_flag` in EPMCNTL and uses values
    from the section titled "EPM Carbon Tax or Carbon Cap Data" in EPMDATA. You
    may also want to set INTLFDBK=1, MACTAX=4, INDBMOVR=1, and
    RFHISTN=$NEMS/.../rfhist_nref.txt in the scedes.

2. Auction of Permits:

    A carbon goal is specified by the user. The goal or cap is achieved by
    requiring an allowance to emit carbon. An auction clearing price is
    determined, through the NEMS iterative process, that will clear the auction
    market. Essentially, this option determines the carbon tax (or allowance
    fee) necessary to achieve the total carbon cap. The auction to price and
    allocate emissions allowances is assumed to operate with no initial
    allocation of allowances. As in option 1, Carbon Tax, revenue from the
    auction is passed to the macroeconomic module, where its effect may require
    additional analysis.

    This policy is triggered by setting `auction_flag` in EPMCNTL and uses
    values from the section titled "EPM Carbon Tax or Carbon Cap Data" in
    EPMDATA. You may also want to set INTLFDBK=1, MACTAX=4, INDBMOVR=1, and
    RFHISTN=$NEMS/.../rfhist_nref.txt in the scedes.

3. Market for Permits:

    Same as option 2, Auction of Permits, but permits are transferable within
    the country (though not bankable). An initial distribution of emissions
    permits, equal to base year emissions, is assumed to take place. As a
    result, the revenue from the sale of allowances is assumed to be
    redistributed back to the individual sectors. For regulated electric
    utilities, the initial revenue from the allowance distribution would be
    considered independent of the cost (or opportunity cost) of purchasing the
    permits. The amount of this initial revenue or subsidy is calculated, but
    no treatment of it is performed for pricing purposes. The full cost of the
    permits, however, does feed through to the electricity price. Therefore,
    the effect on the electricity price is probably overstated (unless marginal
    cost pricing is assumed).

    This policy is triggered by setting `market_flag` in EPMCNTL and uses
    values from the section titled "EPM Carbon Tax or Carbon Cap Data" in
    EPMDATA. You may also want to set INTLFDBK=1, MACTAX=4, INDBMOVR=1, and
    RFHISTN=$NEMS/.../rfhist_nref.txt in the scedes.

4. Market for Permits with Emissions Offsets:

    Same as option 3, Market for Permits, but allows for the total cap on
    emission allowances to increase through a supply of offsets. The amount of
    offsets for reforestation (increasing carbon sequestration) and coal bed
    methane capture are specified at various permit prices; the higher the
    price, the greater the assumed offsets (i.e., a supply curve of exogenous
    emission allowance offsets). The offset (in tons) available at a given
    allowance price is added to the carbon goal.

    This policy is triggered in the same manner as option 3, but additionally
    requires setting `offset_flag` in EPMCNTL.

5. Early-Compliance Banking w/ Cap-and-Trade and Smooth Carbon Fee Growth:

    A cap and trade with banking is implemented by finding the starting carbon
    price, escalated at a fixed rate, that clears the bank over the compliance
    period. The bank is determined as the sum of cap minus emissions over the
    relevant period. The starting price is guessed based on results of prior
    NEMS cycles. The projected prices are set in the start year based on the
    guess, then the case is run as a carbon fee case.

    The banking policy can be combined with either option 2 or option 3, and is
    triggered by setting `bank_flag` in EPMCNTL. The details of the banking
    policy are controlled by additional parameters beginning with the `bank_`
    prefix in EPMCNTL (e.g., `bank_startyr` and `bank_endyr`).
"""

import numpy as np
import tabulate

from epm_adjustments import etax_adjust, price_adjust
from epm_common import log_it
from epm_regula_falsi import regfalsi, regfalsibank
from epm_restart import Restart
from epm_revenue import accntrev, initrev, epm_addoff
from epm_scedes import Scedes
from epm_sum_emissions import sum_emissions
from epm_variables import Variables


def epm(restart: Restart, scedes: Scedes, variables: Variables) -> None:
    """Top-level routine for running the Emissions Policy Module (EPM).

    Calling this function runs the core EPM code, but does not handle the
    reading of EPM's input files. See the module-level documentation for
    details on the Emissions Policy Module as a whole.

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
    epm_read : Read the EPM input files and open the EPM output file.
    sum_emissions : Called by this function to add up U.S. carbon emissions.
    epm_addoff : Called by this function to account for carbon offsets.
    regfalsibank : Called by this function to compute a starting carbon price.
    accntrev : Called by this function to calculate emissions policy revenue.
    initrev : Called by this function to compute value of permit allocation.
    regfalsi : Called by this function to solve for a policy carbon price.
    etax_adjust : Called by this function to calculate energy tax adjustments.
    price_adjust : Called by this function to compute carbon price adjustments.
    """
    # Set this to 1 allowance credit per international offset
    # HR2454 provided 1 allowance credit per 1.25 international offset credit
    # after 2017.  1.00 / 1.25 = 0.8
    allow_per_offset = 1.0

    i_year = restart.ncntrl_curiyr - 1  # Zero-based year index for Python

    if restart.ncntrl_curitr == 1:
        variables.epm_new_tax = restart.emoblk_emtax[i_year]
        # Keep track to write out summary of beginning and ending tax
        variables.epm_begin_and_end[0, i_year] = variables.epm_new_tax

    # Real after-tax escalation rate in % for fee case with banking (or
    # discount rate). Entered as permille in scedes, so change to percent.
    r = float(scedes.get("BANKDSCR", "74")) / 10.0

    # With offsets, bioseqok = 1 means that bio sequestration offsets count
    # towards the goal. On the other hand, bioseqok = 0 means incentive only.
    # Incentives are given but don't count toward the goal.
    bioseqok = int(scedes.get("BIOSEQOK", "1"))

    restart.epmbank_offset[:, i_year] = 0.0
    dollarftab = restart.macout_mc_jpgdp[restart.ncntrl_yearpr - 1987]

    sum_emissions(restart, scedes, variables)
    # Covered emissions only
    restart.emission_emsol[0, i_year] = restart.emoblk_total_emissions[i_year]

    # Energy-related methane emissions no longer estimated. Zero them to avoid
    # confusion.
    restart.emission_emmethane[:18, :] = 0.0

    # Record Data for Electric Utility Resource Calculations
    # Mechanism for setting a phantom carbon fee that will be incorporated for
    # EMM planning purposes but not get added to energy prices
    if (
        restart.emoblk_bank_flag
        and restart.epmbank_bank_startyr > restart.ncntrl_ijumpcalyr
        and restart.ncntrl_curiyr == restart.ncntrl_firsyr
        and restart.ncntrl_curitr == 1
    ):
        log_table_title = "Applying a phantom carbon allowance as follows:"
        log_table_headers = ["Year", "Allowance"]
        year_start = restart.epmbank_bank_priceyr
        year_end = restart.ncntrl_lastcalyr + 1
        etax_factor = 1.0 if restart.emoblk_etax_flag else 1000.0
        log_table_data = []
        for year in range(year_start, year_end):
            iy = year - restart.parametr_baseyr
            log_table_data.append([
                year, restart.emoblk_emtax[iy] * etax_factor
            ])
        log_it(
            log_table_title, "=" * len(log_table_title),
            tabulate.tabulate(log_table_data, headers=log_table_headers),
        )
        restart.emission_emetax[:2, :] = restart.emoblk_emtax
    elif (
        (restart.emoblk_permit_flag or restart.emoblk_market_flag)
        and not restart.emoblk_bank_flag
        and restart.ncntrl_curiyr == restart.ncntrl_firsyr
        and restart.ncntrl_curitr == 1
        and restart.epmbank_bank_startyr <= restart.ncntrl_ijumpcalyr
    ):
        # Start of allowance pricing (earlier than bank_startyr)
        ip = restart.epmbank_bank_priceyr - 1990
        # Start of a cap and trade period, interpreted here even if bank_flag
        # is zero
        is_ = restart.epmbank_bank_startyr - 1990
        if ip < is_:  # Interval between passage and start of cap and trade
            for iy in range(is_ - 1, ip - 1, -1):
                restart.emoblk_emtax[iy] = (
                    restart.emoblk_emtax[iy + 1] / (1.0 + r / 100.0)
                )

    if (
        (
            restart.emoblk_tax_flag or restart.emoblk_permit_flag
            or restart.emoblk_market_flag
        )
        and restart.emoblk_bank_flag
        and restart.ncntrl_curiyr == restart.ncntrl_firsyr
        and restart.ncntrl_curitr == 1
        and restart.epmbank_bank_startyr <= restart.ncntrl_ijumpcalyr
    ):
        # Evaluate bank balance and set new bracket and price guess as
        # appropriate. Sets up the fee case for this cycle to begin in the
        # following model year.

        # Start of allowance pricing (earlier than bank_startyr)
        ip = restart.epmbank_bank_priceyr - 1990

        # Start of a cap and trade period that includes banking (year index)
        is_ = restart.epmbank_bank_startyr - 1990

        # First year within cap-and-trade period in which banking is allowed
        # (normally equal to bank_startyr but can be later if borrowing in
        # first few years would results (year index)
        io = variables.epm_out_bank_onyr - 1990

        if restart.epmbank_bank_endyr == 0:
            restart.epmbank_bank_endyr = (
                restart.parametr_baseyr + restart.ncntrl_lastyr - 1
            )
        ie = restart.epmbank_bank_endyr - 1990  # End of banking, year index

        wgtguess = float(scedes.get("WGTGUESS", "50")) / 100.0

        # Overwrite the taxes read from EPMDATA using those in the restart
        # file, since this banking option requires iterative trials. Exception
        # is when starting from a restart file without taxes set.
        emetax_sum = np.sum(
            restart.emission_emetax[0, ip:restart.ncntrl_lastyr+1]
        )
        if not np.isnan(emetax_sum) and emetax_sum > 0.0:
            restart.emoblk_emtax[ip:restart.ncntrl_lastyr+1] = (
                restart.emission_emetax[0, ip:restart.ncntrl_lastyr+1]
            )

        # Calculate balance based on covered emissions, goal, offset used, and
        # bank balance from last run
        restart.epmbank_balance[:io] = 0.0
        variables.epm_bank[:io] = 0.0

        # Accumulate from onset of banking to projection end
        for iy in range(io, restart.ncntrl_lastyr):
            # The qoff variable temporarily holds the quantity of offsets that
            # count towards the goal.
            qoff = epm_addoff(
                restart.epmbank_offset, restart.parametr_baseyr, iy,
                bioseqok, allow_per_offset, 2012
            )
            variables.epm_bank[iy] = (
                restart.emoblk_emissions_goal[iy]
                + qoff - restart.emission_emsol[0, iy]
            )
            restart.epmbank_balance[iy] = (
                restart.epmbank_balance[iy - 1] + variables.epm_bank[iy]
            )

        log_table_titles = [
            "Before guessing new prices (results of prior run)",
            f"Bank balance end target year = {restart.epmbank_bank_endyr}"
        ]
        log_table_headers = [
            "Year", "Iter",
            "Covered\nEmissions\nSolution", "Covered\nEmissions\nGoal",
            "Offset\nUsed", "Bank\nBalance",
            "Carbon\nFee 87$", " ",  # Blank column is asafety
            f"Carbon\nFee {(restart.ncntrl_yearpr % 100):02}$",
            "Expected\nFee 87$",
        ]
        log_table_data = []
        for iy in range(ip, restart.ncntrl_lastyr):
            # asafety is used to flag price at safety-valve level in EPMOUT
            asafety = ' '
            if (
                restart.emoblk_emtax[iy] >= restart.emoblk_max_tax[iy] - 0.0001
            ):
                asafety = '!'
            if (
                restart.emoblk_bank_flag
                and restart.emoblk_emtax[iy - 1] != 0.0
                and (
                    restart.emoblk_emtax[iy] / restart.emoblk_emtax[iy - 1]
                    > 1.0 + r / 100.0
                )
            ):
                asafety = 'X'
            qoff = epm_addoff(
                restart.epmbank_offset, restart.parametr_baseyr, iy,
                bioseqok, allow_per_offset, 2012
            )
            log_table_data.append([
                iy + restart.parametr_baseyr,
                restart.epmbank_iter,
                restart.emission_emsol[0, iy],
                restart.emoblk_emissions_goal[iy],
                qoff,
                restart.epmbank_balance[iy],
                restart.emoblk_emtax[iy] * 1000.0,
                asafety,
                restart.emoblk_emtax[iy] * 1000.0 * dollarftab,
                restart.emission_emetax[0, iy] * 1000.0,
            ])
        log_it(
            *log_table_titles,
            "=" * max(len(title) for title in log_table_titles),
            tabulate.tabulate(log_table_data, headers=log_table_headers),
        )

        # Only reset banking prices in integrated runs where maxitr > 0. Allows
        # standalone testing
        if restart.ncntrl_maxitr > 0:

            # Banking price path determination: The objective is normally to
            # determine the starting price that results in a zero balance the
            # target year (bank_endyr or year index ie). The prices during the
            # bank period grow at the annual rate of r, the banking discount
            # rate or required rate of return.

            # Real after-tax escalation rate in % for fee case with banking (or
            # discount rate). Entered as permille in scedes, so change to
            # percent.
            r = float(scedes.get("BANKDSCR", "74")) / 10.0

            restart.emoblk_emtax[:ip] = 0.0
            restart.emoblk_emtax[io] = regfalsibank(
                restart, scedes, variables,
                restart.emoblk_emtax[io],
                variables.epmoth_bank_end_balance - restart.epmbank_balance[ie]
            )

            # If there is a safety-valve provision, however, this approach may
            # yield a price path resulting in a zero bank balance but then a
            # jump up in price in the next year exceeding the banking rate of
            # increase. And the solution with a later bank year causes the
            # safety valve price to occur earlier in the banking period. So the
            # option banksafety=1 is used to get the best alternative: the bank
            # period ends and the safety valve price is reached in the same
            # year. With this option, we set the price in the bank_endyr = ie
            # equal to the safety valve price and accept a negative cumulative
            # banking balance in the end year. The bank balance should change
            # from positive to negative in the year the safety-valve is first
            # hit. This option is normally used after the bank_endyr has been
            # determined by trial and error with banksafety = 0
            # The banksafety variable is a runtime option. with banking &
            # safety-valve, if banksafety = 1, sets price in bank_endyr to
            # safety-valve price
            banksafety = int(scedes.get("BANKSAFE", "0"))
            if banksafety == 1:
                restart.emoblk_emtax[io] = (
                    restart.emoblk_max_tax[ie] / (1.0 + r / 100.0) ** (ie - io)
                )
                log_it(
                    "BANKSAFETY = 1, so safety-valve price in",
                    "bank_endyr with starting price of:",
                    restart.emoblk_emtax[io] * 1000.0,
                    sep=" "
                )

            if restart.emoblk_emtax[io] > restart.emoblk_max_tax[io]:
                restart.emoblk_emtax[io] = restart.emoblk_max_tax[io]
                log_it(
                    "Setting starting tax to safety-valve (max) of:",
                    restart.emoblk_emtax[io],
                    sep=" "
                )

            # Have price in banking period grow at the discount rate, r,
            # subject to safety valve, if applicable
            for iy in range(io + 1, ie + 1):
                # The "min(restart.emoblk_max_tax[iy]," applies the allowance
                # cost safety valve
                restart.emoblk_emtax[iy] = min(
                    restart.emoblk_max_tax[iy],
                    restart.emoblk_emtax[iy - 1] * (1.0 + r / 100.0)
                )

            # In non-banking years, applying the perfect foresight guessing
            # algorithm
            for iy in range(is_, restart.ncntrl_lastyr):
                if iy < io or iy > ie:
                    # The "min(restart.emoblk_max_tax[iy]," applies the
                    # allowance cost safety valve
                    restart.emoblk_emtax[iy] = min(
                        restart.emoblk_max_tax[iy],
                        np.average(
                            restart.emission_emetax[:2, iy],
                            weights=[1.0 - wgtguess, wgtguess]
                        )
                    )

            # In years prior to start of the compliance period
            # (is_ = bank_startyr - 1990),
            # establish an allowance price for planning purposes that does not
            # actually get added into delivered energy prices.
            if is_ > ip:
                for iy in range(is_ - 1, ip - 1, -1):
                    restart.emoblk_emtax[iy] = (
                        restart.emoblk_emtax[iy + 1] / (1.0 + r / 100.0)
                    )

            log_table_title = "Emission fees after guessing new start price"
            log_table_headers = [
                "Year", "Iter",
                "Carbon\nFee 87$", " ",  # Blank column is asafety
                f"Carbon\nFee {(restart.ncntrl_yearpr % 100):02}$",
            ]
            log_table_data = []
            for iy in range(ip, restart.ncntrl_lastyr):
                # asafety is used to flag price at safety-valve level in EPMOUT
                asafety = ' '
                if (
                    restart.emoblk_emtax[iy]
                    >= restart.emoblk_max_tax[iy] - 0.0001
                ):
                    asafety = '!'
                if (
                    restart.emoblk_bank_flag
                    and restart.emoblk_emtax[iy - 1] != 0.0
                    and (
                        restart.emoblk_emtax[iy] / restart.emoblk_emtax[iy - 1]
                        > 1.0 + r / 100.0
                    )
                ):
                    asafety = 'X'
                log_table_data.append([
                    restart.parametr_baseyr + iy,
                    restart.epmbank_iter,
                    restart.emoblk_emtax[iy] * 1000.0,
                    asafety,
                    restart.emoblk_emtax[iy] * 1000.0 * dollarftab,
                ])
            log_it(
                log_table_title, "=" * len(log_table_title),
                tabulate.tabulate(log_table_data, headers=log_table_headers),
            )

            # Starting values and expectations
            # Save this run's guess
            restart.emission_emetax[:2, ip:] = restart.emoblk_emtax[ip:]

    # Driver loop for policy options
    # Energy tax only: call etax_adjust, count revenue and emissions
    if restart.emoblk_etax_flag:
        accntrev(restart, scedes, variables)

    restart.epmbank_balance[i_year] = 0.0
    variables.epm_bank[i_year] = 0.0
    restart.ghgrep_banking[i_year] = 0.0
    if (
        restart.emoblk_bank_flag
        and (
            restart.emoblk_tax_flag or restart.emoblk_permit_flag
            or restart.emoblk_market_flag
        )
        and restart.ncntrl_curcalyr >= variables.epm_out_bank_onyr
    ):
        # If banking allowance, accumulate the bank balance by adding in excess
        # emission reductions (or subtracting insufficient emissions, taking
        # into account any offsets allowed.
        # The qoff variable temporarily holds the quantity of offsets that
        # count towards the goal.
        qoff = epm_addoff(
            restart.epmbank_offset, restart.parametr_baseyr, i_year,
            bioseqok, allow_per_offset, 2012
        )
        variables.epm_bank[i_year] = (
            restart.emoblk_emissions_goal[i_year]
            + qoff - restart.emission_emsol[0, i_year]
        )
        restart.ghgrep_banking[i_year] = (
            restart.emoblk_emissions_goal[i_year]
            + qoff - restart.emission_emsol[0, i_year]
        )
        restart.epmbank_balance[i_year] = (
            restart.epmbank_balance[i_year - 1] + variables.epm_bank[i_year]
        )

    # If banking option on, switch to year-by-year cap once compliance period
    # starts.
    if (
        restart.emoblk_bank_flag
        and (
            restart.emoblk_tax_flag or restart.emoblk_permit_flag
            or restart.emoblk_market_flag
        )
    ):
        if restart.ncntrl_curcalyr > restart.epmbank_bank_endyr:
            restart.emoblk_tax_flag = 0
            restart.emoblk_permit_flag = 1
            restart.emoblk_market_flag = 0
        elif (
            restart.ncntrl_curcalyr >= variables.epm_out_bank_onyr
            and restart.ncntrl_curcalyr <= restart.epmbank_bank_endyr
        ):
            restart.emoblk_tax_flag = 1
            restart.emoblk_permit_flag = 0
            restart.emoblk_market_flag = 0
        elif (
            restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr
            and restart.ncntrl_curcalyr < variables.epm_out_bank_onyr
        ):
            restart.emoblk_tax_flag = 0
            restart.emoblk_permit_flag = 1
            restart.emoblk_market_flag = 0
        elif restart.ncntrl_curcalyr < restart.epmbank_bank_startyr:
            restart.emoblk_tax_flag = 1
            restart.emoblk_permit_flag = 0
            restart.emoblk_market_flag = 0

    if (
        (restart.emoblk_permit_flag or restart.emoblk_market_flag)
        and restart.ncntrl_curitr == 1
    ):
        variables.epm_e_goal = restart.emoblk_emissions_goal[i_year]
        if (
            restart.emoblk_bank_flag
            and restart.epmbank_bank_startyr != restart.epmbank_bank_endyr
            and restart.ncntrl_curcalyr > restart.epmbank_bank_endyr
            and restart.ncntrl_curitr == 1
        ):
            # Constrain price growth after bank period ends to be no more than
            # the price trajectory that would encourage more trajectory. See
            # EPM_epmout.txt to see where excess post-banking price growth in
            # any particular year is flagged. If so, a longer banking interval,
            # or maybe another banking interval, is indicated. Since this code
            # only supports one banking interval, the maximum post-banking
            # price growth is set to the banking discount rate.

            # End of banking, converted year index
            ie = restart.epmbank_bank_endyr - 1990

            yr_diff = restart.ncntrl_curcalyr - restart.epmbank_bank_endyr
            restart.emoblk_max_tax[i_year] = min(
                restart.emoblk_max_tax[i_year],
                restart.emoblk_emtax[ie] * (1.0 + r / 100.0) ** yr_diff
            )

            # In post banking period, alter the normal emissions goal to
            # include any over- or under-compliance remaining from prior year
            # Add in any excess end-of-banking balance to current year goal.
            if restart.ncntrl_curcalyr > restart.epmbank_bank_endyr:
                variables.epm_e_goal += (
                    restart.epmbank_balance[i_year - 1]
                    - variables.epmoth_bank_end_balance
                )

    if (
        (restart.emoblk_market_flag or restart.emoblk_permit_flag)
        and restart.ncntrl_curitr <= restart.ncntrl_maxitr + 1
    ):
        # The qoff variable temporarily holds the quantity of offsets that
        # count towards the goal.
        qoff = epm_addoff(
            restart.epmbank_offset, restart.parametr_baseyr, i_year,
            bioseqok, allow_per_offset, 2012
        )

        etax_factor = 1.0 if restart.emoblk_etax_flag else 1000.0
        over_under = (
            restart.emoblk_total_emissions[i_year]
            - qoff - restart.emoblk_emissions_goal[i_year]
        )
        log_table_data = [
            ["Year", restart.parametr_baseyr + i_year],
            ["Iter", restart.ncntrl_curitr],
            ["Current Tax", variables.epm_new_tax * etax_factor],
            ["Covered Emissions", restart.emoblk_total_emissions[i_year]],
            ["Offsets", qoff],
            ["Emissions Goal", restart.emoblk_emissions_goal[i_year]],
            ["Over (under)", over_under],
        ]
        log_it(tabulate.tabulate(
            log_table_data, tablefmt="plain", numalign="right"
        ))

    # Tax only: just compute tax and total revenue raised
    if (
        restart.emoblk_tax_flag or restart.emoblk_permit_flag
        or restart.emoblk_market_flag
    ):
        accntrev(restart, scedes, variables)

        # For markets, sum emissions, revenue,
        # need to subtract initial allocs from revenue and find new tax
        if restart.emoblk_market_flag:
            initrev(restart)

    if (
        restart.ncntrl_ncrl == 1
        and (restart.emoblk_permit_flag or restart.emoblk_market_flag)
    ):
        variables.epm_begin_and_end[1, i_year] = restart.emoblk_emtax[i_year]
        if restart.ncntrl_curiyr == restart.ncntrl_lastyr:
            log_table_title = (
                "Beginning and ending allowance fees during this cycle "
                + f"(cycle {restart.ncntrl_curirun}):"
            )
            log_table_headers = ["Year", "Begin", "End"]
            log_table_data = []
            for i in range(restart.ncntrl_lastyr):
                if np.any(variables.epm_begin_and_end[:, i] != 0.0):
                    log_table_data.append([
                        restart.parametr_baseyr + i,
                        variables.epm_begin_and_end[0, i] * 1000.0,
                        variables.epm_begin_and_end[1, i] * 1000.0,
                    ])
            log_it(
                log_table_title, "=" * len(log_table_title),
                tabulate.tabulate(log_table_data, headers=log_table_headers),
            )

    if restart.ncntrl_ncrl == 1:
        return

    # Call a scalar search routine to determine the new tax/permit price
    if restart.emoblk_permit_flag or restart.emoblk_market_flag:
        # The qoff variable temporarily holds the quantity of offsets that
        # count towards the goal.
        qoff = epm_addoff(
            restart.epmbank_offset, restart.parametr_baseyr, i_year,
            bioseqok, allow_per_offset, 2012
        )
        variables.epm_new_tax = regfalsi(
            restart, variables,
            variables.epm_new_tax,
            (
                restart.emoblk_total_emissions[i_year]
                - qoff - variables.epm_e_goal
            )
        )

    # Store new tax rate in emission block
    restart.emoblk_emtax[i_year] = variables.epm_new_tax

    # If energy tax flag set, adjust prices with btu tax
    if restart.emoblk_etax_flag:
        etax_adjust(restart)

    # For any flag with a carbon tax/trading fee, add the fee to energy prices
    elif (
        restart.emoblk_tax_flag or restart.emoblk_market_flag
        or restart.emoblk_permit_flag
    ):
        price_adjust(restart, scedes)

    if (
        not (restart.emoblk_market_flag or restart.emoblk_permit_flag)
        or restart.emoblk_bank_flag
    ):
        if (
            not restart.emoblk_bank_flag
            or (
                restart.ncntrl_fcrl == 1
                and restart.ncntrl_curcalyr >= restart.epmbank_bank_startyr
            )
        ):
            log_table_data = [
                ["curcalyr", restart.ncntrl_curcalyr],
                ["curitr", restart.ncntrl_curitr],
                ["iter", restart.epmbank_iter],
            ]
            for ii in range(restart.emission_emrev.shape[0]):
                log_table_data.append([
                    f"emrev[{ii}, i_year]", restart.emission_emrev[ii, i_year]
                ])
            log_table_data.extend([
                [
                    "Total Covered Emissions",
                    restart.emoblk_total_emissions[i_year]
                ],
                ["Emissions Goal", restart.emoblk_emissions_goal[i_year]],
                ["e_goal", variables.epm_e_goal],
            ])
            for ii in range(restart.epmbank_offset.shape[0]):
                log_table_data.append([
                    f"Offsets (if any) [{ii}, i_year]",
                    restart.epmbank_offset[ii, i_year],
                ])
            # The qoff variable temporarily holds the quantity of offsets that
            # count towards the goal.
            qoff = epm_addoff(
                restart.epmbank_offset, restart.parametr_baseyr, i_year,
                bioseqok, allow_per_offset, 2012
            )
            log_table_data.extend([
                [
                    "Over (under) compliance",
                    (
                        restart.emoblk_emissions_goal[i_year]
                        + qoff - restart.emoblk_total_emissions[i_year]
                    )
                ],
                ["Allowance Bank Balance", restart.epmbank_balance[i_year]],
                ["Emissions Fee (87$)", restart.emoblk_emtax[i_year] * 1000.0],
                [
                    f"Emissions Fee ({(restart.ncntrl_yearpr % 100):02}$)",
                    restart.emoblk_emtax[i_year] * 1000.0 * dollarftab
                ],
            ])
            log_it(tabulate.tabulate(
                log_table_data, tablefmt="plain", numalign="right"
            ))

    if (
        (restart.emoblk_bank_flag or restart.emoblk_offset_flag)
        and restart.ncntrl_curiyr == restart.ncntrl_lastyr
        and restart.ncntrl_fcrl == 1
    ):
        log_table_titles = [
            '"!" next to fee indicates it is '
            + "greater or equal to max_tax (safety)."
        ]
        if restart.emoblk_bank_flag:
            log_table_titles.append(
                '"X" next to fee indicates it '
                + "grew faster than the bank discount"
            )
            log_table_titles.append(
                f"    rate, {r=} suggesting the bank_endyr "
                + "should be moved out."
            )
        log_table_titles.append("Summary of Results")
        log_table_headers = [
            "Year", "Iter",
            "Covered\nEmissions\nSolution", "Covered\nEmissions\nGoal",
            "Covered\nAbatem.\nOth. GHG", "Uncovered\nOffset\nUsed",
            "Banking", "Bank\nBalance",
            "Carbon\nFee 87$", " ",  # Blank column is asafety
            f"Carbon\nFee {(restart.ncntrl_yearpr % 100):02}$",
            "Expected\nFee 87$",
            f"Expected\nFee {(restart.ncntrl_yearpr % 100):02}$",
        ]
        log_table_data = []
        for iy in range(
            restart.epmbank_bank_priceyr - restart.parametr_baseyr,
            restart.ncntrl_lastyr
        ):
            # asafety is used to flag price at safety-valve level in EPMOUT
            asafety = ' '
            if (
                restart.emoblk_emtax[iy]
                >= restart.emoblk_max_tax[iy] - 0.00001
            ):
                asafety = '!'
            if (
                restart.emoblk_bank_flag
                and restart.emoblk_emtax[iy - 1] != 0.0
                and (
                    restart.emoblk_emtax[iy] / restart.emoblk_emtax[iy - 1]
                    > 1.0 + r / 100.0
                )
            ):
                asafety = 'X'
            # The qoff variable temporarily holds the quantity of offsets that
            # count towards the goal.
            qoff = epm_addoff(
                restart.epmbank_offset, restart.parametr_baseyr, iy,
                bioseqok, allow_per_offset, 2012
            )
            log_table_data.append([
                restart.parametr_baseyr + iy,
                restart.epmbank_iter,
                restart.emission_emsol[0, iy],
                restart.emoblk_emissions_goal[iy],
                (
                    np.sum(restart.ghgrep_ghg_abate[iy, :14])
                    - restart.epmbank_offset[0, iy]
                ),
                qoff,
                variables.epm_bank[iy],
                restart.epmbank_balance[iy],
                restart.emoblk_emtax[iy] * 1000.0,
                asafety,
                restart.emoblk_emtax[iy] * 1000.0 * dollarftab,
                restart.emission_emetax[1, iy] * 1000.0,
                restart.emission_emetax[1, iy] * 1000.0 * dollarftab,
            ])
        log_it(
            *log_table_titles,
            "=" * max(len(title) for title in log_table_titles),
            tabulate.tabulate(log_table_data, headers=log_table_headers),
        )
