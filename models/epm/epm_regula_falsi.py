"""Regula falsi root-finding used to solve for carbon prices in policy cases.

This file implements the `regfalsi` and regfalsibank` functions, which are used
to solve for carbon prices, carbon taxes, and carbon permit prices as needed
for emissions policy cases. These routines iterate in order to find the lowest
price that achieves the desired emissions reduction goal.
"""

import numpy as np
import tabulate

from epm_common import (
    log_it,
    REGFALSI_EPM_FACTOR,
    REGFALSI_TOL,
    REGFALSIBANK_CTB,
    REGFALSIBANK_EPM_FACTOR,
    REGFALSIBANK_TOL
)
from epm_restart import Restart
from epm_scedes import Scedes
from epm_variables import Variables


def regfalsi(
    restart: Restart,
    variables: Variables,
    new_tax: float,
    new_sum: float
) -> float:
    """
    Use regula falsi root-finding to determine the new tax or permit price.

    The regula falsi algorithm finds the root of a function given two points,
    one with positive and one with negative functional value. It has a linear
    convergence rate and is guaranteed to converge if the function is
    continuous. This function also includes some bracketing logic that
    heuristically attempts to rebracket the root if bracketing is lost due to
    anomalies in the function.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    variables : Variables
        The active intermediate variables for EPM.
    new_tax : float
        Latest carbon tax rate or permit price.
    new_sum : float
        Latest sum of emitted pollutants.

    Returns
    -------
    float
        New value for the carbon tax rate or permit price.

    See Also
    --------
    epm : Calls this function to determine the carbon tax or permit price.
    regfalsibank : Similar function which is called to set the starting price.
    """
    # First iteration, set upper and lower points equal to only point
    j = restart.ncntrl_curiyr - 1
    slope = -REGFALSI_EPM_FACTOR
    it = min(11, restart.ncntrl_curitr) - 1  # Zero-based Python index
    variables.regfalsi_newsum[it] = new_sum
    variables.regfalsi_newtax[it] = new_tax

    # First iteration need only find one point
    if restart.ncntrl_curitr == 1:
        variables.regfalsi_bracket = False
        variables.regfalsi_low_sum = new_sum
        variables.regfalsi_high_sum = new_sum
        variables.regfalsi_newsum[1:] = 0.0
        variables.regfalsi_newtax[1:] = 0.0
        # EPM is run last, but the initial tax for this run is already read in.
        # We look at what was actually used through the adjustments in the
        # restart file. If the factors have changed, this is an estimate if
        # jngel = 0 assume no restart, then we use the starting value from the
        # input file, new_tax
        # The base_tax variable is used to calculate tax in first iteration
        if restart.emoblk_etax_flag:
            base_tax = restart.emablk_jngel[j]
        else:
            base_tax = restart.emablk_jngel[j] / restart.emeblk_engel[j]
        variables.regfalsi_high_tax = base_tax
        variables.regfalsi_low_tax = base_tax
        if base_tax > 0.00001 and restart.emoblk_etax_flag:
            new_tax += new_sum / 100.0 * REGFALSI_EPM_FACTOR
        elif base_tax > 0.00001:
            new_tax += new_sum / 1000.0 * REGFALSI_EPM_FACTOR
        new_tax = np.clip(
            new_tax, restart.emoblk_min_tax[j], restart.emoblk_max_tax[j]
        )
        etax_factor = 1.0 if restart.emoblk_etax_flag else 1000.0
        log_table_data = [
            ["Tax Bracket Low", variables.regfalsi_low_tax * etax_factor],
            ["Tax Bracket High", variables.regfalsi_high_tax * etax_factor],
            ["Carbon Bracket Low", variables.regfalsi_low_sum],
            ["Carbon Bracket High", variables.regfalsi_high_sum],
            ["New Tax", new_tax * etax_factor],
            ["Slope", slope],
        ]
        log_it(tabulate.tabulate(
            log_table_data, tablefmt="plain", numalign="right"
        ))
        if restart.ncntrl_fcrl == 1:
            new_tax = variables.regfalsi_newtax[it]
        return new_tax
    # end of iteration 1

    # Second iteration, set low and high correctly and find new point for third
    # This starts with average of low and high and moves appropriately
    if restart.ncntrl_curitr == 2:
        if new_sum > variables.regfalsi_high_sum:
            variables.regfalsi_low_sum = variables.regfalsi_high_sum
            variables.regfalsi_low_tax = variables.regfalsi_high_tax
            variables.regfalsi_high_sum = new_sum
            variables.regfalsi_high_tax = new_tax
        else:
            variables.regfalsi_low_sum = new_sum
            variables.regfalsi_low_tax = new_tax
        # Check if bracketing
        if variables.regfalsi_low_sum * variables.regfalsi_high_sum <= 0.0:
            variables.regfalsi_bracket = True
    # end of iteration 2

    # For iterations greater than two, we continue
    # If not bracketing, we need to find two points bracketing zero
    if restart.ncntrl_curitr > 2:
        if variables.regfalsi_low_sum * variables.regfalsi_high_sum > 0.0:
            # If not bracketed previously, there are four possibilities
            # 1. both still below zero, reset high and keep trying
            # if carbon total still below, but tax higher, use it.
            # this avoids certain anomalies in the function
            if variables.regfalsi_low_sum < 0.0 and new_sum < 0.0:
                variables.regfalsi_low_sum = new_sum
                variables.regfalsi_low_tax = new_tax
            # 2. now bracketing; use reg falsi
            elif variables.regfalsi_low_sum < 0.0 and new_sum > 0.0:
                variables.regfalsi_high_sum = new_sum
                variables.regfalsi_high_tax = new_tax
                variables.regfalsi_bracket = True
            # 3. also bracketing; use reg falsi
            elif variables.regfalsi_low_sum > 0.0 and new_sum < 0.0:
                variables.regfalsi_low_sum = new_sum
                variables.regfalsi_low_tax = new_tax
                variables.regfalsi_bracket = True
            # 4. not bracketing both over user lesser tax value: assume monoton
            elif variables.regfalsi_low_sum > 0.0 and new_sum > 0.0:
                variables.regfalsi_high_sum = new_sum
                variables.regfalsi_high_tax = new_tax
            # four cases, not bracketing before
        else:
            # Bracketed, new positive value replaces old positive
            if new_sum * variables.regfalsi_low_sum > 0.0:
                # new_sum and low_sum both negative. new_sum becomes the NEW
                # low_sum.
                variables.regfalsi_low_sum = new_sum
                variables.regfalsi_low_tax = new_tax
            else:
                # new_sum and high_sum both positive. new_sum becomes the NEW
                # high_sum.
                variables.regfalsi_high_sum = new_sum
                variables.regfalsi_high_tax = new_tax
            # new_sum * low_sum w/r/t 0
        # previous bracket test
    # curitr w/r/t 2

    if not variables.regfalsi_bracket:
        slope = -REGFALSI_EPM_FACTOR
        etax_factor = 100.0 if restart.emoblk_etax_flag else 1000.0
        if variables.regfalsi_newsum[it] != variables.regfalsi_newsum[it - 1]:
            newtax_diff = (
                variables.regfalsi_newtax[it]
                - variables.regfalsi_newtax[it - 1]
            )
            newsum_diff = (
                variables.regfalsi_newsum[it]
                - variables.regfalsi_newsum[it - 1]
            )
            slope = etax_factor * (newtax_diff / newsum_diff)
            if slope >= -0.25:
                slope = -REGFALSI_EPM_FACTOR
            slope = max(slope, -(REGFALSI_EPM_FACTOR ** 2))
        new_tax -= slope * new_sum / etax_factor
        new_tax = np.clip(
            new_tax, restart.emoblk_min_tax[j], restart.emoblk_max_tax[j]
        )

    else:
        # Bracketed, given two points regula-falsi calculates new estimate
        if abs(new_sum) > REGFALSI_TOL:
            if restart.ncntrl_curitr != 2:
                # If estimate bad three times in a row, and carbon tax not
                # changing much (less than 5%), conclude opposite bracket tax
                # is bad. To correct, give that tax bracket a little shove
                # using the heuristic that it takes about a dollar/ton to
                # change 1 mton
                etax_factor = 0.01 if restart.emoblk_etax_flag else 0.001
                if (
                    variables.regfalsi_newsum[it] < 0.0
                    and variables.regfalsi_newsum[it - 1] < 0.0
                    and variables.regfalsi_newsum[it - 2] < 0.0
                    and (
                        abs(
                            variables.regfalsi_newtax[it]
                            - variables.regfalsi_newtax[it - 1]
                        ) / (variables.regfalsi_newtax[it - 1] + 0.0001)
                        < 0.05
                    )
                ):
                    variables.regfalsi_high_tax += new_sum * etax_factor
                    variables.regfalsi_high_tax = max(
                        variables.regfalsi_high_tax, 0.0
                    )
                elif (
                    variables.regfalsi_newsum[it] > 0.0
                    and variables.regfalsi_newsum[it - 1] > 0.0
                    and variables.regfalsi_newsum[it - 2] > 0.0
                    and (
                        abs(
                            variables.regfalsi_newtax[it]
                            - variables.regfalsi_newtax[it - 1]
                        ) / (variables.regfalsi_newtax[it - 1] + 0.0001)
                        < 0.05
                    )
                ):
                    variables.regfalsi_low_tax += new_sum * etax_factor
                    variables.regfalsi_low_tax = min(
                        restart.emoblk_max_tax[j], variables.regfalsi_low_tax
                    )
                new_tax_numerator = (
                    variables.regfalsi_high_tax * variables.regfalsi_low_sum
                    - variables.regfalsi_low_tax * variables.regfalsi_high_sum
                )
                new_tax_denominator = (
                    variables.regfalsi_low_sum - variables.regfalsi_high_sum
                )
                new_tax = new_tax_numerator / new_tax_denominator
            else:
                # on iteration 2, do simple average
                new_tax = 0.5 * (
                    variables.regfalsi_high_tax + variables.regfalsi_low_tax
                )
            new_tax = np.clip(
                new_tax, restart.emoblk_min_tax[j], restart.emoblk_max_tax[j]
            )
        # outside tolerance
    # setting new value

    # If it is last iteration, don't change tax from what models saw because
    # EPM runs last
    if restart.ncntrl_fcrl == 1:
        new_tax = variables.regfalsi_newtax[it]

    etax_factor = 1.0 if restart.emoblk_etax_flag else 1000.0
    log_table_data = [
        ["Tax Bracket Low", variables.regfalsi_low_tax * etax_factor],
        ["Tax Bracket High", variables.regfalsi_high_tax * etax_factor],
        ["Carbon Bracket Low", variables.regfalsi_low_sum],
        ["Carbon Bracket High", variables.regfalsi_high_sum],
        ["New Tax", new_tax * etax_factor],
        ["Slope", slope],
    ]
    log_it(tabulate.tabulate(
        log_table_data, tablefmt="plain", numalign="right"
    ))

    return new_tax


def regfalsibank(
    restart: Restart,
    scedes: Scedes,
    variables: Variables,
    new_tax: float,
    new_sum: float
) -> float:
    """Estimate the starting carbon price of a series using regula falsi.

    Use a regula falsi root-finding algorithm to estimate the starting carbon
    price of an escalating series that results in a carbon emissions cumulative
    bank balance of zero. This is similar to the `regfalsi` function, but runs
    once per cycle prior to the first year of the program, using results
    (brackets) from prior cycles. The regula falsi algorithm finds the root of
    a function given two points, one with positive and one with negative
    functional value. It has a linear convergence rate and is guaranteed to
    converge if the function is continuous. This function includes rebracketing
    logic that is triggered by the REBRACKT key in the scedes, as well as some
    additional bracketing logic that tries to rebracket the root if bracketing
    is lost due to anomalies in the function.

    NOTE: The epmbank common block stores the solution bracket data for this
    function in the restart file. If bracket data is zero, start from scratch.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    scedes : Scedes
        The current scedes file dict.
    variables : Variables
        The active intermediate variables for EPM.
    new_tax : float
        Latest carbon allowance fee or tax.
    new_sum : float
        Latest carbon balance (negative if emissions are below target and
        positive if emissions are above target).

    Returns
    -------
    float
        New value for the carbon allowance fee or tax.

    See Also
    --------
    epm : Calls this function when needed to estimate starting carbon prices.
    regfalsibank_end : Handles the final reporting step for this function.
    regfalsi : Similar function which is called more frequently.
    """
    yesno = ("No", "Yes")

    # First iteration, set upper and lower points equal to only point

    # Zero-based year index for which calculations apply
    iy = variables.epm_out_bank_onyr - 1990

    restart.epmbank_slope = -REGFALSIBANK_EPM_FACTOR
    rebracket = int(scedes.get("REBRACKT", "0"))

    log_table_title = (
        "Banking Bracket--Regula Falsi-Before Changes, prior iterations"
    )
    bank_iter = f"Bank {restart.epmbank_iter}"
    log_table_data = [
        [bank_iter, "low_tax * 1000", restart.epmbank_low_tax * 1000.0],
        [bank_iter, "high_tax * 1000", restart.epmbank_high_tax * 1000.0],
        [bank_iter, "low_sum", restart.epmbank_low_sum],
        [bank_iter, "high_sum", restart.epmbank_high_sum],
        [bank_iter, "Bracketed?", yesno[restart.epmbank_bracket]],
        [bank_iter, "slope", restart.epmbank_slope],
        [bank_iter, "new_sum", new_sum],
        [bank_iter, "new_tax * 1000", new_tax * 1000.0],
    ]
    log_it(
        log_table_title, "=" * len(log_table_title),
        tabulate.tabulate(log_table_data, numalign="right"),
    )

    # iter -- count on how many times this routine is run
    restart.epmbank_iter += 1

    if restart.epmbank_iter > 100:
        restart.epmbank_newsum[:10] = restart.epmbank_newsum[-10:]
        restart.epmbank_newtax[:10] = restart.epmbank_newtax[-10:]
        restart.epmbank_newsum[10:] = 0.0
        restart.epmbank_newtax[10:] = 0.0
        restart.epmbank_iter = 11
        log_it("Bank iter #100 changed to 10. New iter is #11.")

    it = restart.epmbank_iter - 1  # Zero-based Python index

    restart.epmbank_newsum[it] = new_sum  # Record prior run's result
    restart.epmbank_newtax[it] = new_tax

    if restart.epmbank_iter > 1:
        log_table_headers = ["iter", "newtax", "newsum"]
        log_table_data = []
        for i in range(restart.epmbank_iter):
            log_table_data.append([
                i + 1,
                restart.epmbank_newtax[i] * 1000.0,
                restart.epmbank_newsum[i],
            ])
        log_it(tabulate.tabulate(log_table_data, headers=log_table_headers))

    # Start of rebracketing logic
    if rebracket != 0:
        restart.epmbank_bracket = 0
        if restart.ncntrl_curirun == 1:
            log_it("I see that you are requesting a rebracket. Can do.")
        elif (
            restart.ncntrl_curirun == 2
            and (
                restart.epmbank_newsum[it] * restart.epmbank_newsum[it - 1]
                < 0.0
            )
        ):
            restart.epmbank_bracket = 1
        elif (
            restart.ncntrl_curirun > 2
            and restart.epmbank_low_sum * restart.epmbank_high_sum < 0.0
        ):
            restart.epmbank_bracket = 1
        elif (
            restart.ncntrl_curirun > 2
            and (
                restart.epmbank_low_sum < 0.0 < new_sum
                or restart.epmbank_low_sum > 0.0 > new_sum
            )
        ):
            restart.epmbank_bracket = 1

    # Rebracketing request logic
    if rebracket != 0 and restart.ncntrl_curirun == 1:
        restart.epmbank_newsum[2:] = 0.0
        restart.epmbank_newtax[2:] = 0.0
        restart.epmbank_iter, it = 3, 2

        # Note that "low tax" represents high fee resulting in emissions lower
        # than target
        if rebracket == 9999:  # Signal to change both low and high brackets
            log_it("Adjusting both sides of the bracket by 50%")
            restart.epmbank_low_tax *= 1.5
            restart.epmbank_high_tax *= 0.5
            restart.epmbank_newtax[:2] = [
                restart.epmbank_high_tax, restart.epmbank_low_tax
            ]
            restart.epmbank_newsum[:2] = [
                restart.epmbank_high_sum, restart.epmbank_low_sum
            ]
            new_sum = restart.epmbank_low_sum
            new_tax = restart.epmbank_low_tax
        else:
            restart.epmbank_newsum[:2] = [
                restart.epmbank_high_sum, restart.epmbank_low_sum
            ]
            new_sum = restart.epmbank_newsum[1]

            if rebracket < 0:
                # Negative rebracket value means move lower ("high") bracket
                # price
                log_it(
                    "Adjusting lower allowance fee bracket downward by",
                    -rebracket, "percent",
                    sep=" "
                )
                restart.epmbank_newtax[0] = (
                    restart.epmbank_high_tax * (1.0 + rebracket / 100.0)
                )
                restart.epmbank_newtax[1] = restart.epmbank_low_tax
            elif rebracket < 100:
                # Positive rebracket value means move higher ("low") bracket
                # price
                log_it(
                    "Adjusting upper allowance fee bracket upward by",
                    rebracket, "percent",
                    sep=" "
                )
                restart.epmbank_newtax[0] = restart.epmbank_high_tax
                restart.epmbank_newtax[1] = (
                    restart.epmbank_low_tax * (1.0 + rebracket / 100.0)
                )
            else:
                movedown = float(rebracket % 100)
                moveupup = (rebracket - rebracket % 100) / 100.0
                log_it(
                    f"Adjusting high fee up by {int(moveupup)} percent.",
                    f"Adjusting low fee up down {int(movedown)} percent.",
                )
                restart.epmbank_newtax[0] = (
                    restart.epmbank_high_tax * (1.0 - movedown / 100.0)
                )
                restart.epmbank_newtax[1] = (
                    restart.epmbank_low_tax * (1.0 + moveupup / 100.0)
                )
            restart.epmbank_high_tax = restart.epmbank_newtax[0]
            restart.epmbank_low_tax = restart.epmbank_newtax[1]

        restart.epmbank_newsum[it] = new_sum
        restart.epmbank_newtax[it] = new_tax
        new_tax = (
            (
                restart.epmbank_high_tax * restart.epmbank_low_sum
                - restart.epmbank_low_tax * restart.epmbank_high_sum
            )
            / (restart.epmbank_low_sum - restart.epmbank_high_sum)
        )

        log_table_title = "New history:"
        log_table_headers = ["iter", "newtax", "newsum"]
        log_table_data = []
        for i in range(restart.epmbank_iter):
            log_table_data.append([
                i + 1,
                restart.epmbank_newtax[i] * 1000.0,
                restart.epmbank_newsum[i],
            ])
        log_it(
            log_table_title, "=" * len(log_table_title),
            tabulate.tabulate(log_table_data, headers=log_table_headers),
        )

        return regfalsibank_end(restart, new_tax)

    if (
        rebracket != 0
        and restart.ncntrl_curirun >= 3
        and restart.epmbank_bracket == 0
    ):
        # Rebracketed in first cycle, checking progress and possibly adjust
        # rebracketing. There's no need to check cycle 2. It's too early to
        # tell.
        if (
            (
                new_sum < -REGFALSIBANK_CTB
                and (
                    restart.epmbank_newsum[restart.ncntrl_curirun]
                    < -REGFALSIBANK_CTB
                )
            ) or (
                new_sum > REGFALSIBANK_CTB
                and (
                    restart.epmbank_newsum[restart.ncntrl_curirun]
                    > REGFALSIBANK_CTB
                )
            )
        ):
            log_it("I see that our initial rebracketing needs to be adjusted.")
            new_tax -= new_sum * (
                (
                    new_tax
                    - restart.epmbank_newtax[restart.ncntrl_curirun]
                ) / (
                    new_sum
                    - restart.epmbank_newsum[restart.ncntrl_curirun]
                )
            )
            new_tax = min(new_tax, restart.emoblk_max_tax[iy])
            if new_tax < restart.emoblk_min_tax[iy]:
                new_tax = restart.epmbank_newtax[it] * 0.5
            new_tax = max(new_tax, restart.emoblk_min_tax[iy])
            log_it(
                "Adjusting guess to", new_tax * 1000.0,
                "to attempt to bracket.",
                sep=" "
            )
            return regfalsibank_end(restart, new_tax)

    # First iteration need only find one point
    if restart.epmbank_iter == 1:
        restart.epmbank_bracket = 0
        # Assume from under-compliance (new_sum > 0, balance(bank_endyr) < 0),
        # given a zero or low initial price
        restart.epmbank_high_sum = new_sum
        restart.epmbank_high_tax = new_tax
        # Set next price partway between starting and max price.
        new_tax = new_tax * 0.8 + restart.emoblk_max_tax[iy] * 0.2
        # Set up initial bracket values
        restart.epmbank_low_sum = -new_sum
        restart.epmbank_low_tax = restart.emoblk_max_tax[iy]
        restart.epmbank_newsum[1:] = 0.0
        restart.epmbank_newtax[1:] = 0.0
        return regfalsibank_end(restart, new_tax)
    # end of iteration 1

    # Second iteration, set low and high correctly and find new point for
    # third: this starts with average of low and high and moves appropriately
    if restart.epmbank_iter == 2:
        if new_sum > restart.epmbank_high_sum:
            restart.epmbank_low_sum = restart.epmbank_high_sum
            restart.epmbank_low_tax = restart.epmbank_high_tax
            restart.epmbank_high_sum = new_sum
            restart.epmbank_high_tax = new_tax
        else:
            restart.epmbank_low_sum = new_sum
            restart.epmbank_low_tax = new_tax
        # Check if bracketing
        if restart.epmbank_low_sum * restart.epmbank_high_sum <= 0.0:
            restart.epmbank_bracket = 1
    # end of iteration 2

    # For iterations greater than two, we continue
    # If not bracketing, we need to find two points bracketing zero
    if restart.epmbank_iter > 2:
        if restart.epmbank_low_sum * restart.epmbank_high_sum > 0.0:
            # If we are not bracketing there are four cases
            # 1. Both still below zero, reset high and keep trying
            # if carbon total still below, but tax higher, use it.
            # This avoids certain anomalies in the function
            if restart.epmbank_low_sum < 0.0 and new_sum < 0.0:
                restart.epmbank_low_sum = new_sum
                restart.epmbank_low_tax = new_tax
            # 2. Now bracketing; use reg falsi
            elif restart.epmbank_low_sum < 0.0 and new_sum > 0.0:
                restart.epmbank_high_sum = new_sum
                restart.epmbank_high_tax = new_tax
                restart.epmbank_bracket = 1
            # 3. Also bracketing; use reg falsi
            elif restart.epmbank_low_sum > 0.0 and new_sum < 0.0:
                restart.epmbank_low_sum = new_sum
                restart.epmbank_low_tax = new_tax
                restart.epmbank_bracket = 1
            # 4. Not bracketing both over user lesser tax value: assume monoton
            elif restart.epmbank_low_sum > 0.0 and new_sum > 0.0:
                restart.epmbank_high_sum = new_sum
                restart.epmbank_high_tax = new_tax
            # four cases, not bracketing before
        else:
            # Bracketed, new positive value replaces old positive
            # Reinforce bracket (can fall through to here from rebracketing
            # logic)
            restart.epmbank_bracket = 1
            if new_sum * restart.epmbank_low_sum > 0.0:
                # new_sum and low_sum both negative. new_sum becomes the
                # NEW low_sum. Should this be conditional on bracket narrowing?
                restart.epmbank_low_sum = new_sum
                restart.epmbank_low_tax = new_tax
            else:
                # Now bracketed, new value positive, replaces old high_sum
                # (positive)
                restart.epmbank_high_sum = new_sum
                restart.epmbank_high_tax = new_tax
            # fitting new value while bracketed
        # whether or not bracketed previously
    # end of iter > 2

    restart.epmbank_slope = -REGFALSIBANK_EPM_FACTOR
    etax_factor = 100.0 if restart.emoblk_etax_flag else 1000.0
    if restart.epmbank_newsum[it] != restart.epmbank_newsum[it - 1]:
        newtax_diff = (
            restart.epmbank_newtax[it] - restart.epmbank_newtax[it - 1]
        )
        newsum_diff = (
            restart.epmbank_newsum[it] - restart.epmbank_newsum[it - 1]
        )
        restart.epmbank_slope = etax_factor * (newtax_diff / newsum_diff)
        restart.epmbank_slope = np.clip(
            restart.epmbank_slope,
            -10.0 * REGFALSIBANK_EPM_FACTOR,
            -REGFALSIBANK_EPM_FACTOR / 10.0
        )

    if restart.epmbank_bracket == 0:
        new_tax -= restart.epmbank_slope * new_sum / etax_factor
        new_tax = np.clip(
            new_tax, restart.emoblk_min_tax[iy], restart.emoblk_max_tax[iy]
        )

    else:
        # Bracketed, given two points regula-falsi calculates new estimate
        if abs(new_sum) > REGFALSIBANK_TOL:
            if restart.epmbank_iter != 2:

                # If estimate is neg or pos three times in a row, and carbon
                # tax not changing much (less than 1%), then opposite bracket
                # tax needs to open up a bit. To correct, move tax bracket a
                # little on the problem side to allow some movement.
                if (
                    restart.epmbank_newsum[it] < -2.0 * REGFALSIBANK_TOL
                    and (
                        restart.epmbank_newsum[it - 1]
                        < -2.0 * REGFALSIBANK_TOL
                    )
                    and (
                        restart.epmbank_newsum[it - 2]
                        < -2.0 * REGFALSIBANK_TOL
                    )
                    and 0 < restart.ncntrl_curirun < restart.ncntrl_numiruns
                    and (
                        abs(
                            restart.epmbank_newtax[it]
                            - restart.epmbank_newtax[it - 1]
                        ) / (restart.epmbank_newtax[it - 1] + 0.0001)
                        < 0.01
                    )
                ):
                    restart.epmbank_high_tax *= 0.85
                    if restart.epmbank_high_tax == 0.0:
                        restart.epmbank_high_tax = (
                            restart.epmbank_newtax[it] * 0.85
                        )
                    restart.epmbank_high_tax = max(
                        restart.epmbank_high_tax, restart.emoblk_min_tax[iy]
                    )
                elif (
                    restart.epmbank_newsum[it] > 2.0 * REGFALSIBANK_TOL
                    and (
                        restart.epmbank_newsum[it - 1]
                        > 2.0 * REGFALSIBANK_TOL
                    )
                    and (
                        restart.epmbank_newsum[it - 2]
                        > 2.0 * REGFALSIBANK_TOL
                    )
                    and 0 < restart.ncntrl_curirun < restart.ncntrl_numiruns
                    and (
                        abs(
                            restart.epmbank_newtax[it]
                            - restart.epmbank_newtax[it - 1]
                        ) / (restart.epmbank_newtax[it - 1] + 0.0001)
                        < 0.01
                    )
                ):
                    restart.epmbank_low_tax *= 1.15
                    restart.epmbank_low_tax = min(
                        restart.epmbank_low_tax, restart.emoblk_max_tax[iy]
                    )
                new_tax = (
                    (
                        restart.epmbank_high_tax * restart.epmbank_low_sum
                        - restart.epmbank_low_tax * restart.epmbank_high_sum
                    ) / (restart.epmbank_low_sum - restart.epmbank_high_sum)
                )
            else:

                # on iteration 2, do simple average
                new_tax = (
                    (restart.epmbank_high_tax + restart.epmbank_low_tax) / 2.0
                )

            new_tax = np.clip(
                new_tax, restart.emoblk_min_tax[iy], restart.emoblk_max_tax[iy]
            )
        # outside tolerance
    # setting new value

    return regfalsibank_end(restart, new_tax)


def regfalsibank_end(
    restart: Restart,
    new_tax: float
) -> float:
    """Finish reporting for the `regfalsibank` function and return `new_tax`.

    This function is called in return statements within the `regfalsibank`
    function as a replacement for `goto 999` statements in the original
    Fortran. These goto statements would skip almost to the bottom of the
    subroutine, causing it to do its final reporting to EPMOUT and then return.

    Parameters
    ----------
    restart : Restart
        The currently loaded restart file data.
    new_tax : float
        The new_tax value from regfalsibank, which is to be reported and
        returned.

    Returns
    -------
    float
        The value of new_tax that was passed in.
    """
    # Copy the formats and the yesno array from regfalsibank
    yesno = ("No", "Yes")

    log_table_title = "Banking Bracket--Regula Falsi-After Change"
    bank_iter = f"Bank {restart.epmbank_iter}"
    log_table_data = [
        [bank_iter, "low_tax * 1000", restart.epmbank_low_tax * 1000.0],
        [bank_iter, "high_tax * 1000", restart.epmbank_high_tax * 1000.0],
        [bank_iter, "low_sum", restart.epmbank_low_sum],
        [bank_iter, "high_sum", restart.epmbank_high_sum],
        [bank_iter, "Bracketed?", yesno[restart.epmbank_bracket]],
        [bank_iter, "slope", restart.epmbank_slope],
        [bank_iter, "new_tax * 1000", new_tax * 1000.0],
    ]
    log_it(
        log_table_title, "=" * len(log_table_title),
        tabulate.tabulate(log_table_data, numalign="right"),
    )

    return new_tax
