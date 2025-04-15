"""File containing drilling equations used in HSM.

Summary
_______
This file contains the drilling functions used by the **Onshore**, **Offshore** and **Alaska** submodules. Each drilling
equation is accessed from the relevant submodule file, with some submodules containing multiple drilling equations.
The drilling equations below are:

    1. on_next_wells - Generates Onshore submodule wells drilled by project for the current model year
    2. off_drill_schedule - Generates Offshore Submodule developmental drilling schedule by project
    3. off_operating_schedule - Generates Offshore Submodule operating cost schedule
    4. off_production_profile - Generates Offshore submodule project production profiles
    5. ak_calculate_drill_schedule - Generates Alaska submodule project drilling schedules
    6. ak_production_profile - Generates Alaska Submodule project production profiles
    7. ak_decline_profile - Decline schedule for Alaska Submodule producing projects


Model Functions and Class Methods
_________________________________
    * on_next_wells - Generates Onshore submodule wells drilled by project for the current
    * off_drill_schedule - Generates Offshore Submodule developmental drilling schedule by
    * off_operating_schedule - Generates Offshore Submodule operating cost schedule
    * off_production_profile - Generates Offshore submodule project production profiles
    * ak_calculate_drill_schedule - Generates Alaska submodule project drilling schedules
    * ak_production_profile - Generates Alaska Submodule project production profiles
    * ak_decline_profile - Decline schedule for Alaska Submodule producing projects


Input Files
-----------
None


Output Debug Files
__________________
None


Output Restart Variables
________________________
None


Notes
-----
Convention for import alias is, import drilling_equations as drill_eq


Drilling Equations Class Methods
________________________________
"""

import pandas as pd
import numpy as np
import warnings
import names as nam
import math
from hsm import logging

# Get logging
logger = logging.getLogger('common.py')


###Onshore Drilling Equations
def on_next_wells(well_decline_limit,
               max_wells,
               past_drilling,
               last_year_drilling,
               max_drill_rate,
               max_drill_rate_frac,
               drill_predecline,
               ramp_up_years,
               oil_price,
               ng_price,
               well_type_num,
               base_oil_prc,
               base_gas_prc,
               oil_prod,
               gas_prod,
               max_year_drill_percent,
               current_model_year,
               low_price_flag,
               undiscovered_drill_flag = False):
    """Generates Onshore submodule wells drilled by project for current model year.

    The drilling equation operates as follows:

        1. Function variables are instantiated and ramp-up year status is declared (whether drilling is sitll ramping up)

        2. Base drilling is determined in a loop, where drilling is continuously recalculated as long as total drilling < past drilling:
            a. If ramp-up year, base drilling = max drilling * (production year/total ramp-up year)
            b. If normal drilling year, base drilling = max drilling
            c. If past drilling > well decline limit (project area well saturation limit),
               base drilling = max drilling * (1. - drilling fraction) ** (year - decline start)

        3. Set a floor on base drilling of 5 wells/year and add 1 to the current modeled year

        4. Once base drilling is determined when total drilling > past drilling:
            a. Set max drilling at double last year's drilling (or min of 10)
            b. Set max year drilling as maximum of current calculated drilling and calculated maximum drilling percentage
               based on total remaining available patterns
            c. Adjust max drilling for ramp-up years
            d. Set a floor on drilling based on these constraints

        5. Adjust drilling based on crude oil and natural gas prices (i.e. higher oil price means more drilling)

        6. Apply the maximum drilling constraint

        7. Round drilling to be an integer

    Parameters
    ----------
    well_decline_limit : int
        Project pattern / standard project pattern size, used to determine point at which well starts experiencing diminishing returns

    max_wells : int
        Project pattern / min well spacing, used to determine absolute maximum number of wells possible in a given project

    past_drilling : int
        Past drilling during model years

    last_year_drilling : int
        Drilling in last model year

    max_drill_rate : float
        Maximum drill rate allowed by the model

    max_drill_rate_frac : float
        Fraction of maximum drill rate applied in given year (i.e. well will not operate at maximum drill rate if in decline)

    drill_predecline : float
        At 70% of project well_decline_limit, project EURs start to decline

    ramp_up_years : float
        Number of years a project needs to be drilling before reaching max_drill_rate

    oil_price : int
        Model year's oil price

    ng_price : int
        Model year's natural gas price

    well_type_num : int
        Well type number used for price benchmarking

    base_oil_prc : float
        Base oil price set manually in the input file for price at which drilling accelerates

    base_gas_prc : float
        Base natural gas price set manually in the input file for price at which drilling accelerates

    oil_prod : float
        Project oil production

    gas_prod : float
        Project oil production

    max_year_drill_percent : float
        Percentage of remaining produciton that can be drilled in model year

    low_price_flag : boolean
        Flag to throttle drilling responsiveness in low price case

    Returns
    -------
    cur_year_drill : int
        Current model year drilling
    """
    #Instantiate function variables
    drilling_fraction = 0
    dec_start = 0
    cum_sum = 0
    cur_year_drill = 0

    #Instantiate Ramp-up years
    if last_year_drilling > (max_drill_rate * 0.80): #Replaced divide by 2 since there are 5 years of rampup
        year = ramp_up_years
    else:
        year = math.ceil((last_year_drilling / max(1,max_drill_rate)) * ramp_up_years) + 1


    #Determine base well count
    if (well_decline_limit > 0) & (max_wells > past_drilling): #drilling can exceed totpat, but there will be a decline penalty
        while cum_sum <= past_drilling:
            drilling_fraction = cum_sum/well_decline_limit
            #ramp up period
            if year < ramp_up_years:
                cur_year_drill = max(last_year_drilling, int((max_drill_rate / ramp_up_years) * year))
                #check if decline started yet
                if drilling_fraction > drill_predecline:
                    cur_year_drill = int(cur_year_drill * (1. - max_drill_rate_frac) ** (year - dec_start))
                    break
                else:
                    dec_start = year
            #max production period
            else:
                cur_year_drill = int(max_drill_rate)
                #check if decline started yet and apply decline rate as required
                if drilling_fraction > drill_predecline:
                    cur_year_drill = int(cur_year_drill * (1. - max_drill_rate_frac) ** (year - dec_start))
                else:
                    dec_start = year

            #Set floor on base drilling
            if cur_year_drill < 5:
                cur_year_drill = 5

            #Add a year
            cum_sum += cur_year_drill
            year += 1


    ###Set max rate of drilling increase
    if (cur_year_drill > last_year_drilling * 2) & (last_year_drilling > 0):
        cur_year_drill = max(last_year_drilling * 2, 10)


    ###Set Max drilling based on maximum drill percent of remaining wells/year
    max_cur_year_drill = max(((max_wells - past_drilling) * max_year_drill_percent), 0)


    #Adjust for Rampup years
    if year < ramp_up_years:
        max_cur_year_drill = max_cur_year_drill * (year/ramp_up_years)

    ###Set floor on drilling for undiscovered projects at 5, for continuous projects at 0
    if (cur_year_drill < 5) & undiscovered_drill_flag == True:
        cur_year_drill = 5
    elif cur_year_drill < 0:
        cur_year_drill = 0

    ###Adjust Drilling Based on Prices
    #Oil price adjustment
    if well_type_num <= 2:
        price_adj = oil_price / base_oil_prc #price adjustment based on model year's crude price and base crude price
        gas_adj = ng_price / base_gas_prc #Adjust for natural gas as well if high gas-to-oil ratio
        if gas_adj > 1.5:
            gas_adj = 1.5
        if oil_prod > 0:
            gas_oil_ratio = gas_prod * 5.6 * 1000 / oil_prod
            if (price_adj < 1) & (oil_prod >= 1) & (well_type_num == 2): #Increase sensitivity for tight oil
                price_adj = min(price_adj ** (1/(oil_prod/100) ** 0.5), 0.98)
            elif (price_adj > 1) & (oil_prod >= 1) & (well_type_num == 2): #Increase sensitivity for tight oil
                price_adj = max(price_adj ** ((oil_prod/100) ** 0.5), 1.02)
            if (gas_oil_ratio > 6000) & (oil_prod > 0): #Apply extra adjustment adder for AD natgas
                pass
                price_adj = price_adj ** (gas_adj ** 0.5)
        else:
            price_adj = price_adj ** 0.5

    #Natural Gas price adjusment
    elif well_type_num >= 3:
        price_adj = ng_price / base_gas_prc #price adjustment based on model year's ng price and base gas price
        if gas_prod > 0:
            if (price_adj < 1) & (gas_prod >= 10) & ((well_type_num == 4) | (well_type_num == 5)): #Increase sensitivity for shale/tight gas
                price_adj = min(price_adj ** (1/(gas_prod/1000) ** 0.5), 0.98)

            if (price_adj > 1) & (gas_prod >= 10) & ((well_type_num == 4) | (well_type_num == 5)): #Increase sensitivity for shale/tight gas
                price_adj = max(price_adj ** ((gas_prod/1000) ** 0.5), 1.02)

        else:
            price_adj = price_adj ** 0.5

    #Cap Price Adjustment
    if price_adj > 2:
        price_adj = 2

    #Reduce price responsiveness to increasing prices during covid years
    if (price_adj > 1) & (current_model_year <= 2026):
        price_adj = price_adj ** 0.25
    elif (price_adj < 1) & (current_model_year <= 2026):
        price_adj = price_adj ** 1.75

    #Low Price Flag
    if (low_price_flag == 1) & (price_adj < 1):
        price_adj = price_adj * 1.25


    ###Apply price adjustment
    cur_year_drill = cur_year_drill * price_adj


    ###Apply max drill constraint
    if cur_year_drill > max_cur_year_drill:
        cur_year_drill = max_cur_year_drill

    ###Check Drilling for negatives
    if cur_year_drill < 0:
        logger.warning('Negative Drilling was calculated, breaking HSM')
        exit()


    ###Limit rate of drilling decline
    if cur_year_drill <= last_year_drilling * 0.8:
        cur_year_drill = last_year_drilling * 0.8


    #Round Drilling
    cur_year_drill = math.floor(cur_year_drill)

    return cur_year_drill


###Offshore drilling equations

def off_drill_schedule(series, evaluation_years):
    """Generates Offshore Submodule developmental drilling schedule by project.

        * Map drilling/year against total wells that need to be drilled

    Parameters
    ----------
    series : pd.Series
        Series containing drilling variables, specifically, nam.delay_years, nam.drill_per_year, and
        nam.development_wells

    evaluation_years : int
        Number of years for economic or production evaluation


    Returns
    -------
    ser_out : pd.Series
        Project drilling schedule
    """
    #Make empty series length of evaluation period
    ser_out = pd.Series(0, index=list(range(evaluation_years)))

    for year in range(evaluation_years):
        #Don't start until delay
        if year >= series[nam.delay_years]:
            ser_out[year] = series[nam.drill_per_year]
        else:
            continue
        #Once drilling has started, adjust drilling for hitting total wells
        remainder = series[nam.development_wells] - ser_out.sum()
        if remainder <= 0:
            ser_out[year] = series[nam.drill_per_year] + remainder
            break

    return ser_out


def off_operating_schedule(series, evaluation_years):
    """Generates Offshore Submodule operating cost schedule.

        * Map drilling/year against total wells that need to be drilled

    Parameters
    ----------
    series : pd.Series
        Series containing drilling variables, specifically, nam.delay_years, nam.drill_per_year, nam.development_wells,
        and nam.delineation_wells
    evaluation_years : int
        Number of years for economic or production evaluation

    Returns
    -------
    ser_out : pd.Series
        Project operating cost schedule
    """
    # make empty series length of evaluation period
    ser_out = pd.Series(0, index=list(range(evaluation_years)))

    for year in range(evaluation_years):
        # don't start until delay
        if year >= series[nam.delay_years]:
            ser_out[year] = series[nam.drill_per_year]
        else:
            continue
        # once drilling has started, adjust drilling for hitting total wells
        remainder = series[nam.development_wells] + series[nam.delineation_wells] - ser_out.sum()
        if remainder <= 0:
            ser_out[year] = series[nam.drill_per_year] + remainder
            break

    ser_out = ser_out.cumsum()
    return ser_out


def off_production_profile(series, evaluation_years, hist_year, oil_or_gas=nam.oil, past_prod_flag=False, decline_flag = False):
    """Generates Offshore submodule project production profiles.

    The Offshore production profile equation operates as follows:

        1. Instantiate variables

        2. Apply decline flags for producing projects
            a. 0 = new field, apply undeveloped prod forecast even if past production
            b. 1 = field declining, apply field decline profile
            c. 2 = field production increasing, hold production constant for the following year, then start declining
            d. 3 = standard field, apply normal production profile

        3. Base drilling is determined in a loop, where drilling is continuously recalculated as long as total drilling < past drilling:
            a. If ramp-up year, base drilling = max drilling * (production year/total ramp-up year)
            b. If normal drilling year, base drilling = max drilling
            c. If past drilling > well decline limit (project area well saturation limit),
               base drilling = max drilling * (1. - drilling fraction) ** (year - decline start)

    Parameters
    ----------
    series : pd.Series
        Series containing production variables, specifically, nam.oil_resources, nam.predecline_prod_fraction_oil,
        nam.initial_oil_prod_rate, nam.max_oil_production_rate, nam.oil_decline_rate, nam.gas_resources,
        nam.predecline_prod_fraction_gas, nam.initial_gas_prod_rate, nam.max_gas_production_rate, nam.gas_decline_rate,
        nam.past_production, nam.ramp_up_years, and nam.hyper_decline_coef

    evaluation_years : int
        Number of years for economic or production evaluation

    history_year : int
        Last year of produciton history

    oil_or_gas : str
        nam.oil or nam.gas

    past_prod_flag : boolean
        If the field has past production (i.e. producing fields) use past production, otherwise use 0

    decline_flag : boolean
        Apply decline flags for historical production


    Returns
    -------
    ser_out : pd.Series
        Project production profile

    """
    #Same routine for oil and gas, choose with oil_or_gas input
    if oil_or_gas == nam.oil:
        resource    = nam.oil_resources
        pre_decline = nam.predecline_prod_fraction_oil
        initial     = nam.initial_oil_prod_rate
        max_prod    = nam.max_oil_production_rate
        decline     = nam.oil_decline_rate
    elif oil_or_gas == nam.gas:
        resource    = nam.gas_resources
        pre_decline = nam.predecline_prod_fraction_gas
        initial     = nam.initial_gas_prod_rate
        max_prod    = nam.max_gas_production_rate
        decline     = nam.gas_decline_rate
    else:
        resource    = nam.oil_resources
        pre_decline = nam.predecline_prod_fraction_oil
        initial     = nam.initial_oil_prod_rate
        max_prod    = nam.max_oil_production_rate
        decline     = nam.oil_decline_rate
        warnings.warn('oil_or_gas not recognized, assuming oil', UserWarning)

    #If the field has past production (i.e. producing fields) use past production, otherwise use 0
    if past_prod_flag:
        past_prod = series[nam.past_production]
    else:
        past_prod = 0

    #If the field has past production (i.e. producing fields) apply decline flag
    #0 = new field, apply undeveloped prod forecast even if past production
    #1 = field declining, apply field decline profile
    #2 = field production increasing, hold production constant for the following year, then start declining
    #3 = standard field, apply normal production profile
    if decline_flag:
        decline_flag = series[nam.decline_flag]
    else:
        decline_flag = 3

    #Set three years of rampup for new fields
    if decline_flag == 0:
        series[nam.ramp_up_years] = 3

    #Make empty series length of evaluation period
    ser_out = pd.Series(0, index=list(range(evaluation_years)))

    dec_start = 0

    #Start Drilling Eq
    if series[resource] > 0.0:
        for year in range(evaluation_years):
            if (year <= 3) & (decline_flag == 0): # If decline_flag = 0 set as new production without past prod for ramp-up years
                production_fraction = 0
            else:
                production_fraction = (past_prod + ser_out.sum())/series[resource]
            #If decline_flag == 2 then hold production steady until production fraction breached
            if (decline_flag == 2) & (production_fraction < series[pre_decline]):
                ser_out[year] = series[hist_year]
            #Ramp up period
            elif year < series[nam.ramp_up_years]:
                ser_out[year] = (series[initial] +
                                 ((series[max_prod] - series[initial]) / series[nam.ramp_up_years]) * year)
                #Check if decline started yet, if decline_flag == 1 dec_start to first evaluation year
                if (production_fraction > series[pre_decline]) | (decline_flag == 1):
                    ser_out[year] = ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                                                     (-1.0 / series[nam.hyper_decline_coef]))
                else:
                    dec_start = year

            #Max production period
            else:
                ser_out[year] = series[max_prod]
                #Check if decline started yet
                if (production_fraction > series[pre_decline]) | (decline_flag == 1):
                    ser_out[year] = ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                                                     (-1.0 / series[nam.hyper_decline_coef]))
                else:
                    dec_start = year
            production_fraction = (past_prod + ser_out.sum())/series[resource]
            #Check if field has been depleted
            if production_fraction > 1.0:
                ser_out[year] = ser_out[year] - ((past_prod + ser_out.sum()) - series[resource])
                break

    return ser_out


###Alaska Drilling Equations

def ak_calculate_drill_schedule(series, evaluation_years):
    """Generates Alaska submodule project drilling schedules.

    Parameters
    ----------
    series : pd.Series
        Series containing production variables, specifically, nam.oil_resources, nam.predecline_prod_fraction_oil,
        nam.initial_oil_prod_rate, nam.max_oil_production_rate, nam.oil_decline_rate, nam.gas_resources,
        nam.predecline_prod_fraction_gas, nam.initial_gas_prod_rate, nam.max_gas_production_rate, nam.gas_decline_rate,
        nam.past_production, nam.ramp_up_years, and nam.hyper_decline_coef

    evaluation_years : int
        Number of years for economic or production evaluation


    Returns
    -------
    ser_out : pd.Series
        Alaska project drill schedule

    """
    past_drilling = series[nam.total_pattern_size_acres]/series[nam.std_pattern_size_acres] - series[nam.past_wells]

    well_limit = series[nam.total_pattern_size_acres]/series[nam.std_pattern_size_acres]

    # make empty series length of evaluation period
    ser_out = pd.Series(0, index=list(range(evaluation_years)))

    # marker for decline start year
    dec_start = 0

    #Main drilling equation
    if well_limit > 0.0:
        for year in range(evaluation_years):
            drilling_fraction = (past_drilling + ser_out.sum())/well_limit
            #Ramp up period
            if year < series[nam.ramp_up_years]:
                ser_out[year] = int((series[nam.max_drill_rate] / series[nam.ramp_up_years]) * year)
                #Check if decline started yet
                if drilling_fraction > series[nam.drill_predecline]:
                    ser_out[year] = int(ser_out[year] * (1. - series[nam.max_drill_rate_frac]) ** (year - dec_start))
                else:
                    dec_start = year
            #Max production period
            else:
                ser_out[year] = int(series[nam.max_drill_rate])
                #Check if decline started yet
                if drilling_fraction > series[nam.drill_predecline]:
                    ser_out[year] = int(ser_out[year] * (1. - series[nam.max_drill_rate_frac]) ** (year - dec_start))
                else:
                    dec_start = year
            drilling_fraction = (past_drilling + ser_out.sum())/well_limit
            #Check if field has been depleted
            if drilling_fraction > 1.0:
                ser_out[year] = ser_out[year] - ((past_drilling + ser_out.sum()) - well_limit)
                break
    return ser_out


def ak_production_profile(series, evaluation_years, oil_or_gas=nam.oil, past_prod_flag=False):
    """Generates Alaska Submodule project production profiles.

    The Alaska production profile equation operates as follows:

        1. Instantiate variables

        2. Base drilling is determined in a loop, where drilling is continuously recalculated as long as total drilling < past drilling:
            a. If ramp-up year, base drilling = max drilling * (production year/total ramp-up year)
            b. If normal drilling year, base drilling = max drilling
            c. If past drilling > well decline limit (project area well saturation limit),
               base drilling = max drilling * (1. - drilling fraction) ** (year - decline start))

    Parameters
    ----------
    series : pd.Series
        Series containing prodcution variables, specifically, nam.oil_resources, nam.predecline_prod_fraction_oil,
        nam.initial_oil_prod_rate, nam.max_oil_production_rate, nam.oil_decline_rate, nam.gas_resources,
        nam.predecline_prod_fraction_gas, nam.initial_gas_prod_rate, nam.max_gas_production_rate, nam.gas_decline_rate,
        nam.past_production, nam.ramp_up_years, and nam.hyper_decline_coef

    evaluation_years : int
        Number of years for economic or production evaluation

    oil_or_gas : str
        nam.oil or nam.gas

    past_prod_flag : bool
        If the field has past production (i.e. producing fields) use past production, otherwise use 0


    Returns
    -------
    ser_out : pd.Series
        Alaska project production profile
    """
    #Same routine for oil and gas, choose with oil_or_gas input
    if oil_or_gas == nam.oil:
        resource    = nam.oil_resources
        pre_decline = nam.predecline_prod_fraction_oil
        initial     = nam.initial_oil_prod_rate
        max_prod    = nam.max_oil_production_rate
        decline     = nam.fc_decline
    elif oil_or_gas == nam.gas:
        resource    = nam.gas_resources
        pre_decline = nam.predecline_prod_fraction_gas
        initial     = nam.initial_gas_prod_rate
        max_prod    = nam.max_gas_production_rate
        decline     = nam.fc_decline
    else:
        resource    = nam.oil_resources
        pre_decline = nam.predecline_prod_fraction_oil
        initial     = nam.initial_oil_prod_rate
        max_prod    = nam.max_oil_production_rate
        decline     = nam.fc_decline
        warnings.warn('oil_or_gas not recognized, assuming oil', UserWarning)

    #If the field has past production (i.e. producing fields) use past production, otherwise use 0
    if past_prod_flag:
        past_prod = series[nam.past_production]
    else:
        past_prod = 0

    #Make empty series length of evaluation period
    ser_out = pd.Series(0, index=list(range(evaluation_years)))

    # marker for decline start year
    dec_start = 0

    #Main drilling equation
    if series[resource] > 0.0:
        for year in range(evaluation_years):
            production_fraction = (past_prod + ser_out.sum())/series[resource]

            #Ramp up period
            if year < series[nam.ramp_up_years]:
                ser_out[year] = (series[initial] +
                                 ((series[max_prod] - series[initial]) / series[nam.ramp_up_years]) * year)
                #Check if decline started yet
                if production_fraction > series[pre_decline]:
                    #ser_out[year] = ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                    #                                 (-1.0 / series[nam.hyper_decline_coef]))
                    ser_out[year] = ser_out[year] * ((1. - series[decline]) ** (year - dec_start))
                else:
                    dec_start = year

            #Max production period
            else:
                ser_out[year] = series[max_prod]
                #Check if decline started yet
                if production_fraction > series[pre_decline]:
                    #ser_out[year] = ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                    #                                 (-1.0 / series[nam.hyper_decline_coef]))
                    ser_out[year] = ser_out[year] * ((1. - series[decline]) ** (year - dec_start))
                else:
                    dec_start = year
            production_fraction = (past_prod + ser_out.sum())/series[resource]

            #Check if field has been depleted
            if production_fraction > 1.0:
                ser_out[year] = ser_out[year] - ((past_prod + ser_out.sum()) - series[resource])
                break

    ser_out = ser_out.shift(series[nam.production_delay], fill_value=0)
    return ser_out


def ak_decline_profile(series, evaluation_years):
    """Decline schedule for Alaska Submodule producing projects.

    Parameters
    ----------
    series : pd.Series
        Series containing production variables, specifically, nam.oil_decline_rate, nam.max_oil_production_rate,
        nam.oil_resources, nam.avg_3_prod

    evaluation_years : int
        Number of years for economic or production evaluation


    Returns
    -------
    ser_out : pd.Series
        Alaska project decline profile

    """
    decline     = nam.oil_decline_rate
    max_prod    = nam.max_oil_production_rate
    resource    = nam.oil_resources
    avg_3_prod  = nam.avg_3_prod

    #Make empty series length of evaluation period
    tim = np.arange(evaluation_years)
    prod = series[avg_3_prod]*np.exp((-series[decline])*tim)

    ser_out = pd.Series(prod, index=tim)

    #Zero out if production exceeds resource, note could improve with calculating difference in the final year
    mask = ser_out.cumsum() >= (series[resource] - series[nam.past_production])
    ser_out[mask] = 0

    return ser_out
