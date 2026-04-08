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
def _calculate_base_well_count(well_decline_limit,
                                max_wells,
                                past_drilling,
                                last_year_drilling,
                                max_drill_rate,
                                max_drill_rate_frac,
                                drill_predecline,
                                ramp_up_years,
                                start_year):
    """Calculate base well count by simulating drilling forward until cumulative drilling exceeds past drilling.
    
    This function simulates drilling year-by-year, handling ramp-up periods and decline rates,
    until the cumulative drilling count exceeds the past drilling. It returns the calculated
    drilling rate for the current year along with the simulation state.
    
    Parameters
    ----------
    well_decline_limit : int
        Project pattern size, used to determine point at which well starts experiencing diminishing returns
    max_wells : int
        Maximum number of wells possible in the project
    past_drilling : int
        Past drilling during model years
    last_year_drilling : int
        Drilling in last model year
    max_drill_rate : float
        Maximum drill rate allowed by the model
    max_drill_rate_frac : float
        Fraction of maximum drill rate applied when in decline
    drill_predecline : float
        Fraction of well_decline_limit at which decline starts (typically 0.70)
    ramp_up_years : float
        Number of years a project needs to be drilling before reaching max_drill_rate
    start_year : int
        Starting year for the simulation (determined by ramp-up status)
    
    Returns
    -------
    tuple
        (current_year_drilling, final_year, decline_start_year) where:
        - current_year_drilling: Calculated drilling rate for current year
        - final_year: Final year reached in simulation
        - decline_start_year: Year when decline started (if applicable)
    """
    # Initialize simulation variables
    cumulative_drilling = 0
    current_year_drilling = 0
    decline_start_year = 0
    year = start_year
    
    # Simulate drilling forward year-by-year until cumulative drilling exceeds past drilling
    # Note: drilling can exceed well_decline_limit, but there will be a decline penalty
    if (well_decline_limit > 0) & (max_wells > past_drilling):
        while cumulative_drilling <= past_drilling:
            # Calculate what fraction of the decline limit has been drilled
            drilling_fraction = cumulative_drilling / well_decline_limit
            
            # Handle ramp-up period: drilling increases linearly to max_drill_rate
            if year < ramp_up_years:
                # During ramp-up, drilling rate increases proportionally with year
                # Ensure it doesn't drop below last year's drilling
                current_year_drilling = max(
                    last_year_drilling,
                    int((max_drill_rate / ramp_up_years) * year)
                )
                
                # Check if decline has started (drilling fraction exceeds predecline threshold)
                if drilling_fraction > drill_predecline:
                    # Apply decline rate: drilling decreases exponentially from decline start year
                    current_year_drilling = int(
                        current_year_drilling * 
                        (1. - max_drill_rate_frac) ** (year - decline_start_year)
                    )
                    break
                else:
                    # Decline hasn't started yet, mark this as potential decline start
                    decline_start_year = year
            
            # Handle max production period: drilling at full capacity
            else:
                # At full production, drilling rate is at maximum
                current_year_drilling = int(max_drill_rate)
                
                # Check if decline has started and apply decline rate if needed
                if drilling_fraction > drill_predecline:
                    # Apply decline rate: drilling decreases exponentially from decline start year
                    current_year_drilling = int(
                        current_year_drilling * 
                        (1. - max_drill_rate_frac) ** (year - decline_start_year)
                    )
                else:
                    # Decline hasn't started yet, mark this as potential decline start
                    decline_start_year = year
            
            # Set minimum floor on base drilling (5 wells per year)
            if current_year_drilling < 5:
                current_year_drilling = 5
            
            # Advance simulation: add this year's drilling to cumulative total and move to next year
            cumulative_drilling += current_year_drilling
            year += 1
    
    return current_year_drilling, year, decline_start_year


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

    undiscovered_drill_flag : boolean, optional
        Flag indicating if this is an undiscovered project

    Returns
    -------
    cur_year_drill : int
        Current model year drilling
    """
    # ============================================================================
    # SECTION 1: Initialize and Determine Ramp-Up Status
    # ============================================================================
    # Determine if project has completed ramp-up period
    # Threshold: 80% of max_drill_rate indicates ramp-up is complete
    RAMP_UP_COMPLETE_THRESHOLD = 0.8
    
    if last_year_drilling > (max_drill_rate * RAMP_UP_COMPLETE_THRESHOLD):
        # Ramp-up complete: start simulation at full production year
        simulation_start_year = ramp_up_years
    else:
        # Still in ramp-up: calculate current year based on drilling progress
        # Progress is measured as fraction of max_drill_rate achieved
        ramp_up_progress = last_year_drilling / max(1, max_drill_rate)
        simulation_start_year = math.ceil(ramp_up_progress * ramp_up_years) + 1

    # ============================================================================
    # SECTION 2: Calculate Base Well Count
    # ============================================================================
    # Simulate drilling forward to determine base drilling rate for current year
    # This accounts for ramp-up periods and decline rates
    current_year_drilling, simulation_year, decline_start_year = _calculate_base_well_count(
        well_decline_limit=well_decline_limit,
        max_wells=max_wells,
        past_drilling=past_drilling,
        last_year_drilling=last_year_drilling,
        max_drill_rate=max_drill_rate,
        max_drill_rate_frac=max_drill_rate_frac,
        drill_predecline=drill_predecline,
        ramp_up_years=ramp_up_years,
        start_year=simulation_start_year
    )

    # ============================================================================
    # SECTION 3: Apply Drilling Increase Constraints
    # ============================================================================
    # Limit year-over-year drilling increase to prevent unrealistic jumps
    # Maximum increase: 2x last year's drilling (or minimum of 10 wells)
    MAX_INCREASE_MULTIPLIER = 2.0
    MIN_INCREASE_DRILLING = 10
    
    if (current_year_drilling > last_year_drilling * MAX_INCREASE_MULTIPLIER) & (last_year_drilling > 0):
        current_year_drilling = max(last_year_drilling * MAX_INCREASE_MULTIPLIER, MIN_INCREASE_DRILLING)

    # ============================================================================
    # SECTION 4: Calculate Maximum Drilling Limits
    # ============================================================================
    # Set maximum drilling based on percentage of remaining wells that can be drilled per year
    remaining_wells = max_wells - past_drilling
    max_drilling_from_percentage = max(remaining_wells * max_year_drill_percent, 0)
    
    # Adjust maximum drilling for ramp-up years (proportional to ramp-up progress)
    if simulation_year < ramp_up_years:
        ramp_up_factor = simulation_year / ramp_up_years
        max_drilling_from_percentage = max_drilling_from_percentage * ramp_up_factor
    
    max_allowed_drilling = max_drilling_from_percentage

    # ============================================================================
    # SECTION 5: Apply Final Floor Constraints
    # ============================================================================
    # Set minimum drilling floors based on project type
    MIN_DRILLING_FLOOR_UNDISCOVERED = 5
    MIN_DRILLING_FLOOR_CONTINUOUS = 0
    
    if undiscovered_drill_flag:
        # Undiscovered projects: maintain minimum of 5 wells per year
        if current_year_drilling < MIN_DRILLING_FLOOR_UNDISCOVERED:
            current_year_drilling = MIN_DRILLING_FLOOR_UNDISCOVERED
    else:
        # Continuous projects: allow drilling to go to zero
        if current_year_drilling < MIN_DRILLING_FLOOR_CONTINUOUS:
            current_year_drilling = MIN_DRILLING_FLOOR_CONTINUOUS

    # ============================================================================
    # SECTION 6: Apply Price-Based Adjustments
    # ============================================================================
    # Adjust drilling based on commodity prices (higher prices = more drilling)
    price_adjustment = calculate_price_adjustment(
        oil_price, ng_price, well_type_num, base_oil_prc, 
        base_gas_prc, oil_prod, gas_prod, low_price_flag
    )
    current_year_drilling = current_year_drilling * price_adjustment

    # ============================================================================
    # SECTION 7: Apply Maximum Drilling Constraint
    # ============================================================================
    # Cap drilling at the maximum allowed based on remaining wells percentage
    if current_year_drilling > max_allowed_drilling:
        current_year_drilling = max_allowed_drilling

    # ============================================================================
    # SECTION 8: Safety Checks and Final Adjustments
    # ============================================================================
    # Check for negative drilling (should never happen, indicates calculation error)
    if current_year_drilling < 0:
        logger.warning('Negative Drilling was calculated, breaking HSM')
        exit()

    # Limit rate of drilling decline (prevent sudden drops)
    # Minimum drilling: 80% of last year's drilling
    MAX_DECLINE_RATE = 0.8
    if current_year_drilling <= last_year_drilling * MAX_DECLINE_RATE:
        current_year_drilling = last_year_drilling * MAX_DECLINE_RATE

    # Round drilling to integer (drilling is measured in whole wells)
    current_year_drilling = math.floor(current_year_drilling)

    return current_year_drilling


def calculate_price_adjustment(oil_price, ng_price, well_type_num, base_oil_prc, 
                               base_gas_prc, oil_prod, gas_prod, low_price_flag):
    """Calculate price adjustment factor for drilling based on commodity prices.
    
    This function adjusts drilling activity based on current oil/gas prices relative
    to base prices. Higher prices increase drilling (price_adj > 1), lower prices
    decrease drilling (price_adj < 1).
    
    Parameters
    ----------
    oil_price : float
        Current model year's oil price
    ng_price : float
        Current model year's natural gas price
    well_type_num : int
        Well type number (<=2 for oil wells, >=3 for gas wells)
    base_oil_prc : float
        Base oil price threshold for drilling acceleration
    base_gas_prc : float
        Base natural gas price threshold for drilling acceleration
    oil_prod : float
        Project oil production level
    gas_prod : float
        Project gas production level
    low_price_flag : int
        Flag to throttle drilling responsiveness in low price scenarios (0 or 1)
    
    Returns
    -------
    float
        Price adjustment factor (multiplier for drilling count)
    """
    # ============================================================================
    # HARDCODED PARAMETERS
    # ============================================================================
    MAX_PRICE_ADJUSTMENT = 2.0                    # Maximum allowed price adjustment
    MAX_GAS_ADJ_RATIO = 1.5                       # Maximum gas adjustment ratio cap
    GAS_TO_OIL_CONVERSION = 5.6 * 1000            # Conversion factor for gas-to-oil ratio
    HIGH_GAS_OIL_RATIO_THRESHOLD = 6000           # Threshold for high gas-to-oil ratio
    LOW_PRODUCTION_DAMPENING_EXPONENT = 0.5       # Exponent to dampen response when production is zero
    
    # Tight oil well parameters
    TIGHT_OIL_MIN_PROD_THRESHOLD = 1              # Minimum production for tight oil adjustments
    TIGHT_OIL_PROD_DIVISOR = 100                  # Production divisor for tight oil sensitivity
    TIGHT_OIL_MIN_ADJ = 0.98                      # Minimum adjustment bound for tight oil
    TIGHT_OIL_MAX_ADJ = 1.02                      # Maximum adjustment bound for tight oil
    
    # Shale/tight gas well parameters
    SHALE_GAS_MIN_PROD_THRESHOLD = 10             # Minimum production for shale gas adjustments
    SHALE_GAS_PROD_DIVISOR = 1000                 # Production divisor for shale gas sensitivity
    SHALE_GAS_MIN_ADJ = 0.98                      # Minimum adjustment bound for shale gas
    SHALE_GAS_MAX_ADJ = 1.02                      # Maximum adjustment bound for shale gas
    
    # Low price scenario parameters
    LOW_PRICE_FLAG_MULTIPLIER = 1.25              # Multiplier when low_price_flag is active
    
    # ============================================================================
    # NaN GUARD - Safety net for missing price data
    # ============================================================================
    if (np.isnan(oil_price) or np.isnan(ng_price) or 
        np.isnan(base_oil_prc) or np.isnan(base_gas_prc)):
        logger.warning(f'NaN detected in price adjustment inputs: oil_price={oil_price}, '
                       f'ng_price={ng_price}, base_oil_prc={base_oil_prc}, base_gas_prc={base_gas_prc}. '
                       f'Using neutral adjustment (1.0)')
        return 1.0
    
    # ============================================================================
    # CALCULATE BASE PRICE RATIO
    # ============================================================================
    is_oil_well = (well_type_num <= 2)
    
    if is_oil_well:
        # Oil wells: base adjustment from oil price ratio
        price_adj = oil_price / base_oil_prc
    else:
        # Gas wells: base adjustment from gas price ratio
        price_adj = ng_price / base_gas_prc
    
    # ============================================================================
    # OIL WELL ADJUSTMENTS
    # ============================================================================
    if is_oil_well:
        # Calculate gas adjustment for potential gas-to-oil ratio consideration
        gas_adj = ng_price / base_gas_prc
        if gas_adj > MAX_GAS_ADJ_RATIO:
            gas_adj = MAX_GAS_ADJ_RATIO
        
        if oil_prod > 0:
            # Calculate gas-to-oil ratio
            gas_oil_ratio = gas_prod * GAS_TO_OIL_CONVERSION / oil_prod
            
            # Tight oil wells (well_type_num == 2) have increased price sensitivity
            is_tight_oil = (well_type_num == 2) and (oil_prod >= TIGHT_OIL_MIN_PROD_THRESHOLD)
            if is_tight_oil:
                if price_adj < 1:
                    # Low prices: increase sensitivity (more negative response)
                    sensitivity_factor = 1 / ((oil_prod / TIGHT_OIL_PROD_DIVISOR) ** LOW_PRODUCTION_DAMPENING_EXPONENT)
                    price_adj = min(price_adj ** sensitivity_factor, TIGHT_OIL_MIN_ADJ)
                elif price_adj > 1:
                    # High prices: increase sensitivity (more positive response)
                    sensitivity_factor = (oil_prod / TIGHT_OIL_PROD_DIVISOR) ** LOW_PRODUCTION_DAMPENING_EXPONENT
                    price_adj = max(price_adj ** sensitivity_factor, TIGHT_OIL_MAX_ADJ)
            
            # Apply additional adjustment for high gas-to-oil ratio wells (associated gas)
            if gas_oil_ratio > HIGH_GAS_OIL_RATIO_THRESHOLD:
                price_adj = price_adj ** (gas_adj ** LOW_PRODUCTION_DAMPENING_EXPONENT)
        else:
            # No oil production: dampen price response
            price_adj = price_adj ** LOW_PRODUCTION_DAMPENING_EXPONENT
    
    # ============================================================================
    # GAS WELL ADJUSTMENTS
    # ============================================================================
    else:
        if gas_prod > 0:
            # Shale/tight gas wells (well_type_num 4 or 5) have increased price sensitivity
            is_shale_gas = ((well_type_num == 4) or (well_type_num == 5)) and (gas_prod >= SHALE_GAS_MIN_PROD_THRESHOLD)
            if is_shale_gas:
                if price_adj < 1:
                    # Low prices: increase sensitivity (more negative response)
                    sensitivity_factor = 1 / ((gas_prod / SHALE_GAS_PROD_DIVISOR) ** LOW_PRODUCTION_DAMPENING_EXPONENT)
                    price_adj = min(price_adj ** sensitivity_factor, SHALE_GAS_MIN_ADJ)
                
                if price_adj > 1:
                    # High prices: increase sensitivity (more positive response)
                    sensitivity_factor = (gas_prod / SHALE_GAS_PROD_DIVISOR) ** LOW_PRODUCTION_DAMPENING_EXPONENT
                    price_adj = max(price_adj ** sensitivity_factor, SHALE_GAS_MAX_ADJ)
        else:
            # No gas production: dampen price response
            price_adj = price_adj ** LOW_PRODUCTION_DAMPENING_EXPONENT
    
    # ============================================================================
    # FINAL CONSTRAINTS
    # ============================================================================
    # Cap maximum adjustment
    if price_adj > MAX_PRICE_ADJUSTMENT:
        price_adj = MAX_PRICE_ADJUSTMENT
    
    # Apply low price flag adjustment (throttles drilling reduction in low price scenarios)
    if (low_price_flag == 1) and (price_adj < 1):
        price_adj = price_adj * LOW_PRICE_FLAG_MULTIPLIER
    
    return price_adj


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
    ser_out = pd.Series(0.0, index=list(range(evaluation_years)), dtype='float64')

    for year in range(evaluation_years):
        #Don't start until delay
        if year >= series[nam.delay_years]:
            ser_out[year] = float(series[nam.drill_per_year])
        else:
            continue
        #Once drilling has started, adjust drilling for hitting total wells
        remainder = series[nam.development_wells] - ser_out.sum()
        if remainder <= 0:
            ser_out[year] = float(series[nam.drill_per_year] + remainder)
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
    # make empty series length of evaluation period (use 0.0 to ensure float dtype)
    ser_out = pd.Series(0.0, index=list(range(evaluation_years)), dtype='float64')

    for year in range(evaluation_years):
        # don't start until delay
        if year >= series[nam.delay_years]:
            ser_out[year] = float(series[nam.drill_per_year])
        else:
            continue
        # once drilling has started, adjust drilling for hitting total wells
        remainder = series[nam.development_wells] + series[nam.delineation_wells] - ser_out.sum()
        if remainder <= 0:
            ser_out[year] = float(series[nam.drill_per_year] + remainder)
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
    ser_out = pd.Series(0.0, index=list(range(evaluation_years)), dtype='float64')

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
                ser_out[year] = float(series[hist_year])
            #Ramp up period
            elif year < series[nam.ramp_up_years]:
                ser_out[year] = float((series[initial] +
                                 ((series[max_prod] - series[initial]) / series[nam.ramp_up_years]) * year))
                #Check if decline started yet, if decline_flag == 1 dec_start to first evaluation year
                if (production_fraction > series[pre_decline]) | (decline_flag == 1):
                    ser_out[year] = float(ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                                                     (-1.0 / series[nam.hyper_decline_coef])))
                else:
                    dec_start = year

            #Max production period
            else:
                ser_out[year] = float(series[max_prod])
                #Check if decline started yet
                if (production_fraction > series[pre_decline]) | (decline_flag == 1):
                    ser_out[year] = float(ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                                                     (-1.0 / series[nam.hyper_decline_coef])))
                else:
                    dec_start = year
            production_fraction = (past_prod + ser_out.sum())/series[resource]
            #Check if field has been depleted
            if production_fraction > 1.0:
                ser_out[year] = float(ser_out[year] - ((past_prod + ser_out.sum()) - series[resource]))
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

    # make empty series length of evaluation period (use 0.0 to ensure float dtype)
    ser_out = pd.Series(0.0, index=list(range(evaluation_years)), dtype='float64')

    # marker for decline start year
    dec_start = 0

    #Main drilling equation
    if well_limit > 0.0:
        for year in range(evaluation_years):
            drilling_fraction = (past_drilling + ser_out.sum())/well_limit
            #Ramp up period
            if year < series[nam.ramp_up_years]:
                ser_out[year] = float((series[nam.max_drill_rate] / series[nam.ramp_up_years]) * year)
                #Check if decline started yet
                if drilling_fraction > series[nam.drill_predecline]:
                    ser_out[year] = float(ser_out[year] * (1. - series[nam.max_drill_rate_frac]) ** (year - dec_start))
                else:
                    dec_start = year
            #Max production period
            else:
                ser_out[year] = float(series[nam.max_drill_rate])
                #Check if decline started yet
                if drilling_fraction > series[nam.drill_predecline]:
                    ser_out[year] = float(ser_out[year] * (1. - series[nam.max_drill_rate_frac]) ** (year - dec_start))
                else:
                    dec_start = year
            drilling_fraction = (past_drilling + ser_out.sum())/well_limit
            #Check if field has been depleted
            if drilling_fraction > 1.0:
                ser_out[year] = float(ser_out[year] - ((past_drilling + ser_out.sum()) - well_limit))
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
    ser_out = pd.Series(0.0, index=list(range(evaluation_years)), dtype='float64')

    # marker for decline start year
    dec_start = 0

    #Main drilling equation
    if series[resource] > 0.0:
        for year in range(evaluation_years):
            production_fraction = (past_prod + ser_out.sum())/series[resource]

            #Ramp up period
            if year < series[nam.ramp_up_years]:
                ser_out[year] = float((series[initial] +
                                 ((series[max_prod] - series[initial]) / series[nam.ramp_up_years]) * year))
                #Check if decline started yet
                if production_fraction > series[pre_decline]:
                    #ser_out[year] = ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                    #                                 (-1.0 / series[nam.hyper_decline_coef]))
                    ser_out[year] = float(ser_out[year] * ((1. - series[decline]) ** (year - dec_start)))
                else:
                    dec_start = year

            #Max production period
            else:
                ser_out[year] = float(series[max_prod])
                #Check if decline started yet
                if production_fraction > series[pre_decline]:
                    #ser_out[year] = ser_out[year] * ((1. + (series[decline] * (year - dec_start))) **
                    #                                 (-1.0 / series[nam.hyper_decline_coef]))
                    ser_out[year] = float(ser_out[year] * ((1. - series[decline]) ** (year - dec_start)))
                else:
                    dec_start = year
            production_fraction = (past_prod + ser_out.sum())/series[resource]

            #Check if field has been depleted
            if production_fraction > 1.0:
                ser_out[year] = float(ser_out[year] - ((past_prod + ser_out.sum()) - series[resource]))
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
