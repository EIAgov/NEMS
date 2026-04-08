Inputs and Methods
==================

The EPM is called at the end of each NEMS iteration, after all other modules have been called.
The module uses energy consumption projections from other NEMS modules as well as exogenous carbon dioxide (CO\ :sub:`2`) emissions factors to create projections of energy-related CO2 emissions.
In addition to emissions calculations, if CO\ :sub:`2` policy cases are enabled, some form of energy price adjustment is calculated to account for the CO\ :sub:`2` tax, or permit fee, for the next iteration.
The CO\ :sub:`2` fee is either fixed (for a straight CO\ :sub:`2` tax) or is varied in each NEMS iteration until a CO\ :sub:`2` goal is met (for the permit auction and permit market options).

The fee on CO\ :sub:`2` emissions is modeled as an adjustment on the end-use price of the fuel.
Two sets of end-use price variables are maintained in NEMS: an unadjusted set of prices without any CO\ :sub:`2` fee added, and an adjusted set of prices that includes the CO\ :sub:`2` fee.
The unadjusted prices are those determined by the NEMS supply and conversion modules. The adjusted prices, with the CO\ :sub:`2` fee included, are the price variables used by the demand and conversion modules purchasing the fuel.
In the Integrating Module, after each module is executed, the adjusted fuel prices are recalculated based on the current unadjusted fuel price and CO\ :sub:`2` fee.

Module inputs and outputs
-------------------------
The input data for the EPM comes primarily from other modules of NEMS. Exogenous data include the policy options to be implemented and the CO\ :sub:`2` emission factors.
If a CO\ :sub:`2` tax scenario is to be implemented, the tax rate must also be specified.
Alternatively, a CO\ :sub:`2` goal may be specified, and the CO\ :sub:`2` tax to meet that goal will be set in the EPM once per iteration of the NEMS solution algorithm.
Output from the EPM consists of the volumes of CO\ :sub:`2` emissions by fuel and economic sector, adjustments to the end-use prices of fuels consumed by the demand and conversion modules, and revenue accrued based on these adjustments.
The adjustments are additions to prices in 1987 dollars per million Btu.
Revenue, in billions of 1987 dollars, from the CO\ :sub:`2` penalty is also calculated and can be used by the Macroeconomic Activity Module or for offline analysis of macroeconomic feedbacks.

Total energy-related CO\ :sub:`2` emissions from both combustion and non-combustion sources are calculated in the EPM from information in several NEMS common blocks.
In many cases, CO\ :sub:`2` emissions are calculated using QBLK, which contains the projected quantities of end-use fuels consumed, and EMEBLK, which contains the CO\ :sub:`2` emissions factors to convert energy consumption into CO\ :sub:`2` emissions.
Some additional common blocks are called to address more specific emissions estimates:

* QMORE, INDOUT, and BIFURC - are called to calculate non-combustion or feedstock use of some fossil fuels
* PMMRPT - to remove biodiesel and ethanol from transportation petroleum emissions
* UEFDOUT - for electric power sector natural gas consumption
* COALEMM - for projected electric power sector coal consumption by sulfur dioxide classification category and associated CO2 for each category
* TRANREP - to account for transportation sector electric power consumption
* HMMBLK - for heat and power, as well as feedstock, use of natural gas in hydrogen production
* QSBLK - for CO2 calculations specific to California
* CCATSDAT - for CO2 capture and storage volumes
* OGSMOUT - for vented CO2 emissions released during natural gas processing
* COGEN - for vented CO2 emissions released during geothermal electricity generation
* WRENEW - for consumption of biogenic and non-biogenic municipal solid waste for electricity generation
 
The resulting CO\ :sub:`2` emissions estimates are stored in the GHGREP common block.
The inputs and outputs associated with the EPM CO\ :sub:`2` price policies are also stored in the NEMS global data structure.
As input, these common blocks contain the NEMS end-use fuel prices:

* MPBLK
* NGTDMOUT
* COALPRC
* PMORE
* EUSPRC
* PONROAD

These prices, established in the NEMS supply and conversion modules, are the EPM input prices.
As output, the EPM projects a dollar-per-Btu adjustment to each product-sector price to reflect any CO\ :sub:`2` tax or allowance fee.
The adjustment is added to the NEMS end-use fuel prices, and they are stored in a parallel set of price common blocks:

* AMPBLK
* ANGTDM
* ACOALPRC
* APMORE
* AEUSPRC
* APONROAD

When no CO\ :sub:`2` policy options are in effect, the adjusted price common blocks match the unadjusted price common blocks from the supply modules.
The energy price adjustments, equal to the difference between the two sets of prices, are stored in the EMABLK common block.
If nonzero, these price adjustments are used as starting values when either of the CO\ :sub:`2` goal options (auction or permit market) are in effect.
Several policy options result in revenue from the CO\ :sub:`2` penalty flowing to the government. This revenue is furnished to the Macroeconomic Activity Module through the EMISSION common block.

Module algorithm
----------------

The EPM is executed once per iteration to determine total CO\ :sub:`2` emissions produced, the revenue created by any tax or permit fees for CO\ :sub:`2` emissions and, depending upon the scenario, the level of offsets produced.
For CO\ :sub:`2` emission policy options, a heuristic algorithm (Regula Falsi) sets a new CO\ :sub:`2` fee to bring the CO\ :sub:`2` emissions closer to the selected policy CO\ :sub:`2` goal.

The general flow of EPM, including relevant function calls and variable names, is as follows:
* First year, first iteration processing

    * Integrating module nems_flow.py executes run_epm.py in "read" mode, which runs the 'epm_read' function from epm_read.py
    * 'epm_read' reads the policy switches in the control file, epmcntl.toml, and parses emissions-related data from several other EPM input files:

        * Emissions policy options are read in through four binary variables ('tax_flag', 'permit_flag', 'market_flag', 'offset_flag).

            * For emissions tax policies, additional switches are available to apply the tax to specific sectors ('elec_flag', 'tran_flag', 'resd_flag', 'comm_flag')
            * For emissions market policies, parameters can be set to specify program elements ('bank_flag', 'bank_startyr', 'bank_endyr', 'bank_end_balance')

        * Historical CO\ :sub:`2` emissions data are read in through epm_history.csv
        * Data for emissions policy cases is read in from epm_tax_or_cap.csv
        * A mapping of Coal Market Module regions are mapped to Census regions through epm_coal_regions.toml
        * Yearly CO\ :sub:`2` emissions factors for each fuel and sector are read in through epm_carbon_factors.tsv
        * Mercury emission classes and caps (for use in the Electricity and Coal Market Modules) are read from epm_mercury_classes.csv and epm_mercury_caps.csv. Additional mercury parameters, including control technologies and mercury emissions rates are read through epm_mercury_parameters.toml
        * Data and parameters pertaining to the California AB-32 cap and trade program are read in through epm_ab32_data.csv and epm_ab32_parameters.toml
        * Exogenous emissions baselines for other (non-CO\ :sub:`2`) gases and offset assumptions (marginal abatement cost tables) are read from ghgoffx.xlsx
        * Emission allowance auction shares are read from epm_restart.py

* Nems_flow.py executes run_epm.py in "main" mode, which calls the core 'epm' function from epm_core.py
* 'epm' then calls on several functions from other supporting Python files to perform various EPM operations

    * The 'sum_emissions' function (from epm_sum_emissions.py) adds up CO\ :sub:`2` emissions across all sectors of the economy, shares emissions by region and electrical power usage, handle historical benchmarking/overwrites, and report the totals.

        * For the market permit system with offset policy option, the 'sum_emissions' function calls the 'oghg' function (from epm_other_ghg.py) to determine what level of emission offsets is available to raise the CO\ :sub:`2` cap, given the current CO\ :sub:`2` tax

    * The 'accntrev' function (from epm_revenue.py) calculates revenues from CO\ :sub:`2` tax or CO\ :sub:`2` permit fees from emissions policy cases
    * For emissions policy options including a market permit system, the 'initrev' function (from epm_revenue.py) allocates revenue to end-use sectors based on initial sector shares of CO\ :sub:`2` emissions
    * For emissions policy options including a permit auction or market, the 'regfalsi' function (epm_regula_falsi.py) calculates new CO\ :sub:`2` taxes to reduce the absolute difference between the CO\ :sub:`2` emissions and an established CO\ :sub:`2` goal
    * For emissions policy options including a CO2 tax, the 'price_adjust' function (from epm_adjustments.py) adjusts energy prices for end-use fuels are adjusted by multiplying the CO\ :sub:`2` tax by the fuels' emission factor

* After each NEMS module is called, prices are recalculated to include a tax by adding the tax price to the prices projected by the supply modules (though the 'copy_adjusted' function from pyfiler1.py in the NEMS main module).

Emissions policy options
------------------------

In addition to providing estimates of energy-related CO\ :sub:`2` emissions, one of the EPM's primary functions is to model hypothetical emissions policies.
The EPM can model five different policy scenarios in NEMS. Each of these policy cases can be turned on or off by adjusting their associated binary 'flag' variables in the EPM control parameters file (epmcntl.toml).
Descriptions of each policy scenario are provided below.


Carbon dioxide tax
^^^^^^^^^^^^^^^^^^

A tax per kilogram of carbon for fossil fuels is converted to a dollar-per-Btu tax and applied to the prices for each fuel consumed in each sector covered by the tax, based on the CO\ :sub:`2` emission factor for that fuel and sector.
The tax can be input in either nominal or real dollars, and a different tax may be set for each projection year.
Fossil fuel prices are adjusted to include the tax. Variables represent the unadjusted prices that are filled by the supply modules.
The size of the adjustment or tax that the EPM fills yields the adjusted prices. These adjusted prices are used by the demand and conversion sector modules to simulate the effect that the tax has on CO\ :sub:`2` emissions levels.
Projected revenue from the tax is passed to the Macroeconomic Activity Module, where allocation of such revenue (for example, a deficit-neutral return to consumers) depends on a user-specified option setting.
Generally, large changes in government revenue would require additional offline analysis to assess macroeconomic feedbacks.

Permit auctions
^^^^^^^^^^^^^^^

An auction to distribute emissions permits is simulated. The total number of permits sold corresponds to the total CO\ :sub:`2` emission goal that is set by the user.
A different goal may be set for each projection year. Essentially, this option determines the permit fee necessary to achieve the CO\ :sub:`2` goal by clearing the auction market.
The permit fee is treated as a CO\ :sub:`2` emissions tax and used as an adjustment to the fossil fuel prices.
A new auction price is set at the end of each NEMS iteration (where one iteration in the solution algorithm refers to a single execution pass through all NEMS modules for a single projection year) until the emissions reach the goal.
The permit auction is assumed to operate with no initial allocation of emission permits. Similar to the CO\ :sub:`2` tax option, revenue from the auction is passed to the Macroeconomic Activity Module where its effect may require additional analysis.

Market for permits
^^^^^^^^^^^^^^^^^^

A market for tradable carbon dioxide emission permits is simulated with the assumption that an initial distribution of marketable permits to emission sources takes place.
The permits are transferable. Depending on a user-specified model option, the permits may be treated as bankable across years.
As with the CO\ :sub:`2` tax and auction options, the full market price of the permits is added to energy prices on a dollar-per-Btu basis.
The system of marketable permits is implemented in the same way as in the permit auction, except the calculation of revenues from permit sales.
Similar treatment is warranted because the marginal cost of a free permit is equivalent to one purchased at auction, given the opportunity cost of holding the distributed permit.

In an open, competitive permit market, the permit will tend to be priced at the marginal cost of reducing CO\ :sub:`2` emissions, regardless of the initial distribution of permits.
If permits are purchased by suppliers and passed through to the fuel price, the marginal cost of the CO\ :sub:`2` emissions by a particular sector in a region will be reflected in the individual end-use fuel cost for that sector.
The evaluation of the initial distribution of permits depends on the sector.

For those sectors in which the product prices are based on marginal cost, as in the Liquid Fuels Market Module, the value of the initial distribution of permits may be ignored; it does not affect the price of products.
However, in the regulated electricity sector, where the average cost is used to determine price, the revenue attributed to the free use or sales of the initially distributed permits would possibly be passed through to the consumers.
The value of the initial distribution of permits is calculated, but it is not used for electricity pricing. Instead, the full cost of the permits, as though there were no initial distribution, is reflected in the projected electricity price.

As with the auction, a new permit fee is set at the end of each NEMS iteration (where one iteration in the solution algorithm refers to a single execution pass through all NEMS modules for a single projection year).
The fee is adjusted up or down in response to the total CO\ :sub:`2` emissions obtained. The price of an allowance is adjusted until the total carbon dioxide produced is within a tolerance of the goal for that year.

Market for permits with emission offsets
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The offset option allows the goal on tradable emissions permits to increase through a user-specified supply of offsets, expressed as marginal abatement cost tables for other (non-CO\ :sub:`2`) gases.
This option can be used to analyze a greenhouse gas emission reduction policy that credits reductions in emissions from non-covered sources, reforestation, or purchases of emission reductions credits from abroad. Purchases of offsets, in millions of metric tons available at the given allowance price, are added to the CO\ :sub:`2` goal.
Although some test values for offsets are available, any formal use of this option would require additional research to arrive at appropriate assumptions.
The specification of offset supply curves, or marginal abatement cost tables, along with exogenous projections of greenhouse gases other than energy-related CO\ :sub:`2`, are made through the ghgoffx.xlsx input file.

Early-compliance emissions allowance banking with smooth carbon fee growth
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A cap and trade with banking is implemented by finding the starting carbon price, and then escalating the price at a fixed rate that clears the bank over the compliance period.
The bank is determined as the sum of the emissions goal minus projected emissions over the relevant period.
The starting price is guessed based on results of prior NEMS cycles. If no data from prior NEMS cycles are available to construct price estimates, then estimates are created from scratch The projected prices are set in the start year based on the guess, then the case is run as a carbon fee case.

The option to allow banking of emissions permits can also be combined with either the permit auction or permit market by adjusting the 'bank_flag' variable in the EPM control file (epmcntl.toml).
The details of the banking policy are controlled by additional parameters beginning with the "bank" prefix in the control file (e.g., 'bank_startyr' and 'bank_endyr').


