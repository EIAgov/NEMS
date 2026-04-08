Model Assumptions
=================

Overview
--------
The *Annual Energy Outlook 2025* (AEO2025) projects carbon dioxide (CO\ :sub:`2`) emissions by fuel and by sector for three energy-related activities:

* Fossil fuel combustion
* Nonfuel use of fossil fuels (for example, in industrial activities such as manufacturing plastics)
* Naturally occurring CO\ :sub:`2` vented during energy consumption or production (for example, geothermal or natural gas processing)

For each activity, we estimate projected CO\ :sub:`2` emissions by multiplying associated energy consumption of each fuel by a CO\ :sub:`2` emission factor.
Emissions factors reflect the amount of CO\ :sub:`2` emitted per unit of energy consumed and are expressed as millions of metric tons (MMmt) of CO\ :sub:`2` per quadrillion British thermal units (quads) of energy use.

To calculate CO\ :sub:`2` emissions factors, we start with CO\ :sub:`2` coefficients at full combustion for each fuel type.
We adjust each coefficient by multiplying it with a combustion fraction between 0.0 and 1.0, arriving at an adjusted CO\ :sub:`2` emission factor for each fossil fuel.
We assume all fuels are fully emissive when combusted (that is, a combustion fraction of 1.0).
For nonfuel uses, the combustion fraction reflects our estimates of how much carbon remains in the product instead of being released into the atmosphere.
We assume some nonfuel uses of fossil fuels capture all carbon inputs but other nonfuel uses emit some CO\ :sub:`2` during production. 
Emissions factors and combustion fractions for all fossil fuel categories are listed below.

.. in :numref:`Table %s <em_factors>`.

Fossil fuel combustion
----------------------
CO\ :sub:`2` emissions from fuel use vary based on the:

* Carbon content of the fossil fuel
* Fraction of the fuel combusted
* Amount of the fuel consumed

The chemical composition of most fossil fuels is relatively consistent over time, resulting in little to no change in their carbon factors over our AEO projections.
However, some fuel categories have greater variability. For example, coal is reported as a single fuel type, but if the underlying coal ranks that make up the coal category change, the carbon factor can change over time.

For fuel uses of energy, we assume all of the carbon is oxidized, so the combustion fraction is equal to 1.0 (in keeping with international convention).
Some products, such as petroleum coke, have both fuel and nonfuel uses, and we adjust the combustion fraction accordingly.
Lubricants are not used for their energy value, but we assume that half of the lubricants consumed are combusted (therefore, emitted) and half are not.

Nonfuel use (Fuel-dependent processes)
--------------------------------------
CO\ :sub:`2` emitted during nonfuel energy use varies widely across energy products.
For some products, such as asphalt and road oil, we assume that all CO\ :sub:`2` is captured during nonfuel uses.
As a result, the adjusted CO\ :sub:`2` emissions factor is zero.
For other fossil fuel inputs, such as those for petrochemical feedstocks, some CO\ :sub:`2` is emitted during production, and some carbon is stored in a final product (and not emitted into the atmosphere), reducing the fuel's CO\ :sub:`2` emissions relative to full combustion.

Biomass combustion
------------------
By convention, we assume biomass combustion results in net-zero CO\ :sub:`2` emissions.
Specifically, we consider any CO\ :sub:`2` emitted by biogenic energy sources, such as biomass and alcohols, to be balanced by the CO\ :sub:`2` sequestration that occurred during biomass production. 

For fuels or fuel categories containing only biogenic fuels (such as woody biomass or biogenic municipal solid waste), CO\ :sub:`2` emissions are reported as zero.
For fuels that contain both biogenic and non-biogenic components, such as ethanol blended with motor gasoline or biodiesel, biogenic components are excluded from emissions calculations.
To illustrate the potential for these emissions in the absence of any offsetting sequestration-as might occur under related land-use changes, with CO\ :sub:`2` being sequestered in terrestrial carbon sinks-we calculate and report the CO\ :sub:`2` emissions from biogenic fuel use separately.
However, these values are not included in total or sectoral emissions estimates.

Fuel-independent processes
--------------------------
Some industrial processes release CO\ :sub:`2` as a result of natural chemical processes, rather than through the fuel or nonfuel use of energy products.
One example is CO\ :sub:`2` released from limestone during cement production.
Although these emissions contribute to an overall national total, they are outside the scope of what we consider to be energy related.
As such, we calculate and report these CO\ :sub:`2` emissions separately, but we do not include these values in our total or sectoral energy-related emissions estimates.

Reporting
---------
:numref:`Figure %s <figure-epm_categories>` clarifies how we distinguish energy and non-energy CO\ :sub:`2` emissions in our published AEO tables.

.. _label-figure-epm_categories:

.. figure:: figures/epm_categories.png
   :alt: Energy and non-energy emissions categories in NEMS
   :name: figure-epm_categories

   Energy and non-energy emissions categories in NEMS

The CO\ :sub:`2` emissions reported in AEO2025 `Table 18 <https://www.eia.gov/outlooks/aeo/data/browser/#/?id=17-AEO2025&region=1-0&cases=ref2025>`_ and `Table 70 <https://www.eia.gov/outlooks/aeo/data/browser/#/?id=22-AEO2025&cases=ref2025>`_ include all energy-related emissions from fossil fuel combustion, fuel-dependent processes, and venting.
Table 18 groups these emissions by economic sector and fuel type, and Table 70 groups them by economic sector and end use.
`Table 71 <https://www.eia.gov/outlooks/aeo/data/browser/#/?id=69-AEO2025&cases=ref2025>`_ reports CO\ :sub:`2` emissions by all categories shown in :numref:`Figure %s <figure-epm_categories>`, with subtotals for energy and non-energy CO\ :sub:`2` emissions.

.. COMMENT OUT TABLE FOR NOW 
.. csv-table:: Emissions factors
    :file: tables/factors.csv
    :name: em_factors
    :header-rows: 1

    Data source: U.S. Energy Information Administration, *Annual Energy Outlook 2025*, National Energy Modeling System run:ref2025.d032025a, and Appendix tables A-20, A-32, and A-226, U.S. Environmental Protection Agency (EPA), *Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2022*

    Note: Emissions coefficients from EPA are converted from units of carbon to CO\ :sub:`2` by multiplying by a factor of (44/12).

    \ :sup:`a` For feedstocks, the combustion fraction includs fuel-dependent process emissions as well as inputs that might be combusted onsite.

    \ :sup:`b` *Other industrial petroleum* includes industrial lubricants, special naphtha (solvents), waxes, and miscellaneous products such as sulfur.

    \ :sup:`c` *Industrial other coal* is for process heat, and qualitatively differs from coal used for steel prodution (metallurgical coal).

    \ :sup:`d` The National Energy Modeling System specifies emission factors for coal used for electric power generation by coal supply region and types of coal, so the average CO\ :sub:`2` content varies throughout the projection period. The electric power value of 95.81 shown here illustrates a typical coal-fired emission factor.

    \ :sup:`e` We include biogenic sources for informational purposes, but we do not count them in total energy-related CO\ :sub:`2` emissions.

.. Add white space so that fixed sidebar does not overlap with footer

|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
|
