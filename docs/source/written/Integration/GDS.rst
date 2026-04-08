Global Data Structure
========================

The Global Data Structure defines the subset of NEMS variables used for
communication between modules and for external reporting such as the
Annual Energy Outlook Tables. The variables consist of variables shared
among modules, such as prices, consumption, and macroeconomic
information. The variables also include reporting variables, as well as
model control parameters and assumptions.

The variables in the Global Data Structure are defined and organized in
*blocks* that designate groups of variables.

The specific elements of the block structure are defined in the
*include* files that contain declarations for variables. In addition, a
data dictionary for the Global Data Structure includes definitions for
each variable.

.. table:: Table 1. Key Blocks in the NEMS global data structure

   +----------------+------------+----------------------------------------+
   | Modules        | Common     | Description                            |
   | filling the    | block      |                                        |
   | common block   | names      |                                        |
   +================+============+========================================+
   | Integrating,   | QBLK       | End-use sector quantities              |
   | multiple       |            |                                        |
   | contributors,  | QMORE      | Additional end-use sector quantities   |
   | or exogenous   |            |                                        |
   |                | MPBLK      | End-use sector prices)                 |
   |                |            |                                        |
   |                | PMORE      | Additional end-use sector prices       |
   |                |            |                                        |
   |                | MXQBLK     | Expected quantities for foresight      |
   |                |            |                                        |
   |                | MXPBLK     | Expected prices for foresight          |
   |                |            |                                        |
   |                | QSBLK      | State Energy Data System historical    |
   |                |            | data corresponding to QBLK             |
   |                | NCNTRL     |                                        |
   |                |            | Control variables                      |
   |                | COGEN      |                                        |
   |                |            | Combined heat and power                |
   |                | CONVFACT   |                                        |
   |                |            | Thermal conversion factors             |
   |                | CONVERGE   |                                        |
   |                |            | Convergence variable data and          |
   |                | COALEMM    | reporting summary                      |
   |                |            |                                        |
   |                | HMMBLK     | Variables exchanged between the Coal   |
   |                |            | Market Module and the Electricity      |
   |                | CYCLEINFO  | Market Module                          |
   |                |            |                                        |
   |                | CONTINEW   | Hydrogen module variables (future use) |
   |                |            |                                        |
   |                | NCHAR      | Current cycle number and total cycles  |
   |                |            | in overall run                         |
   |                |            |                                        |
   |                |            | Information related to continuation of |
   |                |            | cycling                                |
   |                |            |                                        |
   |                |            | Character variables such as scenario   |
   |                |            | name or module names                   |
   +----------------+------------+----------------------------------------+
   | Emissions      | EMABLK     | Price adjustments for carbon dioxide   |
   |                |            | fees, if any                           |
   |                | EMEBLK     |                                        |
   |                |            | Carbon dioxide emissions factors by    |
   |                | EPMBANK    | fuel/sector                            |
   |                |            |                                        |
   |                | REGCO2     | Parameters for an emissions constraint |
   |                |            | banking option                         |
   |                | GHGREP     |                                        |
   |                |            | Regional carbon dioxide emissions by   |
   |                | EMISSION   | fuel and sector                        |
   |                |            |                                        |
   |                | AMPBLK,    | Greenhouse gas abatement costs and     |
   |                | ANGTDM,    | offsets                                |
   |                | ACOALPRC,  |                                        |
   |                | APMORE,    | Emissions and related results          |
   |                | AEUSPRC,   |                                        |
   |                |            | Copies of MPBLK, NGTDMOUT, COALPRC,    |
   |                | APONROAD   | PMORE, EUSPRC, and PONROAD with prices |
   |                |            | adjusted by any energy tax or emission |
   |                | AB32       | allowance fees                         |
   |                |            |                                        |
   |                | RGGI       | California Assembly Bill 32 cap and    |
   |                |            | trade variables                        |
   |                | CSAPR      |                                        |
   |                |            | Regional Greenhouse Gas Initiative     |
   |                |            | variables                              |
   |                |            |                                        |
   |                |            | Cross-State Air Pollution Rule         |
   |                |            | variables                              |
   +----------------+------------+----------------------------------------+
   |                | EMOBLK     | Emissions                              |
   +----------------+------------+----------------------------------------+
   |                | CALSHR     | California shares for estimating AB32  |
   |                |            | covered emissions                      |
   +----------------+------------+----------------------------------------+
   |                | INDEPM     | Cement-related CO2 process emissions   |
   |                |            | passed from IDM to EPM                 |
   +----------------+------------+----------------------------------------+
   | Macroeconomic  | MACOUT     | Output variables                       |
   |                |            |                                        |
   |                | MCDETAIL   | Reporting variables                    |
   +----------------+------------+----------------------------------------+
   | International  | INTOUT     | All International Energy Module global |
   | Energy         |            | variables                              |
   +----------------+------------+----------------------------------------+
   | Residential    | RESDREP    | Reporting variables                    |
   | Demand         |            |                                        |
   |                | RSCON      | Energy consumption by end use          |
   |                |            |                                        |
   |                | RSEFF      | Energy efficiency by end use           |
   +----------------+------------+----------------------------------------+

**Table 1. Common Blocks in the NEMS global data structure (continued)**

+----------------+------------+----------------------------------------+
| Modules        | Common     | Description                            |
| filling the    | block      |                                        |
| common block   | names      |                                        |
+================+============+========================================+
| Commercial     | COMPARM    | Control parameters, assumptions        |
| Demand         |            |                                        |
|                | COMMREP    | Reporting variables                    |
|                |            |                                        |
|                | BLDGLRN    | Cumulative shipments of distributed    |
|                |            | generation technologies for *learning* |
|                |            | curves                                 |
+----------------+------------+----------------------------------------+
| Industrial     | INDOUT     | Industrial variables for use in other  |
| Demand         |            | modules                                |
|                | INDREP     |                                        |
|                |            | Industry-level consumption reporting   |
|                | INDREP2    | variables                              |
|                |            |                                        |
|                | BIFURC     | Industry-level combined-heat-and-power |
|                |            | reporting variables                    |
|                |            |                                        |
|                |            | Energy by fuel/region classified by    |
|                |            | covered and uncovered industry groups  |
|                |            | for emission cap and trade analysis    |
+----------------+------------+----------------------------------------+
| Transportation | TRANREP    | All global transportation variables    |
| Demand         |            |                                        |
+----------------+------------+----------------------------------------+
| Electricity    | UEFPOUT    | Electricity pricing outputs            |
| Market         |            |                                        |
|                | EFPOUT     | Electricity pricing outputs            |
|                |            |                                        |
|                | UEFDOUT    | Fuel-dispatch outputs                  |
|                |            |                                        |
|                | UDATOUT    | Electricity central data outputs       |
|                |            |                                        |
|                | UECPOUT    | Capacity planning outputs              |
|                |            |                                        |
|                | DSMTFEFP   | Demand side management/electricity     |
|                |            | pricing                                |
|                | UETTOUT    |                                        |
|                |            | Electricity trade outputs              |
|                | EUSPRC     |                                        |
|                |            | Electricity prices for end uses by     |
|                | CAPEXP     | sector                                 |
|                |            |                                        |
|                | TCS45Q     | Capital expenditures                   |
|                |            |                                        |
|                | ULDSMOUT   | Variables for modeling U.S. tax code   |
|                |            | section 45Q credits                    |
|                | E111D      |                                        |
|                |            | DSM variables                          |
|                |            |                                        |
|                |            | EMM/CMM interface                      |
+----------------+------------+----------------------------------------+
| Carbon Capture | CCATSDAT   | Carbon capture, transport and          |
|                |            | sequestration variables.               |
+----------------+------------+----------------------------------------+
| Renewable      | WRENEW     | All Renewable Fuel Module global       |
| Fuels          |            | variables                              |
+----------------+------------+----------------------------------------+
| Hydrocarbon    | OGSMOUT    | All Hydrocarbon Supply Module global   |
| Supply         |            | variables                              |
+----------------+------------+----------------------------------------+
| Natural Gas    | NGTDMOUT   | Output variables                       |
| Market         |            |                                        |
|                | NGTDMREP   | Reporting variables                    |
|                |            |                                        |
|                | NGRPT      | Supplementary reporting variables      |
+----------------+------------+----------------------------------------+
| Liquid Fuels   | PMMOUT     | Output variables                       |
| Market         |            |                                        |
|                | PMMRPT     | Output variables                       |
|                |            |                                        |
|                | PMMFTAB    | Reporting variables                    |
|                |            |                                        |
|                | QONROAD    | On-road distillate quantity,           |
|                |            | conversion factor                      |
|                | PONROAD    |                                        |
|                |            | On-road distillate price               |
|                | LFMMOUT    |                                        |
|                |            | Output variables                       |
+----------------+------------+----------------------------------------+
| Coal Market    | COALOUT    | Output variables                       |
|                |            |                                        |
|                | COALREP    | Reporting variables                    |
|                |            |                                        |
|                | COALPRC    | Electric power sector coal prices at   |
|                |            | the coal demand region level           |
|                | USO2GRP    |                                        |
|                |            | Coal output by emission categories for |
|                |            | Electricity Capacity Planning          |
|                |            | interface                              |
+----------------+------------+----------------------------------------+

PyFiler
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NEMS2023 introduced PyFiler, which allows GDS variables to be shared
between Python and Fortran programs in memory using `NumPy’s F2py
library <https://numpy.org/doc/stable/f2py/>`__. F2PY facilitates
creating/building native Python `C/API extension
modules <https://docs.python.org/3/extending/extending.html#extending-python-with-c-or-c>`__
that make it possible to call Fortran from Python. This interface
enables the fast, seamless transfer of data between the Python
integration code and the legacy Fortran module code in NEMS. We
significantly expanded PyFiler in NEMS2025, so it now serves as an
access point for the NEMS GDS. In order to work with PyFiler, the NEMS
Fortran code is now compiled as a library for Python rather than as a
standalone executable.

PyFiler is used to support most reads and writes out of NEMS.

Energy market data representation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Energy Market Data define the energy quantity and price variables
for NEMS. These variables are the principal values subject to
convergence testing in the integrating algorithm. The Energy Market Data
are part of the NEMS Global Data Structure and are stored in the
following blocks:

- QBLK Energy consumption quantities by fuel and sector

- MPBLK Energy prices by fuel and sector, excluding any CO\ :sub:`2`
  fees in effect

- AMPBLK Energy prices by fuel and sector, including any carbon dioxide
  fees in effect

- MXQBLK Expectations for energy consumption quantities

- MXPBLK Expectations for energy prices

The quantity and price structure does not attempt to represent all
energy flows, but instead it focuses on the primary variables needed to
design the NEMS equilibrating methodology. In addition, the Energy
Market Data structure defines the fuel and sectoral energy
classification for the NEMS energy balance .

In general, the energy prices match the corresponding consumption
quantities . The exceptions include:

- Detailed refinery sector prices are omitted even though refinery fuel
  quantities are included because the projections don’t require refinery
  sector prices to be separate from the rest of the industrial sector.
  The industrial fuel prices are the delivered prices to industrial fuel
  consumers, including refineries. As a result, the industrial sector
  prices match the coverage of the corresponding industrial consumption
  quantities.

- Prices for some industrial petroleum categories are combined in the
  industrial *Other petroleum* category to eliminate unnecessary detail.
  That is, the industrial *Other petroleum* price is defined as the
  average price of three consumption categories: still gas, petroleum
  coke, and other petroleum. The *Other petroleum* price is not needed
  by any NEMS module but is required for reporting purposes to determine
  the average price of all petroleum products.

Delivered prices for renewable energy categories are left undefined
because there are no meaningful market prices for them. For example, no
delivered prices are associated with hydroelectric, geothermal, wind,
solar thermal, and photovoltaic energy sources. In the case of biomass,
supply curves for four different feedstocks (forestry residues, urban
wood waste and mill residues, agricultural residues, and energy crops)
are generated for the Liquid Fuels Market Module and the Electricity
Market Module, and a composite average price is calculated.

NEMS uses variable names for consumption quantities and prices, along
with a two-character product code mnemonic for each product. Each array
is a two-dimensional, floating-point array. The first dimension
represents the nine census divisions as well as a tenth position that is
blank and an eleventh position reserved for the national total. The
second dimension represents 61 years from 1990 to 2050. Quantities are
stored in trillions of British thermal units (Btu). Prices are stored in
1987 dollars per million Btu, as deflated by the chain-weighted price
deflator for gross domestic product.

A related part of the Energy Market Data structure is made up of the
variables that hold energy market expectations. The Integrating Module
maintains a separate set of arrays to store consumption and price
expectations. The expectations arrays are updated according to the
foresight options under consideration. The expectations arrays are
defined like the standard energy market arrays, each with an additional
leading character, *X*. Not all fuel price and demand quantity detail is
represented in the expectation arrays.

Restart file
~~~~~~~~~~~~

At the beginning of a run, the Integrating Module reads initial values
for all data in the Global Data Structure from a user-specifiable
version of a special file, called the Restart file. The Restart file
contains a starting point for the case under consideration, consisting
of results from a previous simulation. During the run, much of these
data are updated and changed. For example, alternative values for key
module parameters and input assumptions, read separately from the user
interface file or other sources, override the values stored in the
Restart file. At the end of the run, a new Restart file is created with
all the data from the run. The file is available for future runs, as
well as to link with reporting and database management routines.

The restart file promotes modularity by supplying values for all shared
variables, regardless of whether the module that creates them is active
in the run. Prices, quantities demanded or supplied, and other variables
normally generated by a module that is switched off for the current run
are provided instead by the Restart file.

NEMS2023 is in the midst of the transition between the legacy
unformatted (.unf) data file and the
`npz <https://numpy.org/doc/stable/reference/generated/numpy.savez.html>`__
data file that will be used in future NEMS versions.

The global data are separated into groups of variables known as
*blocks*. The NEMS modules may access data from, and write results to,
the block variables once the data are loaded into memory.
