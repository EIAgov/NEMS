# ​The National Energy Modeling System

## What is it?

The National Energy Modeling System (NEMS) is a long-term energy-economy modeling system of U.S. energy markets. The model is used to project production, imports, exports, conversion, consumption, and prices of many energy products, subject to user-defined assumptions. The assumptions encompass macroeconomic and financial factors, world energy markets, resource availability and costs, behavioral and technological choice criteria, technology characteristics, and demographics.

NEMS produces a general equilibrium solution for energy supply and demand in the U.S. energy markets on an annual basis.

EIA's Office of Energy Analysis develops and maintains NEMS to support the _Annual Energy Outlook_ (AEO). EIA analysts perform policy analyses requested by decisionmakers in the White House; the U.S. Congress; offices within the U.S. Department of Energy, including program offices; and other government agencies. Users outside of EIA use NEMS for a variety of purposes.

The [AEO page](https://www.eia.gov/outlooks/aeo/) contains results from many EIA analysis products that rely on NEMS.

The [NEMS documentation page](https://www.eia.gov/outlooks/aeo/nems/documentation/) has more details about each module. The [Assumptions page](https://www.eia.gov/outlooks/aeo/assumptions/) contains separate reports about the latest assumptions for each module. The [Case Descriptions page](https://www.eia.gov/outlooks/aeo/assumptions/case_descriptions.php) explains the cases preconfigured in the repository.

We also publish the [World Energy Projection System](https://www.eia.gov/outlooks/ieo/ieoweps_sourcecode.php) (WEPS), which we use for global analysis and to support the _International Energy Outlook_.

## Main features

NEMS contains three main parts:

- A central database to store and pass inputs and outputs between the various components
- Energy modules that represent various demand, transformation, and supply projection activities
- An integration, or convergence, module that determines when the system has reached an equilibrium between supply and demand

NEMS is a modular system. The modules represent each of the fuel supply markets, conversion sectors, and end-use consumption sectors of the energy system. The modular design also allows us to use the methodology and level of detail most appropriate for each energy sector. NEMS executes each of the component modules to solve for the prices of energy delivered to end users and the quantities consumed by product, region, and sector. The delivered fuel prices encompass all activities required to produce, import, and transport fuels to end users. The information flows also include economic activity, domestic production, and international petroleum supply.

NEMS consists of the following modules:

Four supply modules:

- Renewable Fuels Module (RFM)
- Natural Gas Market Module (NGMM)
- Hydrocarbon Supply Module (HSM)
- Coal Market Module (CMM)

Four demand modules:

- Residential Demand Module (RDM)
- Commercial Demand Module (CDM)
- Industrial Demand Module (IDM)
- Transportation Demand Module (TDM)

Three conversion modules:

- Electricity Market Module (EMM)
- Liquid Fuels Market Module (LFMM)
- Hydrogen Market Module (HMM)

Five other modules:

- Macroeconomic Activity Module (MAM)
- International Energy Module (IEM)
- Emissions Policy Module (EPM)
- Integrating Module
- Carbon Capture, Allocation, Transportation, and Sequestration (CCATS) Module

NEMS calls each supply, conversion, and end-use demand module in sequence until the modeling system reaches an equilibrium between supply and demand. The AEO offers a solution for each year through 2050.

This latest version of NEMS was used in AEO2025.

## Where to get it

NEMS development is a year-round process led by EIA. A snapshot of the source code corresponding to the regularly scheduled annual release of the AEO is hosted on GitHub at: [https://github.com/EIAgov/NEMS](https://github.com/EIAgov/NEMS).

## Dependencies

NEMS is a computationally intensive 64-bit application, which runs on 64-bit Windows. Our servers that run NEMS have large amounts of RAM to accommodate multiple runs and users simultaneously, but a single copy of NEMS may execute on a single-user system with smaller amounts of RAM.

We run the integrated NEMS run in two parts simultaneously (in parallel) to speed run time, so we use two processor cores per integrated run. If a (non-integrated) run is done with modules off (for example, turning off coal, electricity, and renewables), then we need only one processor core per run. With a quad core processor, you can run two integrated parallel runs or four non-integrated runs with little speed loss if sufficient memory is available. You can run NEMS as one process, but it requires about two hours more per cycle.

One run of all components of the NEMS model, or a cycle, takes about four hours on our servers, but most cases are solved by making a series of cycles. A four-cycle NEMS run takes 20+ hours. Runs of some individual parts of NEMS, such as the energy demand models, if run separately, can take only a few minutes. All files of a completed run, input and output, take up about 30 GB of storage, but they compress to about 15 GB once the run finishes.

#### NEMS supporting software

- Intel® Fortran Compiler Classic and Intel® Fortran Compiler

NEMS was compiled using a free download of the 23.2.0 release for Intel Fortran Compiler Classic and Intel Fortran Compiler for Windows (2023.2.1). You can find more information on the [Intel](https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran) website.

- Meson Build System

NEMS Fortran functions were compiled using Meson Build. You can find more information on the [Meson](https://mesonbuild.com/Quick-guide.html) website.

- Fair-Isaac Corporation's (FICO) Xpress optimizer (license required)

We use the Xpress optimizer to solve the LFMM LP and the CCATS LP. To purchase an Xpress license, please refer to [FICO's webpage](https://www.fico.com/en/fico-xpress-trial-and-licensing-options).

- A Fortran-to-SQLite library to use with EMM that holds regional electricity data. SQLite is not required for running NEMS with the default data provided.


- GAMS. It is used for the LFMM. We used GAMS version 43.2 for this release. Our GAMS implementation uses the Xpress solver mentioned above. We purchased the development license (rather than the run-time license), which allows changes to the linear program matrix during execution of a NEMS run.


- Advanced Interactive Multidimensional Modeling Software (AIMMS). AIMMS is used for the CMM; the NGMM; the HMM; and the Electricity Capacity Planning (ECP), the Electricity Fuel Dispatch (EFD), and the Renewable Electricity Storage (REStore) Submodules within the EMM. We used AIMMS version 4 (release 4.96.4) for this release licensed with the CPLEX solver.


- (Optional) The [S&P Global Macroeconomic Model](https://www.spglobal.com/en/) as implemented with the EViews13 software package. We currently use the EViews13 standard edition. The S&P Global macro module is executed as a subprocess to NEMS if the macro feedback switch in NEMS is turned on. You can elect to run NEMS without macro feedback, in which case the run uses static macroeconomic inputs taken as is. When the macro feedback switch is on, NEMS will attempt to call the macro model by executing EViews using the S&P Global model's work files as the input to EViews. These EViews work files are not included in the EIA archive, but you can obtain them from EIA after licensing the S&P Global model. You must also get a copy of EViews to run the S&P Global model. Contact S&P Global for more information.


- Cygwin for certain NEMS utility programs.


- Python 3.11. You can find the required packages and versions in the requirements.txt

## Installation

- From the NEMS GitHub repository, clone or download NEMS.


- Extract the zipped files under the output/aeo2025 directory before using NEMS


- For more information about this repository, please refer to NEMS_Public_Release.pdf

## Licenses 

The NEMS code, as distributed here, is governed by [specific licenses](https://github.com/eiagov).

## Getting help

You can direct questions about the NEMS model to <EIA-NEMS-GITHUB@eia.gov>.

## Discussion and development

As part of developing each AEO, EIA schedules working group meetings to gather input from various sectors of the industry. You can find more information on the [AEO Working Groups](https://www.eia.gov/outlooks/aeo/workinggroup/) webpage.

## Contributing to NEMS 

Contributions to NEMS are governed by [specific rules](https://github.com/eiagov).  You can direct questions about the NEMS model to <EIA-NEMS-GITHUB@eia.gov>.
