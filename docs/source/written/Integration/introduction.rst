Introduction
===============

The National Energy Modeling System (NEMS) is a long-term energy-economy
modeling system of U.S. energy markets. The model is used to project
production, imports, exports, conversion, consumption, and prices of
many energy products, subject to user-defined assumptions. The
assumptions encompass macroeconomic and financial factors, world energy
markets, resource availability and costs, behavioral and technological
choice criteria, technology characteristics, and demographics.

NEMS produces a general equilibrium solution for energy supply and
demand in the U.S. energy markets on an annual basis.

EIA's Office of Energy Analysis develops and maintains NEMS to support
the *Annual Energy Outlook* (AEO). EIA analysts perform policy analyses
requested by decisionmakers in the White House; the U.S. Congress;
offices within the U.S. Department of Energy, including program offices;
and other government agencies. Users outside of EIA use NEMS for a
variety of purposes.

NEMS was first used for projections presented in the *Annual Energy
Outlook 1994*.

Scope and organization
----------------------

Publication of this document is supported by Public Law 93-275, Federal
Energy Administration Act of 1974, Section 57(B)(1) (as amended by
Public Law 94-385, Energy Conservation and Production Act), which
states, in part:

   *...that adequate documentation for all statistical and forecast
   reports prepared...is made available to the public at the time of
   publication of such reports.*

In particular, this report meets EIA’s model documentation standard
2015-1, established under these laws. [1]_

The individual components of NEMS are
`documented <https://www.eia.gov/outlooks/aeo/nems/documentation/index.php>`__
individually. Although the NEMS Integrating Module is a distinct
component of NEMS, the Integrating Module is not by itself a model.
Rather, it is a framework that connects the subject matter modules, and
a component of the overall NEMS model. The Integrating module implements
specific aspects of the overall modeling methodology that are not
documented elsewhere. The documentation is organized accordingly.

Readers interested in a more comprehensive summary of NEMS should see
the latest *The National Energy Modeling System: An Overview*. [2]_

Chapter 3 describes the NEMS global data structure, which is used for
inter-module communication, solution initialization and storage, and
certain database operations.

Chapter 4 provides the mathematical specification for the solution
algorithm and describes the convergence techniques we use. Chapter 4
also documents other modeling functions of the Integrating Module,
including generation of foresight assumptions and carbon dioxide
emission policy routines.

Chapter 5 discusses the NEMS job queue and run management, which are
used to manage NEMS runs in a distributed environment.

Chapter 6 discusses the NEMS Report Writer, which produces diagnostic
tools and the published output from the model.

Chapter 7 discusses the NEMS validator.

.. [1]
   See https://www.eia.gov/about/eia_standards.php#standard2015_1.

.. [2]
   See https://www.eia.gov/outlooks/aeo/nems/documentation/index.php.