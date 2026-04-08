Introduction
============


**EPM** is the **E**\ missions **P**\ olicy **M**\ odule of the National Energy Modeling System (NEMS). 
The main purpose of the EPM is to handle calculations of energy-related carbon dioxide (CO\ :sub:`2`) emissions at the U.S. economic sector and regional levels.
These calculations are performed using energy consumption estimates (which vary by sector, region, and year) and applying applying appropriate sector-by-fuel emissions factors (which are established using the latest historical data and are static over the projection).
In addition to providing projections of CO\ :sub:`2` emissions, the EPM is also responsible for implementing various CO\ :sub:`2` policy evaluation options. These options can be used to simulate proposed market-based approaches to meet national CO\ :sub:`2` emission objectives.


The CO\ :sub:`2` emissions estimates and policies modeled by the EPM focus specifically on energy-related CO\ :sub:`2` emissions. 
We define energy-related CO\ :sub:`2` emissions as those resulting from fossil fuel combustion, released during non-fuel use of energy products (such as industrial feedstocks), and released during energy production (such as CO\ :sub:`2` vented from geothermal wells).
This distinction between energy- and non-energy CO\ :sub:`2` emissions categories in NEMS is discussed further in the Model Assumptions section.

Annual Model Updates
--------------------

This edition of the Emissions Policy Module (EPM)-Model Documentation 2025 reflects changes made to the EPM since the publication of the 2023 Annual Energy Outlook. These changes include:

* Updates to carbon dioxide (CO\ :sub:`2`) emissions factors
* Additional CO\ :sub:`2` emissions factors to represent new fuel usages for AEO2025
* Added representation of vented CO\ :sub:`2` emissions associated with natural gas processing
* Changes to the code base from Fortran to Python