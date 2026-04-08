
How to Run CCATS
================

CCATS can be run either standalone in a python IDE, or within the NEMS integrated model framework. The CCATS module is written in Python and Pyomo. Documentation for the source code of the CCATS module can be found found in the :ref:`Model API Reference Section <api-ccats>`.


Running CCATS Standalone
------------------------

When CCATS is run standalone, CO₂ supply and demand volumes are exogenous.

To run CCATS standalone:

    1. Open the CCATS model directory in a Python IDE (Pycharm, Microsoft Visual Studio, Spyder, etc.)

    2. Create and assign a python interpreter including all the Python libraries listed in :ref:`CCATS Dependencies`.

    3. Select CCATS run options from the *setup* files stored in the CCATS *input* directories (i.e. model solver, debug outputs, etc.)

    4. Run CCATS from **ccats.py**.

    5. Review results in the CCATS *debug* directories.


Running CCATS in NEMS
---------------------

    1. Select CCATS run options from the *setup* files stored in the CCATS *input* directories (i.e. model solver, debug outputs, etc.)

    2. Select NEMS run options from the *scedes* files store in the NEMS *scedes* directory

    3. Run the *Run_NEMS.bat* file and select a run repository and scedes file.

    4. Review results in NEMS report writer and CCATS *debug* directories.

           
CCATS Dependencies
---------------------

CCATS relies on the below list of Python libraries to run.

* Libraries included with the default distribution or available via pip or conda:

    * sys
    * os
    * io
    * shutil
    * pathlib
    * logging
    * argparse
    * shutil
    * pylint
    * tabulate
    * pylab
    * itertools
    * warnings
    * pickle
    * numpy
    * pandas
    * pyomo
    * matplotlib
    * folium (for mapping)
    * xpress (if using the FICO Xpress solver)
    
* NEMS specific libraries:
    * pyfiler1 - maintined by the NEMS Integration Team.


.. _section-inputs-methods:

Inputs and Methods
---------------------

Inputs to CCATS are contained in .CSV files. These inputs determine high level assumptions and how the primary CCATS modules and submodules operate. They also include switches for various outputs or features of the model.


Module inputs
~~~~~~~~~~~~~

:numref:`Table %s <label-table-setup>` includes the inputs that are used by :class:`~module.Module`.

.. _label-table-setup:

.. csv-table:: General inputs
    :file: ../../../../models/ccats/input/setup.csv
    :name: table-setup
    :header-rows: 1


Financial inputs
~~~~~~~~~~~~~~~~

:numref:`Table %s <label-table-fin-setup>` shows the inputs that are used by :class:`~ccats_financial.CCATS_Finance`.

.. _label-table-fin-setup:

.. csv-table:: Financial assumptions
    :file: ../../../../models/ccats/input/fin_setup.csv
    :name: table-fin-setup
    :header-rows: 1


Preprocessor inputs
~~~~~~~~~~~~~~~~~~~

:numref:`Table %s <label-table-preproc-setup>` shows the inputs that are used by :class:`~preprocessor.Preprocessor`.

.. _label-table-preproc-setup:

.. csv-table:: Preprocessor inputs
    :file: ../../../../models/ccats/input/preproc_setup.csv
    :name: table-preproc-setup
    :header-rows: 1


Optimization model
~~~~~~~~~~~~~~~~~~

:numref:`Table %s <label-table-opt-setup>` includes the inputs that are used by :class:`~opmodels.ccats_optimization.OptimizationModel`.

.. _label-table-opt-setup:

.. csv-table:: Optimization model inputs
    :file: ../../../../models/ccats/input/opt_setup.csv
    :name: table-opt-setup
    :header-rows: 1


Postprocessor inputs
~~~~~~~~~~~~~~~~~~~~

:numref:`Table %s <label-table-postproc-setup>` shows the inputs that are used by :class:`~postprocessor.Postprocessor`.

.. _label-table-postproc-setup:

.. csv-table:: Postprocessor inputs
   :file: ../../../../models/ccats/input/postproc_setup.csv
   :name: table-postproc-setup
   :header-rows: 1

.


