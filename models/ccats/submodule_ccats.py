"""Generic submodule class of CCATS.

    * Used to declare variables at the child-level that are universally used across CCATS submodules.
    * i.e. self.input_path  = self.parent.input_path

Example
-------
import submodule as sub

Submodule: Code
_______________
"""

import pandas as pd
from ccats_common import common as com


class Submodule:
    """Generic submodule for CCATS.

    Parameters
    ----------
    parent : module.Module
        Pointer to head module
    submodule_name : str
        Name of submodule (e.g. 'Offshore')
    """

    def __init__(self, parent, submodule_name):
        self.parent         = parent                # Module.Module head module
        self.input_path     = ''                    # Path to directory containing general ctus input files
        self.output_path    = ''                    # Path to directory containing output files
        self.setup_table    = pd.DataFrame()        # Setup table
        self.name           = submodule_name        # Str name of i

        #Declare Restart File
        self.restart        = parent.restart  # Restart submodule
        self.preproc        = parent.preproc  # Preprocessor submodule
        self.ccats_fin      = parent.ccats_fin # Financial submodule
        self.year_current   = 0         # current year


    def setup(self, setup_filename):
        """Setup General Submodule for CCATS

        Parameters
        ----------
        setup_filename : str
            Path to submodule setup file.

        Examples
        --------
        def setup(self):
            super().setup(setup_filename)

        Returns
        -------
        None
        """
        self.input_path  = self.parent.input_path
        self.output_path = self.parent.output_path
        self.setup_table = com.read_dataframe(self.input_path + setup_filename, index_col=0)
        self.logger = self.parent.logger
        self.year_final = self.parent.year_final


    def run(self):
        """Run General Submodule for CCATS.

        Examples
        --------
        def run(self):
            logger.info('Running Preprocessor')
            super().run()

        Returns
        -------
        None
        """
        self.year_current = self.parent.year_current
