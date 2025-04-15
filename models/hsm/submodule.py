"""Generic submodule class of HSM

Example
-------
import submodule as sub

"""
import pandas as pd
import common as com
import names as nam


class Submodule:
    """
    Generic submodule for HSM.

    Parameters
    ----------
    parent : module_unf.Module
        Pointer to head module
    submodule_name : str
        Name of submodule (e.g. 'Offshore')
    """

    def __init__(self, parent, submodule_name):

        self.parent         = parent                # module.Module head module
        self.input_path     = ''                    # path to directory containing general hsm input files
        self.onshore_input_path  = ''               # path to directory containing onshore input files
        self.offshore_input_path = ''               # path to directory containing offshore input files
        self.alaska_input_path = ''                 # path to directory containing alaska input files
        self.canada_input_path = ''                 # path to directory containing canada input files
        self.output_path    = ''                    # path to directory containing output files
        self.setup_table    = pd.DataFrame()        # setup table
        self.name           = submodule_name        # str name of i

        #Declare Restart File
        self.restart = parent.restart  # restart submodule UNF
        self.rest_curcalyr  = 0  # (NCNTRL, CURCALYR)


    def setup(self, setup_filename):
        """
        Setup General Submodule for HSM

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
        self.logger                 = self.parent.logger
        self.rest_curcalyr          = int(self.parent.current_year)
        self.input_path             = self.parent.input_path
        self.output_path            = self.parent.output_path
        self.onshore_input_path     = self.parent.onshore_input_path
        self.offshore_input_path    = self.parent.offshore_input_path
        self.alaska_input_path      = self.parent.alaska_input_path
        self.canada_input_path      = self.parent.canada_input_path
        self.ngp_input_path         = self.parent.ngp_input_path
        self.setup_table            = com.read_dataframe(self.input_path + setup_filename, index_col=0)


    def run(self):
        """
        Run General Submodule for HSM

        Examples
        --------
        def run(self):
            com.print_out('running offshore submodule')
            super().run()

        Returns
        -------
        None
        """
        self.rest_curcalyr = self.parent.current_year


    def _load_dataframe(self, input_path, filename, index_col=None, skiprows=1):
        """
        Load dataframe (for input tables), accounts for input file directory.

        Parameters
        ----------
        filename : str
            Name of input file.
        index_col : str or list
            Name of indexing column (see pd.read_csv)
        skiprows : int
            Number of rows to skip before header (see pd.read_csv)

        Returns
        -------
        pd.DataFrame
        """
        temp_filename = input_path + self.setup_table.at[filename, nam.filename]

        return com.read_dataframe(temp_filename, skiprows=skiprows, index_col=index_col).copy()
