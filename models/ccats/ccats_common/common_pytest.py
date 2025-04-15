"""Run Pytest for CCATS

Common Pytest: Summary
______________________

CCATS utility submodule for running Pytest.


Common Pytest: Input Files
__________________________

None


Common Pytest: Model Functions and Class Methods
________________________________________________

* :meth:`~ccats_common.common_pytest.TestCollection.__init__` - Initialize variables for running Pytest.
* :meth:`~ccats_common.common_pytest.TestCollection.run` - Run Pytest.


Common Pytest: Output Debug Files
_________________________________

* ccats_common//tests_files//logs//**pytestlog_<model_year>.log** - log of Pytest results where <model_year> is the year of the current run.


Common Pytest: Output Restart Variables
_______________________________________


Common Pytest: Code
___________________
"""
import pytest
import pandas as pd
import pickle
import sys, io, os
import module


class TestCollection():
    """Class for running Pytest for CCATS.

    """
    def __init__(self,parent):
        """Initializes TestCollection object.

        Parameters
        ----------

        parent : Class
            Parent class for Pytest model testing.

        Returns
        -------
        None
        """
        self.parent = parent

        pass

    def run(self):
        """Run Pytest for CCATS.

        Parameters
        ----------
        
        None

        
        Returns
        -------

        None
        """
        # Get Data
        model_year = str(self.year_current)
        self.parent = ''
        with open('test.pkl', 'wb') as f:  # open a text file
            pickle.dump(self, f)

        # Save to file
        save_to_file = True
        if save_to_file:
            stdout_bak = sys.stdout  # backup stdout
            sys.stdout = io.StringIO()
            pytest.main()
            output = sys.stdout.getvalue()
            sys.stdout.close()
            sys.stdout = stdout_bak  # resto
            file = 'pytestlog_' + model_year + '.log'
            try:
                path = os.getcwd() +'\\ccats_common\\tests_files\\logs\\' + file
                with open(path, "w") as f:
                    f.write(output)
            except:
                path = os.getcwd() +'\\ccats\\ccats_common\\tests_files\\logs\\' + file
                with open(path, "w") as f:
                    f.write(output)
        else:
            pytest.main()
        
        pass
