# -*- coding: utf-8 -*-
"""
Created on Apr 3 2025

@author: Claire Su
"""
from DataModel.Model import Model
try:
    from Controller.StatusHelper import StatusHelper
except ImportError:
    from Controller import StatusHelper

from validate import get_lastyr_entry

import os 

scedyear = get_lastyr_entry()
start_year = 2023
end_year = scedyear
DISCREPANCY_TOLERANCE = 5
num_years = end_year - start_year + 1

def load_table():
    """Load the unif.api.csv table, locate the Refinery Capacity Ultilization Rate record, and return the dataframe values

    Returns
    -------
    dataframe
        one row data for Refinery Capacity Utilization Rate (percent)
    """
    df = Model.getInstance().files.csv.load_table_entire()
    df = df[df['DaType'] == "utilization rate"]
    year_columns = [str(i) for i in range(start_year, end_year+1)]
    df = df.loc[:, year_columns]
    df[year_columns] = df[year_columns].astype(float)
    return df

def test_refinery_util_rate_less_than_threshold(record_property):
    """The test to monitor if Refinery Capacity Utilization Rate (percent) is greater than 93.5%

    Parameters
    ----------
    record_property : method
        pytest built-in method
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_refinery_util_rate_less_than_threshold'))

    df_discrepancy = load_table()
    record_property("csv_header", "Year, Refinery Utilization Rate")
    errors = False
    for y in range(start_year, end_year+1):
        value = df_discrepancy[str(y)].values[0]
        if value > 93.5:
            record_property("ERROR",f"{str(y)},{value}")
            errors = True
    assert not errors