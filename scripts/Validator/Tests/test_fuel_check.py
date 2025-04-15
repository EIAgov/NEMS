# -*- coding: utf-8 -*-
"""
Created on Jan 10 2025

@author: Claire Su
"""
try:
    from Controller.StatusHelper import StatusHelper
except ImportError:
    from Controller import StatusHelper

import npz_load

def test_key_fuels_no_negative_quads_and_prices(record_property):
    """Monitor if no negative quads for certain key fuels and potentially no negative prices.

    Parameters
    ----------
    record_property : method
        pytest built-in method        
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_key_fuels_no_negative_quads_and_prices'))

    error_list = npz_load.main()
    record_property("csv_header", "NEGATIVE VALUE VARIABLES")
    errors = False
    for i in error_list:
        record_property("ERROR",i)
        errors = True        
    assert not errors