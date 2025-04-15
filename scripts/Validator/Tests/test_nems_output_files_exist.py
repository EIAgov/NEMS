from DataModel.Model import Model
import os
import fnmatch
import pytest

from Controller.StatusHelper import StatusHelper

def check_year_exists(year, files):
    for file in files:
        if fnmatch.fnmatch(file, f"*{year}*"):
            return True

    return False


def test_gas_model_aimms_files_exist(record_property):
    """
    P1/ngas/fromAIMMS folder has files for every year 2016-2050
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_gas_model_aimms_files_exist'))
    
    START_YEAR = 2023
    END_YEAR = 2050

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    try:
        ngas_files = Model.getInstance().files.p1.ngas.files
    except AttributeError:
        record_property("csv_header", "Error")
        record_property("ERROR", "p1/ngas folder does not exist.")
        assert False

    record_property("csv_header", "Error Message")
    errors = False

    for year in range(START_YEAR, END_YEAR+1):
        if not check_year_exists(year, ngas_files):
            record_property("ERROR", f"Missing ngas/fromAIMMS file for year {year}.")
            errors = True

    assert not errors


def test_coal_model_aimms_files_exist(record_property):
    """
    P2/coal/fromAIMMS folder has files for every year 2021-2050
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_coal_model_aimms_files_exist'))
    
    START_YEAR = 2022
    END_YEAR = 2050

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    try:
        coal_files = Model.getInstance().files.p2.coal.files
        print(coal_files)
    except AttributeError:
        record_property("csv_header", "Error")
        record_property("ERROR", "p2/coal folder does not exist.")
        assert False

    record_property("csv_header", "Error Message")
    errors = False

    for year in range(START_YEAR, END_YEAR+1):
        if not check_year_exists(year, coal_files):
            record_property("ERROR", f"Missing coal/fromAIMMS file for year {year}.")
            errors = True

    assert not errors

def test_restore_model_aimms_files_exist(record_property):
    """
    P2/rest/fromAIMMS folder has files for every year 2024-2050
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_restore_model_aimms_files_exist'))
    
    START_YEAR = 2024
    END_YEAR = 2050

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    try:
        rest_files = Model.getInstance().files.p2.rest.files
    except AttributeError:
        record_property("csv_header", "Error")
        record_property("ERROR", "p2/rest folder does not exist.")
        assert False

    record_property("csv_header", "Error Message")
    errors = False

    for year in range(START_YEAR, END_YEAR+1):
        if not check_year_exists(year, rest_files):
            record_property("ERROR", f"Missing rest/fromAIMMS file for year {year}.")
            errors = True

    assert not errors


def test_hmm_model_aimms_files_exist(record_property):
    """
    P2/hmm/fromAIMMS folder has files for every year 2023-2050
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_hmm_model_aimms_files_exist'))
    
    START_YEAR = 2023
    END_YEAR = 2050

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    try:
        hmm_files = Model.getInstance().files.p2.hmm.files
    except AttributeError:
        record_property("csv_header", "Error")
        record_property("ERROR", "p2/hmm folder does not exist.")
        assert False

    record_property("csv_header", "Error Message")
    errors = False

    for year in range(START_YEAR, END_YEAR+1):
        if not check_year_exists(year, hmm_files):
            record_property("ERROR", f"Missing hmm/fromAIMMS file for year {year}.")
            errors = True

    assert not errors

def test_lfmm_gams_model_gdx_files_exist(record_property):
    """
    P1 folder has lfmm_pYEAR.gdx files for all years 2010-2050.
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_lfmm_gams_model_gdx_files_exist'))
    
    START_YEAR = 2018
    END_YEAR = 2050

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    try:
        lfmm_files = Model.getInstance().files.p1.lfmm
    except AttributeError:
        record_property("csv_header", "Error")
        record_property("ERROR", "p1 folder does not exist.")
        assert False

    record_property("csv_header", "Error Message")
    errors = False

    for year in range(START_YEAR, END_YEAR+1):
        if not check_year_exists(year, lfmm_files):
            record_property("ERROR", f"Missing lfmm_p{year}.gdx.gz file from p1 folder.")
            errors = True

    assert not errors
