from DataModel.Model import Model
import os
#import re
#import pytest
#from xml.sax.saxutils import escape

from Controller.StatusHelper import StatusHelper

#List out the keywords that will appear in a nohup error message
log_errors = [
    "ERROR"
]

# Make a regex that matches if any of our regexes match.
combined = "(" + ")|(".join(log_errors) + ")"

def check_if_lfshell_err_exists(isParnems, record_property):
    errors = False

    lfmmlog_file = os.path.join(Model.getInstance().files.path,'lfshell_err.lst')
    if isParnems:
        lfmmlog_file = os.path.join(Model.getInstance().files.path,'p1','lfshell_err.lst')

    if os.path.exists(lfmmlog_file):
        record_property("ERROR", "The lfshell_err.lst file is generated. Pls open the file and investigate the issue.")
        errors = True

    return errors

def test_lfshell_err_logfile_existence(record_property):
    """
    Tests that p1/lfshell_err.lst file (parnems) or root folder (jognems) exist or not
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_lfshell_err_logfile_existence'))

    record_property("csv_header", "Error Message")
    errors = False

    if not Model.getInstance().files.parnems:
        if check_if_lfshell_err_exists(False, record_property):
            errors = True
    else:
        if check_if_lfshell_err_exists(True, record_property):
            errors = True

    assert not errors

# use test_lfshell_err_logfile_existence instead but temp reserve the following test till we well know the function of the lfmm.log.
'''
def check_for_errors(logfile, record_property):
    with open(logfile, "r") as f:
        pattern = re.compile(combined, re.IGNORECASE)
        line_no = 1
        errors = False

        for line in f:
            if pattern.search(line) is not None:
                record_property("ERROR", escape(f"{logfile}, {line_no}, \"{line.strip().replace(',', ' ')}\""))
                errors = True

            line_no += 1

        return errors

def test_lfmmlog_contains_no_errors(record_property):
    """
    Tests that p1/lfmm.log file does not contain any error messages
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_lfmmlog_contains_no_errors'))

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    # need to check the file existence in case the LFMM model gets turned off
    try:
        lfmmlog_file = Model.getInstance().files.p1.lfmmlog.path
        print(lfmmlog_file)
    except AttributeError:
        record_property("csv_header", "Log File, Line #, Message")
        record_property("ERROR", "none,none,p1/LFMM.log does not exist. Pls make sure LFMM is on in the scedes file.")
        assert False

    record_property("csv_header", "Log File, Line #, Message")
    errors = False

    if check_for_errors(Model.getInstance().files.p1.lfmmlog.path, record_property):
        errors = True

    assert not errors
'''