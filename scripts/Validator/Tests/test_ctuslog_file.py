from DataModel.Model import Model
import re
import pytest
from xml.sax.saxutils import escape

from Controller.StatusHelper import StatusHelper

#List out the keywords that will appear in a nohup error message
log_errors = [
    "ERROR"
]

# Make a regex that matches if any of our regexes match.
combined = "(" + ")|(".join(log_errors) + ")"


def check_for_errors(logfile, record_property):
    with open(logfile, "r") as f:
        pattern = re.compile(combined, re.IGNORECASE)
        line_no = 1
        errors = False

        for line in f:
            if pattern.search(line) is not None:
                print(line)
                print(f'pattern={pattern}')
                record_property("ERROR", escape(f"{logfile}, {line_no}, \"{line.strip().replace(',', ' ')}\""))
                errors = True

            line_no += 1

        return errors

def test_ctuslog_contains_no_errors(record_property):
    """
    Check the existence of the 3 CTUS log p2\{CTS.log, CTSmodel.log, CTSreprt.log} files in case the CTUS model gets turned off.
    If the log files exist, extract the error messages.
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_ctuslog_contains_no_errors'))

    if not Model.getInstance().files.parnems:
        pytest.skip("Must be Parallel run to run this test.")

    # now check the existence of the 3 CTUS log files in case the CTUS model gets turned off, and extract errors
    record_property("csv_header", "Log File, Line #, Message")
    errors = False
    if Model.getInstance().files.p2.cts_log is None:
        record_property("ERROR", "none,none,p2/CTS.log does not exist. Pls make sure CTUS is on in the scedes file.")
        errors = True
    else:
        if check_for_errors(Model.getInstance().files.p2.cts_log.path, record_property):
            errors = True

    if Model.getInstance().files.p2.cts_model_log is None:
        record_property("ERROR", "none,none,p2/CTSmodel.log does not exist. Pls make sure CTUS is on in the scedes file.")
        errors = True
    else:
        if check_for_errors(Model.getInstance().files.p2.cts_model_log.path, record_property):
            errors = True

    if Model.getInstance().files.p2.cts_report_log is None:
        record_property("ERROR", "none,none,p2/CTSreprt.log does not exist. Pls make sure CTUS is on in the scedes file.")
        errors = True
    else:
        if check_for_errors(Model.getInstance().files.p2.cts_report_log.path, record_property):
            errors = True

    assert not errors