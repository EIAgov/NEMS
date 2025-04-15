from DataModel.Model import Model
import re
from xml.sax.saxutils import escape
from Controller.StatusHelper import StatusHelper

#List out the keywords that will appear in a nohup error message
nohup_errors = [
    "ERROR",
    "ABORTED",
    "No retrieve",
    "This is a very bad Thing",
    "Turning Xpresssw off",
    "not loaded",
    "AIMMS license issue.  Will retry. Number of tries so far:  9"
]

infeas_errors = [
    "INFEAS"
]

variable_errors = [
    "DUPLICATE VARIABLE FOUND:",
    "WILL SKIP THAT ENTRY",
    "COULD NOT FIND ATTRIBUTE",
    "NOT FOUND IN THE DICTIONARY UNDER COMMON BLOCK"
]

# Make a regex that matches if any of our regexes match.
combined = "(" + ")|(".join(nohup_errors) + ")"
combined_infeas = "(" + ")|(".join(infeas_errors) + ")"
combined_variable = "(" + ")|(".join(variable_errors) + ")"

def check_for_errors_exclusive(nohup, record_property):
    """A method to check errors with combined error pattern but exclude AIMMS exited early and CPLEX 12.10 licnese AIMMS error.

    Parameters
    ----------
    nohup : file
        the nohup.out file
    record_property : method
        pytest built-in method

    Returns
    -------
    errors : list
        the list of nohups error records
    """    
    with open(nohup, "r") as f:
        pattern = re.compile(combined, re.IGNORECASE)
        line_no = 1
        errors = False

        for line in f:
            if pattern.search(line) is not None:
                sub = 'AIMMS Exited early. Likely a licensing issue or error in the AIMMS code or transfer data. Displaying log/messages.log:'
                sub2 = 'error: Could not find CPLEX 12.10 license file supplied by AIMMS.'
                s = line.strip().replace(',', ' ')
                if (sub not in s) and (sub2 not in s):
                    record_property("ERROR", escape(f"{nohup}, {line_no}, \"{line.strip().replace(',', ' ')}\""))
                    errors = True
            line_no += 1

        return errors

def check_for_errors(nohup, record_property, combined):
    """A generic method to check errors with combined error pattern

    Parameters
    ----------
    nohup : file
        the nohup.out file
    record_property : method
        pytest built-in method
    combined : list
        the combined error pattern

    Returns
    -------
    errors : list
        the list of nohups error records
    """
    with open(nohup, "r") as f:
        pattern = re.compile(combined, re.IGNORECASE)
        line_no = 1
        errors = False

        for line in f:
            if pattern.search(line) is not None:
                record_property("ERROR", escape(f"{nohup}, {line_no}, \"{line.strip().replace(',', ' ')}\""))
                errors = True
            line_no += 1

        return errors

def setup_nohups(files):
    """To build the nohup.out file list.

    Parameters
    ----------
    files : obj
        the files in output directory

    Returns
    -------
    nohups : list
        a list of nohup.out files
    """
    nohups =[]
    nohups.append(files.nohup.path)

    if files.parnems:
        nohups.append(files.p1.nohup.path)
        nohups.append(files.p2.nohup.path)
        nohups.append(files.p3.nohup.path)
    return nohups

def get_gpa_from_nohup(nohup):
    """
    returns array of length 2
    first element is final US gpa
    second element is final Regional gpa

    Parameters
    ----------
    nohup : file
        the nohup.out file
    """

    gpa_pattern = r'[0-4]\.[0-9]+'
    #gpa_string_pattern = "GPA.*on a 4 point scale"
    gpa_string_pattern = "GPA \(.*\)"
    pattern = re.compile(gpa_string_pattern, re.IGNORECASE)

    with open(nohup, "r") as f:
        final_gpa_string = pattern.findall(f.read()) or ['ERROR 0.0 ERROR 0.0']

    final_gpa_string = final_gpa_string[-1]

    return [float(i) for i in re.findall(gpa_pattern, final_gpa_string)]


def test_nohup_contains_no_errors(record_property):
    """
    Tests that Nohup.out files do not contain any error messages

    Parameters
    ----------
    record_property : method
        pytest built-in method
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_nohup_contains_no_errors'))

    files = Model.getInstance().files
    nohups =setup_nohups(files)
    record_property("csv_header", "Nohup File, Line #, Message")
    errors = False

    for n in nohups:
        if check_for_errors_exclusive(n, record_property):
            errors = True

    assert not errors

def test_nohup_contains_no_infeas(record_property):
    """
    Tests that Nohup.out files do not contain any "ECP INFEASIBILITY PROGRESS REPORT :  Xpress depress :" or "EFD INFEASIBILITY PROGRESS REPORT" messages

    Parameters
    ----------
    record_property : method
        pytest built-in method
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_nohup_contains_no_infeas'))

    files = Model.getInstance().files
    nohups = setup_nohups(files)
    record_property("csv_header", "Nohup File, Line #, Message")
    errors = False

    for n in nohups:
        if check_for_errors(n, record_property, combined_infeas):
            errors = True

    assert not errors

def test_nohup_contains_no_duplicate_variable(record_property):
    """
    Tests that nohup.out files do not contain any "COULD NOT FIND ATTRIBUTE: QUNTY  FOR BLOCK: QBLK", "DUPLICATE VARIABLE FOUND:" or " WILL SKIP THAT ENTRY" messages.
    or "NOT FOUND IN THE DICTIONARY UNDER COMMON BLOCK".

    Parameters
    ----------
    record_property : method
        pytest built-in method
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_nohup_contains_no_duplicate_variable'))

    files = Model.getInstance().files
    nohups = setup_nohups(files)
    record_property("csv_header", "Nohup File, Line #, Message")
    errors = False

    for n in nohups:
        if check_for_errors(n, record_property, combined_variable):
            errors = True

    assert not errors

# This test need more preventive code. In corner cases, it throws an empty unformated .cvs file and makes Validator_FAIL.xlsx not open.
def test_convergence(record_property):
    """
    Is the final cycle gpa >= 3.50

    Parameters
    ----------
    record_property : method
        pytest built-in method
    """
    h = StatusHelper()
    h.skip_test(h.get_status('test_convergence'))

    nohup = Model.getInstance().files.nohup.path
    [US_gpa, REG_gpa] = get_gpa_from_nohup(nohup)

    record_property("csv_header", "GPA Type, GPA value")
    errors = False

    if US_gpa == 0 and REG_gpa == 0:
        record_property("ERROR", r"No GPA found, 0.0")
        errors = True
    else:

        if US_gpa < 3.5:
            record_property("ERROR", f"US, {US_gpa}")
            errors = True

        if REG_gpa < 3.5:
            record_property("ERROR", f"REG, {REG_gpa}")
            errors = True

    assert not errors