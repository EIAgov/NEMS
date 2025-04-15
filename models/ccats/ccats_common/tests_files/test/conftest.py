import pytest
import pickle

def pytest_addoption(parser):
    parser.addoption("--testdata", action="store", default="test.pkl")

@pytest.fixture(scope='session')
def testdata(request):
    test_data_vals = request.config.getoption("--test_data")
    if test_data_vals is None:
        pytest.skip()
    with open('test.pkl', 'rb') as f:
        test_object = pickle.load(f)
    return test_object