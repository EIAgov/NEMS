import pickle
import pytest
import os

@pytest.fixture()
def tdata():
    path = os.getcwd()
    with open(path +'\\test.pkl', 'rb') as f:
        test_object = pickle.load(f)
    return test_object


def test_null(tdata):
    '''Test that model returns results.

    Parameters
    ----------
    tdata

    Returns
    -------
    Non
    '''
    if (tdata.O_flows_b0_df['co2_volume'].sum() == 0) & (tdata.year_current > tdata.year_start):
        tdata.logger.warning('CO2 flows == 0, model may be infeasible, check model log.')
        assert False
    else:
        assert True

def test_slack(tdata):
    '''Test whether model is directing CO2 flows to slack variables.

    Parameters
    ----------
    tdata

    Returns
    -------
    None
    '''
    if (tdata.O_infeasible_supply_b0_df['infeasible_slack'].sum() >= 1):
            tdata.logger.warning('CO2 supply directed to supply slack variables in block 0. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
            assert False
    elif (tdata.O_infeasible_supply_b1_df['infeasible_slack'].sum() >= 1):
            tdata.logger.warning('CO2 supply directed to supply slack variables in block 0. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
            assert False
    elif (tdata.O_infeasible_supply_b2_df['infeasible_slack'].sum() >= 1):
            tdata.logger.warning('CO2 supply directed to supply slack variables in block 2. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
    else:
            assert True
    
def test_flows(tdata):
    '''Test asserting that CO2 flows do not exceed CO2 pipeline capacity.

    Parameters
    ----------
    tdata

    Returns
    -------
    None
    '''
    # Merge CO2 flows and CO2 pipeline throughput to a single DataFrame
    capacity = tdata.transport_add + tdata.transport_existing
    flows = tdata.O_flows_b0_df[['node_i_id', 'node_j_id', 'pipe_segment', 'co2_volume']]
    flows = flows.set_index(['node_i_id', 'node_j_id', 'pipe_segment'])
    capacity_flows = flows.merge(capacity.to_frame(), left_index=True, right_index=True)

    # Test
    if any(capacity_flows['co2_volume'] > capacity_flows['thruput_tonnes']):
        tdata.logger.warning('CO2 flows exceed CO2 pipeline capacity. Check "optimization_results/flows" and "optimization_inputs/transport" '
                             'debug folders for Block 0 results and "optimization_results/flows" and "optimization_results/transport_cap_add" folders'
                             'for other model block results.')
        assert False
    else:
        assert True