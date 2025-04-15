import pickle
import pytest
import os

@pytest.fixture()
def tdata():
    path = os.getcwd()
    with open(path +'\\test.pkl', 'rb') as f:
        test_object = pickle.load(f)
    return test_object

def test_reconcile_supply(tdata):
    '''Test asserting that model CO2 supply input == model CO2 supply output.

    Parameters
    ----------
    tdata : object
        Test data from CCATS Module parent class.

    Returns
    -------
    None
    '''
    # Block 0
    results = tdata.O_flows_b0_df.loc[tdata.O_flows_b0_df['node_i_type'].isin(['other_existing',
                                                                       'ethanol',
                                                                       'ammonia',
                                                                       'ng_processing',
                                                                       'cement',
                                                                       'pp_coal',
                                                                       'pp_natgas',
                                                                       'beccs'])]['co2_volume'].copy().sum()
    if ((tdata.co2_supply_b0.sum() - results) > 1) | ((tdata.co2_supply_b0.sum() - results) < -1):
        tdata.logger.warning('Block_0 CO2 supply input does not match Block_0 CO2 supply output. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    # Block 1
    results = tdata.O_flows_b1_df.loc[tdata.O_flows_b1_df['node_i_type'].isin(['other_existing',
                                                                       'ethanol',
                                                                       'ammonia',
                                                                       'ng_processing',
                                                                       'cement',
                                                                       'pp_coal',
                                                                       'pp_natgas',
                                                                       'beccs'])]['co2_volume'].copy().sum()
    if ((tdata.co2_supply_b1.sum() - results) > 1) & ((tdata.co2_supply_b1.sum() - results) < -1):
        tdata.logger.warning('Block_1 CO2 supply input does not match Block_1 CO2 supply output. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    # Block 2
    results = tdata.O_flows_b2_df.loc[tdata.O_flows_b2_df['node_i_type'].isin(['other_existing',
                                                                       'ethanol',
                                                                       'ammonia',
                                                                       'ng_processing',
                                                                       'cement',
                                                                       'pp_coal',
                                                                       'pp_natgas',
                                                                       'beccs'])]['co2_volume'].copy().sum()
    if ((tdata.co2_supply_b2.sum() - results) > 1) & ((tdata.co2_supply_b2.sum() - results) < -1):
        tdata.logger.warning('Block_0 CO2 supply input does not match Block_0 CO2 supply output. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    else:
        assert True

    pass

# todo replace with single function
def test_restart_file_outputs_emm(tdata):
    '''Test asserting CCATS EMM CO2 inputs from the restart file == CO2 supply outputs to the restart file where relevant.

    Parameters
    ----------
    Test data from CCATS Module parent class.

    Returns
    -------

    '''
    # Get CO2 supply input
    i_co2_supply_df = tdata.rest_industrial_co2_supply_45q.copy()
    i_co2_supply_df['capture_volume_tonnes'] = i_co2_supply_df['capture_volume_tonnes'] + tdata.rest_industrial_co2_supply_ntc['capture_volume_tonnes']
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['facility_type'].isin(['pp_coal','pp_natgas','beccs'])]
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['year'] == tdata.year_current]

    # Get CO2 supply output
    o_co2_supply_df = tdata.rest_co2_sup_out.copy()
    o_co2_supply_df = o_co2_supply_df.loc[~o_co2_supply_df.index.get_level_values(0).isin([10,11])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(1).isin([1,2,3])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(2) == tdata.year_current]

    # Run Test
    if ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) > 1) | ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) < -1):
        tdata.logger.warning('Model year CO2 supply inputs from EMM do not equal model year CO2 supply outputs. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    else:
        assert True

    pass


def test_restart_file_outputs_hsm(tdata):
    '''Test asserting CCATS HSM CO2 inputs from the restart file == CO2 supply outputs to the restart file where relevant.

    Parameters
    ----------
    Test data from CCATS Module parent class.

    Returns
    -------

    '''
    # Get CO2 supply input
    i_co2_supply_df = tdata.rest_industrial_co2_supply_45q.copy()
    i_co2_supply_df['capture_volume_tonnes'] = i_co2_supply_df['capture_volume_tonnes'] + tdata.rest_industrial_co2_supply_ntc['capture_volume_tonnes']
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['facility_type'].isin(['ng_processing'])]
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['year'] == tdata.year_current]

    # Get CO2 supply output
    o_co2_supply_df = tdata.rest_co2_sup_out.copy()
    o_co2_supply_df = o_co2_supply_df.loc[~o_co2_supply_df.index.get_level_values(0).isin([10,11])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(1).isin([4])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(2) == tdata.year_current]

    # Run Test
    if ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) > 1) | ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) < -1):
        tdata.logger.warning('Model year CO2 supply inputs from HSM do not equal model year CO2 supply outputs. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    else:
        assert True

    pass


def test_restart_file_outputs_idm(tdata):
    '''Test asserting CCATS IDM CO2 inputs from the restart file == CO2 supply outputs to the restart file where relevant.

    Parameters
    ----------
    Test data from CCATS Module parent class.

    Returns
    -------

    '''
    # Get CO2 supply input
    i_co2_supply_df = tdata.rest_industrial_co2_supply_45q.copy()
    i_co2_supply_df['capture_volume_tonnes'] = i_co2_supply_df['capture_volume_tonnes'] + tdata.rest_industrial_co2_supply_ntc['capture_volume_tonnes']
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['facility_type'].isin(['cement'])]
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['year'] == tdata.year_current]

    # Get CO2 supply output
    o_co2_supply_df = tdata.rest_co2_sup_out.copy()
    o_co2_supply_df = o_co2_supply_df.loc[~o_co2_supply_df.index.get_level_values(0).isin([10,11])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(1).isin([5])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(2) == tdata.year_current]

    # Run Test
    if ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) > 1) | ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) < -1):
        tdata.logger.warning('Model year CO2 supply inputs from IDM do not equal model year CO2 supply outputs. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    else:
        assert True

    pass


def test_restart_file_outputs_lfmm(tdata):
    '''Test asserting CCATS LFMM CO2 inputs from the restart file == CO2 supply outputs to the restart file where relevant.

    Parameters
    ----------
    Test data from CCATS Module parent class.

    Returns
    -------

    '''
    # Get CO2 supply input
    i_co2_supply_df = tdata.rest_industrial_co2_supply_45q.copy()
    i_co2_supply_df['capture_volume_tonnes'] = i_co2_supply_df['capture_volume_tonnes'] + tdata.rest_industrial_co2_supply_ntc['capture_volume_tonnes']
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['facility_type'].isin(['ethanol'])]
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['year'] == tdata.year_current]

    # Get CO2 supply output
    o_co2_supply_df = tdata.rest_co2_sup_out.copy()
    o_co2_supply_df = o_co2_supply_df.loc[~o_co2_supply_df.index.get_level_values(0).isin([10,11])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(1).isin([6])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(2) == tdata.year_current]

    # Run Test
    if ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) > 1) | ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) < -1):
        tdata.logger.warning('Model year CO2 supply inputs from LFMM do not equal model year CO2 supply outputs. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    else:
        assert True

    pass


def test_restart_file_outputs_hmm(tdata):
    '''Test asserting CCATS HMM CO2 inputs from the restart file == CO2 supply outputs to the restart file where relevant.

    Parameters
    ----------
    Test data from CCATS Module parent class.

    Returns
    -------

    '''
    # Get CO2 supply input
    i_co2_supply_df = tdata.rest_industrial_co2_supply_45q.copy()
    i_co2_supply_df['capture_volume_tonnes'] = i_co2_supply_df['capture_volume_tonnes'] + tdata.rest_industrial_co2_supply_ntc['capture_volume_tonnes']
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['facility_type'].isin(['ammonia'])]
    i_co2_supply_df = i_co2_supply_df.loc[i_co2_supply_df['year'] == tdata.year_current]

    # Get CO2 supply output
    o_co2_supply_df = tdata.rest_co2_sup_out.copy()
    o_co2_supply_df = o_co2_supply_df.loc[~o_co2_supply_df.index.get_level_values(0).isin([10,11])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(1).isin([7])]
    o_co2_supply_df = o_co2_supply_df.loc[o_co2_supply_df.index.get_level_values(2) == tdata.year_current]

    # Run Test
    if ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) > 1) | ((i_co2_supply_df['capture_volume_tonnes'].sum() - o_co2_supply_df['value'].sum()) < -1):
        tdata.logger.warning('Model year CO2 supply inputs from HMM do not equal model year CO2 supply outputs. Check "optimization_inputs/supply" ' \
                             'and "optimization_results/flows" debug folders to reconcile differences.')
        assert False

    else:
        assert True

    pass