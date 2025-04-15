import pickle
import pytest
import os

@pytest.fixture()
def tdata():
    path = os.getcwd()
    with open(path +'\\test.pkl', 'rb') as f:
        test_object = pickle.load(f)
    return test_object

def test_co2_supply_selection_matching(tdata):
    '''Test that all CO2 supplies from NEMS are matched with CO2 facilities in :ref:`preprocessor`.

    Parameters
    ----------
    tdata

    Returns
    -------
    None
    '''
    # Merge Model year CO2 supply from NEMS to selected model year CO2 capture sites and produce difference
    nems_co2_supply = tdata.nems_co2_supply # Model year CO2 supply from NEMS
    supply_sources = tdata.supply_sources # Model year CO2 capture sites
    supply_sources = supply_sources[['co2_volume','census_division','facility_type','eligibility_45q']].groupby(['census_division','facility_type','eligibility_45q']).sum()
    nems_co2_supply = nems_co2_supply.merge(supply_sources,
                                            how = 'left',
                                            left_index = True,
                                            right_index = True).fillna(0.0)
    nems_co2_supply['diff'] = nems_co2_supply['capture_volume_tonnes'] - nems_co2_supply['co2_volume']

    # Test
    if (nems_co2_supply['diff'].sum() < 1) | (nems_co2_supply['diff'].sum() > -1):
        assert True
    else:
        tdata.logger.warning('Model year CO2 supply from NEMS is not being allocated to CO2 supply sites in the model. Check model'
            'inputs to ensure there are sufficient CO2 supply facilities available by "facility_type" and "eligibility_45q".')
        assert False


def test_eor_demand(tdata):
    '''Test that all CO2 demand from NEMS is loaded into the main optimization model.

    Parameters
    ----------
    tdata

    Returns
    -------
    None
    '''
    mask = ~tdata.co2_demand_b0.index.str.contains('hsm_cent')
    co2_demand_b0 = tdata.co2_demand_b0[mask]
    if (co2_demand_b0.sum() - tdata.rest_eor_demand_df[tdata.year_current].sum() > 10) | (co2_demand_b0.sum() - tdata.rest_eor_demand_df[tdata.year_current].sum() < -10):
        tdata.logger.warning('Model year CO2 demand from NEMS does not match CO2 demand input to the optimization model. '
                             'Check model year CO2 demand in "optimization_inputs/demand".')
        assert False
    else:
        assert True

    pass


def test_ts_nodes(tdata):
    '''Test that each ts node that connects to a supply node also connects to a sequestration node (doesn't have to be direct).

    Parameters
    ----------
    tdata

    Returns
    -------
    None
    '''
    #Get ts nodes that connect CO2 sources
    pipeline_types = ['source_to_ts_node']
    source_to_ts_node_df = tdata.pipeline_lookup_df[tdata.pipeline_lookup_df['pipeline_type'].isin(pipeline_types)]
    ts_nodes = list(source_to_ts_node_df['node_j_id'].unique())

    #Get ts nodes that connect CO2 sequestrations sites or other ts nodes
    ts_to_demand_node_types = ['ts_node_to_storage', 'ts_node_to_demand', 'ts_node_to_ts_node']
    ts_to_demand_df = tdata.pipeline_lookup_df[tdata.pipeline_lookup_df['pipeline_type'].isin(ts_to_demand_node_types)]
    ts_i_nodes = list(ts_to_demand_df['node_i_id'].unique())

    # check that each ts node id that connected to source is in one of the ts_to_demand_node_types. Also make sure the nodes they conncect to
    # are not nans
    if (set(ts_nodes) <= set(ts_i_nodes)) & ~ts_to_demand_df['node_j_id'].isna().any():
        assert True
    else:
        tdata.logger.warning('Not all ts nodes connect to both supply and sequestration sites. Check model year pipeline network in "optimization_inputs/supply".')
        assert False


def test_node_count(tdata):
    '''Test that total node count == sum(component node type counts)

    Parameters
    ----------
    tdata

    Returns
    -------
    None
    '''
    if tdata.nodes == tdata.nodes_supply.union(tdata.nodes_trans_ship, tdata.nodes_demand, tdata.nodes_sequester):
        assert True
    else:
        tdata.logger.warning('Total node count != component node type counts')
        assert False