"""Utility file containing Folium functions used in CCATS for mapping.

Folium Pipeline Map: Summary
____________________________
This is a utility file containing Folium functions used in CCATS for mapping.


Folium Pipeline Map: Model Functions and Class Methods
______________________________________________________
* :func:`~ccats_common.folium_pipeline_map.scale_values` - Creates a width scale for Folium elements based on volume (called by :func:`~ccats_common.folium_pipeline_map.calculate_node_volumes` and :func:`~ccats_common.folium_pipeline_map.calculate_pipeline_volumes`).
* :func:`~ccats_common.folium_pipeline_map.create_nodetypes` - Creates the types of groups to place nodes (called by :func:`~ccats_common.folium_pipeline_map.run`).
* :func:`~ccats_common.folium_pipeline_map.calculate_node_volumes` - Calculates volume for each node and assign a weight(radius) for Folium object (called by :func:`~ccats_common.folium_pipeline_map.create_nodes`).
* :func:`~ccats_common.folium_pipeline_map.calculate_pipeline_volumes` - Calculates volume for each pipeline and assign a weight(radius) for Folium object (called by * :func:`~ccats_common.folium_pipeline_map.create_pipelines`).
* :func:`~ccats_common.folium_pipeline_map.create_nodes` - Iterate through the pipeline DataFrame and create each node (called by :func:`~ccats_common.folium_pipeline_map.create_nodes_pipelines`).
* :func:`~ccats_common.folium_pipeline_map.create_pipelines` - Iterate through the pipeline DataFrame and create each pipeline (called by :func:`~ccats_common.folium_pipeline_map.create_nodes_pipelines`).
* :func:`~ccats_common.folium_pipeline_map.create_nodes_pipelines` - Create nodes and pipelines (called by :func:`~ccats_common.folium_pipeline_map.run`).
* :func:`~ccats_common.folium_pipeline_map.run` - Create a map of CCATS data (called by :func:`ccats_common.common_visual.create_folium_map`).


Folium Pipeline Map: Input Files
_________________________________
None


Folium Pipeline Map: Output Debug Files
_______________________________________
None


Folium Pipeline Map: Output Restart Variables
_____________________________________________
None


Folium Pipeline Map: Code
_________________________
"""
import numpy as np
import pandas as pd
import folium
import os
from ccats_common import folium_objects as fo
from folium.plugins import FloatImage
from folium.plugins import GroupedLayerControl


def scale_values(df, column_name, scale_start=1, scale_end=5):
    """Creates a width scale for Folium elements based on volume.

    Parameters
    ----------
    df: DataFrame
       DataFrame with node, pipeline data, and CO\ :sub:`2` volume.
    column_name: str
       Name of column to base the scale on, generally will be co2_volume.
    scale_start: int
        Min value of scale.
    scale end: int
        Max value of scale.

    Returns
    -------
    DataFrame with scale 
    """
    scale = pd.DataFrame(columns=['max_value', 'scale'])
    if (len(df.index) == 1) | (sum(df.volume) == 0):
        new_row = [df.iloc[0].volume, scale_start]
        scale.loc[len(scale)] = new_row
        scale.loc[len(scale)] = new_row
        return scale

    max = round(df[column_name].max())
    #df[column_name].min()
    min_v = round(min(i for i in df[column_name] if i > 0))
    diff = max - min_v
    scale_increments = scale_end - scale_start
    value_increments = diff/scale_increments
    value = 0
   
    for i in range(scale_start, scale_end+1, 1):
        new_row = [min_v+value, float(i)]
        #new_row.append(min_v, i)
        scale.loc[len(scale)] = new_row
        value = value + value_increments
    
    return scale


def create_nodetypes(types):
    """Creates the types of groups to place nodes.

        * create a Folium.FeatureGroup for each node type

    Parameters
    ----------
    types : None or list
        Types of nodes to be plotted.

    Returns
    -------
    List of node groups : list
    """
    node_types = []
    if types == None:
       
        ### Create a feature group for each node type

        # Supply
        ethanol_group = folium.FeatureGroup("ethanol")
        ammonia_group = folium.FeatureGroup("ammonia")
        cement_group = folium.FeatureGroup("cement")
        ng_processing_group = folium.FeatureGroup("ng_processing")
        pp_coal_group = folium.FeatureGroup("pp_coal")
        pp_natgas_group = folium.FeatureGroup("pp_natgas")
        beccs_group = folium.FeatureGroup("beccs")



        # Trans-shipment
        ts_group = folium.FeatureGroup("uniform_ts_node")
        existing_ts_group = folium.FeatureGroup("operational_ts_node")

        # Demand
        co2_eor_group = folium.FeatureGroup("co2_eor")
        storage_group = folium.FeatureGroup("storage")
        hsm_centroid_group = folium.FeatureGroup('hsm_centroid')

        node_types.append(ethanol_group)
        node_types.append(ammonia_group)
        node_types.append(cement_group)
        node_types.append(ng_processing_group)
        node_types.append(pp_coal_group)
        node_types.append(pp_natgas_group)
        node_types.append(beccs_group)
        node_types.append(ts_group)
        node_types.append(existing_ts_group)
        node_types.append(co2_eor_group)
        node_types.append(storage_group)
        node_types.append(hsm_centroid_group)

    else:
        for t in types:
            new_group =  folium.FeatureGroup(t)
            node_types.append(new_group)
    
    return node_types


def calculate_node_volumes(df):
    """Calculates volume for each node and assign a weight(radius) for Folium object.

        * Create a dataframe with id, volume and weight
        * Sum the volume of nodes with the same id
        * Assign a weight based on volume for each node id
        
    Parameters
    ----------
    df: DataFrame
       DataFrame with node, pipeline data, and CO\ :sub:`2` volume 

    Returns
    -------
    DataFrame with id, volume and weight: DataFrame
    """
    df = df.replace(np.nan, None)
    nodes = pd.DataFrame(columns=['node_id', 'volume', 'weight'])
    for index,row in df.iterrows():
        node_ids = nodes['node_id'].tolist()
        if 'node_j_id' in row.keys():
            if row.node_j_id:
                if row.node_j_id in node_ids:
                    nodes.loc[nodes['node_id'] == row.node_j_id, 'volume'] = row.co2_volume + nodes.loc[nodes['node_id'] == row.node_j_id, 'volume']
                else:
                    new_node = [row.node_j_id, row.co2_volume, 1.0]
                    nodes.loc[len(nodes)] = new_node
        if 'node_i_id' in row.keys():
            if row.node_i_id in node_ids:
                if (row.node_i_type == 'uniform_ts_node') | (row.node_i_type == 'operational_ts_node'):
                    pass
                else:
                    nodes.loc[nodes['node_id'] == row.node_i_id, 'volume'] = row.co2_volume + nodes.loc[nodes['node_id'] == row.node_i_id, 'volume']

     # set the marker radius size
    volume_scale = scale_values(nodes,'volume',7,15)
    for index,row in nodes.iterrows():
        volume = round(row.volume)
        for j,scale_row in volume_scale.iterrows():
            if round(volume) <= scale_row.max_value:
                nodes.loc[index, 'weight'] = scale_row.scale
                break
    return nodes


def calculate_pipeline_volumes(df):
    """Calculates volume for each pipeline and assign a weight(radius) for Folium object.

        * Create a dataframe with id, volume and weight,
        * Sum the volume of j nodes with the same id, and
        * Assign a weight based on volume for each node id.
        
    Parameters
    ----------
    df: Dataframe
        DataFrame with node, pipeline data, and CO\ :sub:`2` volume.

    Returns
    -------
    DataFrame id, volume and weight : DataFrame
    """
    p = pd.DataFrame(columns=['route_id', 'volume', 'weight'])
    df = df.replace(np.nan, None)
    for index,row in df.iterrows():
        p_ids = p['route_id'].tolist()
        if row.route_id in p_ids:
            p.loc[p['route_id'] == row.route_id, 'volume'] = row.co2_volume + p.loc[p['route_id'] == row.route_id, 'volume']
        else:
            new_pipeline = [row.route_id, round(row.co2_volume), 2.0]
            p.loc[len(p)] = new_pipeline

    # set the line weight
    volume_scale = scale_values(p,'volume',2,10)
    for index,row in p.iterrows():
        volume = row.volume
        for j,scale_row in volume_scale.iterrows():
            if volume < scale_row.max_value:
                p.loc[index, 'weight'] = scale_row.scale
                break
        
    return p 


def create_nodes(df, node_types, color_map):
    """Iterate through the pipeline dataframe and create each node.

        * Calls calculate_node_volumes to get dataframe with weight for each node
            if id is not in dataframe, min value is assigned
        * Call create marker to create a Folium marker for each node
        
    Parameters
    ----------
    df: DataFrame
       DataFrame with node, pipeline data, and CO` :sub:`2` volume.
    node_types: list
        List of folium.FeatureGroup node groups.
    color_map: dict
        Color assigned to each node type.

    Returns
    -------
    node_types with Folium makers representing nodes: list
    """
    df_volumes = calculate_node_volumes(df)
    rds = 7.0
    vol = 0
    for index,row in df.iterrows():
        n_ids = df_volumes['node_id'].tolist()
        vol = str(row.co2_volume) 
        if row.node_i_type:
            if row.node_i_id in n_ids:
                idx = df_volumes.loc[df_volumes['node_id'] == row.node_i_id,'volume'].index.values[0]
                rds =  df_volumes.iloc[idx].weight
                vol = str(round(df_volumes.iloc[idx].volume))
            popup_data = 'type: ' + row.node_i_type + '<br>' + ' id: ' + str(row.node_i_id) + '<b>' + ' volume: ' +  vol
            if rds < 7.0:
                x = 5
            node_types = fo.create_marker(row.i_latitude, row.i_longitude, node_types, color = color_map[row.node_i_type], radius = rds,
                popup_data = popup_data, featureGroupName = row.node_i_type)
            rds = 7.0
        if 'node_j_type' in row.keys():
            if row.node_j_type:
                if row.node_j_id in n_ids:
                    idx = df_volumes.loc[df_volumes['node_id'] == row.node_j_id,'volume'].index.values[0]
                    rds =  df_volumes.iloc[idx].weight
                    vol = str(round(df_volumes.iloc[idx].volume))            
                    popup_data = 'type: ' + row.node_j_type + '<b>' + ' id: ' + str(row.node_j_id) + '<b>' + ' volume: ' +  vol
                if rds < 7.0:
                        x = 5
                node_types = fo.create_marker(row.j_latitude, row.j_longitude, node_types, color = color_map[row.node_j_type], radius = rds,
                                popup_data = popup_data, featureGroupName = row.node_j_type)
                rds = 7.0

    return node_types


def create_pipelines(df, color, line_type=None):
    """Iterate through the pipeline dataframe and create each pipeline.

        * Calls calculate_node_volumes to get dataframe with weight for each pipeline
            * If id is not in dataframe, min value is assigned
        * Calls create_pipeline to create a Folium line for each pipeline
        
    Parameters
    ----------
    df: DataFrame
        DataFrame with node, pipeline data, and CO\ :sub:`2` volume.
   
    Returns
    -------
    list of Folium polylines representing pipelines: list
    """
    pipelines = []
    df_volumes = calculate_pipeline_volumes(df)
    wght = 2.0
    for index,row in df.iterrows():
        # note you need to determine weight based on volume
        p_ids = df_volumes['route_id'].tolist()
        if row.route_id in p_ids:
            wght = df_volumes.loc[df_volumes['route_id'] == row.route_id, 'weight'].values[0]
            volume = df_volumes.loc[df_volumes['route_id'] == row.route_id, 'volume'].values[0]
        #if row.NodeA_latitude and row.NodeB_latitude:
        p = fo.create_pipeline(row.route_id, round(volume),  [row.i_latitude,row.i_longitude], [row.j_latitude,row.j_longitude], 
                               color = color, weight = wght)
        if line_type == 'arrow':
            a = fo.create_arrowline(color,p,weight =  20)
        if line_type == 'dash':
             a = fo.create_dashedline(color,p,weight =  35)

        wght = 2.0
        if p:
            pipelines.append(p)
            pipelines.append(a)

    return pipelines


def create_nodes_pipelines(pipeline_data, node_groups, color_map, pipeline_color='orange', line_type=None):
    """Create nodes and pipelines.
    
    Parameters
    ----------
    pipeline_data : DataFrame
        Pipeline data for plotting.
    node_groups : list
        List of node groups
    color_map : dict
        Dictionary matching node types to their plot colors.
    pipeline_color : str
        Color for plotting pipelines.
    line_type : None or str
        Type of lines to be plotted.

    Returns
    -------
    node_groups : list
        List of Folium objects representing nodes.
    pipelines : list
        List of Folium objects representing pipelines.
    """
    # create nodes and pipelines
    node_groups = create_nodes(pipeline_data, node_groups, color_map)
    pipelines = create_pipelines(pipeline_data, pipeline_color, line_type=line_type)
    return node_groups, pipelines


def run(m, df, outdir, outfile):
    """Create a map of CCATS results.

    Parameters
    ----------
    m : Folium Map
        Map object for plotting data.
    df : list
        List of DataFrames of data for plotting.
    outdir : str
        Directory to sainge the map.
    outfile : str
        Filename for saving the map.
    
    Returns
    -------
    Folium Map
    """
    # create legend
    color_map = {'ethanol':'blue',
                 'ammonia':'deepskyblue',
                 'cement':'cyan',
                 'ng_processing': 'navy',
                 'pp_coal': 'lightcoral',
                 'pp_natgas': 'red',
                 'beccs': 'pink',
                 'uniform_ts_node': 'forestgreen',
                 'operational_ts_node': 'mediumseagreen',
                 'co2_eor': 'fuchsia',
                 'storage': 'purple',
                 'hsm_centroid':'orchid'}
                
    legendimage_file = outdir +'legend.PNG'
    if not os.path.exists(legendimage_file):
        fo.create_legend(color_map,legendimage_file)
    FloatImage(legendimage_file, bottom=2, left=3).add_to(m)

    if type(df) is list:
        # filter df into two dfs: one with co2_volume>0 and one with co2_volume == 0
        pipeline_data = df[0].copy()
        pipeline_data = pipeline_data.loc[pipeline_data.co2_volume != 0]
        pipeline_data_0vol = df[1]
    else:
        pipeline_data = df.copy()
        pipeline_data = pipeline_data.loc[pipeline_data.co2_volume != 0]
        pipeline_data_0vol = pipeline_data.loc[pipeline_data.co2_volume < 1]

    # create the feature groups for nodes and pipeline
    node_groups = create_nodetypes(['ethanol',
                                    'ammonia',
                                    'cement',
                                    'ng_processing',
                                    'pp_coal',
                                    'pp_natgas',
                                    'beccs',
                                    'uniform_ts_node',
                                    'operational_ts_node',
                                    'co2_eor',
                                    'storage',
                                    'hsm_centroid'])
    node_groups_0vol = create_nodetypes(['ethanol0',
                                         'ammonia0',
                                         'cement0',
                                         'ng_processing0',
                                         'pp_coal0',
                                         'pp_natgas0',
                                         'beccs0',
                                         'uniform_ts_node0',
                                         'operational_ts_node0',
                                         'co2_eor0',
                                         'storage0',
                                         'hsm_centroid0'])
    pipeline_group = folium.FeatureGroup("pipeline")
    pipeline_group0 = folium.FeatureGroup("pipeline_0_volume")

    # create the groups for the layers
    fg = folium.FeatureGroup(name="groups")
    m.add_child(fg)
    g1 = folium.plugins.FeatureGroupSubGroup(fg, "group: co2 volume")
    m.add_child(g1)
    g2 = folium.plugins.FeatureGroupSubGroup(fg, "group: no co2 volume", show = False)
    m.add_child(g2)


    if not pipeline_data.empty:
        # create nodes and pipelines
        node_groups, pipelines = create_nodes_pipelines(pipeline_data,node_groups, color_map, pipeline_color = 'orange', line_type='arrow')
        # add each to pipeline folium object the the feature group
        for p in pipelines:
                p.add_to(pipeline_group)
        # append to the list of feature groups
        node_groups.append(pipeline_group)
        # add the node and pipeline feature groups to the map and to the group
        for c in  node_groups:
            m.add_child(c)
            c.add_to(g1)          
    else: 
        # create an empty feature group and add it to the group and map 
        node_groups = [folium.FeatureGroup('N/A')]
        m.add_child(node_groups[0])
        node_groups[0].add_to(g1)
        
    # add 0 volume layer
    if not pipeline_data_0vol.empty:
        # create nodes and pipelines
        node_groups_0vol, pipelines_0vol = create_nodes_pipelines(pipeline_data_0vol,node_groups_0vol, color_map, 
                                                                  pipeline_color = 'black', line_type='dash')
       # add each to pipeline folium object the the feature group
        for p in pipelines_0vol:
            p.add_to(pipeline_group0)
        # append to the list of feature groups
        node_groups_0vol.append(pipeline_group0)
         # add the node and pipeline feature groups to the map and to the group
        for c in  node_groups_0vol:
            m.add_child(c)
            c.add_to(g2)
    else: 
        # create an empty feature group and add it to the group and map 
        node_groups_0vol = [folium.FeatureGroup('N/A')]
        m.add_child(node_groups_0vol[0])
        node_groups_0vol[0].add_to(g2)

    folium.TileLayer('openstreetmap').add_to(m)      
    folium.LayerControl(collapsed=False).add_to(m)

    # this control allows us to group the layers and featuregroups, groups must be a list
    GroupedLayerControl(
        groups={'Co2_vol': node_groups},
        collapsed=False,
        exclusive_groups=False,
                ).add_to(m)
    GroupedLayerControl(
        groups={'zero co2_vol': node_groups_0vol},
        collapsed=True,
        exclusive_groups=False,
        position = 'topleft'
                ).add_to(m)

    m.save(outfile)
    return m

def run_preprocessor_map(m, df,outdir, outfile):
    types = list(set(df.node_i_type))
    color_map = {'ethanol':'blue',
                 'ammonia':'deepskyblue',
                 'cement':'cyan',
                 'ng_processing': 'navy',
                 'pp_coal': 'lightcoral',
                 'pp_natgas': 'red',
                 'beccs': 'pink',
                 'nodal_hub': 'forestgreen',
                 'operational_ts_node': 'mediumseagreen',
                 'co2_eor': 'fuchsia',
                 'storage': 'purple',
                 'hsm_centroid':'orchid'}
    
    color_map = dict ([(k, color_map[k]) for k in types])
    
    
    legendimage_file = outfile[:-5]+'_legend.PNG'
    #if not os.path.exists(legendimage_file):
    fo.create_legend(color_map,legendimage_file)
    FloatImage(legendimage_file, bottom=50, left=75).add_to(m)

if __name__ == '__main__':
    # initialize the map and store it in a m object
    m = folium.Map(location=[40, -95],tiles="CartoDB Positron", zoom_start=4)

    m = run(m, None, None)
