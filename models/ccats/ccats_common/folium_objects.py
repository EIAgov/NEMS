"""Utility file containing folium objects used in CCATS for mapping.

Folium Objects: Summary
_______________________
This is a utility file containing folium objects used in CCATS for mapping.


Folium Objects: Input Files
___________________________
None


Folium Objects: Model Functions and Class Methods
_________________________________________________

* :func:`~ccats_common.folium_objects.create_marker` - creates folium markers (called by :func:`ccats_common.folium_pipeline_map.create_nodes`).
* :func:`~ccats_common.folium_objects.create_pipeline` - creates folium polylines to represent pipelines between two points (called by :func:`ccats_common.folium_pipeline_map.create_pipelines`).
* :func:`~ccats_common.folium_objects.create_arrowline` - creates folium polyline to represent pipelines between two points (called by :func:`ccats_common.folium_pipeline_map.create_pipelines`).
* :func:`~ccats_common.folium_objects.create_dashedline` - creates dashed folium polyline to represent pipelines between two points (called by :func:`ccats_common.folium_pipeline_map.create_pipelines`).
* :func:`~ccats_common.folium_objects.create_legend` - creates legend for folium map (called by :func:`~ccats_common.folium_pipeline_map.run`).
* :func:`~ccats_common.folium_objects.export_legend` - saves legend (called by :func:`~ccats_common.folium_objects.create_legend`).


Folium Objects: Output Debug Files
__________________________________
None


Folium Objects: Output Restart Variables
________________________________________
None


Folium Objects: Code
____________________
"""

import os
import pandas as pd
import folium
import folium.plugins as fp
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

import pylab


def create_marker(lat, long, node_types, color='red',radius = 5.0, popup_data = 'N/A', featureGroupName=None):
    """Creates Folium markers.

    Parameters
    ----------
    lat: float
        Latitude
    long: float
       Longitude
    node_types: list
        List of folium featuregroups representing node groups
    color: string
        Color for marker
    radius: float
        Radius for marker
    popup_data: str
        Data shown in popup when marker is clicked
    featureGroupName: str
        Tells with node_types to add marker to

    Returns
    -------
    Node_types with marker representing each node : list
    """
    nodetype_idx = ''
    if featureGroupName:
        for idx, feature in enumerate(node_types):
            if featureGroupName in feature.layer_name:
                nodetype_idx = idx
                break
    folium.CircleMarker(
        location=[lat, long],
        radius=radius,
        popup=popup_data,
        #tooltip=group_data.iloc[i]['name'],
        fill_color=color,
        stroke = False, 
        fill_opacity = 1
        ).add_to(node_types[nodetype_idx])
    return node_types


def create_pipeline(id, volume, pointA, pointB, color, weight = 1):
    """Creates Folium polylines to represent pipelines between two points.

    Parameters
    ----------
    id: str
       Pipeline ID.
    volume: float
      CO\ :sub:`2` volume
    pointA: list
       Lat and long for a point.
    pointB: list
       Lat and long for a point.
    color: str
       Color of the pipeline.
    weight: int
        Weight of line.

    Returns
    -------
    Folium polyline
    """
    
    points = []
    if any(pointA) and any(pointB):
        points.append(pointA)
        points.append(pointB)
        p = folium.PolyLine(points, color=color, popup = id + '\n' + 'volume: ' + str(volume) + '\n' + 'diameter: ' + str(weight) , weight=weight)
        return p
    else:
        return None
    

def create_arrowline( color, path, weight = 24):
    """Creates Folium polyline to represent pipelines between two points.

    Parameters
    ----------
    color : str
       Color for the line.
    path : Folium Polyline
      Folium Polyline to render.
    weight: int
        Weight of line

    Returns
    -------
    Folium PolyLineTextPath
    """
    attr = {"font-weight": "bold", "font-size": weight, "fill": color}
    a = fp.PolyLineTextPath(path, "\u25BA    ", repeat=True, offset=8, attributes=attr)
    return a


def create_dashedline( color, path, weight = 24):
    """Creates dashed folium polyline to represent pipelines between two points.

    Parameters
    ----------
    color : str
       Color for the line.
    path : Folium Polyline
      Folium Polyline to render.
    weight: int
        Weight of line

    Returns
    -------
    Folium PolyLineTextPath
    """
    attr = {"font-weight": "bold", "font-size": weight, "fill": color}
    a = fp.PolyLineTextPath(path, "\u21E2   ", repeat=True, offset=8, attributes=attr)
    return a


def create_legend(color_map, fname):
    """Creates legend for folium map.

    Parameters
    ----------
    color_map: dict
       Color assigned to each node type.
    fname: str
       Name to save the legend output file.

    Returns
    -------
    None
    """
    legend_handles = []
    for key in color_map:
        color_patch = mpatches.Patch(color=color_map[key], label=key)
        legend_handles.append(color_patch)
    leg = plt.legend(handles=legend_handles)
    export_legend(legend= leg, filename=fname)


def export_legend(legend, filename="legend.png"):
    """Saves legend.

    Parameters
    ----------
    legend: Matplotlib legend
        Legend to be saved.
    fname: str
        Name to save the legend output file.

    Returns
    -------
    None
    """
    fig  = legend.figure
    fig.canvas.draw()
    bbox  = legend.get_window_extent().transformed(fig.dpi_scale_trans.inverted())
    fig.savefig(filename, dpi="figure", bbox_inches=bbox)
