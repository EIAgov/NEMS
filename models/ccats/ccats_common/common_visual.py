"""Utility file containing common visualization functions used in CCATS.

Common Visual: Summary
______________________
This is a utility file containing visualization functions used in CCATS.


Common Visual: Input Files
__________________________
None


Common Visual: Model Functions and Class Methods
________________________________________________

* :func:`~ccats_common.common_visual.create_folium_map` - Map a CCATS solution.
* :func:`~ccats_common.common_visual.print_datatable_to_html` - Create an html data table


Common Visual: Output Debug Files
_________________________________

* :func:`~ccats_common.common_visual.create_folium_map` outputs a map in a user specified path and filename.
* :func:`~ccats_common.common_visual.print_datatable_to_html` outputs a table in a user specified path and filename.


Common Visual: Output Restart Variables
_______________________________________
None


Common Visual: Code
___________________
"""
import pandas as pd
import numpy as np
import folium
import sys
import os
from ccats_common import folium_pipeline_map as map

def create_folium_map(df, outdir, map_name = 'pipeline_map.html'):
    """Create a folium map.

    Parameters
    ----------
    
    df : DataFrame
       Dataframe with node, pipeline data, and co2 volume.

    outdir : str
        Path for saving the map.
    
    map_name : str
        Filename for the map.

   
    Returns
    -------

    None
    """
    # Instantiate folium map
    m = folium.Map(location=[40, -95],tiles="CartoDB Positron", zoom_start=4)
    map_name = outdir + map_name

    # Assign CCATS model parameters to map
    map.run(m, df, outdir, map_name)

    pass


def print_datatable_to_html(df, outfile, headear = None):
    """Print map input datatable to html format.

    Parameters
    ----------

    df : DataFrame
        Data to map.

    outdir : str
        Path for saving the map.
    
    map_name : str
        Filename for the map.

    
    Returns
    -------

    None
    """
    if os.path.exists(outfile):
        with open(outfile, 'w') as file:
            file.write("\n\n" + df.to_html())
    else:
        datatable = df.to_html(outfile)

    pass