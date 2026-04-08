"""Chart visualization module for HSM debugging.

This module provides a flexible system for generating charts from HSM restart variables
based on CSV configuration files. It supports multiple chart types (line, bar, stacked_area)
and various aggregation methods.

Summary
-------
The visualization system allows users to configure which charts are displayed after a run
via a CSV configuration file. Charts can be displayed in windows following an offline HSM run, saved to files, or both.

Functions
---------
    * generate_charts - Main entry point that reads config and generates charts
    * load_chart_config - Loads and validates chart configuration CSV
    * create_chart - Creates a single chart based on config
    * get_variable_data - Extracts and filters data from restart object
    * get_variable_metadata - Loads dimension info from variable_mapping
    * parse_filter_string - Parses filter strings like "1:4" or "1,2,3,4"
    * parse_year_range - Parses year range strings
    * create_line_chart - Creates a line chart
    * create_bar_chart - Creates a bar chart
    * create_stacked_area_chart - Creates a stacked area chart
    * add_value_annotations - Adds value annotations to charts
"""
import os
import logging
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from typing import Dict, List, Optional, Tuple, Union

# Suppress matplotlib font manager debug messages
logging.getLogger('matplotlib.font_manager').setLevel(logging.WARNING)
# Suppress PIL/Pillow debug messages
logging.getLogger('PIL.PngImagePlugin').setLevel(logging.WARNING)

logger = logging.getLogger('visualizations.py')


def generate_charts(hsm, config_path: str):
    """Main entry point that reads config and generates charts.
    
    Parameters
    ----------
    hsm : Module
        HSM module object containing restart data
    config_path : str
        Path to chart configuration CSV file
        
    Returns
    -------
    None
    """
    # Only show charts in standalone mode
    if hsm.integrated_switch == True:
        return
    
    # Check if config file exists
    if not os.path.exists(config_path):
        logger.warning(f'Chart configuration file not found: {config_path}')
        return
    
    try:
        # Load chart configuration
        chart_configs = load_chart_config(config_path)
        
        # Get variable mapping path from hsm
        var_mapping_path = getattr(hsm, 'variable_mapping_input_path', None)
        if var_mapping_path is None:
            # Try to construct it from integrated_input_path
            integrated_input_path = getattr(hsm, 'integrated_input_path', None)
            if integrated_input_path:
                var_mapping_path = os.path.join(integrated_input_path, 'variable_mapping')
            else:
                logger.warning('Could not determine variable_mapping_input_path')
                var_mapping_path = 'input/variable_mapping'
        
        # Generate each enabled chart
        for config in chart_configs:
            if not config.get('enabled', False):
                continue
            
            try:
                var_name = config['variable_name']
                var_metadata = get_variable_metadata(var_name, var_mapping_path)
                
                # If original_units not specified in config, populate from metadata
                if not config.get('original_units'):
                    config['original_units'] = var_metadata.get('units', '')
                
                create_chart(hsm, config, var_metadata, var_mapping_path)
            except Exception as e:
                logger.warning(f'Could not create chart for {config.get("variable_name", "unknown")}: {str(e)}')
                import traceback
                logger.warning(traceback.format_exc())
        
        logger.info('Chart generation completed')
        
        # Keep charts open - block until all windows are closed
        if any(config.get('display', True) for config in chart_configs if config.get('enabled', False)):
            plt.show()
        
    except Exception as e:
        logger.warning(f'Could not generate charts: {str(e)}')
        import traceback
        logger.warning(traceback.format_exc())


def load_chart_config(config_path: str) -> List[Dict]:
    """Loads and validates chart configuration CSV.
    
    Parameters
    ----------
    config_path : str
        Path to chart configuration CSV file
        
    Returns
    -------
    List[Dict]
        List of chart configuration dictionaries
    """
    try:
        df = pd.read_csv(config_path)
        
        # Validate required columns
        required_cols = ['variable_name', 'enabled', 'chart_type']
        missing_cols = [col for col in required_cols if col not in df.columns]
        if missing_cols:
            raise ValueError(f'Missing required columns: {missing_cols}')
        
        # Convert to list of dictionaries
        configs = []
        for _, row in df.iterrows():
            # Parse unit conversion (multiply factor)
            unit_conv = row.get('unit_conversion', '')
            if pd.notna(unit_conv) and str(unit_conv).strip():
                try:
                    unit_conv = float(unit_conv)
                except (ValueError, TypeError):
                    unit_conv = None
            else:
                unit_conv = None
            
            # Parse original_units
            original_units = row.get('original_units', '')
            if pd.notna(original_units):
                original_units = str(original_units).strip()
            else:
                original_units = ''
            
            # Helper function to convert filter values to string, handling float/int types from CSV
            def _convert_filter_value(val):
                if pd.isna(val):
                    return ''
                # Convert to string, handling float/int types (e.g., 3.0 -> "3")
                if isinstance(val, (int, float)):
                    # For integers stored as floats (e.g., 3.0), convert to int first
                    if isinstance(val, float) and val.is_integer():
                        return str(int(val))
                    return str(val)
                return str(val).strip()
            
            # Parse save_path - filter out boolean-like values and empty strings
            save_path_raw = row.get('save_path', '')
            if pd.notna(save_path_raw):
                save_path_str = str(save_path_raw).strip()
                # Check if it's a boolean-like string (TRUE, FALSE, true, false, etc.)
                if save_path_str.upper() in ('TRUE', 'FALSE', 'YES', 'NO', '1', '0'):
                    save_path = ''
                else:
                    save_path = save_path_str
            else:
                save_path = ''
            
            config = {
                'variable_name': str(row['variable_name']),
                'enabled': _parse_bool(row.get('enabled', False)),
                'chart_type': str(row.get('chart_type', 'line')),
                'aggregation': str(row.get('aggregation', 'total')),
                'filter_dim0': _convert_filter_value(row.get('filter_dim0', '')),
                'filter_dim1': _convert_filter_value(row.get('filter_dim1', '')),
                'filter_dim2': _convert_filter_value(row.get('filter_dim2', '')),
                'year_range': str(row.get('year_range', '2020:2050')) if pd.notna(row.get('year_range')) else '2020:2050',
                'title': str(row.get('title', '')) if pd.notna(row.get('title')) else '',
                'ylabel': str(row.get('ylabel', '')) if pd.notna(row.get('ylabel')) else '',
                'save_path': save_path,
                'display': _parse_bool(row.get('display', True)),
                'unit_conversion': unit_conv,
                'original_units': original_units
            }
            configs.append(config)
        
        return configs
        
    except Exception as e:
        logger.error(f'Error loading chart config: {str(e)}')
        raise


def _parse_bool(value) -> bool:
    """Parse boolean value from various formats."""
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        return value.lower() in ('true', '1', 'yes', 'on')
    return bool(value)


def get_variable_metadata(var_name: str, var_mapping_path: str) -> Dict:
    """Loads dimension info from variable_mapping.
    
    Parameters
    ----------
    var_name : str
        Variable name (e.g., 'ogsmout_ogqcrrep')
    var_mapping_path : str
        Path to variable_mapping directory
        
    Returns
    -------
    Dict
        Dictionary containing variable metadata:
        - dimension: Number of dimensions
        - index_names: List of index names
        - units: Units string
        - description: Variable description
    """
    master_table_path = os.path.join(var_mapping_path, 'master_table_selection.csv')
    
    if not os.path.exists(master_table_path):
        logger.warning(f'Master table not found: {master_table_path}')
        return {
            'dimension': 2,
            'index_names': [],
            'units': '',
            'description': var_name
        }
    
    try:
        master_df = pd.read_csv(master_table_path)
        # Try 'variable' first, then 'variable_name'
        var_col = 'variable' if 'variable' in master_df.columns else 'variable_name'
        var_row = master_df[master_df[var_col] == var_name]
        
        if var_row.empty:
            logger.warning(f'Variable {var_name} not found in master table')
            return {
                'dimension': 2,
                'index_names': [],
                'units': '',
                'description': var_name
            }
        
        var_row = var_row.iloc[0]
        dimension = int(var_row.get('dimension', 2))
        
        index_names = []
        for i in range(dimension):
            idx_name = var_row.get(f'index_{i}', '')
            if pd.notna(idx_name) and idx_name:
                index_names.append(str(idx_name))
        
        units = str(var_row.get('UNITS', '')) if pd.notna(var_row.get('UNITS')) else ''
        description = str(var_row.get('description', var_name)) if pd.notna(var_row.get('description')) else var_name
        
        return {
            'dimension': dimension,
            'index_names': index_names,
            'units': units,
            'description': description
        }
        
    except Exception as e:
        logger.warning(f'Error loading variable metadata: {str(e)}')
        return {
            'dimension': 2,
            'index_names': [],
            'units': '',
            'description': var_name
        }


def get_dimension_labels(index_name: str, var_mapping_path: str) -> Dict[int, str]:
    """Get labels for dimension values from parameter reference tables.
    
    Parameters
    ----------
    index_name : str
        Name of the index (e.g., 'nogcro')
    var_mapping_path : str
        Path to variable_mapping directory
        
    Returns
    -------
    Dict[int, str]
        Dictionary mapping index values to labels
    """
    if not index_name:
        return {}
    
    ref_table_path = os.path.join(var_mapping_path, 'parameter_ref_tables', f'{index_name}.csv')
    
    if not os.path.exists(ref_table_path):
        return {}
    
    try:
        ref_df = pd.read_csv(ref_table_path)
        labels = {}
        
        # Try to find the index column (could be named after the index or 'Index')
        index_col = None
        if index_name in ref_df.columns:
            index_col = index_name
        elif 'Index' in ref_df.columns:
            index_col = 'Index'
        elif len(ref_df.columns) > 0:
            index_col = ref_df.columns[0]
        
        # Try to find the description column
        desc_col = None
        if 'Description' in ref_df.columns:
            desc_col = 'Description'
        elif 'Category' in ref_df.columns:
            desc_col = 'Category'
        elif 'Region' in ref_df.columns:
            desc_col = 'Region'
        elif len(ref_df.columns) > 1:
            desc_col = ref_df.columns[1]
        
        if index_col and desc_col:
            for _, row in ref_df.iterrows():
                try:
                    idx_val = int(row[index_col])
                    label = str(row[desc_col]) if pd.notna(row[desc_col]) else str(idx_val)
                    labels[idx_val] = label
                except (ValueError, KeyError):
                    continue
        
        return labels
        
    except Exception as e:
        logger.warning(f'Error loading dimension labels for {index_name}: {str(e)}')
        return {}


def parse_filter_string(filter_str: str) -> List[int]:
    """Parses filter strings like "1:4" or "1,2,3,4" or "1:10,15".
    
    Parameters
    ----------
    filter_str : str
        Filter string (e.g., "1:4" or "1,2,3,4" or "1:10,15")
        
    Returns
    -------
    List[int]
        List of integer values
    """
    if not filter_str or not filter_str.strip():
        return []
    
    filter_str = filter_str.strip()
    result = []
    
    # Split by comma first to handle mixed syntax like "1:10,15"
    parts = filter_str.split(',')
    
    for part in parts:
        part = part.strip()
        if not part:
            continue
        
        # Handle range syntax (e.g., "1:4")
        if ':' in part:
            range_parts = part.split(':')
            if len(range_parts) == 2:
                try:
                    start = int(range_parts[0].strip())
                    end = int(range_parts[1].strip())
                    result.extend(list(range(start, end + 1)))
                except ValueError:
                    continue
        else:
            # Single value
            try:
                result.append(int(part))
            except ValueError:
                continue
    
    # Remove duplicates and sort
    return sorted(list(set(result)))


def parse_year_range(year_range_str: str) -> List[int]:
    """Parses year range strings.
    
    Parameters
    ----------
    year_range_str : str
        Year range string (e.g., "2020:2050" or "2020,2025,2030,2040,2050")
        
    Returns
    -------
    List[int]
        List of years
    """
    return parse_filter_string(year_range_str)


def get_variable_data(hsm, var_name: str, filters: Dict, year_range: List[int]) -> pd.DataFrame:
    """Extracts and filters data from restart object.
    
    Parameters
    ----------
    hsm : Module
        HSM module object
    var_name : str
        Variable name (e.g., 'ogsmout_ogqcrrep')
    filters : Dict
        Dictionary with keys 'dim0', 'dim1', 'dim2' containing filter lists
    year_range : List[int]
        List of years to include
        
    Returns
    -------
    pd.DataFrame
        Filtered and processed data
    """
    # Get variable from restart object
    if not hasattr(hsm.restart, var_name):
        raise ValueError(f'Variable {var_name} not found in restart object')
    
    var_df = getattr(hsm.restart, var_name).copy()
    
    if var_df.empty:
        raise ValueError(f'Variable {var_name} is empty')
    
    # Apply dimension filters
    # Note: filters are applied sequentially, so we check nlevels at each step
    if filters.get('dim0'):
        dim0_values = filters['dim0']
        if var_df.index.nlevels > 0:
            var_df = var_df[var_df.index.get_level_values(0).isin(dim0_values)]
    
    if filters.get('dim1'):
        dim1_values = filters['dim1']
        if var_df.index.nlevels > 1:
            var_df = var_df[var_df.index.get_level_values(1).isin(dim1_values)]
    
    if filters.get('dim2'):
        dim2_values = filters['dim2']
        if var_df.index.nlevels > 2:
            var_df = var_df[var_df.index.get_level_values(2).isin(dim2_values)]
    
    # Extract 'value' column if it exists
    if 'value' in var_df.columns:
        value_series = var_df['value']
    else:
        # If no 'value' column, assume the DataFrame itself is the values
        value_series = var_df.iloc[:, 0] if len(var_df.columns) > 0 else var_df
    
    # Filter by year range (year is typically the last index level)
    if year_range:
        year_level = var_df.index.nlevels - 1
        year_values = var_df.index.get_level_values(year_level)
        year_mask = year_values.isin(year_range)
        var_df = var_df[year_mask]
        value_series = value_series[year_mask]
    
    return var_df, value_series


def create_chart(hsm, chart_config: Dict, var_metadata: Dict, var_mapping_path: str):
    """Creates a single chart based on config.
    
    Parameters
    ----------
    hsm : Module
        HSM module object
    chart_config : Dict
        Chart configuration dictionary
    var_metadata : Dict
        Variable metadata dictionary
    var_mapping_path : str
        Path to variable_mapping directory
    """
    var_name = chart_config['variable_name']
    chart_type = chart_config['chart_type']
    aggregation = chart_config['aggregation']
    
    # Parse filters
    filters = {
        'dim0': parse_filter_string(chart_config.get('filter_dim0', '')),
        'dim1': parse_filter_string(chart_config.get('filter_dim1', '')),
        'dim2': parse_filter_string(chart_config.get('filter_dim2', ''))
    }
    
    # Parse year range
    year_range = parse_year_range(chart_config.get('year_range', '2020:2050'))
    
    # Get data
    try:
        var_df, value_series = get_variable_data(hsm, var_name, filters, year_range)
        
        # Debug: Log filter application for troubleshooting
        if any(filters.values()):
            logger.info(f'Applied filters for {var_name}: dim0={filters.get("dim0")}, dim1={filters.get("dim1")}, dim2={filters.get("dim2")}')
            logger.info(f'Filtered data shape: {value_series.shape}, unique dim0 values: {sorted(value_series.index.get_level_values(0).unique().tolist()) if value_series.index.nlevels > 0 else "N/A"}')
            if value_series.index.nlevels > 1:
                logger.info(f'Unique dim1 values after filter: {sorted(value_series.index.get_level_values(1).unique().tolist())}')
    except Exception as e:
        logger.warning(f'Could not extract data for {var_name}: {str(e)}')
        return
    
    # Get dimension labels
    dim_labels = {}
    for i, idx_name in enumerate(var_metadata.get('index_names', [])):
        labels = get_dimension_labels(idx_name, var_mapping_path)
        dim_labels[i] = labels
    
    # Apply unit conversion if specified
    unit_conversion = chart_config.get('unit_conversion')
    if unit_conversion is not None:
        value_series = value_series * unit_conversion
    
    # Create chart based on type
    if chart_type == 'line':
        create_line_chart(var_df, value_series, chart_config, var_metadata, dim_labels, aggregation)
    elif chart_type == 'bar':
        create_bar_chart(var_df, value_series, chart_config, var_metadata, dim_labels, aggregation)
    elif chart_type == 'stacked_area':
        create_stacked_area_chart(var_df, value_series, chart_config, var_metadata, dim_labels, aggregation)
    else:
        logger.warning(f'Unknown chart type: {chart_type}')


def prepare_chart_data(value_series: pd.Series, aggregation: str, var_metadata: Dict, dim_labels: Dict) -> Tuple[List[int], Union[pd.Series, pd.DataFrame], Dict]:
    """Prepares data for charting based on aggregation method.
    
    Parameters
    ----------
    value_series : pd.Series
        Series with MultiIndex and values
    aggregation : str
        Aggregation method: 'total', 'by_dim0', 'by_dim1', 'by_dim2'
    var_metadata : Dict
        Variable metadata
    dim_labels : Dict
        Dictionary of dimension labels
        
    Returns
    -------
    Tuple[List[int], Union[pd.Series, pd.DataFrame], Dict]
        (years, data, series_labels)
    """
    # Get year level (last level of index)
    year_level = value_series.index.nlevels - 1
    years = sorted(value_series.index.get_level_values(year_level).unique().tolist())
    
    if aggregation == 'total':
        # Sum across all dimensions except year
        unstacked = value_series.unstack(level=year_level)
        total = unstacked.sum(axis=0)
        total = total.reindex(years)
        return years, total, {}
    
    elif aggregation == 'by_dim0':
        # Group by first dimension
        dim0_level = 0
        unstacked = value_series.unstack(level=year_level)
        grouped = unstacked.groupby(level=dim0_level).sum()
        
        # Create DataFrame with years as columns
        data_dict = {}
        series_labels = {}
        for dim0_val in grouped.index:
            series_data = grouped.loc[dim0_val].reindex(years)
            data_dict[dim0_val] = series_data
            # Get label for this dimension value
            label = dim_labels.get(0, {}).get(dim0_val, f'Dim0={dim0_val}')
            series_labels[dim0_val] = label
        
        data_df = pd.DataFrame(data_dict)
        return years, data_df, series_labels
    
    elif aggregation == 'by_dim1' and value_series.index.nlevels > 2:
        # Group by second dimension
        dim1_level = 1
        unstacked = value_series.unstack(level=year_level)
        grouped = unstacked.groupby(level=dim1_level).sum()
        
        data_dict = {}
        series_labels = {}
        for dim1_val in grouped.index:
            series_data = grouped.loc[dim1_val].reindex(years)
            data_dict[dim1_val] = series_data
            label = dim_labels.get(1, {}).get(dim1_val, f'Dim1={dim1_val}')
            series_labels[dim1_val] = label
        
        data_df = pd.DataFrame(data_dict)
        return years, data_df, series_labels
    
    elif aggregation == 'by_dim2' and value_series.index.nlevels > 3:
        # Group by third dimension
        dim2_level = 2
        unstacked = value_series.unstack(level=year_level)
        grouped = unstacked.groupby(level=dim2_level).sum()
        
        data_dict = {}
        series_labels = {}
        for dim2_val in grouped.index:
            series_data = grouped.loc[dim2_val].reindex(years)
            data_dict[dim2_val] = series_data
            label = dim_labels.get(2, {}).get(dim2_val, f'Dim2={dim2_val}')
            series_labels[dim2_val] = label
        
        data_df = pd.DataFrame(data_dict)
        return years, data_df, series_labels
    
    else:
        # Default to total
        unstacked = value_series.unstack(level=year_level)
        total = unstacked.sum(axis=0)
        total = total.reindex(years)
        return years, total, {}


# Maximum length for single-line titles before splitting to multi-line
MAX_SINGLE_LINE_TITLE = 80


def build_chart_title(chart_config: Dict, var_metadata: Dict, dim_labels: Dict) -> str:
    """Builds a comprehensive chart title with filter information and other details.
    Returns multi-line title if too long, keeping base title on first line.
    
    Parameters
    ----------
    chart_config : Dict
        Chart configuration dictionary
    var_metadata : Dict
        Variable metadata dictionary
    dim_labels : Dict
        Dictionary of dimension labels (maps dimension index to {value: label} dict)
        
    Returns
    -------
    str
        Complete chart title (may contain \n for multi-line)
    """
    # Start with base title
    base_title = chart_config.get('title', '') or f'{var_metadata.get("description", chart_config["variable_name"])}'
    
    # Get variable short name
    var_name = chart_config['variable_name']
    if '_' in var_name:
        var_short_name = var_name.split('_', 1)[1]  # Get everything after first underscore
    else:
        var_short_name = var_name
    
    # Parse filters
    filters = {
        'dim0': parse_filter_string(chart_config.get('filter_dim0', '')),
        'dim1': parse_filter_string(chart_config.get('filter_dim1', '')),
        'dim2': parse_filter_string(chart_config.get('filter_dim2', ''))
    }
    
    # Build condensed filter information string
    filter_parts = []
    index_names = var_metadata.get('index_names', [])
    
    for dim_idx in [0, 1, 2]:
        filter_key = f'dim{dim_idx}'
        filter_values = filters.get(filter_key, [])
        
        # Debug: log filter values to help diagnose issues
        if filter_values:
            logger.debug(f'Processing filter dim{dim_idx}: values={filter_values}, index_name={index_names[dim_idx] if dim_idx < len(index_names) else "N/A"}')
        
        if filter_values:
            # Get dimension name (use index name if available, otherwise use dim0/dim1/dim2)
            if dim_idx < len(index_names) and index_names[dim_idx]:
                dim_name = index_names[dim_idx]
            else:
                dim_name = f'dim{dim_idx}'
            
            # Convert filter values to labels if available
            label_parts = []
            for val in sorted(filter_values):
                # Try to get human-readable label
                if dim_idx in dim_labels and val in dim_labels[dim_idx]:
                    label_parts.append(dim_labels[dim_idx][val])
                else:
                    # Fall back to numeric value if label not found
                    label_parts.append(str(val))
                    # Debug: log when label not found
                    if dim_idx in dim_labels:
                        logger.debug(f'Label not found for dim{dim_idx} value {val}. Available labels: {list(dim_labels[dim_idx].keys())}')
                    else:
                        logger.debug(f'No labels loaded for dim{dim_idx}')
            
            # Format: always use "dimension name: value" for clarity
            if len(label_parts) == 1:
                # Single value: shorten common labels but always show dimension name
                label = label_parts[0]
                if "Tertiary Recovery" in label or "EOR" in label:
                    label = "EOR"
                elif "Primary" in label:
                    label = "Primary"
                elif "Conventional" in label:
                    label = "Conventional"
                elif "Tight gas" in label or "Tight" in label:
                    label = "Tight"
                elif "Shale gas" in label or "Shale" in label:
                    label = "Shale"
                elif "Coalbed Methane" in label or "CBM" in label:
                    label = "CBM"
                # Always include dimension name for single values: "dimension name: value"
                filter_parts.append(f"{dim_name}: {label}")
            elif len(label_parts) <= 3:
                # Few values: show compact list with dimension name
                # If all are numeric, show as range if consecutive
                try:
                    numeric_vals = [int(v) for v in filter_values]
                    if max(numeric_vals) - min(numeric_vals) == len(numeric_vals) - 1:
                        filter_parts.append(f"{dim_name}: {min(numeric_vals)}-{max(numeric_vals)}")
                    else:
                        filter_parts.append(f"{dim_name}: {', '.join(map(str, sorted(numeric_vals)))}")
                except (ValueError, TypeError):
                    # Non-numeric labels
                    filter_parts.append(f"{dim_name}: {', '.join(label_parts)}")
            else:
                # Many values: show range if consecutive, otherwise count
                try:
                    numeric_vals = [int(v) for v in filter_values]
                    if max(numeric_vals) - min(numeric_vals) == len(numeric_vals) - 1:
                        filter_parts.append(f"{dim_name}: {min(numeric_vals)}-{max(numeric_vals)}")
                    else:
                        filter_parts.append(f"{dim_name}: {len(label_parts)} values")
                except (ValueError, TypeError):
                    filter_parts.append(f"{dim_name}: {len(label_parts)} values")
    
    # Build condensed metadata line (filters, aggregation, unit conversion)
    metadata_parts = []
    
    # Add condensed filter information
    if filter_parts:
        metadata_parts.append(', '.join(filter_parts))
    
    # Add condensed aggregation method if not 'total'
    aggregation = chart_config.get('aggregation', 'total')
    if aggregation != 'total':
        agg_dim_map = {'by_dim0': 0, 'by_dim1': 1, 'by_dim2': 2}
        if aggregation in agg_dim_map:
            agg_dim_idx = agg_dim_map[aggregation]
            if agg_dim_idx < len(index_names) and index_names[agg_dim_idx]:
                agg_dim_name = index_names[agg_dim_idx]
            else:
                agg_dim_name = f'dim{agg_dim_idx}'
            metadata_parts.append(f"by {agg_dim_name}")
    
    # Add unit conversion note if conversion was applied
    unit_conversion = chart_config.get('unit_conversion')
    # Use original_units from config first, fall back to variable metadata
    original_units = chart_config.get('original_units', '') or var_metadata.get('units', '')
    if unit_conversion is not None and original_units:
        metadata_parts.append(f"(from {original_units})")
    
    # Build title with variable name
    line1 = f"{base_title} [{var_short_name}]"
    line2 = " | ".join(metadata_parts) if metadata_parts else ""
    
    # Check if we need multi-line
    single_line_title = f"{line1}"
    if line2:
        single_line_title += f" | {line2}"
    
    # If single line is too long, split into two lines
    if len(single_line_title) > MAX_SINGLE_LINE_TITLE and line2:
        return f"{line1}\n{line2}"
    else:
        return single_line_title


def add_value_annotations(ax, years_list: List[int], values_series: Union[pd.Series, np.ndarray], 
                         label_years: List[int] = [2025, 2030, 2040, 2050]):
    """Add text annotations showing values for specific years.
    
    Parameters
    ----------
    ax : matplotlib.axes.Axes
        Axes object to annotate
    years_list : List[int]
        List of years
    values_series : Union[pd.Series, np.ndarray]
        Series or array of values
    label_years : List[int]
        Years to annotate
    """
    for label_year in label_years:
        if label_year in years_list:
            idx = years_list.index(label_year)
            if hasattr(values_series, 'iloc'):
                value = values_series.iloc[idx]
            elif hasattr(values_series, '__getitem__'):
                value = values_series[idx]
            else:
                continue
            
            ax.annotate(f'{label_year}: {value:.2f}',
                       xy=(label_year, value),
                       xytext=(10, 10), textcoords='offset points',
                       bbox=dict(boxstyle='round,pad=0.3', facecolor='yellow', alpha=0.7),
                       fontsize=9, fontweight='bold')


def create_line_chart(var_df: pd.DataFrame, value_series: pd.Series, chart_config: Dict, 
                     var_metadata: Dict, dim_labels: Dict, aggregation: str):
    """Creates a line chart.
    
    Parameters
    ----------
    var_df : pd.DataFrame
        Variable DataFrame
    value_series : pd.Series
        Value series
    chart_config : Dict
        Chart configuration
    var_metadata : Dict
        Variable metadata
    dim_labels : Dict
        Dimension labels
    aggregation : str
        Aggregation method
    """
    years, data, series_labels = prepare_chart_data(value_series, aggregation, var_metadata, dim_labels)
    
    if len(years) == 0:
        logger.warning(f'No years found for {chart_config["variable_name"]}')
        return
    
    # Create figure
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Build comprehensive title with filter info
    title = build_chart_title(chart_config, var_metadata, dim_labels)
    ylabel = chart_config.get('ylabel', '') or var_metadata.get('units', '')
    
    # Plot data
    if isinstance(data, pd.Series):
        # Single series
        ax.plot(years, data.values, 'b-', linewidth=2, marker='o', markersize=3)
        add_value_annotations(ax, years, data)
    elif isinstance(data, pd.DataFrame):
        # Multiple series
        for col in data.columns:
            label = series_labels.get(col, f'Series {col}')
            ax.plot(years, data[col].values, label=label, marker='.', markersize=4)
        
        if len(data.columns) > 1:
            ax.legend(loc='best', fontsize=8)
        
        # Add annotations for total if multiple series
        if len(data.columns) > 1:
            total = data.sum(axis=1)
            add_value_annotations(ax, years, total)
    
    ax.set_title(title, fontsize=12, fontweight='bold')
    ax.set_xlabel('Year')
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    
    # Save if path provided (skip if empty or boolean-like)
    save_path = chart_config.get('save_path', '')
    if save_path and save_path.strip() and save_path.strip().upper() not in ('TRUE', 'FALSE', 'YES', 'NO', '1', '0'):
        os.makedirs(os.path.dirname(save_path) if os.path.dirname(save_path) else '.', exist_ok=True)
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        logger.info(f'Saved chart to {save_path}')
    
    # Display if requested
    if chart_config.get('display', True):
        plt.show(block=False)
    else:
        plt.close()


def create_bar_chart(var_df: pd.DataFrame, value_series: pd.Series, chart_config: Dict,
                     var_metadata: Dict, dim_labels: Dict, aggregation: str):
    """Creates a bar chart.
    
    Parameters
    ----------
    var_df : pd.DataFrame
        Variable DataFrame
    value_series : pd.Series
        Value series
    chart_config : Dict
        Chart configuration
    var_metadata : Dict
        Variable metadata
    dim_labels : Dict
        Dimension labels
    aggregation : str
        Aggregation method
    """
    years, data, series_labels = prepare_chart_data(value_series, aggregation, var_metadata, dim_labels)
    
    if len(years) == 0:
        logger.warning(f'No years found for {chart_config["variable_name"]}')
        return
    
    fig, ax = plt.subplots(figsize=(12, 6))
    
    # Build comprehensive title with filter info
    title = build_chart_title(chart_config, var_metadata, dim_labels)
    ylabel = chart_config.get('ylabel', '') or var_metadata.get('units', '')
    
    if isinstance(data, pd.Series):
        # Single series - simple bar chart
        ax.bar(years, data.values, alpha=0.7, width=0.8)
        add_value_annotations(ax, years, data)
    elif isinstance(data, pd.DataFrame):
        # Multiple series - grouped bars
        n_series = len(data.columns)
        width = 0.8 / n_series  # Width of each bar group
        x_pos = np.arange(len(years))
        
        for i, col in enumerate(data.columns):
            offset = (i - n_series/2 + 0.5) * width
            label = series_labels.get(col, f'Series {col}')
            ax.bar(x_pos + offset, data[col].values, width=width, 
                   label=label, alpha=0.7)
        
        ax.set_xticks(x_pos)
        ax.set_xticklabels(years)
        
        if len(data.columns) > 1:
            ax.legend(loc='best', fontsize=8)
        
        # Add annotations for total if multiple series
        if len(data.columns) > 1:
            total = data.sum(axis=1)
            add_value_annotations(ax, years, total)
    else:
        # Fallback
        ax.bar(years, data.values if hasattr(data, 'values') else data, alpha=0.7)
    
    ax.set_title(title, fontsize=12, fontweight='bold')
    ax.set_xlabel('Year')
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.3, axis='y')
    
    plt.tight_layout()
    
    save_path = chart_config.get('save_path', '')
    if save_path:
        os.makedirs(os.path.dirname(save_path) if os.path.dirname(save_path) else '.', exist_ok=True)
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        logger.info(f'Saved chart to {save_path}')
    
    if chart_config.get('display', True):
        plt.show(block=False)
    else:
        plt.close()


def create_stacked_area_chart(var_df: pd.DataFrame, value_series: pd.Series, chart_config: Dict,
                              var_metadata: Dict, dim_labels: Dict, aggregation: str):
    """Creates a stacked area chart.
    
    Parameters
    ----------
    var_df : pd.DataFrame
        Variable DataFrame
    value_series : pd.Series
        Value series
    chart_config : Dict
        Chart configuration
    var_metadata : Dict
        Variable metadata
    dim_labels : Dict
        Dimension labels
    aggregation : str
        Aggregation method
    """
    years, data, series_labels = prepare_chart_data(value_series, aggregation, var_metadata, dim_labels)
    
    if len(years) == 0:
        logger.warning(f'No years found for {chart_config["variable_name"]}')
        return
    
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Build comprehensive title with filter info
    title = build_chart_title(chart_config, var_metadata, dim_labels)
    ylabel = chart_config.get('ylabel', '') or var_metadata.get('units', '')
    
    if isinstance(data, pd.Series):
        # Single series - just use area chart
        # Convert to numeric and handle NaN/inf
        data_values = pd.to_numeric(data.values, errors='coerce').astype(float)
        data_values = np.nan_to_num(data_values, nan=0.0, posinf=0.0, neginf=0.0)
        years_numeric = np.array(years, dtype=float)
        
        ax.fill_between(years_numeric, data_values, alpha=0.5)
        ax.plot(years_numeric, data_values, linewidth=2)
        add_value_annotations(ax, years, pd.Series(data_values, index=years))
    elif isinstance(data, pd.DataFrame):
        # Stacked area - convert all columns to numeric and handle NaN/inf
        years_numeric = np.array(years, dtype=float)
        stack_data = []
        
        for col in data.columns:
            col_values = pd.to_numeric(data[col].values, errors='coerce').astype(float)
            col_values = np.nan_to_num(col_values, nan=0.0, posinf=0.0, neginf=0.0)
            stack_data.append(col_values)
        
        ax.stackplot(years_numeric, *stack_data,
                     labels=[series_labels.get(col, f'Series {col}') for col in data.columns],
                     alpha=0.7)
        if len(data.columns) > 1:
            ax.legend(loc='best', fontsize=8)
        total = pd.Series([sum(vals) for vals in zip(*stack_data)], index=years)
        add_value_annotations(ax, years, total)
    
    ax.set_title(title, fontsize=12, fontweight='bold')
    ax.set_xlabel('Year')
    ax.set_ylabel(ylabel)
    ax.grid(True, alpha=0.3)
    
    plt.tight_layout()
    
    save_path = chart_config.get('save_path', '')
    if save_path:
        os.makedirs(os.path.dirname(save_path) if os.path.dirname(save_path) else '.', exist_ok=True)
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        logger.info(f'Saved chart to {save_path}')
    
    # Display if requested
    if chart_config.get('display', True):
        plt.show(block=False)
    else:
        plt.close()

