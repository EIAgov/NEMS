"""
Created on Oct 20 2025

Export NEMS run data from SQLite DB file.

@author: gregory.miller
"""
import argparse
import sqlite3
import pandas as pd
from datetime import datetime, timedelta
import re
import sys
import os

def parse_datekey(date_key_str):
    """
    Parses a datekey string (e.g., 'dMMDDYYx') into a datetime.date object.
    Assumes years from 2000-2099 for 'YY' format.
    """
    if not isinstance(date_key_str, str):
        return None
    match = re.match(r'd(\d{2})(\d{2})(\d{2})\w*', date_key_str)
    if match:
        month, day, year = match.groups()
        year_exp = 2000 + int(year) # Assuming 2000s for YY format
        try:
            return datetime(year_exp, int(month), int(day)).date()
        except ValueError:
            return None
    return None

def generate_html_from_dataframe(df, output_path, max_height='800px'):
    """
    Generates an HTML file from a pandas DataFrame, retaining only the main data table
    with sorting and filtering functionality.
    """
    generation_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    html_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>Historical Run Information</title> <!-- Changed title -->
    <style>
        table {{
            width: 100%;
            border-collapse: collapse;
        }}
        table, th, td {{
            border: 1px solid black;
        }}
        th, td {{
            padding: 8px;
            text-align: left;
        }}
        th {{
            background-color: #4CAF50; /* Green */
            color: white;
            position: sticky;
            top: 0;
            z-index: 2;
            cursor: pointer;
        }}
        th.sort-asc::after {{
            content: ' \\2191';
        }}
        th.sort-desc::after {{
            content: ' \\2193';
        }}
        td {{
            background-color: #f5f5dc; /* Light tan color */
        }}
        h2 {{
            text-align: center;
            font-weight: bold;
        }}
        h3 {{
            text-align: center;
            font-weight: bold;
            margin-top: 30px;
        }}
        input[type="text"] {{
            margin-bottom: 10px;
            padding: 5px;
            width: 200px;
        }}
        .table-wrapper {{
            max-height: {max_height};
            overflow-y: auto;
            display: block;
            margin-bottom: 20px;
        }}
        .copy-to-clipboard {{
            cursor: pointer;
            position: relative;
            text-decoration: underline;
            color: #0000EE;
        }}
        .copy-to-clipboard:hover {{
            color: #551A8B;
        }}
        .copy-to-clipboard::after {{
            content: '\\1F4CB';
            position: absolute;
            right: 5px;
            top: 50%;
            transform: translateY(-50%);
            font-size: 0.8em;
            opacity: 0;
            transition: opacity .2s ease-in-out;
        }}
        .copy-to-clipboard:hover::after {{
            opacity: 1;
        }}
    </style>

    <script>
        function filterTable(tableId, inputId) {{
            var input, filter, table, tr, td, i, j;
            input = document.getElementById(inputId);
            filter = input.value.toUpperCase();
            table = document.getElementById(tableId);
            tr = table.getElementsByTagName("tr");
            for (i = 1; i < tr.length; i++) {{ // Start from 1 to skip header row
                var rowFound = false;
                td = tr[i].getElementsByTagName("td");
                for (j = 0; j < td.length; j++) {{
                    if (td[j]) {{
                        if (td[j].innerText.toUpperCase().indexOf(filter) > -1) {{
                            rowFound = true;
                            break;
                        }}
                    }}
                }}
                tr[i].style.display = rowFound ? "" : "none";
            }}
        }}

        function sortTable(n, tableId) {{
            var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
            table = document.getElementById(tableId);
            switching = true;
            dir = "asc";
            while (switching) {{
                switching = false;
                rows = table.rows;
                for (i = 1; i < (rows.length - 1); i++) {{
                    shouldSwitch = false;
                    x = rows[i].getElementsByTagName("TD")[n];
                    y = rows[i + 1].getElementsByTagName("TD")[n];
                    // Handle numeric sorting for appropriate columns
                    var xContent = x.innerHTML.toLowerCase();
                    var yContent = y.innerHTML.toLowerCase();

                    if (!isNaN(xContent) && !isNaN(yContent) && xContent !== '' && yContent !== '') {{
                        if (dir == "asc") {{
                            if (parseFloat(xContent) > parseFloat(yContent)) {{
                                shouldSwitch = true;
                                break;
                            }}
                        }} else if (dir == "desc") {{
                            if (parseFloat(xContent) < parseFloat(yContent)) {{
                                shouldSwitch = true;
                                break;
                            }}
                        }}
                    }} else {{ // Default to string sorting
                        if (dir == "asc") {{
                            if (xContent > yContent) {{
                                shouldSwitch = true;
                                break;
                            }}
                        }} else if (dir == "desc") {{
                            if (xContent < yContent) {{
                                shouldSwitch = true;
                                break;
                            }}
                        }}
                    }}
                }}
                if (shouldSwitch) {{
                    rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
                    switching = true;
                    switchcount ++;
                }} else {{
                    if (switchcount == 0 && dir == "asc") {{
                        dir = "desc";
                        switching = true;
                    }}
                }}
            }}
            updateSortIndicator(n, dir, tableId);
        }}

        function updateSortIndicator(columnIndex, direction, tableId) {{
            var headers = document.querySelectorAll("#" + tableId + " th");
            headers.forEach((th, index) => {{
                th.classList.remove("sort-asc", "sort-desc");
                if (index === columnIndex) {{
                    th.classList.add(direction === "asc" ? "sort-asc" : "sort-desc");
                }}
            }});
        }}

        function copyOutputDirectory(cellElement) {{
            const directoryPath = cellElement.dataset.path;
            navigator.clipboard.writeText(directoryPath)
                .then(() => {{
                    console.log('Output directory copied to clipboard:', directoryPath);
                    const originalText = cellElement.innerText;
                    cellElement.innerText = 'Copied!';
                    setTimeout(() => {{
                        cellElement.innerText = originalText;
                    }}, 1500);
                }})
                .catch(err => {{
                    console.error('Failed to copy output directory:', err);
                    alert('Failed to copy output directory. Please copy manually.');
                }});
        }}

        document.addEventListener("DOMContentLoaded", function() {{
            updateSortIndicator(0, "asc", "runStatusTable"); // Apply initial sort indicator
        }});

    </script>
</head>
<body>

<h2>Historical Run Information</h2> <!-- Changed header -->

<p>Generated: {generation_time}</p>
<p>Currently showing {len(df)} rows.</p>
<input type="text" id="userInput" onkeyup="filterTable('runStatusTable', 'userInput')" placeholder="Filter rows by search term">

<div class="table-wrapper">
    <table id="runStatusTable">
        <thead>
            <tr>"""

    for i, column in enumerate(df.columns):
        html_content += f"<th onclick='sortTable({i}, \"runStatusTable\")'>{column}</th>"

    html_content += "</tr></thead><tbody>"

    for _, row in df.iterrows():
        html_content += "<tr>"
        for j, value in enumerate(row):
            col_name = df.columns[j]
            if col_name == 'Output Directory':
                html_content += f"<td class='copy-to-clipboard' onclick='copyOutputDirectory(this)' data-path='{value}'>{value}</td>"
            elif col_name == 'Work Directory':
                html_content += f"<td class='copy-to-clipboard' onclick='copyOutputDirectory(this)' data-path='{value}'>{value}</td>"
            else:
                html_content += f"<td>{value}</td>"
        html_content += "</tr>"

    html_content += """
        </tbody>
    </table>
</div>

</body>
</html>"""

    with open(output_path, 'w', encoding='utf-8') as file:
        file.write(html_content)

def get_start_of_week(date_obj):
    """
    Calculates the start of the current week (Sunday at midnight) for a given date.
    """
    days_since_sunday = (date_obj.weekday() + 1) % 7
    start_of_week = date_obj - timedelta(days=days_since_sunday)
    return start_of_week.replace(hour=0, minute=0, second=0, microsecond=0)

def get_start_of_month(date_obj):
    """
    Calculates the start of the current month (first day at midnight) for a given date.
    """
    return date_obj.replace(day=1, hour=0, minute=0, second=0, microsecond=0)

def parse_date_argument(date_str):
    """
    Parses a date string into a datetime object. Tries common US formats.
    """
    formats = [
        "%m-%d-%Y", "%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d", # Month-Day-Year, Year-Month-Day
        "%m-%d-%y", "%m/%d/%y"                          # Two-digit year
    ]
    for fmt in formats:
        try:
            return datetime.strptime(date_str, fmt)
        except ValueError:
            pass
    raise ValueError(f"Unable to parse date: '{date_str}'. Please use a common format like MM-DD-YYYY.")

def confirm_large_export(row_count, output_format):
    """Prompts user to confirm if the export is large."""
    if row_count > 100:
        print(f"Warning: You are about to export {row_count} rows in {output_format} format.")
        response = input("This may create a large file. Do you want to proceed? (Y/n)? ").strip().lower()
        if response == 'n':
            print("Export cancelled by user.")
            sys.exit(0)
    return True

def parse_time_elapsed_string(time_str):
    """Parses 'X day(s), HH:MM:SS' or 'HH:MM:SS' into a timedelta."""
    if not isinstance(time_str, str):
        return timedelta(0)

    days = 0
    hours = 0
    minutes = 0
    seconds = 0

    day_match = re.match(r'(\d+) day(?:s)?, (\d{2}):(\d{2}):(\d{2})', time_str)
    if day_match:
        days = int(day_match.group(1))
        hours = int(day_match.group(2))
        minutes = int(day_match.group(3))
        seconds = int(day_match.group(4))
    else:
        time_match = re.match(r'(\d{2}):(\d{2}):(\d{2})', time_str)
        if time_match:
            hours = int(time_match.group(1))
            minutes = int(time_match.group(2))
            seconds = int(time_match.group(3))

    return timedelta(days=days, hours=hours, minutes=minutes, seconds=seconds).total_seconds()


def export_data(db_file, table_name, start_date=None, end_date=None,
                username=None, scenario=None, status_filter=None,
                export_all=False, output_format='html', output_file_base='export_report'):
    """
    Exports data from the SQLite database based on filters and output format.
    """
    conn = None
    try:
        conn = sqlite3.connect(db_file)

        where_clauses = []
        params = []

        if start_date:
            where_clauses.append('"Time Submitted" >= ?')
            params.append(start_date.strftime('%Y-%m-%d %H:%M:%S'))

        if end_date:
            end_of_day = end_date.replace(hour=23, minute=59, second=59, microsecond=999999)
            where_clauses.append('"Time Submitted" <= ?')
            params.append(end_of_day.strftime('%Y-%m-%d %H:%M:%S.%f'))

        if username:
            where_clauses.append('"User ID" = ?')
            params.append(username)

        if scenario:
            where_clauses.append('"Scenario" = ?')
            params.append(scenario)

        # selector for finished, failed or all
        if status_filter:
            if status_filter == 'finished':
                where_clauses.append("LOWER(\"Status\") IN ('finished', 'completed')")
            elif status_filter == 'failed':
                where_clauses.append("LOWER(\"Status\") = 'failed'")

        # selector for _pub or all rows
        if not export_all:
            where_clauses.append('Host LIKE ?')
            params.append('%_pub%')

        columns_to_select = [
            '"User ID"', '"Scenario"', '"Date Key"', '"Part"', '"Host"', '"Cycle"',
            '"Year"', '"Iteration"', '"Module"', '"Status"', '"Work Directory"',
            '"Output Directory"', '"Time Submitted"', '"Time Elapsed"'
        ]
        columns_str = ", ".join(columns_to_select)

        query = f'SELECT {columns_str} FROM "{table_name}"'
        if where_clauses:
            query += " WHERE " + " AND ".join(where_clauses)

        df = pd.read_sql_query(query, conn, params=params)

        if df.empty:
            print("No data found matching the specified criteria.")
            return

        df['Time Elapsed'] = df['Time Elapsed'].apply(parse_time_elapsed_string)
        df.rename(columns={'Time Elapsed': 'Time Elapsed (seconds)'}, inplace=True)

        confirm_large_export(len(df), output_format)

        timestamp_str = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_full_path = f"{output_file_base}_{timestamp_str}.{output_format}"
        abs_path = os.path.abspath(output_full_path)

        if output_format == 'csv':
            df.to_csv(output_full_path, index=False)
            print(f"Data exported successfully to: {abs_path}")
        elif output_format == 'html':
            generate_html_from_dataframe(df, output_full_path)
            print(f"HTML report generated successfully to: {abs_path}")
        else:
            print(f"Unsupported output format: {output_format}")

    except sqlite3.Error as e:
        print(f"Database error: {e}")
    except ValueError as e:
        print(f"Error: {e}")
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
    finally:
        if conn:
            conn.close()

if __name__ == "__main__":
    # Argparse Arguments...
    parser = argparse.ArgumentParser(description="Export data from SQLite database to CSV or HTML.")
    parser.add_argument("--db_file", "-d", type=str, required=True,
                        help="Path to the SQLite database file (e.g., C:/path/to/nems_runs.db).")
    parser.add_argument("--table_name", "-t", type=str, default="nems_runs",
                        help="Name of the table to export data from. Default: 'nems_runs'")
    parser.add_argument("--username", "-u", type=str,
                        help="Filter by User ID (e.g., 'ABC').")
    parser.add_argument("--scenario", "-c", type=str,
                        help="Filter by Scenario (e.g., 'energy').")
    parser.add_argument("--finished", action="store_true",
                        help="Filter to include only runs with the 'Finished' status.")
    parser.add_argument("--failed", action="store_true",
                        help="Filter to include only runs with the 'Failed'.")
    parser.add_argument("--output_format", "-f", type=str, choices=['html', 'csv'], default='html',
                        help="Output format: 'html' or 'csv'. Default: 'html'.")
    parser.add_argument("--output_file", "-o", type=str, default="nems_runs_export",
                        help="Base name for the output file (e.g., 'my_report' will create 'my_report_YYYYMMDD_HHMMSS.html' or '.csv').")
    parser.add_argument("--all", "-a", action="store_true",
                        help="The default behavior is to only export runs from the '_pub' queue. Using -a or --all will export all runs.")

    # Mutual exclusive group for certain args
    date_group = parser.add_mutually_exclusive_group()
    date_group.add_argument("--start_date", "-s", type=str,
                            help="Start date for filtering (e.g., '01-01-1970'). Rows with 'Time Submitted' after this date will be included.")
    date_group.add_argument("--weekly", action="store_true",
                            help="Filter to include data submitted after midnight on Sunday of the current week.")
    date_group.add_argument("--monthly", action="store_true",
                            help="Filter to include data submitted after midnight on the first day of the current month.")
    parser.add_argument("--end_date", "-e", type=str,
                        help="End date for filtering (e.g., '01-01-1970'). Rows with 'Time Submitted' up to midnight of this date will be included. Only applicable with --start_date.")

    args = parser.parse_args()

    if args.end_date and not args.start_date:
        print("Error: --end_date can only be used in conjunction with --start_date.")
        sys.exit(1)

    status_filter = None
    if args.finished and args.failed:
        print("Error: Specifying both --finished and --failed is meaningless.")
        print("Please choose one or neither to see all statuses.")
        sys.exit(1)
    elif args.finished:
        status_filter = 'finished'
    elif args.failed:
        status_filter = 'failed'

    parsed_start_date = None
    parsed_end_date = None

    now = datetime.now()

    if args.weekly:
        parsed_start_date = get_start_of_week(now)
        parsed_end_date = now
        print(f"Filtering for data from: {parsed_start_date.strftime('%Y-%m-%d %H:%M:%S')} (start of current week) to {parsed_end_date.strftime('%Y-%m-%d %H:%M:%S')} (now).")
    elif args.monthly:
        parsed_start_date = get_start_of_month(now)
        parsed_end_date = now
        print(f"Filtering for data from: {parsed_start_date.strftime('%Y-%m-%d %H:%M:%S')} (start of current month) to {parsed_end_date.strftime('%Y-%m-%d %H:%M:%S')} (now).")
    elif args.start_date:
        parsed_start_date = parse_date_argument(args.start_date)
        if args.end_date:
            parsed_end_date = parse_date_argument(args.end_date)
        else:
            parsed_end_date = now
        print(f"Filtering for data between {parsed_start_date.strftime('%Y-%m-%d')} and {parsed_end_date.strftime('%Y-%m-%d')}.")
    else:
        parsed_end_date = now

    export_data(
        db_file=args.db_file,
        table_name=args.table_name,
        start_date=parsed_start_date,
        end_date=parsed_end_date,
        username=args.username,
        scenario=args.scenario,
        status_filter=status_filter,
        output_format=args.output_format,
        output_file_base=args.output_file,
        export_all=args.all
    )