# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 13:53:37 2024
Modified Oct 20 2025

@author: peter.whitman
@author: gregory.miller
"""

import pandas as pd
from datetime import datetime, timedelta
import re

def parse_datekey(date_key_str):
    # regex to parse datekeys into 3 groups month, day, year
    match = re.match(r'd(\d{2})(\d{2})(\d{2})\w', str(date_key_str))
    if match:
        month, day, year = match.groups()
    year_exp = 2000+int(year)
    try:
        return datetime(year_exp, int(month), int(day)).date()
    except ValueError:
        return None
    return None



def generate_html_from_dataframe(df, output_path, max_height='800px'):
    """
    The section below creates a text string with the html code.
    Section <head> includes meta-data including the title.
    The <style> section is CSS style guide of the table.
    <th> describes the header while <td> describes the data
    th.sort-asc and th.sort-desc add the arrows for the sort direction
    input[type="text"] sets the input box for filtering by username
    table-wrapper sets the height before adding the scroll bar.
    """

    #-- Parsing DataFrame for count of each run status --#
    target_statuses = ['Running', 'Pending', 'In Queue']
    temp_df = df.copy()

    # calculate the count of each status in the df
    status_counts = temp_df.groupby('Host')['Status'].value_counts().unstack(fill_value=0)  
    summary_df = pd.DataFrame(index=status_counts.index, columns=target_statuses).fillna(0)
    
    # populate summary_df with the counts from temp_df
    for run_status in target_statuses:
        if run_status in status_counts.columns:
            summary_df[run_status] = status_counts[run_status]
    
    summary_df['Total'] = summary_df.sum(axis=1)
    summary_df = summary_df.reset_index()

    #-- Parsing DF for weekly run counts --#
    today = datetime.now()
    start_of_week = today - timedelta(days=(today.weekday() + 1) % 7) # Sunday as the start of the week
    cal_headers = []
    cal_dates = []
    for i in range(7):
        cur_date = start_of_week + timedelta(days = i)
        day_abbr = cur_date.strftime('%a')
        month_day = cur_date.strftime('%m/%d')
        cal_headers.append(f"{day_abbr} ({month_day})")
        cal_dates.append(cur_date.date())

    temp_df['Time Submitted'] = temp_df['Date Key'].apply(parse_datekey)

    finished_count = [0]*7
    failed_count = [0]*7
    total_count = [0]*7

    for idx, cal_date in enumerate(cal_dates):
        daily_jobs = temp_df[temp_df['Time Submitted'] == cal_date]

        finished_statuses = ['Finished', 'Completed']
        finished_count[idx] = daily_jobs[daily_jobs['Status'].isin(finished_statuses)].shape[0]

        failed_statuses = ['Failed']
        failed_count[idx] = daily_jobs[daily_jobs['Status'].isin(failed_statuses)].shape[0]

        total_count[idx] = daily_jobs.shape[0]

    weekly_count_data = {
        'Finished': finished_count,
        'Failed': failed_count,
        'Total Submitted': total_count
    }

    html_content = """<!DOCTYPE html>
<html>
<head>
    <title>Run Status</title>
    <style>
        body {
            max-height: 100vh;
            overflow-y: auto;
            margin: 0;
            padding: 20px;
            box-sizing: border-box;
        }
        table {
            width: 100%;
            border-collapse: collapse;
        }
        table, th, td {
            border: 1px solid black;
        }
        th, td {
            padding: 8px;
            text-align: left;
        }
        th {
            background-color: green;
            color: white;
            position: sticky;
            top: 0;
            z-index: 2;
            cursor: pointer;
        }
        th.sort-asc::after {
            content: ' \\2191';
        }
        th.sort-desc::after {
            content: ' \\2193';
        }
        td {
            background-color: #f5f5dc; /* Light tan color */
        }
        h2 {
            text-align: center;
            font-weight: bold;
        }
        h3 {
            text-align: center;
            font-weight: bold;
            margin-top: 30px;
        }
        input[type="text"] {
            margin-bottom: 10px;
            padding: 5px;
            width: 200px;
        }
        .table-wrapper { /* max-height and overflow-y are removed from here */
            margin-bottom: 20px;
        }
        .copy-to-clipboard {
            cursor: pointer;
            position: relative;
            text-decoration: underline;
            color: #0000EE;
        }
        .copy-to-clipboard::hover {
        color: #551A8B
        }
        .copy-to-clipboard::after {
            content: '\\1F4CB';
            position: absolute;
            right: 5px;
            top: 50%;
            transform: translateY(-50%);
            font-size: 0.8em;
            opacity: 0;
            transition: opacity .2s ease-in-out;
        }
        .copy-to-clipboard:hover::after {
            opacity: 1;
        }
    </style>

    <script>
// Filter job rows by searchkey
    
        function filterTable(tableId, inputId) {
            var input, filter, found, table, tr, td, i, txtValue;
            input = document.getElementById(inputId);
            filter = input.value.toUpperCase();
            table = document.getElementById(tableId);
            tr = table.getElementsByTagName("tr");
    
            for (i = 1; i < tr.length; i++) {
				td = tr[i].getElementsByTagName("td");
                var rowFound = false;
				for (j = 0; j < td.length; j++) {
					if (td[j] && td[j].innerHTML.toUpperCase().indexOf(filter) > -1) {
						rowFound = true;
						break;
					}
					
				}
				if (rowFound) {
					tr[i].style.display = "";
				} else {
					tr[i].style.display = "none";
				}
			}
        }

// Sort table by field
     
        function sortTable(n, tableId) {
            var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
            table = document.getElementById(tableId);
            switching = true;
            dir = "asc"; 
            while (switching) {
                switching = false;
                rows = table.rows;
                for (i = 1; i < (rows.length - 1); i++) {
                    shouldSwitch = false;
                    x = rows[i].getElementsByTagName("TD")[n];
                    y = rows[i + 1].getElementsByTagName("TD")[n];
                    // Handle numeric sorting for appropriate columns
                    var xContent = x.innerHTML.toLowerCase();
                    var yContent = y.innerHTML.toLowerCase();
                    
                    if (!isNaN(xContent) && !isNaN(yContent) && xContent !== '' && yContent !== '') {
                        if (dir == "asc") {
                            if (parseFloat(xContent) > parseFloat(yContent)) {
                                shouldSwitch = true;
                                break;
                            }
                        } else if (dir == "desc") {
                            if (parseFloat(xContent) < parseFloat(yContent)) {
                                shouldSwitch = true;
                                break;
                            }
                        }
                    } else { // Default to string sorting
                        if (dir == "asc") {
                            if (xContent > yContent) {
                                shouldSwitch = true;
                                break;
                            }
                        } else if (dir == "desc") {
                            if (xContent < yContent) {
                                shouldSwitch = true;
                                break;
                            }
                        }
                    }
                }
                if (shouldSwitch) {
                    rows[i].parentNode.insertBefore(rows[i + 1], rows[i]);
                    switching = true;
                    switchcount ++; 
                } else {
                    if (switchcount == 0 && dir == "asc") {
                        dir = "desc";
                        switching = true;
                    }
                }
            }
            updateSortIndicator(n, dir, tableId);
        }
// Updates arrow to show sort direction    
        function updateSortIndicator(columnIndex, direction, tableId) {
            var headers = document.querySelectorAll("#" + tableId + " th");
            headers.forEach((th, index) => {
                th.classList.remove("sort-asc", "sort-desc");
                if (index === columnIndex) {
                    th.classList.add(direction === "asc" ? "sort-asc" : "sort-desc");
                }
            });
        }
//  sets up  action when arrow is clicked to sort table

        document.addEventListener("DOMContentLoaded", function() {
            updateSortIndicator(0, "asc", "runStatusTable"); // Set the initial sorting indicator for the first column
            updateSortIndicator(0, "asc", "hostSummaryTable"); // Set the initial sorting indicator for the summary table
        });

// sets up a copy to clipboard action when you click the associated element
        function copyOutputDirectory(cellElement) {
            const directoryPath = cellElement.dataset.path;
            navigator.clipboard.writeText(directoryPath)
                .then(() => {
                    console.log('Output directory copied to clipboard:', directoryPath);
                    const originalText = cellElement.innerText;
// remove these lines to remove the 'copied' effect
                    cellElement.innerText = 'Copied!';
                    setTimeout(() => {
                        cellElement.innerText = originalText;
                    }, 1500); // Revert text after 1.5 seconds
                })
                .catch(err => {
                    console.error('Failed to copy output directory:', err);
                    // Optional: Alert user of failure
                    alert('Failed to copy output directory. Please copy manually.');
                });
        }

        document.addEventListener("DOMContentLoaded", function() {
            updateSortIndicator(0, "asc", "runStatusTable");
            updateSortIndicator(0, "asc", "hostSummaryTable");
        });

</script>
</head>
<body>


<h2>Run Status Information</h2>

<input type="text" id="userInput" onkeyup="filterTable('runStatusTable', 'userInput')" placeholder="filter rows by search term">

<div class="table-wrapper">
    <table id="runStatusTable">
        <thead>
            <tr>"""
    
    # Add table headers from DataFrame column names with sort functionality
    for i, column in enumerate(df.columns):
        html_content += f"<th onclick='sortTable({i}, \"runStatusTable\")'>{column}</th>"
    
    html_content += "</tr></thead><tbody>"

    # Add table rows from DataFrame
    for _, row in df.iterrows():
        html_content += "<tr>"
        for j, value in enumerate(row):
            # assign the script to the output directory row
            if df.columns[j] == 'Output Directory':
                html_content += f"<td class='copy-to-clipboard' onclick='copyOutputDirectory(this)' data-path='{value}'>{value}</td>"
            elif df.columns[j] == 'Work Directory':
                parsed_val = ""
                if "D:\\" in value and 'NEM' in row['Host'].upper():
                    parsed_val = value.replace('D:\\', f"\\\\{row['Host']}\\")
                else:
                    parsed_val = value
                html_content += f"<td class='copy-to-clipboard' onclick='copyOutputDirectory(this)' data-path='{parsed_val}'>{parsed_val}</td>"
            else:
                html_content += f"<td>{value}</td>"
        html_content += "</tr>"

    html_content += """
        </tbody>
    </table>
</div>

<h3>Host Status Summary</h3>

<div class="table-wrapper">
    <table id="hostSummaryTable">
        <thead>
            <tr>"""
    
    # Add headers for the summary table
    for i, column in enumerate(summary_df.columns):
        html_content += f"<th onclick='sortTable({i}, \"hostSummaryTable\")'>{column}</th>"
    
    html_content += "</tr></thead><tbody>"

    # Add rows for the summary table
    for _, row in summary_df.iterrows():
        html_content += "<tr>"
        for value in row:
            html_content += f"<td>{value}</td>"
        html_content += "</tr>"

    html_content += """
        </tbody>
    </table>
</div>

<h3>Weekly Runs</h3>
<div class="table-wrapper">
    <table id="weeklyRuns">
        <thead>
            <tr>
                <th>Status</th>"""
    for i, header in enumerate(cal_headers):
        html_content += f"<th>{header}</th>"
    html_content += """
            </tr>
        </thead>
        <tbody>"""
    for row_label, count in weekly_count_data.items():
        html_content += "<tr>"
        html_content += f"<td><strong>{row_label}<strong></td>"
        for c in count:
            html_content += f"<td>{c}</td>"
        html_content += "</tr>"
    html_content += """
        </tbody>
    </table>
</div>

</body>
</html>"""

    with open(output_path, 'w') as file:
        file.write(html_content)


if __name__ == "__main__":
    # Example DataFrame
    data = {
        'User ID': ['PCW', 'XYZ', 'ABC', 'DEF', 'GHI', 'JKL', 'MNO', 'PQR', 'STU', 'VWX', 'YZA', 'BCD', 'EFG', 'HIJ', 'KLM', 'NOP', 'QRS', 'TUV'],
        'Scenario': ['OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25', 'OP24', 'OP25'],
        'Date Key': ['d092825a', 'd092925b', 'd092825c', 'd093025d', 'd092925e', 'd100125f', 'd093025g', 'd100225h', 'd092825i', 'd092925j', 'd093025k', 'd100125l', 'd092925m', 'd093025n', 'd100225o', 'd100325p', 'd100125q', 'd100425r'],
        'Part': ['P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2'],
        'Host': ['NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3'],
        'Cycle': [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2],
        'Year': [2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051],
        'Iteration': [3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4],
        'Status': ['running', 'Completed', 'Running', 'failed', 'RUNNING', 'completed', 'running', 'Completed', 'running', 'error', 'pending', 'In queue', 'running', 'completed', 'Pending', 'in Queue', 'running', 'completed'],
        'Message ID': ['qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr'],
        'Output Directory': ['g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25'],
        'Time Submitted': ['2025-09-28 10:00:00', '2025-09-29 11:30:00', '2025-09-28 12:00:00', '2025-09-30 08:00:00', '2025-09-29 14:00:00', '2025-10-01 09:00:00', '2025-09-30 15:00:00', '2025-10-02 10:00:00', '2025-09-28 16:00:00', '2025-09-29 17:00:00', '2025-09-30 18:00:00', '2025-10-01 19:00:00', '2025-09-29 20:00:00', '2025-09-30 21:00:00', '2025-10-02 22:00:00', '2025-10-03 23:00:00', '2025-10-01 07:00:00', '2025-10-04 08:00:00']
    }

    df = pd.DataFrame(data)
    num_replications = (50 // len(df)) + (1 if 50 % len(df) > 0 else 0)
    df = pd.concat([df] * num_replications, ignore_index=True)

    output_file_path = "run_status.html"
    generate_html_from_dataframe(df, output_file_path, max_height='400px')
    print(f"HTML file has been generated: {output_file_path}")
