# -*- coding: utf-8 -*-
"""
Created on Wed Jun 26 13:53:37 2024

@author: peter.whitman
"""

import pandas as pd
"""
 This subroutine takes a Pandas dataframe and creates a table within an html page using its column headers.

"""


def generate_html_from_dataframe(df, output_path, max_height='400px'):
    
    """
    The section below creates a text string with the html code.
    Section <head> includes meta-data including the title.
    The <style> section is CSS style guide of the table.
    <th> describes the header while <td> describes the data
    th.sort-asc and th.sort-desc add the arrows for the sort direction
    input[type="text"] sets the input box for filtering by username
    table-wrapper sets the height before adding the scroll bar.
    """

    html_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>Run Status</title>
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
            background-color: green;
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
        input[type="text"] {{
            margin-bottom: 10px;
            padding: 5px;
            width: 200px;
        }}
        .table-wrapper {{
            max-height: {max_height};
            overflow-y: auto;
            display: block;
        }}
    </style>

    <script>
// Filter job rows by username 
    
        function filterTable() {{
            var input, filter, table, tr, td, i, txtValue;
            input = document.getElementById("userInput");
            filter = input.value.toUpperCase();
            table = document.getElementById("runStatusTable");
            tr = table.getElementsByTagName("tr");
    
            for (i = 1; i < tr.length; i++) {{
                td = tr[i].getElementsByTagName("td")[0];
                if (td) {{
                    txtValue = td.textContent || td.innerText;
                    if (txtValue.toUpperCase().indexOf(filter) > -1) {{
                        tr[i].style.display = "";
                    }} else {{
                        tr[i].style.display = "none";
                    }}
                }}
            }}
        }}
// Sort table by field
     
        function sortTable(n) {{
            var table, rows, switching, i, x, y, shouldSwitch, dir, switchcount = 0;
            table = document.getElementById("runStatusTable");
            switching = true;
            dir = "asc"; 
            while (switching) {{
                switching = false;
                rows = table.rows;
                for (i = 1; i < (rows.length - 1); i++) {{
                    shouldSwitch = false;
                    x = rows[i].getElementsByTagName("TD")[n];
                    y = rows[i + 1].getElementsByTagName("TD")[n];
                    if (dir == "asc") {{
                        if (x.innerHTML.toLowerCase() > y.innerHTML.toLowerCase()) {{
                            shouldSwitch = true;
                            break;
                        }}
                    }} else if (dir == "desc") {{
                        if (x.innerHTML.toLowerCase() < y.innerHTML.toLowerCase()) {{
                            shouldSwitch = true;
                            break;
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
            updateSortIndicator(n, dir);
        }}
// Updates arrow to show sort direction    
        function updateSortIndicator(columnIndex, direction) {{
            var headers = document.querySelectorAll("th");
            headers.forEach((th, index) => {{
                th.classList.remove("sort-asc", "sort-desc");
                if (index === columnIndex) {{
                    th.classList.add(direction === "asc" ? "sort-asc" : "sort-desc");
                }}
            }});
        }}
//  sets up  action when arrow is clicked to sort table

        document.addEventListener("DOMContentLoaded", function() {{
            updateSortIndicator(0, "asc"); // Set the initial sorting indicator for the first column
        }});

</script>
</head>
<body>


<h2>Run Status Information</h2>

<input type="text" id="userInput" onkeyup="filterTable()" placeholder="Search for user ID..">



<div class="table-wrapper">
    <table id="runStatusTable">
        <thead>
            <tr>"""
    
    # Add table headers from DataFrame column names with sort functionality
    for i, column in enumerate(df.columns):
        html_content += f"<th onclick='sortTable({i})'>{column}</th>"
    
    html_content += "</tr></thead><tbody>"

    # Add table rows from DataFrame
    for _, row in df.iterrows():
        html_content += "<tr>"
        for value in row:
            html_content += f"<td>{value}</td>"
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
        'Date Key': ['d052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b', 'd052024a', 'd062024b'],
        'Part': ['P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2', 'P1', 'P2'],
        'Host': ['NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3', 'NEM1', 'NEM2', 'NEM3'],
        'Cycle': [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2],
        'Year': [2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051, 2050, 2051],
        'Iteration': [3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4],
        'Status': ['running', 'completed', 'running', 'completed', 'running', 'completed', 'running', 'completed', 'running', 'completed', 'running', 'completed', 'running', 'completed', 'running', 'completed', 'running', 'completed'],
        'Message ID': ['qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr', 'qqq', 'rrr'],
        'Output Directory': ['g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25', 'g:/runs/op24', 'g:/runs/op25']
    }
    df = pd.DataFrame(data)

    output_file_path = "run_status.html"
    generate_html_from_dataframe(df, output_file_path, max_height='400px')  # Change the max_height value here
    print(f"HTML file has been generated: {output_file_path}")
