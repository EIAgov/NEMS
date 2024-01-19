import os
import pandas as pd
from pandas import ExcelWriter
from DataModel.junitparser import JUnitXml
from DataModel.junitparser import Failure, Skipped
from xml.sax.saxutils import unescape

from Controller.StatusHelper import StatusHelper
import warnings
# for aeo2023_py37_b Pandas, import from pandas.core.common:
#from pandas.core.common import SettingWithCopyWarning
# for aeo2024_py311 Pandas, import from pandas.errors:
from pandas.errors import SettingWithCopyWarning
warnings.simplefilter(action="ignore", category=SettingWithCopyWarning)

files = []

def save_to_csv(xml, filename):
    # specifying the fields for main csv file
    fields = 'File, Test Case, Time, Result, Control Status, Report'
    otherfiles = []

    i = 1
    with open(filename, 'w') as csvfile:
        csvfile.writelines(fields + "\n")
        for suite in xml:
            for case in suite:
                line = case.to_csv() + ',,'
                h_link=f'Failed_Test_{i}'
                tmp = line.split(',')
                tmp = [x.strip(' ') for x in tmp]
                if (tmp[3] == 'Fail'):
                    line += h_link
                    i += 1

                csvfile.writelines(line + "\n")

                if case.result:
                    if isinstance(case.result[0], Failure):
                        other_report = ""
                        for prop in case.properties():
                            s = unescape(prop.value, {"&quot;": '"'})
                            other_report += f"{s}\n"

                        otherfiles.append(other_report)

    i = 0
    for f in otherfiles:
        f_name = os.path.join(os.getcwd(), f'report_page_{i}.csv')
        files.append(f_name)
        with open(f_name, 'w') as file:
            file.write(f)
            i = i + 1


def save_to_excel():
    # error_bad_lines=False is deprecated by Pandas
    #df = pd.read_csv(os.path.join(os.getcwd(), 'validator_report.csv'), delimiter=",", error_bad_lines=False)
    df = pd.read_csv(os.path.join(os.getcwd(), 'validator_report.csv'), delimiter=",", on_bad_lines='skip')

    # clean up df data bug: Index(['File', ' Test Case', ' Time', ' Result', ' Report'], dtype='object')
    df = df.rename(columns=lambda x: x.strip())
    df = df.apply(lambda x: x.str.strip() if x.dtype == "object" else x)
    df['Result'] = df['Result'].replace('Pass', 'Passed')
    df['Result'] = df['Result'].replace('Fail', 'Failed')
    df['Report'] = df['Report'].fillna('')

    # assemble report hyperlink and the test case status defined in validator_controller.csv
    isFailed=False
    h = StatusHelper()
    for idx, row in df.iterrows():
        df['Control Status'][idx] = h.get_status(df['Test Case'][idx].strip())
        n = df['Report'][idx]
        if (len(n) > 0):
            tmp = df['Result'][idx]
            df['Result'][idx] = f'=HYPERLINK("#{n}!A1", "{tmp}")'

            # flag the report is failed when an active test is failed
            if df['Control Status'][idx] == 'active' and tmp == 'Failed':
                isFailed=True

    df = df.drop(['Report'], axis=1)

    # now write to the file
    excel_filename='validator_' + ('FAIL' if isFailed else 'PASS') + '.xlsx'
    writer = ExcelWriter(os.path.join(os.getcwd(), excel_filename))
    df.to_excel(writer, sheet_name="All Tests", index=False)

    # color the result for each test case
    workbook  = writer.book
    worksheet = writer.sheets["All Tests"]
    rng = "D2:D255"
    red_format = workbook.add_format({'font_color':'red', 'underline':True})
    green_format = workbook.add_format({'font_color':'green'})
    worksheet.conditional_format(rng, {'type': 'text',
                                                    'criteria': 'containing',
                                                    'value':     'Failed',
                                                    'format': red_format})
    worksheet.conditional_format(rng, {'type': 'text',
                                                    'criteria': 'containing',
                                                    'value':   'Passed',
                                                    'format':  green_format})


    # beautify report display. Auto-adjust columns' width
    for column in df:
        column_width = max(df[column].astype(str).map(len).max(), len(column))
        col_idx = df.columns.get_loc(column)
        writer.sheets["All Tests"].set_column(col_idx, col_idx, column_width)
    i = 1
    for f in files:
        # error_bad_lines=False is deprecated by Pandas
        #df = pd.read_csv(f,delimiter=",", error_bad_lines=False)
        df = pd.read_csv(f,delimiter=",", on_bad_lines='skip')
        df = df.sort_values(by=df.columns[0])
        df.to_excel(writer, sheet_name=f"Failed_Test_{i}", index=False)

        # beautify report display. Auto-adjust columns' width
        for column in df:
            column_width = max(df[column].astype(str).map(len).max(), len(column))
            col_idx = df.columns.get_loc(column)
            writer.sheets[f"Failed_Test_{i}"].set_column(col_idx, col_idx, column_width)

        i = i+1

    # in aeo2024_py311 Pandas 2.0.2, writer.save() is is depreciated, use writer.close()
    writer.close()
    # in aeo2023_py37_b Pandas, writer.save() is supported
    #writer.save()

def delete_csv_files():
    os.remove(os.path.join(os.getcwd(), 'validator_report.csv'))
    for f in files:
        os.remove(f)


def main():
    cwd = os.getcwd()
    report_path = os.path.join(cwd, 'validator_report.xml')
    xml = JUnitXml.fromfile(report_path)

    save_to_csv(xml, 'validator_report.csv')
    save_to_excel()
    delete_csv_files()


if __name__ == "__main__":
    main()