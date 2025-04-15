import os
import pandas as pd
import sys


def time_conversion(dfuel_path, template_path, cmd_line_year):
    # read in electricity fuel demand from hmm
    os.chdir('ephrts/output')
    df_dfuel = pd.read_csv(dfuel_path)

    os.chdir('../../ephrts/input')

    # read in the template file from hmm
    df_template = pd.read_excel(template_path, sheet_name='template')

    # now back to ephrts debug
    os.chdir('../../ephrts_debug')

    # merge the template with the fuel demand with the same season, region and hours
    #df_dfuel = df_dfuel[0:len(df_dfuel.index) - 1]
    df_dfuel = df_dfuel.astype({'regions': 'int', 'hours': 'int', 'seasons': 'int'})

    df_merged = pd.merge(left=df_template, right=df_dfuel, left_on=['season', 'r', 'h'],
                         right_on=['seasons', 'regions', 'hours'])

    # now compute the hydrogen electricity consumed and mwhrs consumed using the dayweights and days from the template file
    # this converts these values to 864
    df_merged['h2_elec_consumed'] = df_merged['D_Fuel'] / df_merged['totaldays']
    df_merged['h2_mwh_consumed'] = (df_merged['D_Fuel'] / df_merged['totaldays']) * df_merged['daytq']

    # now compute annual amounts and merge back into dataframe before writing back to nems
    df_annual_total = pd.DataFrame(df_merged.groupby(['r'])['h2_mwh_consumed'].sum()).reset_index()
    df_annual_total.columns = ['r', 'annual_total']

    # merge annual amount back before writing
    df_merged = pd.merge(left=df_merged, right=df_annual_total, on=['r'])
    df_merged.to_csv('df_HmmTimeConversion_' + cmd_line_year + '.csv')


if __name__ == "__main__":

    # this production switch is used only for debugging, else it should be set to True for production runs
    production = True

    dfuel_path = r'D_Fuel.csv'
    template_path = r'HmmLoadHourConversion.xlsx'

    if production:
        cmd_line_year = sys.argv[1]
        time_conversion(dfuel_path, template_path, cmd_line_year)
    else:
        cmd_line_year = '2045'
        os.chdir(r'R:\output\VLA\ref2024_solo\d120723j')
        time_conversion(dfuel_path, template_path, cmd_line_year)

print("")
