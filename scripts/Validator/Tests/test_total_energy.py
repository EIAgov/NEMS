from DataModel.Model import Model
import pandas as pd
import numpy as np

from Controller.StatusHelper import StatusHelper

start_year = 2023
end_year = 2050
DISCREPANCY_TOLERANCE = 5
num_years = end_year - start_year + 1

h = StatusHelper()
# ideally we shall check all tests under test_steo_benchmark test class. Here I grab 3 models instead of checking all test status
status1=h.get_status('test_steo_integration_equals_aeo') == "inactive"
status2=h.get_status('test_steo_industrial_equals_aeo') == "inactive"
status3=h.get_status('test_steo_buildings_equals_aeo') == "inactive"
# Warning: hardcoded 2026 start date. Remove for AEO2026
if status1 and status3 and status3: start_year=2026

def load_table():
    df = Model.getInstance().files.csv.load_table(1)
    df = df[df['DaType'] != "price"]
    year_columns = [str(i) for i in range(start_year, end_year+1)]
    column_names = ['SubDat', 'Source'] + year_columns
    df = df.loc[:, column_names]
    df[year_columns] = df[year_columns].astype(float)
    return df

def get_total_supply_minus_demand(df):
    totals = df.groupby(by=['SubDat']).sum()

    # aeo2024_py311 Pandas explores this bug. In aeo2023_py37_b Pandas, the SubDat groupby will naturally drop the Source column, but aeo2024_py311 keeps it.
    totals = totals.drop(['Source'], axis=1, errors='ignore')

    supply = totals.loc['production'] + totals.loc['imports'] - totals.loc['exports']
    demand = totals.loc['use']

    return (supply - demand).to_numpy()

def get_supply_minus_demand_by_source(df):
    totals = df.groupby(by=['Source'])

    #coal, hydro, nuclear, renewable are already nicely formatted
    discrepancy_dict = {}
    for name, group in totals:
        use = np.zeros(num_years)
        production = np.zeros(num_years)
        imports = np.zeros(num_years)
        exports = np.zeros(num_years)

        for row_index, row in group.iterrows():
            data = pd.to_numeric(row, errors='coerce').dropna().to_numpy()
            if row['SubDat'] == "production":
                production = data
            elif row['SubDat'] == "exports":
                exports = data
            elif row['SubDat'] == "imports":
                imports = data
            elif row['SubDat'] == "use":
                use = data

        discrepancy = (production + imports - exports) - use
        discrepancy_dict[name] = discrepancy

    return discrepancy_dict


def test_total_supply_equals_total_demand(record_property):
    '''
    Tests that (total supply - total demand) is within the set discrepancy tolerance
    '''
    h = StatusHelper()
    h.skip_test(h.get_status('test_total_supply_equals_total_demand'))

    discrepancy = get_total_supply_minus_demand(load_table())
    record_property("csv_header", "year, calculation, discrepancy")
    errors = False

    for year in range(len(discrepancy)):
        if abs(discrepancy[year]) > DISCREPANCY_TOLERANCE:
            record_property("ERROR", f"{start_year+year}, supply - demand, {discrepancy[year]}")
            errors = True

    assert not errors

""" Turn this test off for now
def test_supply_equals_demand_by_fuel_source(record_property):
    '''
    Tests that (total supply - total demand) for each fuel source is within the set discrepancy tolerance
    '''

    discrepancy_dict = get_supply_minus_demand_by_source(load_table())
    record_property("csv_header", "year, fuel source, calculation, discrepancy")
    errors = False

    for fuel, discrepancy in discrepancy_dict.items():
        for year in range(len(discrepancy)):
            if abs(discrepancy[year]) > DISCREPANCY_TOLERANCE:
                record_property("ERROR", f"{start_year+year}, {fuel}, supply - demand, {discrepancy[year]}")
                errors = True

    assert not errors
"""