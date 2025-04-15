try:
    from Controller.StatusHelper import StatusHelper
except ImportError:
    from Controller import StatusHelper

from steo_benchmark_base import STEOBenchmarkBase

def test_steo_cmm_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_cmm_equals_aeo'))

    catg = 'cmm'
    base = STEOBenchmarkBase(catg, 2023, 2025)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)
    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (CMM),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_emm_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_emm_equals_aeo'))

    catg = 'emm'
    base = STEOBenchmarkBase(catg, 2023, 2025)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)

    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (EMM),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_ngmm_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_ngmm_equals_aeo'))

    catg = 'ngmm'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)
    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (NGMM),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_buildings_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_buildings_equals_aeo'))

    catg = 'blg'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)

    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (Buildings),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        END_YEAR_TOLERANCE = df_discrepancy[base.column_headers[3]][n]
        # special handling for RDM, CRM model. Use Tolerance_EndYear for the end year tolerance checking
        for y in range(base.start_year, base.end_year+1):
            if (y == base.end_year):
                if abs(df_discrepancy[str(y)][n]) > END_YEAR_TOLERANCE:
                    tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[3]][n])
                    value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                    record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                    errors = True
            else:
                if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                    tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                    value_percent = "{:.2%}".format(df_discrepancy[str(y)][n])
                    record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                    errors = True

    assert not errors


def test_steo_industrial_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_industrial_equals_aeo'))

    catg = 'ind'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)

    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (Industrial),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_integration_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_integration_equals_aeo'))

    catg = 'itg'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)

    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (Integration),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_total_liquids_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_total_liquids_equals_aeo'))

    catg = 'liq'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)

    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (Liquids),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_hsm_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_hsm_equals_aeo'))

    catg = 'hsm'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)

    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (OGSM),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors


def test_steo_macro_equals_aeo(record_property):
    h = StatusHelper()
    h.skip_test(h.get_status('test_steo_macro_equals_aeo'))

    catg = 'mco'
    base = STEOBenchmarkBase(catg)
    df_unifapi = base.load_table(1)

    df_unifapi = df_unifapi[df_unifapi['Geogr'].isin(['united states','united states territories'])]
    df_unifapi = df_unifapi.drop('Geogr', axis=1)

    df_steo = base.load_table(2)
    df_discrepancy = base.get_aeo_steo(df_unifapi, df_steo)
    record_property("csv_header", "year,AEO Name,STEO Name,Group By,Tolerance (Macro),Discrepancy")
    errors = False

    for n in range(df_discrepancy.shape[0]):
        DISCREPANCY_TOLERANCE = df_discrepancy[base.column_headers[2]][n]
        for y in range(base.start_year, base.end_year+1):
            if abs(df_discrepancy[str(y)][n]) > DISCREPANCY_TOLERANCE:
                tolerance_percent = "{:.1%}".format(df_discrepancy[base.column_headers[2]][n])
                value_percent = "{:.6%}".format(df_discrepancy[str(y)][n])
                record_property("ERROR",f"{str(y)},{df_discrepancy[base.column_headers[0]][n]},{df_discrepancy[base.column_headers[1]][n]},{df_discrepancy[base.rpt_header][n]},{tolerance_percent},{value_percent}")
                errors = True

    assert not errors