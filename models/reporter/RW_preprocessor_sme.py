"""
A collection of complicated Reporter formulas that seem to require subject matter expertise (SME).
For the Leidos NEMS Reporter project, let's park these formulas here so that EIA can consider them
more carefully at a later date.
"""
import os
# import toml
import tomllib
import pandas as pd

from RW_preprocessor import parse_parameter_file, read_stuff
# from RW_debug import set_logging, log_execution


def preprocessor_sme(table_spec):  # dfd):  # , table_id):
    """
    Perform preprocessing tasks for SME data.

    This function performs preprocessing tasks on SME (Subject Matter Expert) data. 
    - Reads coefficients from a CSV file and adds them to the 'dfd_sme' dictionary. 
    - Processes tables for complicated formulas that seem to require subject matter 
      expertise (SME). The results are also stored in the 'dfd_sme' dictionary and returned.

    Parameters
    ----------
    table_spec : dict
        A dictionary containing the specifications for table processing.

    Returns
    -------
    dict
        A dictionary containing the processed SME data.

        Key : str
            The name of a coefficient or the name/id of a special SME calculation.
        Value : int/float or DataFrame/Series
            The value of a coefficient or the value of a SME calculation result.

    """

    # config, logging = set_logging()
    # logging.info('\n============ preprocessor_sme ============')

    dfd_sme = {}

    # Get input file info
    dat = table_spec["dat"]
    coefficients_path = table_spec["coefficients_path"]
    table_var_def_path = table_spec["table_var_def_path"]

    # Read coefficients and create coefficient dict
    df = pd.read_csv(coefficients_path).set_index("name")
    df.columns = df.columns.str.strip()

    def str_to_num(s):
        """Try converting a string to int, then to float, or keep as string 
        if both conversions fail.
        """
        try:
            return int(s)
        except ValueError:
            try:
                return float(s)
            except ValueError:
                return s

    # Function to adjust "value" based on "type"
    def adjust_value(row):
        if row["type"] == "int":
            # Convert to float, then to int, and back to string
            row["value"] = str(int(row["value"]))
        else:
            row["value"] = str(row["value"])
        return row

    # Apply the function to each row
    df = df.apply(adjust_value, axis=1)
    d_coef = df.T.to_dict()
    d_coef = {key: str_to_num(value["value"]) for key, value in d_coef.items()}

    # Add coefficients to dfd_sme
    dfd_sme.update(d_coef)

    # Keep using dimensional variable names directly in coding
    # - code easier and look better !
    MNUMOR = d_coef["MNUMOR_rwpre"]  # 14
    MNUMPR = d_coef["MNUMPR_rwpre"]  # 10
    MNUMLR = d_coef["MNUMLR_rwpre"]  # 17
    MNUMCR = d_coef["MNUMCR_rwpre"]  # 11
    RDAYS = d_coef["RDAYS_rwpre"]  # 365

    #'''----------- Perform SME calculations by table --------------- '''
    # This shows an example of how to use 'rwpre' in SME calculations

    # Load table variable definitions from the TOML file
    # with open(table_spec['table_var_def_path'], 'r', encoding='utf-8') as f:
    #    temp = toml.load(f)

    with open(table_var_def_path, "rb") as f:
        temp = tomllib.load(f)

    # TN 001 -------------------------
    #    Biomass 12/
    # T1(33,IY,IS)=QBMAS(11,IY)-QBMRF(11,IY) +CORNCD(3,11,IY)*CFCORN/1000000000. -RFBIOBUTECD(MNUMCR,IY)*RDAYS*CFBIOBUTE(IY)/1000000.
    #         -0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000.*CFPET*RDAYS/1000. +SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS
    #         *(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+UBAVOL(MNUMPR,IY)*5.763)
    #T1(33,IY,IS) = T1(33,IY,IS) + 0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.
    table_id = "TN 001"
    my_vars = temp[table_id]["v"]
    # dfd = read_stuff(dat, my_vars)
    dfd = table_spec["dfd"]
    dfd.update(d_coef)

    dfd_sme["T1_33_rwpre"] = (
        dfd["QBMAS"].loc[11] / 1000
        - dfd["QBMRF"].loc[11] / 1000
        + dfd["CORNCD"].loc[(3, 11)] * dfd["CFCORN"].iloc[0] / 1000000000.0
        - dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"] / 1000000.0
        - dfd["ETHANOL_PARAM_rwpre"]
        * (dfd["CRNETHCD"].loc[11] + dfd["CLLETHCD"].loc[11] + dfd["OTHETHCD"].loc[11])
        / 1000.0
        * dfd["CFPET"].iloc[0]
        * RDAYS
        / 1000.0
        + dfd["BIMQTYCD"].loc[(slice(None), 11), :].sum()
        / 1000000.0
        * RDAYS
        * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
        - RDAYS
        / 1000000.0
        * (
            dfd["BTLFRAC"].loc[(slice(None), MNUMPR), :].sum() * dfd["CFBTLLIQ"]
            + dfd["CBTLFRAC"].loc[(2, (1, 2, 3, 4), MNUMPR), :].sum()
            * dfd["CFCBTLLIQ"].loc[2]
            + dfd["UBAVOL"].loc[MNUMPR] * dfd["CFUBA_rwpre"]
        )
    ) * 1000

    # TN 002 -------------------------
    table_id = "TN 002"
    my_vars = temp[table_id]["v"]
    # dfd = read_stuff(dat, my_vars)
    dfd.update(d_coef)

    dfd_sme["QHYTR_rwpre"] = dfd["QHYTR"] / dfd["NATURAL_GAS_HYDROGEN_PARAM_rwpre"]

    dfd_sme["T2_73_rwpre"] = (
        (dfd["QMGTR"] - dfd["QMGBS"]) / (dfd["QMGTR"].loc[11] - dfd["QMGBS"].loc[11])
    ) * (
        dfd["CORNCD"].loc[(1, 11), :] * dfd["CFCORN"].iloc[0] / 1000.0
        - dfd["ETHANOL_PARAM_rwpre"]
        * (dfd["CRNETHCD"].loc[11] + dfd["OTHETHCD"].loc[11])
        * RDAYS
        * dfd["CFPET"].iloc[0]
    ) / 1000000.0 + (
        (dfd["QDSTR"] - dfd["QDSBS"]) / (dfd["QDSTR"].loc[11] - dfd["QDSBS"].loc[11])
    ) * dfd[
        "BIMQTYCD"
    ].groupby(
        "MNUMCR"
    ).sum().loc[
        11
    ] / 1000000.0 * RDAYS * (
        dfd["CFVEGGIE"] - dfd["CFBIOD"]
    )

    # with open(table_var_def_path, 'r', encoding='utf-8') as f:
    #     temp = toml.load(f)
    #     my_vars = temp[table_id]['v']
    #     dfd = read_stuff(dat, my_vars) #, table_id)

    #     dfd.update(d_coef)

    #     # T1(33,IY,IS)=QBMAS(11,IY)-QBMRF(11,IY)+CORNCD(3,11,IY)*CFCORN/1000000000.-RFBIOBUTECD(MNUMCR,IY)*RDAYS*CFBIOBUTE(IY)/1000000.-
    #     # 0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000.*CFPET*RDAYS/1000.+
    #     # SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-
    #     # RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763)
    #     dfd_sme['T1_33_rwpre'] = \
    #         (dfd['QBMAS'].loc[11] - dfd['QBMRF'].loc[11]
    #             + dfd['CORNCD'].loc[(3, 11)] * dfd['CFCORN'][0].values /
    #             1000000000.
    #             - dfd['RFBIOBUTECD'].loc[MNUMCR] * RDAYS * dfd['CFBIOBUTE'] /
    #             1000000.
    #             - dfd["ETHANOL_PARAM_rwpre"] * (dfd['CRNETHCD'].loc[11] + dfd['CLLETHCD'].loc[11]
    #                                             + dfd['OTHETHCD'].loc[11]) / 1000. * dfd['CFPET'][0].values * RDAYS / 1000.
    #             + dfd['BIMQTYCD'].loc[(slice(None), 11),
    #                                   :].sum() / 1000000. * RDAYS
    #             * (dfd['CFVEGGIE'] - dfd['CFBIOD'])
    #             - RDAYS / 1000000. * (dfd['BTLFRAC'].loc[(slice(None), MNUMPR), :].sum() * dfd['CFBTLLIQ']
    #                                   + dfd['CBTLFRAC'].loc[(2, (1, 2, 3, 4), MNUMPR), :].sum() * dfd['CFCBTLLIQ'].loc[2]
    #                                   + dfd['UBAVOL'].loc[MNUMPR] * dfd["CFUBA_rwpre"]))

    # TN 034 -------------------------
    # table_id = 'TN 034'
    # # Load table variable definitions from the TOML file
    # with open(table_var_def_path, 'r', encoding='utf-8') as f:
    #     temp = toml.load(f)
    #     my_vars = temp[table_id]['v']
    #     dfd = read_stuff(dat, my_vars) #, table_id)
    #     dfd.update(d_coef)

    #     # T34(47,IR,IY,IS)=T34(47,IR,IY,IS)+1000.*((CORNCD(3,11,IY)*CFCORN/1000.-0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*365.*CFPET-RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY))/1000000.+SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763))
    #     # TODO: convert the Fortran code in the following to Python
    #     dfd["Biofuels Heat and Coproducts rwpre"] = \
    #     (CORNCD(3,11,IY)*CFCORN/1000.-0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*RDAYS*CFPET-RFBIOBUTECD(MNUMCR,IY)*RDAYS*CFBIOBUTE(IY))/1000000. #+SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)
    #     +SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763)

    # table_id = 'TN 006'  # in ['TN 112', 'TN 035', 'TN 048', 'TN 056']:
    # pass
    # # In the .py files for tables 6, 24, 35, and 44, use the following code:
    # # T6(112,IY,IS) = dfd["Biofuels Heat and Coproducts rwpre"]
    # # T24(35,IY,IS) = dfd["Biofuels Heat and Coproducts rwpre"]
    # # T35(48,IY,IS) = dfd["Biofuels Heat and Coproducts rwpre"] * dfd["QUADS_TO_TRILS"]
    # # T44(56,IY,IS) = dfd["Biofuels Heat and Coproducts rwpre"] * dfd["QUADS_TO_TRILS"]

    # TN 006 -------------------------
    # table_id = 'TN 006'
    # pass
    # # In the .py file for Table 6, T6(118,IY,IS): use RDAYS, d["ETHANOL_PARAM_rwpre"] rather than hardcode

    # table_id = 'TN 019'
    # pass
    # # the following is complex enough to be in the preprocessor_sme function as dfd['T19_1_rwpre]
    # # T19(1,IY,IS)=RFQTDCRD(MNUMOR+2,IY)+RFQNGPL(MNUMPR,IY,6)/1000.+RFQPRCG(MNUMPR,IY)+0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000.
    # # +SUM(BIMQTYCD(1:4,11,IY))/1000.+SUM(GTLFRAC(1:4,MNUMPR,IY))/1000.+SUM(CTLFRAC(1:4,MNUMPR,IY))/1000+SUM(CBTLFRAC(1,1:4,MNUMPR,IY))/1000.
    # # +SUM(BTLFRAC(1:4,MNUMPR,IY))/1000+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))/1000.+UBAVOL(MNUMPR,IY)/1000.+RFBIOBUTECD(MNUMCR,IY)/1000.
    # # +(GRD2DSQTY(MNUMPR,IY)+GRN2MGQTY(MNUMPR,IY))/1000.+RFHCXH2IN(MNUMPR,IY)+RFMETM85(MNUMPR,IY)

    return dfd_sme
