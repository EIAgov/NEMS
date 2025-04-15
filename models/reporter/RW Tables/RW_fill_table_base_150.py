# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_150(dfd, table_spec, table_id):
    """Fill table for  Convergence Indicators

    The function returns a dict in which each key is an integer
    references a table row, and each value is a dataframe indexed
    by region number. The integer keys are the same as "irow" in
    the old layin.xls / ftab.f.

    Parameters
    ----------
    dfd : dict of restart variables
        key = variable name
        value = pandas series of variable values

    Returns
    -------
    dict
        dict of dataframes, with integers as keys. The dict values
        are dataframes indexed by region number.



    """

    z = {}

    #        do i=1,102
    #          T150(i,IY,IS)=CVTAB(i,4,IY)
    #        enddo
    #        t150(103,iy,is)=cvscore(iy)
    #        t150(104,iy,is)=cvscore_US(iy)
    #        t150(105,iy,is)=oscor
    #        t150(106:115,iy,is)=cvscorehist(iy,:)

    #   Convergence Indicators
    #   (percentage change)
    #   Convergence Variable Aggregates

    #   NATIONAL Changes from Prior Run or Cycle
    #    National Quantities

    #     End-Use Consumption 1/

    #       Electricity
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[1] = dfd["CVTAB"].loc[1].loc[4]

    #       Natural Gas
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[2] = dfd["CVTAB"].loc[2].loc[4]

    #       Liquefied Petroleum Gases
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[3] = dfd["CVTAB"].loc[3].loc[4]

    #       Distillate Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[4] = dfd["CVTAB"].loc[4].loc[4]

    #       Residual Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[5] = dfd["CVTAB"].loc[5].loc[4]

    #       Motor Gasoline
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[6] = dfd["CVTAB"].loc[6].loc[4]

    #       Jet Fuel
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[7] = dfd["CVTAB"].loc[7].loc[4]

    #       Steam Coal
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[8] = dfd["CVTAB"].loc[8].loc[4]

    #      Total End-Use Sector Fuel 2/
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[9] = dfd["CVTAB"].loc[9].loc[4]

    #     Electric Power Sector Consumption 3/

    #       Natural Gas
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[10] = dfd["CVTAB"].loc[10].loc[4]

    #       Distillate Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[11] = dfd["CVTAB"].loc[11].loc[4]

    #       Residual Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[12] = dfd["CVTAB"].loc[12].loc[4]

    #       Steam Coal
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[13] = dfd["CVTAB"].loc[13].loc[4]

    #      Nuclear
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[49] = dfd["CVTAB"].loc[49].loc[4]

    #      Total Electric Power Sector 2/
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[50] = dfd["CVTAB"].loc[50].loc[4]

    #

    #     Total Primary Fuel All Sectors
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[51] = dfd["CVTAB"].loc[51].loc[4]

    #

    #    National Prices (Expenditures)

    #     End-Use Sector Prices 1/

    #       Electricity
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[52] = dfd["CVTAB"].loc[52].loc[4]

    #       Natural Gas
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[53] = dfd["CVTAB"].loc[53].loc[4]

    #       Liquefied Petroleum Gases
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[54] = dfd["CVTAB"].loc[54].loc[4]

    #       Distillate Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[55] = dfd["CVTAB"].loc[55].loc[4]

    #       Residual Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[56] = dfd["CVTAB"].loc[56].loc[4]

    #       Motor Gasoline
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[57] = dfd["CVTAB"].loc[57].loc[4]

    #       Jet Fuel
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[58] = dfd["CVTAB"].loc[58].loc[4]

    #       Steam Coal
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[59] = dfd["CVTAB"].loc[59].loc[4]

    #     Electric Power Sector Prices 3/

    #       Natural Gas
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[60] = dfd["CVTAB"].loc[60].loc[4]

    #       Distillate Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[61] = dfd["CVTAB"].loc[61].loc[4]

    #       Residual Fuel Oil
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[62] = dfd["CVTAB"].loc[62].loc[4]

    #       Steam Coal
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[63] = dfd["CVTAB"].loc[63].loc[4]

    #

    #     Emission Allowances (if appl.)

    #       Sulfur Dioxide
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[101] = dfd["CVTAB"].loc[101].loc[4]

    #       Mercury
    # T150(I,IY,IS)=CVTAB(I,4,IY)
    z[102] = dfd["CVTAB"].loc[102].loc[4]

    #

    #   Convergence Test Grade (0-4.0)

    #     Intercycle GPA, Regional Basis
    # T150(103,IY,IS)=CVSCORE(IY)
    z[103] = dfd["CVSCORE"]

    #     Intercycle GPA, National Basis
    # T150(104,IY,IS)=CVSCORE_US(IY)
    z[104] = dfd["CVSCORE_US"]

    #

    #   Oscillation Score (based on instances found)
    # T150(105,IY,IS)=OSCOR
    z[105] = dfd["OSCOR"]

    #

    #   GPAs in previous cycles, most recent first

    #     GPA prior 1
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[106] = dfd["CVSCOREHIST"].loc[1]

    #     GPA prior 2
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[107] = dfd["CVSCOREHIST"].loc[2]

    #     GPA prior 3
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[108] = dfd["CVSCOREHIST"].loc[3]

    #     GPA prior 4
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[109] = dfd["CVSCOREHIST"].loc[4]

    #     GPA prior 5
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[110] = dfd["CVSCOREHIST"].loc[5]

    #     GPA prior 6
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[111] = dfd["CVSCOREHIST"].loc[6]

    #     GPA prior 7
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[112] = dfd["CVSCOREHIST"].loc[7]

    #     GPA prior 8
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[113] = dfd["CVSCOREHIST"].loc[8]

    #     GPA prior 9
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[114] = dfd["CVSCOREHIST"].loc[9]

    #     GPA prior 10
    # T150(106:115,IY,IS)=CVSCOREHIST(IY,:)
    z[115] = dfd["CVSCOREHIST"].loc[10]

    return z
