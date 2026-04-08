# Perform relaxation


def perform_relax(dfd_prev, dfd_cur, my_vars, df_rlx, fyear):
    """Relaxes the selected variables by weighted average between iterations/cycles

    Parameters
    ----------
    dfd_prev : dict
        containing variable dataframes of the previous iteration data.

    dfd_cur : dict
        containing variable dataframes of the current iteration data.

    my_vars : list
        A list of variables to apply weighted average between iteration or cycles
    
    df_rlx : pandas.DataFrame
        containing relaxation parameters
        
    fyear : int
        number of years from start year to last year (TestYears = range(FirstYear-1,LastYear))

    Returns
    -------
    dict
        dict of variable pandas.DataFrame with relaxed values between iteration or cycles

    """

    dfd_updated = dfd_cur
    for var in my_vars:

        df_cur = dfd_cur[var].iloc[:, fyear]
        df_prev = dfd_prev[var].iloc[:, fyear]

        # Get the Relaxation factor
        RLXParam = df_rlx.at[var, 'RelaxFactor']

        # Perform relaxation on each variable
        dfd_updated[var].iloc[:, fyear] = df_prev + RLXParam * (df_cur - df_prev)

    return dfd_updated