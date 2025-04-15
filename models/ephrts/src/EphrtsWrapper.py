import sys
import restore_elec_prices
import supply_curve_algorithm
import RunEphrts
import os
import time


def main(root_path, cmd_line_year, iteration, start_year, aimms_path,debug):
    '''

    This main routine is the python ephrts wrapper that is called inside of ephrts.f90 from fortran.
    It wraps the preprocessor, aimms project, and then the post processor.

    Parameters
    ----------
    root_path - work dir
    cmd_line_year - nems projection year
    iteration - ephrts iteration
    start_year - nems start year
    aimms_path - path to ephrts aimms project file
    debug - flag to run in standalone or not

    Returns
    -------
    void
    '''

    if int(iteration) == 0:
        restore_elec_prices.main(root_path, cmd_line_year, iteration, start_year)
        RunEphrts.run_ephrts(aimms_path, cmd_line_year, iteration, debug, root_path)
        supply_curve_algorithm.main(cmd_line_year, iteration, root_path)
    elif int(iteration) == 1:
        restore_elec_prices.main(root_path,cmd_line_year,iteration,start_year)
        RunEphrts.run_ephrts(aimms_path, cmd_line_year, iteration, debug, root_path)
        supply_curve_algorithm.main(cmd_line_year, iteration, root_path)
    elif int(iteration) == 2:
        restore_elec_prices.main(root_path,cmd_line_year,iteration,start_year)
        RunEphrts.run_ephrts(aimms_path, cmd_line_year, iteration, debug, root_path)


if __name__ == "__main__":

    # this production switch is used only for debugging, else it should be set to True for production runs
    production = True

    if production:
        root_path = os.getcwd()
        cmd_line_year = sys.argv[1]
        iteration = sys.argv[2]
        start_year = sys.argv[3]
        aimms_path = sys.argv[4]
        debug = False
    else:
        root_path = r"Q:\output\edt\ref2024_solo\d092123k"
        cmd_line_year = "2050"
        iteration = "1"
        start_year = "2024"
        aimms_path = "C:\\AIMMS_Installation_Free_Releases\\4.76.11.11-x64-VS2017\\Bin\\AimmsCmd.exe"
        debug = True

    main(root_path, cmd_line_year, iteration, start_year, aimms_path,debug)

