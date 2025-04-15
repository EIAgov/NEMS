"""
A collection of AIMMS utility functions
"""
from datetime import datetime
import numpy as np
import pandas as pd
from tabulate import tabulate


def read_aimms_output(calendar_year, iteration):
    """Read a text file comprising one or more AIMMS composite tables.

    Note: Due to the "assert" near the end of the function, any call
    to this function must be wrapped in try/except.

    Parameters
    ----------
    calendar_year : int
        calendar year, such as 2025
        
    iteration : int
        iteration number

    Returns
    -------
    dict
        dict of pandas dataframes
    """
    y = calendar_year
    my_iter = iteration

    my_file = f"fromAIMMS/GlobalDataToNEMS_{str(y)}_{str(my_iter).zfill(2)}.txt"

    with open(my_file) as f:
        lines = f.readlines()

    # record start (and therefore end) of each composite table
    # ignore comment lines (with ! as firt character) and strip newlines
    my_list = [i.strip().replace('\n','') for i in lines if i[0]!='!']
    x2 = [i for i, x in enumerate(my_list) if x.upper().strip().endswith('COMPOSITE TABLE:')]
    x2.append(len(my_list))
    
    dfd = {}
    for tc in range(len(x2) - 1):
        z = my_list[x2[tc] + 1 : x2[tc + 1] - 2]
        z = [i for i in z[:] if i != '']
        df = pd.DataFrame(columns=z[0].split(), data=[row.split() for row in z[1:]])
        
        # replace "zero" in last column with 0.0
        my_col = df.columns[-1]
        df.loc[df[my_col] == "zero", my_col] = 0.0

        assert not df.isnull().any().any()

        dfd[df.columns[-1]] = df.copy()

    return dfd


def write_aimms_input(my_module, pyfiler, my_var, base_year):
    """Write AIMMSS input file comprising multiple composite tables.

    Parameters
    ----------
    my_module : string
        module name
        
    pyfiler : module
        pyfiler fortran module
        
    my_var : dict
        dict of NEMS variables
        
    base_year : int

    Returns
    -------
    dict
        currently empty
    """ 
    # TODO: every dimension parameter is an integer...
    #INTVAR = ["M2", "M3", "M4", "MNUMC2", "MNUMYR", "MXNCL", "MNXYRS", "MX_RNK", "MX_SO2", "MXSO2T", "MX_UNT", "NDREGN", "NUTSEC"] 
    
    y_int = int(pyfiler.ncntrl.curiyr)
    y = y_int + 1989  # TODO: or, curcalyr

    if y > base_year:
        pass  # for debugging breakpoint

    iter = int(pyfiler.ncntrl.curitr)
    
    ncrl = int(pyfiler.ncntrl.ncrl)
    f_out = 'monitor.in.txt'
    with open(f_out, 'w', encoding='utf-8') as f:
        f.write(f"ncntrl_curcalyr('1'):={y};\n")
        f.write(f"ncntrl_curitr('1'):={iter};\n")
        f.write(f"ncntrl_ncrl('1'):={ncrl};\n")  # not read if zero (sparsity)
        f.write('sAction:="MainExecution";')

    f_out = 'monitor.in.debug.txt'
    with open(f_out, 'a', encoding='utf-8') as f:
        f.write(f"\n{datetime.now()}\n")
        f.write(f"ncntrl_curcalyr('1'):={y};\n")
        f.write(f"ncntrl_curitr('1'):={iter};\n")
        f.write(f"ncntrl_ncrl('1'):={ncrl};\n")  # not read if zero (sparsity)
        f.write('sAction:="MainExecution";\n\n')

    # only if parquet switch is ON
    #if not(os.path.isdir(f'ngas/toAIMMS/{y}_{iter}')):
    #    os.mkdir(f'ngas/toAIMMS/{y}_{iter}')

    f_out = 'monitor.out.txt'
    with open(f_out, 'w', encoding='utf-8') as f:
        f.write(' ')

    # requires that os.getcwd() be the proper subdirectory (ngas or coal, for example)
    gds = f"toAIMMS/GlobalDataToAIMMS_{y}_{str(iter).zfill(2)}.txt"
    with open(gds, 'w', encoding='utf-8') as f:
        f.write("")
    f = open(gds, 'a', encoding='utf-8')

    my_tables = {}
    for k, v in my_var.items():
#        if k.upper() in ['COALEMM.QCLCLNR', 'COALEMM.RCLCLNR', 'COALEMM.XCLCLNR']:
#            x = np.copy(eval(f"pyfiler.utils.{k.split('.')[1].lower()}"))       
#        else: 
#            x = np.copy(eval(f"pyfiler.{k.lower()}"))
        try:
            x = np.copy(eval(f"pyfiler.{k.lower()}"))
        except:
            # print(f"in pyfiler.utils?: {k}")
            x = np.copy(eval(f"pyfiler.utils.{k.split('.')[1].lower()}")) 

        my_col = k.replace('.', '_')
        s = x.shape

        #if (k.upper() == "QBLK.QCLEL"):
        #    x[0]  # MNUMCR = 1, all years
        #    pass  # for debugging breakpoint

        if k == 'COALEMM.EMM_CL_UNITS':
            # print('Do not process restart variable COALEMM.EMM_CL_UNITS, per Coal team')
            pass

        elif str(k.upper()).startswith('NCHAR.'):  # (also check len(s)==0 ?)
            f.write("\nCOMPOSITE TABLE:\n")
            temp = {'M1': [1], my_col: [str(x, encoding='utf-8').strip()]}
            temp[my_col][0] = '"' + temp[my_col][0] + '"'
            f.write(tabulate(temp, v + [k.replace(".", "_")], tablefmt="plain" ))
            f.write('\n;')
            pass

        elif len(s) == 0:
            f.write("\nCOMPOSITE TABLE:\n")
            if "CONVFACT" in k:
                temp = {'M1': [1], my_col: [float(x)]}    
            else:
                temp = {'M1': [1], my_col: [int(x)]}
            f.write(tabulate(temp, v + [k.replace(".", "_")], tablefmt="plain"))
            f.write('\n;')

            # #  TODO: needed?
            # if k=='CYCLEINFO.CURIRUN':
            #     pass
            
        elif len(s) == 1:
            L = [[i1+1, x[i1]] 
                  for i1 in range(x.shape[0])]
            my_df = pd.DataFrame(L)
            my_df.columns = v + [k.replace(".", "_")]

            if (my_module=="coal" or my_module=="hmm") and (y > base_year) and ("MNUMYR" in my_df.columns):
                my_df = my_df[my_df["MNUMYR"] == y_int]
                
            f.write("\nCOMPOSITE TABLE:\n")
            f.write(my_df.to_string(index=False, float_format=lambda x: "{:.15f}".format(x)))
            f.write("\n;\n")

        elif len(s) == 2:
            L = [[i1+1, i2+1, x[i1, i2]] 
                  for i1 in range(x.shape[0]) 
                  for i2 in range(x.shape[1])]
            my_df = pd.DataFrame(L)
            my_df = my_df.sort_values([my_df.columns[1], my_df.columns[0]])
            my_df.columns = v + [k.replace(".", "_")]

            if (my_module=="coal" or my_module=="hmm") and (y > base_year) and ("MNUMYR" in my_df.columns):
                my_df = my_df[my_df["MNUMYR"] == y_int]
                
            f.write("\nCOMPOSITE TABLE:\n")
            f.write(my_df.to_string(index=False, float_format=lambda x: "{:.15f}".format(x)))
            f.write("\n;\n")

        elif len(s) == 3:
            L = [[i1+1, i2+1, i3+1, x[i1, i2, i3]] 
                  for i1 in range(x.shape[0]) 
                  for i2 in range(x.shape[1])
                  for i3 in range(x.shape[2])]
            my_df = pd.DataFrame(L)
            my_df = my_df.sort_values([my_df.columns[2], my_df.columns[1], my_df.columns[0]])
            my_df.columns = v + [k.replace(".", "_")]

            if (my_module=="coal" or my_module=="hmm") and (y > base_year) and ("MNUMYR" in my_df.columns):
                my_df = my_df[my_df["MNUMYR"] == y_int]

            f.write("\nCOMPOSITE TABLE:\n")
            f.write(my_df.to_string(index=False, float_format=lambda x: "{:.15f}".format(x)))
            f.write("\n;\n")

        elif len(s) == 4:
            L = [[i1+1, i2+1, i3+1, i4+1, x[i1, i2, i3, i4]] 
                  for i1 in range(x.shape[0]) 
                  for i2 in range(x.shape[1])
                  for i3 in range(x.shape[2])
                  for i4 in range(x.shape[3])]
            my_df = pd.DataFrame(L)
            my_df = my_df.sort_values([my_df.columns[3], my_df.columns[2], my_df.columns[1], my_df.columns[0]])
            my_df.columns = v + [k.replace(".", "_")]

            if (my_module=="coal" or my_module=="hmm") and (y > base_year) and ("MNUMYR" in my_df.columns):
                my_df = my_df[my_df["MNUMYR"] == y_int]

            f.write("\nCOMPOSITE TABLE:\n")
            f.write(my_df.to_string(index=False, float_format=lambda x: "{:.15f}".format(x)))
            f.write("\n;\n")
            
        elif len(s) == 5:
            L = [[i1+1, i2+1, i3+1, i4+1, i5+1, x[i1, i2, i3, i4, i5]] 
                  for i1 in range(x.shape[0]) 
                  for i2 in range(x.shape[1])
                  for i3 in range(x.shape[2])
                  for i4 in range(x.shape[3])
                  for i5 in range(x.shape[4])]
            my_df = pd.DataFrame(L)
            my_df = my_df.sort_values([my_df.columns[4], my_df.columns[3], my_df.columns[2], my_df.columns[1], my_df.columns[0]])
            my_df.columns = v + [k.replace(".", "_")]

            if (my_module=="coal") and (y > base_year) and ("MNUMYR" in my_df.columns):
                my_df = my_df[my_df["MNUMYR"] == y_int]

            f.write("\nCOMPOSITE TABLE:\n")
            f.write(my_df.to_string(index=False, float_format=lambda x: "{:.15f}".format(x)))
            f.write("\n;\n")

        elif len(s) == 6:
            print("6 dimensions not yet handled: ", k, v, s)
            my_table = 'nil'
   
    f.close()

    # read toAIMMS file and check for string "NaN"
    # if found, write nan_found.txt to parent folder (p1 or p2)
    with open(gds, 'r', encoding='utf-8') as f:
        z = f.readlines()
    if len([i for i in z if "NaN" in i]) > 0:
        with open("../nan_found.txt", "w") as f:
            f.write("")  # empty file
    
    return my_tables  # TODO: still filled ?
