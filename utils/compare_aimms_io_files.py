import os.path
import pandas as pd


def read_aimms_io_file(filepath):
    """Read a text file comprising one or more AIMMS composite tables.

    Note: Due to the "assert" near the end of the function, any call
    to this function must be wrapped in try/except.
    """
    my_file = filepath  # f"fromAIMMS/GlobalDataToNEMS_{str(y)}_{str(my_iter).zfill(2)}.txt"

    with open(my_file) as f:
        lines = f.readlines()

    # record start (and therefore end) of each composite table
    # ignore comment lines (with ! as firt character) and strip newlines
    my_list = [i.strip().replace('\n','') for i in lines if i[0]!='!']
    x2 = [i for i, x in enumerate(my_list) if x.upper() == 'COMPOSITE TABLE:']
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

        val_col = df.columns[-1]
        if val_col != "COALEMM_EMM_CL_UNITS":
            df[val_col] = df[val_col].astype(float)
            dfd[val_col] = df.set_index(list(df.columns[:-1]))

        if val_col == 'COALOUT_TTONMILE':
            pass

    return dfd


def main():

    EPS = 0.00001

    run_a = r"S:\output\bto\carbfee35mpy\d062824b"
    #run_b = r"D:\output\mc6\ref2024\d062024b"
    run_b = r"M:\ogs\mc6\d062024b"

    my_year = 2027
    my_iter = 1

    # for now, let's read ToAIMMS files (compare FromAIMMS files later...)
    end_path = f"coal/ToAIMMS/GlobalDataToAIMMS_{str(my_year)}_{str(my_iter).zfill(2)}.txt"

    end_path = f"coal/FromAIMMS/GlobalDataToNEMS_{str(my_year)}_{str(my_iter).zfill(2)}.txt"


    # 'USO2GRP_ECP_PECP'
    a = read_aimms_io_file(os.path.join(run_a, end_path))
    b = read_aimms_io_file(os.path.join(run_b, end_path))

    key_diff = ([i for i in a.keys() if i not in b.keys()] +
                [i for i in b.keys() if i not in a.keys()])
    if key_diff:
        print(key_diff)

    my_diff = {}
    for k in a.keys():
        temp = a[k] - b[k]
        temp[abs(temp) < EPS] = 0
        my_diff[k] = temp.sum()

    temp = [k for k, v in my_diff.items() if v.iloc[0] > 0]
    print(temp)

    z = pd.concat([a['COALOUT_TTONMILE'], b['COALOUT_TTONMILE']], axis=1)

    pass


if __name__ == "__main__":
    main()
