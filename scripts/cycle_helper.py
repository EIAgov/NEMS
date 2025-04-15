from datetime import datetime
import os
import pandas as pd
import sys

temp = os.path.join(os.path.dirname(os.path.abspath(__file__)), "reporter")
if temp not in sys.path:
    sys.path.append(temp)
from reporter import RW_reporter_main

LOGFILE = "nohup.out"
MODULE_NAME = "cycle_helper.py"


def log_it(n, s):
    """
    arg n: cycle number
    """
    with open(LOGFILE, "a", encoding="utf-8") as f:
        f.write(f"{datetime.now()} :: cycle {n} :: {MODULE_NAME} :: {s}\n")
        sys.stdout.flush()
        sys.stderr.flush()


def generate_report(SCEDES, curirun):
    cwd = os.getcwd()
    if not cwd.endswith("reporter"):
        os.chdir("reporter")
    RW_reporter_main.run(SCEDES, curirun)
    os.chdir(cwd)


def generate_model_runtime_report(my_cycle, my_path):
    """Generate runtime report by cycle, year, iteration for all models.

    The report is based on rows in the nohup file that include the
    string ", Runtime". This works for folders p1 and p2, but not for p3.

    Parameters
    ----------
    my_cycle : integer
        _
    my_path : string, such as d:/output/mc6/junk/p1

    Returns
    -------
    dataframe
        dataframe of model runtimes
    """
    log_it(my_cycle, "In generate_model_runtime_report")
    log_it(my_cycle, f"args: {locals()}")

    n = my_cycle

    with open(os.path.join(my_path, "nohup.out"), "r") as nemlog:
        z = nemlog.readlines()

    temp = [i.strip().split("::") for i in z if ", Runtime" in i]
    df = pd.DataFrame(temp, columns=["timestamp", "cycle_temp", "filename", "data"])

    if df.shape[0] > 0:
        # only "nexec.py" should appear in the filename column
        if len(df["filename"].unique()) > 1:
            log_it(n, f"Unexpected: {df['filename'].unique()=} length > 1")

        # process the timestamp column
        df['timestamp'] = df['timestamp'].str.strip()

        # process the main columns of interest
        cols = ["module", "time", "cycle", "year", "iter"]
        df[cols] = df["data"].str.split(",", expand=True)
        for i in cols:
            df[i] = df[i].str.split(":", expand=True)[1].str.strip()
        df["time"] = df["time"].astype(float)
        df["cycle"] = df["cycle"].astype(int)
        df["year"] = df["year"].astype(int)
        df["iter"] = df["iter"].astype(int)

        # construct final dataframe and write to csv and markdown
        df = df[["timestamp"] + cols]
        df = df.sort_values("timestamp")
        df.to_csv(os.path.join(my_path, "nems_time_by_model.csv"), index=False)
        df.to_markdown(os.path.join(my_path, "nems_time_by_model.md"), index=False)

    return df


if __name__ == "__main__":
    z = r"D:\output\mc6\short\d083024m"

    my_cycle = 99

    # ASSUME it's a parnems run...
    for i in ["p1", "p2", "p3"]:
        my_path = os.path.join(z, i)
        print(f"{my_path=}")
        df = generate_model_runtime_report(my_cycle, my_path)
        print(f"Report generated: {my_path}")
    pass