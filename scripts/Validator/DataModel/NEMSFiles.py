import os
import fnmatch
import pandas as pd
from os.path import exists

class BaseFile:
    def __init__(self, path):
        self.path = path

    def open(self):
        pass


class Nohup(BaseFile):
    def __init__(self, path):
        super(Nohup, self).__init__(path=path)

class UnifApiCsv(BaseFile):
    def __init__(self, path):
        super(UnifApiCsv, self).__init__(path=path)
        self.df = pd.read_csv(path)

    def load_table(self, table_number):
        if self.df is not None:
            return self.df[self.df["TableNumber"] == table_number]

    def load_table_entire(self):
        if self.df is not None:
            return self.df

class BaseFolder:
    def __init__(self, path):
        self.path = path
        self.nohup = None


class FromAIMMS(BaseFolder):
    def __init__(self, path):
        super(FromAIMMS, self).__init__(path)
        self.files = os.listdir(self.path)


class Lfmmlog(BaseFile):
    def __init__(self, path):
        super(Lfmmlog, self).__init__(path=path)

class CtsLog(BaseFile):
    def __init__(self, path):
        super(CtsLog, self).__init__(path=path)

class CtsModelLog(BaseFile):
    def __init__(self, path):
        super(CtsModelLog, self).__init__(path=path)
class CtsReportLog(BaseFile):
    def __init__(self, path):
        super(CtsReportLog, self).__init__(path=path)

class P1(BaseFolder):
    def __init__(self, path):
        super(P1, self).__init__(path=path)
        self.ngas = None
        self.lfmm = []
        self.lfmmlog = None
        self.scan_directory()

    def scan_directory(self):
        for file in os.listdir(self.path):
            if fnmatch.fnmatch(file, "nohup.out"):
                self.nohup = Nohup(path=os.path.join(self.path, file))
            elif fnmatch.fnmatch(file, "ngas"):
                self.ngas = FromAIMMS(os.path.join(self.path, "ngas/fromAIMMS"))
            elif fnmatch.fnmatch(file, "LFMM_p*.gdx.gz"):
                self.lfmm.append(os.path.join(self.path, file))
            elif fnmatch.fnmatch(file, "LFMM.log"):
                self.lfmmlog=Lfmmlog(path=os.path.join(self.path, file))


class P2(BaseFolder):
    def __init__(self, path):
        super(P2, self).__init__(path=path)
        self.rest = None
        self.coal = None
        self.ctus = None
        self.cts_log = None
        self.cts_model_log = None
        self.cts_report_log = None
        self.scan_directory()

    def scan_directory(self):
        for file in os.listdir(self.path):
            if fnmatch.fnmatch(file, "nohup.out"):
                self.nohup = Nohup(path=os.path.join(self.path, file))
            elif fnmatch.fnmatch(file, "coal"):
                self.coal = FromAIMMS(os.path.join(self.path, "coal/fromAIMMS"))
            elif fnmatch.fnmatch(file, "rest"):
                self.rest = FromAIMMS(os.path.join(self.path, "rest/fromAIMMS"))
            elif fnmatch.fnmatch(file, "CTSSoln.gdx*"):
                self.ctus = os.path.join(self.path, file)
            elif fnmatch.fnmatch(file, "CTS.log"):
                self.cts_log=CtsLog(path=os.path.join(self.path, file))
            elif fnmatch.fnmatch(file, "CTSmodel.log"):
                self.cts_model_log=CtsModelLog(path=os.path.join(self.path, file))
            elif fnmatch.fnmatch(file, "CTSreprt.log"):
                self.cts_report_log=CtsReportLog(path=os.path.join(self.path, file))

class P3(BaseFolder):
    def __init__(self, path):
        super(P3, self).__init__(path=path)
        self.scan_directory()

    def scan_directory(self):
        for file in os.listdir(self.path):
            if fnmatch.fnmatch(file, "nohup.out"):
                self.nohup = Nohup(path=os.path.join(self.path, file))


class CsvHolder(BaseFile):
    def __init__(self, path):
        super(CsvHolder, self).__init__(path=path)
        self.df = pd.read_csv(path)

    def load_table(self):
        if self.df is not None:
            return self.df

class XlsxHolder(BaseFile):
    def __init__(self, path):
        super(XlsxHolder, self).__init__(path=path)
        self.df = pd.read_excel(path)

    def load_table(self):
        if self.df is not None:
            return self.df

class StatusControl(BaseFile):
    def __init__(self, path):
        super(StatusControl, self).__init__(path=path)
        self.df = pd.read_csv(path)
        self.dict_status_list = self.load_dictionary()

    def load_table(self):
        if self.df is not None:
            return self.df

    def load_dictionary(self):
        df = self.load_table()

        column_names = ['Test Case', 'Status']
        df = df.loc[:, column_names]
        df[column_names[0]].str.strip()
        df[column_names[1]].str.strip()
        df[column_names[0]] = df[column_names[0]].str.split('(').str[0].str.strip()
        #df[column_names[0]] = df[column_names[0]].str.split('(', 1).str[0].str.strip()
        # aeo2023_py37_b doesn't seem support the regx groups
        #df[column_names[0]]= df[column_names[0]].str.extract("^\s*(\w+)\s*\((.*)\)")
        # for debugging:
        df.to_csv(r'.\validator_debug_status.csv', index=False)

        return dict(df.values)

class NEMSFiles(BaseFolder):

    def __init__(self, path):
        super(NEMSFiles, self).__init__(path=path)
        self.csv = None
        self.nohup = None
        self.parnems = False

        self.status_control = None

        self.csv_steo = None
        self.csv_aeo2steo_ngmm = None
        self.csv_aeo2steo_cmm = None
        self.csv_aeo2steo_emm = None
        self.csv_aeo2steo_blg = None
        self.csv_aeo2steo_ind = None
        self.csv_aeo2steo_itg = None
        self.csv_aeo2steo_liq = None
        self.csv_aeo2steo_ogsm = None
        self.csv_aeo2steo_mco = None

        self.scan_directory()

    def scan_directory(self):
        # this code block is for debugging:
        lg = r'.\validator_debug_NEMSFiles.log'
        with open(lg, 'a') as f:
            f.write(f'self.path={self.path}\n')
            p = f'{self.path}\\Validator\\input\\validator_controller.csv'
            f.write(f'validator_controller={exists(p)}\n')

        for file in os.listdir(self.path):
            if fnmatch.fnmatch(file, '*.unif.api.csv'):
                self.csv = UnifApiCsv(path=os.path.join(self.path, file))
            elif fnmatch.fnmatch(file, "nohup.out"):
                self.nohup = Nohup(path=os.path.join(self.path, file))
            elif file == "p1":
                self.parnems = True
                self.p1 = P1(path=os.path.join(self.path, file))
            elif file == "p2":
                self.parnems = True
                self.p2 = P2(path=os.path.join(self.path, file))
            elif file == "p3":
                self.parnems = True
                self.p3 = P3(path=os.path.join(self.path, file))

        p = f'{self.path}\\Validator\\input\\validator_controller.csv'
        if exists(p):
            self.status_control = StatusControl(path=p)
        # for debugging:
        #self.status_control = StatusControl(path=p)

        p = f'{self.path}\\Validator\\input\\steovars.csv'
        if exists(p):
            self.csv_steo = CsvHolder(path=p)

        p = f'{self.path}\\Validator\\input\\steo_benchmark_ngmm.csv'
        if exists(p):
            self.csv_aeo2steo_ngmm = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_cmm.csv'
        if exists(p):
            self.csv_aeo2steo_cmm = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_emm.csv'
        if exists(p):
            self.csv_aeo2steo_emm = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_buildings.csv'
        if exists(p):
            self.csv_aeo2steo_blg = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_industrial.csv'
        if exists(p):
            self.csv_aeo2steo_ind = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_integration.csv'
        if exists(p):
            self.csv_aeo2steo_itg = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_liquids.csv'
        if exists(p):
            self.csv_aeo2steo_liq = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_ogsm.csv'
        if exists(p):
            self.csv_aeo2steo_ogsm = CsvHolder(path=p)
        p = f'{self.path}\\Validator\\input\\steo_benchmark_macro.csv'
        if exists(p):
            self.csv_aeo2steo_mco = CsvHolder(path=p)
