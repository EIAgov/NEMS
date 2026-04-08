import os
import pandas as pd


filelist_header = """******************************************************************************
*FILE MANAGER DATA FILE.
*
* RCS Information on this file follows:
* $Header: m:/default/scripts/RCS/filemgr.shell,v 1.350 2020/08/24 20:00:54 EDT Exp $
*
*THIS CONTAINS LIST OF FILES TO OPEN/CLOSE WITH THE NEMS
*SYSTEM.  RECORDS IN THIS FILE STARTING WITH A NONBLANK CHARACTER ARE COMMENTS.
*THE FIRST DATA LINE REPRESENTS THE FILE NAMES.  IT IS A DELIMITED RECORD.  THE
*NEXT RECORD IS POSITIONAL.  THE FILE IS FORMATTED AS FOLLOWS:
*
* -------------   ------------------------------------------------------ ------
* VARIABLE        DESCRIPTION                                            CHARS
* --------------- ------------------------------------------------------ ------
* F_ID            PC FILE NAME:  EG., "FILENAME.EXE"                        8
* F_IOTYPE        INPUT OR OUTPUT FILE:  "READ", "WRITE", OR "READWRITE"    9
* F_ACCESS        FILE ACCESS TYPE:  "SEQUENTIAL"OR "DIRECT"               10
* F_FORM          FILE FORMAT:  "UNFORMATTED"OR "FORMATTED"                11
* F_LRECL         LOGICAL RECORD LENGTH:  INTEGER
* F_DESCRIPTION   FILE DESCRIPTION                                         60
* --------------- ------------------------------------------------------ ------
* FILE NAME CHARACTERISTICS MUST BEGIN IN COLUMN 2
*******************************************************************************
*
"""



class ScedesProcessor:
    scedes = pd.DataFrame()
    filelist = pd.DataFrame()
    moreopt = pd.DataFrame()
    scedes_all = pd.DataFrame()
    outdir = None
    scen = None
    datekey = None
    

    def create_filelist(self):
        #make an exception for restartI in filelist and restartn in scedes
        
        # The ModuleOwner and UsedInFilelist are used in conjunctin to determine whether the file is written into the FILELIST
        # e.g. If a Module is switched on, all keys to be UsedInFileList with ModuleOwner that does not match, will not appear in the FILELIST

        filelist = self.filelist.copy().fillna('')
        def editKeys(row):
            exempt_keys= ['RBENALLN', 'INDN']
            if row['Key'] == 'RESTARTN':
                row['Key'] = 'RESTARTI'
            if row['Key'] == 'RBENALLN':
                pass
            if row['Key'].endswith('N') and (row['Key'] not in exempt_keys):
                row['Key'] =  row['Key'][:-1]
            return row['Key']

        filelist['Key'] = filelist.apply(editKeys, axis=1)
        
        new_row = filelist['Filelist_format'].str.strip() + ' ' +  filelist['Description'].str.strip()
        filelist['new'] = new_row
        filelist = filelist.drop(columns=['Description', 'UsedInMoreOpt', 'UsedInFilelist', 'Filelist_format'])
        
        filename = 'FILELIST.' + self.scen + '.' + self.datekey
        outpath = os.path.join(self.outdir, filename)
        with open(outpath, 'w') as file:
            file.write(filelist_header)
            # Iterate over each row
            for index, row in filelist.iterrows():
                if row.ModuleOwner.startswith('EX'):
                    if str(self.scedes.loc[self.scedes['Key'] == row.ModuleOwner, 'Value'].values[0]) == str(0):
                        continue
                # Convert row to string, joining elements with a space
                row = row.drop('ModuleOwner')
                row_string = ' '.join(str(x) for x in row.values)
                # Write the string to the file, adding a newline character
                file.write(' ' + row_string + '\n')
    def create_moreopt(self):
        def is_path(text):
            """Checks if a string is an int, otherwise it is a path."""
            try:
                float(text)
                return ''
            except:
                return '0000'
        
        def join_and_pad(row):
            """join key and value. Add spaces so the value starts at postion 10"""
            exempt_keys = ['AIMECPPAR', 'H2GETVARS', 'H2PUTVARS', 'RUNEMMSQL', 'NEMSPYENV','NGTEXASN', 'H2AIMMSN']
            matching = [x for x in exempt_keys if row['Key']  in x.strip()]
        
            position_int = 9
            if matching:
                if len(matching[0]) > 8:
                    row['Key'] = matching[0]
                    position_int = 10
                else:
                    row['Key'] = matching[0][:-1]
                   
            position = (len(row['Key'])) + (position_int - len(row['Key']))

            path = ''
            if row['path_exists']:
                path = row['path_exists'] + ' '
            val = row['Key'].ljust(position,' ') + path  + row['Value']
            return val      
       
        e = {'H2PUTVARS', 'CLHEWQ2N', 'CLCPSR2N', 'NEMSPYENV', 'NGTEXAS', 'H2AIMMS', 'AIMECPPAR', 'CLRCAF2N', 'RUNEMMSQL', 'CLCDSR2N', 'H2GETVARS'}


        moreopt = self.moreopt.copy()
        moreopt['Key'] = moreopt['Key'].str[:8]
        # do more logic to check if key ends in N for example ngtexasn is the scedes key and 8 chars, but the moreopt key is ngtextas
        moreopt = moreopt.drop(columns=['ModuleOwner', 'Description', 'UsedInMoreOpt', 'UsedInFilelist', 'Filelist_format'])
        moreopt['path_exists'] = moreopt['Value'].apply(is_path)
        moreopt = moreopt[['Key', 'path_exists', 'Value']]
        val = moreopt.apply(join_and_pad, axis=1) 
        filename = 'MOREOPT.' + self.scen + '.' + self.datekey
        outpath = os.path.join(self.outdir, filename)
        with open(outpath, 'w') as f:
            for item in val:
                f.write(item + '\n')

        pass
    def create_scedes_all(self):
        scedes = self.scedes.copy()
        new_rows = {'SCEN': self.scen, 'DATE': self.datekey, 'OUTDIR': self.outdir}
        df_new_row = pd.DataFrame(new_rows.items(), columns=['Key', 'Value'])
        scedes =  pd.concat([df_new_row, scedes], ignore_index=True)
        scedes_all = scedes['Key'] + '=' + scedes['Value']
        filename = 'scedes.all.' + self.scen + '.' + self.datekey
        outpath = os.path.join(self.outdir, filename)
        with open(outpath, 'w') as f:
                for item in scedes_all:
                    f.write(item + '\n')

        pass
    def scedes_to_user_obj():
        pass
    def process_scedes_file(self,scedes_inputfile):
        self.scedes = pd.read_csv(scedes_inputfile,header=0,comment='#')
        scedes = self.scedes.copy()
        s = scedes['Key'].duplicated()
        ind = s[s].index
        scedes = scedes.drop(ind)
        self.filelist = scedes[scedes['UsedInFilelist'] != 0]
        self.moreopt = scedes[scedes['UsedInMoreOpt'] != 0]
        self.create_filelist()
        self.create_moreopt()
        self.create_scedes_all()
        pass
    def run(self,scedes_input):
        if type(scedes_input) is list:
            scedes_inputfile = scedes_input[0]
            self.outdir = scedes_input[1]
            self.scen = scedes_input[2]
            self.datekey = scedes_input[3]
        elif os.path.isfile(scedes_input):
            scedes_inputfile = scedes_input
            self.outdir = os.getcwd()
            self.scen = 'test'
            self.datekey = 'd1234'
        else:
            print('incorrect file input')
            exit()
        self.process_scedes_file(scedes_inputfile)
        pass


if __name__=="__main__":
    file = 'scedes.ref2025.csv'
    sp = ScedesProcessor()
    sp.run(file)