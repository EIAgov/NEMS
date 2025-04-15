# -*- coding: utf-8 -*-
"""
Created on Feb 24 2023

@author: Claire Su
"""

import os
import pandas as pd

class NEMSxilink:
    def __init__(self, NEMS):
        '''
        The init method of NEMSxilink class.

        Parameters
        ----------
        NEMS : string
            the NEMS path.

        Returns
        -------
        None.

        '''
        self.dir_scripts = os.path.join(NEMS.replace('/', '\\'), 'scripts')
        self.NEMS = NEMS.replace('/', '\\')

        
    def parse_config_prepare_xilink(self):
        '''
        Prepare the required environment variable configuration for xilink command, to generate nems.exe etc.
        Read nems_config_xilink_nems_ftab.csv and generate tmp-nems_config_set_xilink_ftab.bat for xilink to use.

        Returns
        -------
        None.

        '''
        file = os.path.join(self.dir_scripts,'setup','input','nems_config_xilink_nems_ftab.csv')
        dst = os.path.join(self.dir_scripts,'setup','lib','tmp-nems_config_set_xilink_ftab.bat')
        
        df = pd.read_csv(file)
        df['value'] = df.groupby(['key'])['value'].transform(lambda x : ';'.join(x))
        # drop duplicate data
        df = df.drop_duplicates()
        df = df.astype(str)
        # make sure indexes pair with number of rows. (We remove the duplicates, need to reindex)
        df = df.reset_index()

        # replace '###' with $NEMS
        df['value'] = df['value'].str.replace('###',f'{self.NEMS}',regex=False)
        
        with open(dst, 'w', encoding='utf-8') as d:
            d.write('::Do not touch this file. It will be auto rewritten when a NEMS run starts (nemsxilink.py calls and rewrite this)\n')
            for idx,row in df.iterrows():
                d.write('SET '+df['key'][idx]+'='+df['value'][idx]+'\n')