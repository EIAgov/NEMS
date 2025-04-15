import datetime
import os
import pandas as pd
import subprocess
import sys
import shutil
import numpy as np
import pickle
import logging
import time

from multiprocessing import Process
from subprocess import Popen, PIPE, STDOUT
import threading

import run_ecp_parallel as rp

sys.path.append(os.getcwd())


if __name__ == "__main__":
    """
    This function is to launch three AIMMS runs in parallel
    :param keylist: :type- object
    :param user: :type- object
    """
    start = time.time()
    user = 'augustine'
    strAppPath = os.path.dirname(os.path.realpath(__file__));
    strWrkFolder1 = strAppPath + "\\ecp_ge";
    strWrkFolder2 = strAppPath + "\\ecp_gsw";
    keylist = {
        'ecp_g_fldr_1': strWrkFolder1,
        'ecp_g_fldr_2': strWrkFolder2
    }
    LOG_DIR = '.'
    LOG_FILE = 'run_ecp_parallel.log'
    LOG_FQFN = os.path.join(LOG_DIR, LOG_FILE)

    # logging config
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)
    logging.basicConfig(filename=LOG_FQFN,
                        level=logging.DEBUG,
                        format='[%(asctime)s] [%(name)s] [%(threadName)s] '
                               '[%(funcName)s] [%(levelname)s] :: %(message)s')

    logging.info('Starting python handler for ECP parallel AIMMS runs')

    # Variable declarations
    # ------------------------------------
    f = open("CurrentRunYear.txt", "r")
    CurrentRunYear = ''
    CurrentRunYear = f.readline().strip()
    f.close()
    if CurrentRunYear.isnumeric():
        if int(CurrentRunYear) > 2060 or int(CurrentRunYear) <= 2020:
            logging.info('Incorrect run year found in CurrentRunYear.txt ')
    strWorkFolders = [keylist['ecp_g_fldr_1'] , keylist['ecp_g_fldr_2'] ]
    processes = []
    standalone = False

    for ecp_g_work_folder in strWorkFolders:
        strWrkFolder = ecp_g_work_folder
        print("sub national ECP AIMMS model run was launched from: ")
        os.chdir(strWrkFolder)
        print(os.getcwd())
        strWrkFolder = strWrkFolder.replace("\\","/")
        if (strWrkFolder.find(':')>0):
        # cmdstr = 'C:/AIMMS_Installation_Free_Releases/4.96.4.6-x64-VS2017/Bin/AimmsCmd.exe '  + strWrkFolder.split('/')[3]+ ':'+ strWrkFolder.split(strWrkFolder.split('/')[3],1)[1] +'/' +\
        #          strWrkFolder.split('/')[-1] + '.aimms < ' + strWrkFolder.split('/')[3]+ ':'+ strWrkFolder.split(strWrkFolder.split('/')[3],1)[1] +'/'+ strWrkFolder.split('/')[
        #              -1] + '_cmds_' + CurrentRunYear + '.txt' + ' > ' + strWrkFolder.split('/')[3]+ ':'+ strWrkFolder.split(strWrkFolder.split('/')[3],1)[1] +'/' + 'AimmsCmd_'+ CurrentRunYear +'_log.txt '
            cmdstr = 'C:/AIMMS_Installation_Free_Releases/4.96.4.6-x64-VS2017/Bin/AimmsCmd.exe '  + strWrkFolder +'/' +\
                    'ecp.aimms < ' + strWrkFolder +'/'+ strWrkFolder.split('/')[
                        -1] + '_cmds_' + CurrentRunYear + '.txt' + ' > ' + strWrkFolder + '/' + 'AimmsCmd_'+ CurrentRunYear +'_log.txt '
        else:
            cmdstr = 'C:/AIMMS_Installation_Free_Releases/4.96.4.6-x64-VS2017/Bin/AimmsCmd.exe '  + strWrkFolder.split('/')[3]+ ':'+ strWrkFolder.split(strWrkFolder.split('/')[3],1)[1] +'/' +\
                    'ecp.aimms < ' + strWrkFolder.split('/')[3]+ ':'+ strWrkFolder.split(strWrkFolder.split('/')[3],1)[1] +'/'+ strWrkFolder.split('/')[
                        -1] + '_cmds_' + CurrentRunYear + '.txt' + ' > ' + strWrkFolder.split('/')[3]+ ':'+ strWrkFolder.split(strWrkFolder.split('/')[3],1)[1] +'/' + 'AimmsCmd_'+ CurrentRunYear +'_log.txt '
        p = subprocess.Popen(cmdstr,stdout=subprocess.PIPE,stderr=subprocess.PIPE,start_new_session=True,close_fds=False,shell=True)
        processes.append(p)
        logging.info('current working folder: ' + strWrkFolder)
        logging.info('command string is: '+ cmdstr)
    time.sleep(2)
    
    # for ecp_g_work_folder in strWorkFolders:
    #     monitor_in_file = ecp_g_work_folder  + '/monitor.in.txt'
    #     #monitor_out_file = ecp_g_work_folder  + '/monitor.out.txt'
    #     #fmo = open(monitor_out_file,"r")
    #     #fmoline=fmo.readline().strip()
    #     #if (fmoline.lower().startwith("executing")):
    #     #     time.sleep(10)
    #     # fmo.close()
    #     with open(monitor_in_file, "w") as f:
    #         f.write('sAction             := "Quit";')
    #         f.close()      
    logging.info('Ending the AIMMS launcher for ECP parallel AIMMS runs ')
    #end = time.time()
    #print('')
    #print('')
    #print('Total run time without append operation was ' + str(end - start)+ ' seconds.')
    exit()



