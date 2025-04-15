# -*- coding: utf-8 -*-
"""
Created on Wen Jan 17 08:40:21 2024

@author: SZO
"""

import os
import time
import logging
from configparser import ConfigParser


def setup_logging(logger_name, log_file_name, debug, display_on_screen):
    """
    Configures logging for a specified logger.

    Parameters
    ----------
    logger_name : str
        The name of the logger.
    log_file_name : str
        The name of the log file.
    display_on_screen : bool, optional
        If True, logs will also be displayed on the screen (default is True).

    Returns
    -------
    logging.Logger
        The configured logging instance.
    """
    
    # TODO: mc6 test to keep the logger from overwriting nohip,out
    display_on_screen = False
    
    logger = logging.getLogger(logger_name)
    logger.setLevel(logging.DEBUG)
    
    # Clear previous handlers
    logger.handlers = []

    # # Create handlers
    # file_handler = logging.FileHandler(log_file_name, mode='w')
    # file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
    # logger.addHandler(file_handler)
    
    # if display_on_screen:
    #     stream_handler = logging.StreamHandler()
    #     stream_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
    #     logger.addHandler(stream_handler)

    # Create handlers
    if debug:
        file_handler = logging.FileHandler(log_file_name, mode='w')
        file_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
        logger.addHandler(file_handler)

        if display_on_screen:
            stream_handler = logging.StreamHandler()
            stream_handler.setFormatter(logging.Formatter('%(asctime)s - %(levelname)s - %(message)s'))
            logger.addHandler(stream_handler)

    return logger


def log_execution(logger, message):
    """
    Implementes a decorator to log execution time of a function using the provided logger 
    along with the specified message.

    Parameters
    ----------
    logger : Logger
        The logger object used for logging.
    message : str
        The message to be logged along with the execution time.

    Returns
    -------
    function
        The decorated function that logs the execution time.

    Example
    -------
        @log_execution(logging.getLogger(__name__), 'make_base_tables')
        def make_base_tables(table_spec, dfd_sme):
    """

    def decorator(func):
        def wrapper(*args, **kwargs):
            start_time = time.perf_counter()
            result = func(*args, **kwargs)
            end_time = time.perf_counter()
            execution_time = end_time - start_time
            logger.debug("%s - Execution time: %f seconds",
                         message, execution_time)
            return result
        return wrapper
    return decorator

# Get debug settings from config.ini
config = ConfigParser()
#print(os.getcwd())
if os.getcwd()[-8:] != 'reporter':
    configpath = os.getcwd() + '\\reporter\\config.ini'
else:
    configpath = os.getcwd()+'\\config.ini'
config.read(configpath)
config.sections()

# Create loggers for reporter main and make ran seperately
if os.getcwd()[-8:] != 'reporter':
    logpath = os.path.join(os.getcwd(),'reporter')
else:
    logpath = os.getcwd()
        
main_logger = setup_logging('main', os.path.join(logpath,'RW_log.log'),
                            int(config.get('debugging', 'debug')) == 1,
                            int(config.get('debugging', 'debug_screen')) == 1)
RAN_logger = setup_logging('ran', os.path.join(logpath,'RAN_log.log'),
                            int(config.get('debugging', 'debug')) == 1,
                            int(config.get('debugging', 'debug_screen')) == 1)

# Write debug settings to log 
main_logger.debug("Read config.ini")
main_logger.debug(f"debug = {config.get('debugging', 'debug')}")
main_logger.debug(f"debug_base ={config.get('debugging', 'debug_base')}")
main_logger.debug(f"debug_screen ={config.get('debugging', 'debug_screen')}")
