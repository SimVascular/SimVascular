#!/usr/bin/env pytho

import logging
import os 

def get_logger_name():
    return 'extract-results'

def get_log_file_name():
    return 'extract-results.log'

def init_logging(outputDir="./"):
    import logging
    logger = logging.getLogger(get_logger_name())
    logger.setLevel(logging.INFO)
    console_handler = logging.StreamHandler()
    formatter = logging.Formatter('[%(name)s] %(levelname)s - %(message)s')
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)

    logFile = os.path.join(outputDir, get_log_file_name())
    file_handler = logging.FileHandler(logFile, mode="w")
    logger.addHandler(file_handler)
    file_handler.setFormatter(formatter)

