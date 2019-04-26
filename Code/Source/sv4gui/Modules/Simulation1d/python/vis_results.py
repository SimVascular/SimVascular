#!/usr/bin/env python

""" 
This script is used to compare and visualize 1D solver results. 
"""
import argparse
import os 
import sys

from manage import get_logger_name, init_logging
from parameters import Parameters
from centerlines import *
from mesh import *
from utils import write_polydata, read_polydata

from matplotlib import pyplot as plt

logger = logging.getLogger(get_logger_name())

class Parameters():
    """ The Parameter class stores the input parameters.
    """
    class Units(object):
        MM = "mm"
        CM = "cm"

    def __init__(self):
        self.output_directory = None
        self.compare_model_name = None
        self.compare_output_directory = None
        self.model_name = None
        self.group = None
        self.segment = None
        self.time_step = 0.000588 
 
class Args(object):
    """ This class defines the command line arguments to the vis script.
    """
    PREFIX = "--"
    COMPARE_MODEL_NAME = "compare_model_name"
    COMPARE_OUTPUT_DIRECTORY = "compare_output_directory"
    DATA_NAME = "data_name"
    GROUP = "group"
    MODEL_NAME = "model_name"
    OUTPUT_DIRECTORY  = "output_directory"
    SEGMENT = "segment"
    TIME_STEP = "time_step"
    
def cmd(name):
    """ Create an argparse command argument.
    """
    return Args.PREFIX + name.replace("_", "-")

def parse_args():
    """ Parse command-line arguments."""
    parser = argparse.ArgumentParser()

    parser.add_argument(cmd(Args.COMPARE_MODEL_NAME), 
      help="Compare model name.")

    parser.add_argument(cmd(Args.COMPARE_OUTPUT_DIRECTORY), 
      help="Compare output directory.")

    parser.add_argument(cmd(Args.DATA_NAME), required=True,
      help="Data name.")

    parser.add_argument(cmd(Args.GROUP), required=True,
      help="Group number.")

    parser.add_argument(cmd(Args.MODEL_NAME), required=True,
      help="File name.")

    parser.add_argument(cmd(Args.OUTPUT_DIRECTORY), required=True,
      help="Compare output directory.")

    parser.add_argument(cmd(Args.SEGMENT), required=True,
      help="Segment number.")

    parser.add_argument(cmd(Args.TIME_STEP), 
      help="Time step.")

    return parser.parse_args(), parser.print_help

def set_parameters(**kwargs):
    """ Set the values of parameters input from the command line.
    """
    logger.info("Parse arguments ...")

    ## Create a Parameters object to store parameters.
    params = Parameters()

    ## Process arguments.
    #
    if kwargs.get(Args.COMPARE_MODEL_NAME):
        params.compare_model_name = kwargs.get(Args.COMPARE_MODEL_NAME)
        logger.info("Compare model name: %s" % params.compare_model_name)

    if kwargs.get(Args.COMPARE_OUTPUT_DIRECTORY):
        params.compare_output_directory = kwargs.get(Args.COMPARE_OUTPUT_DIRECTORY)
        if not os.path.exists(params.compare_output_directory):
            logger.error("The compare output directory '%s' was not found." % params.compare_output_directory)
            return None
        logger.info("Compare output directory: '%s'." % params.compare_output_directory)

    params.data_name = kwargs.get(Args.DATA_NAME)
    logger.info("Data name: %s" % params.data_name)

    params.group = kwargs.get(Args.GROUP)
    logger.info("Group number: %s" % params.group)

    params.model_name = kwargs.get(Args.MODEL_NAME)
    logger.info("Model name: %s" % params.model_name)

    params.output_directory = kwargs.get(Args.OUTPUT_DIRECTORY)
    if not os.path.exists(params.output_directory):
        logger.error("The output directory '%s' was not found." % params.output_directory)
        return None
    logger.info("Output directory: '%s'." % params.output_directory)

    params.model_name = kwargs.get(Args.MODEL_NAME)
    logger.info("Model name: %s" % params.model_name)

    params.segment = kwargs.get(Args.SEGMENT)
    logger.info("Segment number: %s" % params.segment)

    if kwargs.get(Args.TIME_STEP): 
        params.time_step = float(kwargs.get(Args.TIME_STEP))
    logger.info("Time step: %g" % params.time_step)

    return params 

class Results():
    def __init__(self, params):
        self.compare_data = None
        self.file_name = None 
        self.data = None
        self.data_name = params.data_name
        self.group = params.group
        self.model_name = params.model_name
        self.params = params
        self.save_freq = 20
        self.segment = params.segment
        self.time = None
        self.time_step = None
        self.vis_block_id = 0

    def read_data(self, time_step):
        """ Read in data.
        """
        file_name = self.get_file_name(self.params.output_directory, self.model_name)
        self.time_step = time_step
        data = []
        with open(file_name) as dfile:
            for line in dfile:
                data.append([float(v) for v in line.strip().split()])
        #__with open(file_name) as file
        self.data = data
        dt = self.time_step * self.save_freq
        self.time = [i*dt for i in range(len(data[0]))] 
        logger.info("Time size: %d" % (len(self.time)))
        logger.info("Time range: %g  %g" % (self.time[0], self.time[-1]))
        logger.info("Read %d data blocks from '%s'" % (len(data), self.file_name))
        logger.info("Data block size: %d" % (len(data[0])))

    def read_compare_data(self):
        """ Read in data to compare to the first data read.
        """
        model_name = self.params.compare_model_name
        output_dir = self.params.compare_output_directory
        file_name = self.get_file_name(output_dir, model_name)
        data = []
        with open(file_name) as dfile:
            for line in dfile:
                data.append([float(v) for v in line.strip().split()])
        #__with open(file_name) as file
        self.compare_data = data
        logger.info("Read compare %d data blocks from '%s'" % (len(data), self.file_name))
        logger.info("Compare data block size: %d" % (len(data[0])))

    def press_key(self, event):
        """ Add key events.

        Keys:
           q: key to quit.
        """
        if event.key == 'q':
            plt.close(event.canvas.figure)
        elif event.key.isdigit():
            self.vis_block_id = int(event.key)
            logger.info("Block number: %d" % self.vis_block_id)
            self.plot(self.vis_block_id)

    def get_file_name(self, output_dir, model_name):
        """ Get the file name associated with model, group, segment and data name.
        """
        file_name = "%sGroup%s_Seg%s_%s.dat" % (model_name, self.group, self.segment, self.data_name)
        return os.path.join(output_dir, file_name)

    def plot(self, block_id):
        """ Plot data for the given block.
        """
        t = self.time
        v = self.data[block_id]
        ylabel = self.data_name
        title = self.data_name + " : block " + str(block_id)

        fig, ax = plt.subplots()
        ax.plot(t, v, 'r')
        #ax.plot(t, v, 'r', label='first')
        max_val = max(self.data[block_id])
        min_val = min(self.data[block_id])

        if self.compare_data:
            ax.plot(t, self.compare_data[block_id], 'b.', label='compare')
            max_diff = 0.0
            for (v1, v2) in zip(self.data[block_id],self.compare_data[block_id]):
                diff = abs(v1 - v2)
                max_diff = diff if diff > max_diff else max_diff
            logger.info("Maximum difference: %f  (%f precent)" % (max_diff, 100.0*(max_diff / (max_val-min_val))))

        ax.set(xlabel='time (s)', ylabel=ylabel, title=title)
        ax.grid()
        ax.legend()

        # Set the figure window position.
        plt.get_current_fig_manager().window.wm_geometry("+200+100")

        # Add key events.
        cid = plt.gcf().canvas.mpl_connect('key_press_event', self.press_key)

        plt.show() 


if __name__ == '__main__':
    init_logging()
    args, print_help = parse_args()
    params = set_parameters(**vars(args))

    if params == None:
        sys.exit()

    results = Results(params)
    results.read_data(params.time_step)

    if params.compare_output_directory: 
        results.read_compare_data()
    results.plot(0)

