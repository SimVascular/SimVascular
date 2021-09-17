# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

""" 
This script is used to extract data from SimVascular 0D or 1D solver results files and
write them to CSV format files. The script can be executed from the command line 
or from a C++ program.

When run from the command line the script can optionally be used to

  1) Plot selected segment data

  2) Visualize 1D network geometry


Results can be converted and plotted for

  1) A list of segment names

  2) Outlet segments

  3) All segments


Segments can also be interactively selected from 1D network geometry displayed 
in a graphics window. The results of the selected segments can be converted but 
not plotted.

----------------------
Command Line Arguments
----------------------
The script accepts named arguments of the form

    --NAME VALUE

"""

import argparse
import os 
import sys
import logging

from .manage import get_logger_name, init_logging, get_log_file_name
from .parameters import Parameters
from .solver import Solver
from .post import Post

try:
    import vtk
    from .graphics import Graphics 
except ImportError:
    pass

logger = logging.getLogger(get_logger_name())

class Args(object):
    """ 
    This class defines the command line arguments for the extract_results.py script.
    """
    PREFIX = "--"
    ALL_SEGMENTS = "all_segments"
    CENTERLINES_FILE = "centerlines_file"
    DATA_NAMES = "data_names"
    DISPLAY_GEOMETRY = "display_geometry"
    VOLUME_MESH_FILE = "volume_mesh_file"
    MODEL_ORDER = "model_order"
    NODE_SPHERE_RADIUS = "node_sphere_radius"
    OUTPUT_DIRECTORY  = "output_directory"
    OUTPUT_FILE = "output_file_name"
    OUTPUT_FORMAT = "output_format"
    OUTLET_SEGMENTS = "outlet_segments"
    PLOT = "plot"
    RESULTS_DIRECTORY  = "results_directory"
    SEGMENTS = "segments"
    SELECT_SEGMENTS = "select_segments"
    SOLVER_FILE_NAME = "solver_file_name"
    TIME_RANGE = "time_range"
    WALLS_MESH_FILE = "walls_mesh_file"
    
def cmd(name):
    """ 
    Create an argparse command argument.
    """
    return Args.PREFIX + name.replace("_", "-")

def parse_args():
    """ 
    Parse command-line arguments.
    """
    parser = argparse.ArgumentParser()

    parser.add_argument(cmd(Args.ALL_SEGMENTS), action='store_true',
      help="If given then read all segment data.")

    parser.add_argument(cmd(Args.CENTERLINES_FILE),
      help="Centerlines file.")

    parser.add_argument(cmd(Args.DATA_NAMES),
      help="Data name.")

    parser.add_argument(cmd(Args.DISPLAY_GEOMETRY),
      help="Display geometry.")

    parser.add_argument(cmd(Args.MODEL_ORDER),
      help="Model order (0 or 1).")

    parser.add_argument(cmd(Args.NODE_SPHERE_RADIUS), 
      help="Radius of node sphere markers.")

    parser.add_argument(cmd(Args.OUTLET_SEGMENTS), default=False, action='store_true', 
      help="If given then read all outlet segment data.")

    parser.add_argument(cmd(Args.OUTPUT_DIRECTORY), 
      help="Output directory.")

    parser.add_argument(cmd(Args.OUTPUT_FILE), 
      help="Output file name.")

    parser.add_argument(cmd(Args.OUTPUT_FORMAT), 
      help="Output format.")

    parser.add_argument(cmd(Args.PLOT), 
      help="Plot results.")

    parser.add_argument(cmd(Args.RESULTS_DIRECTORY), required=True,
      help="Results directory.")

    parser.add_argument(cmd(Args.SEGMENTS), 
      help="Segment names to convert.")

    parser.add_argument(cmd(Args.SELECT_SEGMENTS), action='store_true',
      help="Select segments to convert.")

    parser.add_argument(cmd(Args.SOLVER_FILE_NAME), required=True,
      help="Solver .in file.")

    #parser.add_argument(cmd(Args.SURFACE_MESH_FILE),
    #  help="Surface mesh file.")

    parser.add_argument(cmd(Args.TIME_RANGE), 
      help="Time range to save and plot.")

    parser.add_argument(cmd(Args.VOLUME_MESH_FILE),
      help="Volume mesh file.")

    parser.add_argument(cmd(Args.WALLS_MESH_FILE),
      help="Combined walls mesh file.")

    return parser.parse_args(), parser.print_help

def set_parameters(**kwargs):
    """ 
    Set the values of parameters input from the command line.
    """
    logger.info("Parse arguments ...")

    ## Create a Parameters object to store parameters.
    params = Parameters()

    ## Process arguments.
    #
    if kwargs.get(Args.MODEL_ORDER) is not None:
        params.model_order = int(kwargs.get(Args.MODEL_ORDER))
        logger.info("Model order: %s" % params.model_order)
        if params.model_order not in [0, 1]:
            logger.error("The model order '%s' is unknown (must be 0 or 1)." % params.model_order)
            return None
    else:
        logger.error("The model order must be given (0 or 1).")
        return None

    if kwargs.get(Args.OUTPUT_DIRECTORY):
        params.output_directory = kwargs.get(Args.OUTPUT_DIRECTORY)
        if not os.path.exists(params.output_directory):
            logger.error("The output directory '%s' was not found." % params.output_directory)
            return None
    logger.info("Output directory: '%s'." % params.output_directory)

    if kwargs.get(Args.RESULTS_DIRECTORY):
        params.results_directory = kwargs.get(Args.RESULTS_DIRECTORY)
        if not os.path.exists(params.results_directory):
            logger.error("The results directory '%s' was not found." % params.results_directory)
            return None
    logger.info("Results directory: '%s'." % params.results_directory)

    if kwargs.get(Args.OUTPUT_FILE):
        params.output_file_name = kwargs.get(Args.OUTPUT_FILE)
    logger.info("Output file name: %s" % params.output_file_name)

    if kwargs.get(Args.OUTPUT_FORMAT):
        params.output_format = kwargs.get(Args.OUTPUT_FORMAT)
    logger.info("Output format: %s" % params.output_format)

    params.solver_file_name = kwargs.get(Args.SOLVER_FILE_NAME)
    logger.info("Solver file name: %s" % params.solver_file_name)

    if kwargs.get(Args.DATA_NAMES):
        params.data_names = kwargs.get(Args.DATA_NAMES).split(",")
    logger.info("Data names: %s" % ','.join(params.data_names))

    if kwargs.get(Args.OUTLET_SEGMENTS):
        params.outlet_segments = True 
    logger.info("Outlet segments: %s" % params.outlet_segments)

    if kwargs.get(Args.ALL_SEGMENTS):
        params.all_segments = True 
    logger.info("All segments: %s" % params.all_segments)

    if kwargs.get(Args.SELECT_SEGMENTS):
        params.select_segment_names = True
    logger.info("Select segments: %s" % params.select_segment_names)

    if kwargs.get(Args.DISPLAY_GEOMETRY):
        params.display_geometry = (kwargs.get(Args.DISPLAY_GEOMETRY) in ["on", "true"])
    logger.info("Display geometry: %s" % params.display_geometry)

    if kwargs.get(Args.NODE_SPHERE_RADIUS):
        params.node_sphere_radius = float(kwargs.get(Args.NODE_SPHERE_RADIUS)) 

    if kwargs.get(Args.PLOT):
        params.plot_results = (kwargs.get(Args.PLOT) in ["on", "true"])
    logger.info("Plot results: %s" % params.plot_results)

    if kwargs.get(Args.SEGMENTS):
        params.segment_names = kwargs.get(Args.SEGMENTS).split(",")
        logger.info("Segments: %s" % ','.join(params.segment_names))

    if kwargs.get(Args.TIME_RANGE):
        time_range = kwargs.get(Args.TIME_RANGE).replace('"', '')
        params.time_range = [float(s) for s in time_range.split(",")]
        logger.info("Time range: %s" % ','.join(map(str,params.time_range)))

    if kwargs.get(Args.CENTERLINES_FILE):
        params.centerlines_file = kwargs.get(Args.CENTERLINES_FILE)
        if not os.path.exists(params.centerlines_file):
            logger.error("The centerlines file '%s' was not found." % params.centerlines_file)
            return None
        logger.info("Centerlines file: %s" % params.centerlines_file)

    if kwargs.get(Args.VOLUME_MESH_FILE):
        params.volume_mesh_file = kwargs.get(Args.VOLUME_MESH_FILE)
        if not os.path.exists(params.volume_mesh_file):
            logger.error("The volume mesh file '%s' was not found." % params.volume_mesh_file)
            return None
        logger.info("Volume mesh file: %s" % params.volume_mesh_file)

    if kwargs.get(Args.WALLS_MESH_FILE):
        params.walls_mesh_file = kwargs.get(Args.WALLS_MESH_FILE)
        if not os.path.exists(params.walls_mesh_file):
            logger.error("The walls mesh file '%s' was not found." % params.walls_mesh_file)
            return None
        logger.info("Walls mesh file: %s" % params.walls_mesh_file)

    if params.model_order == 1:
        if not (params.outlet_segments and params.all_segments) and (params.segment_names == None):
            logger.warning("No segment options are set therefore no segment data will be read.")

    return params 

def run(**kwargs):
    """ 
    Execute the convert 1D simulation results using passed parameters.
    """
    result = ""

    ## Set input parameters.
    params = set_parameters(**kwargs)

    if not params:
        msg = "Error in parameters."
        raise Exception(msg)

    if params.model_order == 1:
        ## Read in the solver file.
        solver = Solver(params)
        solver.read_solver_file()

        ## Read segment data.
        solver.read_segment_data()

        ## Write segment data.
        solver.write_segment_data()

        # post-process results
        params.time_indices = solver.time_indices
        params.times = solver.times
        if 0.0 in params.times:
            params.times.remove(0.0)

    post = Post(params, logger)
    post.process()

    logger.info("Converted results finished.")
    result = "Successfully converted results\n" 
    return result 

def run_from_c(*args, **kwargs):
    """ 
    Execute the convert 0D or 1D solver results using passed parameters from c++.

    The '*args' argument contains the directory to write the log file.
    """
    output_dir = args[0]

    # Inialize logging.
    init_logging(output_dir)
    log_file_name = os.path.join(output_dir, get_log_file_name())
    open(log_file_name, 'w').close()

    # Convert results.
    #
    msg = "Status: OK\n"

    try:
        result = run(**kwargs)
        msg += result
    except Exception as e:
        logger.error(str(e))
        msg = "Status: Error\n"
        msg += str(e) + "\n"

    ## Attach log file to returned result.
    #
    msg += "Log:\n"
    with open(log_file_name, 'r') as file:
        msg += file.read()

    return msg

if __name__ == '__main__':
    """
    Execute the convert 0D or 1D solver results form the command line.
    """
    args = sys.argv

    init_logging()

    args, print_help = parse_args()

    params = set_parameters(**vars(args))

    if params == None:
        sys.exit()

    ## Create graphics interface.   
    #
    # Creating a Graphics() object fails if vtk is not installed.
    try:
        graphics = Graphics(params)
    except:
        graphics = None
        logger.warning("Graphics could not be initialized.")

    ## Read in the solver file.
    solver = Solver(params)
    solver.graphics = graphics 
    solver.read_solver_file()
    if graphics:
        graphics.solver = solver

    ## Read segment data.
    solver.read_segment_data()

    ## Write segment data if segments are not going
    #  to be interactively selected.
    if params.output_file_name and not params.select_segment_names:
        solver.write_segment_data()

    ## Plot results.
    if params.plot_results:
        solver.plot_results()

    ## If displaying geometry then show the network.
    if graphics and params.display_geometry:
        graphics.add_graphics_points(solver.points_polydata, [0.8, 0.8, 0.8])
        graphics.add_graphics_edges(solver.lines_polydata, solver.lines_segment_names, [0.8, 0.8, 0.8])
        graphics.show()
