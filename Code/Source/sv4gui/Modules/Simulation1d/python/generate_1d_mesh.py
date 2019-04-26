#!/usr/bin/env python

""" 
This module provides the interface to the modules creating a 1D mesh used for 1D simulations. 

Centerlines can be computed or read in. If centerline are read from a file then they must have 
been split and grouped along branches. If you are reading the centerlines computed by the SimVascular 
Models plugin then the Full_Centerlines.vtp file must be used.
"""
import argparse
import sys
import os 

from manage import get_logger_name, init_logging, get_log_file_name
from parameters import Parameters
from centerlines import *
from mesh import *
from utils import write_polydata, read_polydata

logger = logging.getLogger(get_logger_name())

class Args(object):
    """ This class defines the command line arguments to the generate-1d-mesh script.
    """
    PREFIX = "--"
    BOUNDARY_SURFACE_DIR = "boundary_surfaces_directory"
    CENTERLINE_INPUT_FILE = "centerlines_input_file"
    CENTERLINE_OUTPUT_FILE = "centerlines_output_file"
    COMPUTE_CENTERLINES = "compute_centerlines"
    COMPUTE_MESH = "compute_mesh"
    INLET_FACE_INPUT_FILE = "inlet_face_input_file"
    INFLOW_INPUT_FILE = "inflow_input_file"
    MESH_OUTPUT_FILE = "mesh_output_file"
    MINIMUM_NUMBER_ELEMENTS = "min_num_elements"
    MODEL_NAME = "model_name"
    NUM_TIME_STEPS = "num_time_steps"
    OUTFLOW_BC_INPUT_FILE = "outflow_bc_input_file"
    OUTFLOW_BC_TYPE = "outflow_bc_type"
    OUTLET_FACE_NAMES_INPUT_FILE = "outlet_face_names_input_file"
    OUTPUT_DIRECTORY = "output_directory"
    SOLVER_OUTPUT_FILE = "solver_output_file"
    SAVE_DATA_FREQUENCY = "save_data_frequency"
    SURFACE_MODEL = "surface_model"
    TIME_STEP = "time_step"
    UNIFORM_BC = "uniform_bc"
    UNITS = "units"
    WALL_PROPERTIES_INPUT_FILE = "wall_properties_input_file"
    WALL_PROPERTIES_OUTPUT_FILE = "wall_properties_output_file"
    WRITE_MESH_FILE = "write_mesh_file"
    WRITE_SOLVER_FILE = "write_solver_file"
    
def cmd(name):
    """ Create an argparse command argument.
    """
    return Args.PREFIX + name.replace("_", "-")

def parse_args():
    """ Parse command-line arguments."""
    parser = argparse.ArgumentParser()

    parser.add_argument(cmd(Args.BOUNDARY_SURFACE_DIR), 
      help="Directory containing the boundary (inlet/outlet) surface files")

    parser.add_argument(cmd(Args.CENTERLINE_INPUT_FILE), 
      help="The name of the file to read centerline geometry from.")

    parser.add_argument(cmd(Args.CENTERLINE_OUTPUT_FILE), 
      help="The name of the file to write centerline geometry to.")

    parser.add_argument(cmd(Args.COMPUTE_CENTERLINES), const=True, nargs='?', default=False, 
      help="If given or value is set to 1 then compute centerlines.")

    parser.add_argument(cmd(Args.COMPUTE_MESH), const=True, nargs='?', default=False, 
      help="If given or value is set to 1 then compute a mesh from centerlines..")

    parser.add_argument(cmd(Args.INFLOW_INPUT_FILE), 
      help="The name of the file to read inflow data from.")

    parser.add_argument(cmd(Args.INLET_FACE_INPUT_FILE), 
      help="The name of the file (.vtp) defining the inlet face geometry.")

    parser.add_argument(cmd(Args.MESH_OUTPUT_FILE), 
      help="The name of the file to write the mesh to.")

    parser.add_argument(cmd(Args.MINIMUM_NUMBER_ELEMENTS), type=int,
      help="The minimum number of finite elements per segment.")

    parser.add_argument(cmd(Args.MODEL_NAME), required=True,
      help="The name of the model.")

    parser.add_argument(cmd(Args.NUM_TIME_STEPS), type=int,
      help="The number of simulation time steps.")

    parser.add_argument(cmd(Args.OUTFLOW_BC_INPUT_FILE), 
      help="The output boundary condition input file (RESISTANCE, RCR).")

    parser.add_argument(cmd(Args.OUTFLOW_BC_TYPE), 
      help="The output boundary condition type (RESISTANCE, RCR).")

    parser.add_argument(cmd(Args.OUTPUT_DIRECTORY), required=True, 
      help="The directory where output files are written.")

    parser.add_argument(cmd(Args.SAVE_DATA_FREQUENCY), type=int,  
      help="The frequency to save data as the number of time steps between saves.")

    parser.add_argument(cmd(Args.SOLVER_OUTPUT_FILE), 
      help="The name of the file to write the solver input to.")

    parser.add_argument(cmd(Args.SURFACE_MODEL), 
      help="The surface model used to compute centerlines.")

    parser.add_argument(cmd(Args.TIME_STEP), type=float,
      help="The simulation time step.")

    parser.add_argument(cmd(Args.UNIFORM_BC),
      help = "If set to (true,1,on) then read BC files")

    parser.add_argument(cmd(Args.UNITS),
      help = "The units used to scale geometry. (cm or mm)")

    parser.add_argument(cmd(Args.OUTLET_FACE_NAMES_INPUT_FILE),
      help = "The file containing outlet face names.")

    parser.add_argument(cmd(Args.WALL_PROPERTIES_INPUT_FILE), 
      help = "The name of the file read surface wall material properties from.")

    parser.add_argument(cmd(Args.WALL_PROPERTIES_OUTPUT_FILE), 
      help = "The name of the file write grouped wall material properties to.")

    parser.add_argument(cmd(Args.WRITE_MESH_FILE), const=True, nargs='?', default=False, 
      help="If given or value is set to 1 then write a mesh file..")

    parser.add_argument(cmd(Args.WRITE_SOLVER_FILE), const=True, nargs='?', default=False, 
      help="If given or value is set to 1 then write a solver file..")

    return parser.parse_args(), parser.print_help

def set_parameters(**kwargs):
    """ Set the values of parameters input from the command line.

    The **kwargs argument can originate from the command line or from a direct
    call to the 'run()' function, in which case parser.parse_args() is not called
    and so we need to check here if required arguments have been passed.
    """
    logger.info("Parse arguments ...")

    ## Create a Parameters object to store parameters.
    params = Parameters()

    ## Process arguments.
    #
    true_values = ["on", "true", "1"]

    if kwargs.get(Args.BOUNDARY_SURFACE_DIR): 
        params.boundary_surfaces_dir = kwargs.get(Args.BOUNDARY_SURFACE_DIR)
        logger.info("Surface directory: %s" % params.boundary_surfaces_dir)
        if not os.path.exists(params.boundary_surfaces_dir):
            logger.error("The surface directory '%s' was not found." % params.boundary_surfaces_dir)
            return None

    if kwargs.get(Args.CENTERLINE_INPUT_FILE):
        params.centerlines_input_file = kwargs.get(Args.CENTERLINE_INPUT_FILE)
        logger.info("Centerlines input file: %s" % params.centerlines_input_file)
        if not os.path.exists(params.centerlines_input_file):
            logger.error("The centerlines input file '%s' was not found." % params.centerlines_input_file)
            return None

    if kwargs.get(Args.CENTERLINE_OUTPUT_FILE): 
        params.centerlines_output_file = kwargs.get(Args.CENTERLINE_OUTPUT_FILE)
        logger.info("Centerlines output file: %s" % params.centerlines_output_file)

    # The 'compute_centerlines' parameter is set to True if the COMPUTE_CENTERLINES 
    # argument is given with no value. Otherwise it is set to the value given.
    params.compute_centerlines = (kwargs.get(Args.COMPUTE_CENTERLINES) == True) or \
      (kwargs.get(Args.COMPUTE_CENTERLINES) in true_values)
    logger.info("Compute centerlines: %s" % params.compute_centerlines) 

    # The 'compute_mesh' parameter is set to True if the COMPUTE_MESH 
    # argument is given with no value. Otherwise it is set to the value given.
    params.compute_mesh = (kwargs.get(Args.COMPUTE_MESH) == True) or \
      (kwargs.get(Args.COMPUTE_MESH) in true_values)

    if kwargs.get(Args.INFLOW_INPUT_FILE):
        params.inflow_input_file = kwargs.get(Args.INFLOW_INPUT_FILE)
        logger.info("Inflow input file: %s" % params.inflow_input_file)
        if not os.path.exists(params.inflow_input_file):
            logger.error("The inflow input file '%s' was not found." % params.inflow_input_file) 
            return None

    if kwargs.get(Args.INLET_FACE_INPUT_FILE):
        params.inlet_face_input_file = kwargs.get(Args.INLET_FACE_INPUT_FILE)
        if os.path.dirname(params.inlet_face_input_file):
            logger.error("The inlet face input file '%s' should not have a full path." % params.inlet_face_input_file) 
            return None
        inlet_file = os.path.join(params.boundary_surfaces_dir,params.inlet_face_input_file)
        if not os.path.exists(inlet_file):
            logger.error("The inlet face input file '%s' was not found in the boundary surfaces diretory '%s'." % \
                (params.inlet_face_input_file,params.boundary_surfaces_dir))
            return None
        logger.info("Inlet face input file: %s" % params.inlet_face_input_file)

    if kwargs.get(Args.OUTPUT_DIRECTORY):
        params.output_directory = kwargs.get(Args.OUTPUT_DIRECTORY)
        if not os.path.exists(params.output_directory):
            logger.error("The output directory '%s' was not found." % params.output_directory)
            return None
    else:
        logger.error("An output directory argument must be given.")
        return None

    if kwargs.get(Args.MESH_OUTPUT_FILE):
        params.mesh_output_file = kwargs.get(Args.MESH_OUTPUT_FILE)
        logger.info("Mesh output file: %s" % params.mesh_output_file)

    if kwargs.get(Args.MINIMUM_NUMBER_ELEMENTS):
        params.min_num_elems = kwargs.get(Args.MINIMUM_NUMBER_ELEMENTS)
    logger.info("Minimum number of finite elements per segment: %d" % params.min_num_elems)

    params.model_name = kwargs.get(Args.MODEL_NAME)
    logger.info("Model name: %s" % params.model_name)

    if kwargs.get(Args.NUM_TIME_STEPS):
        params.num_time_steps = kwargs.get(Args.NUM_TIME_STEPS)
    logger.info("Number of time steps: %d" % params.num_time_steps)

    if kwargs.get(Args.OUTFLOW_BC_INPUT_FILE):
        params.outflow_bc_file = kwargs.get(Args.OUTFLOW_BC_INPUT_FILE)
        if not os.path.exists(params.outflow_bc_file):
            logger.error("The outflow input bc file '%s' was not found." % params.outflow_bc_file)
            return None
        logger.info("Outflow bc file: %s" % params.outflow_bc_file)

    if kwargs.get(Args.OUTFLOW_BC_TYPE):
        bc_type = kwargs.get(Args.OUTFLOW_BC_TYPE).lower()
        if not bc_type in params.OUTFLOW_BC_TYPES:
            logger.error("Unknown BC type '%s'." % bc_type) 
            return None
        params.outflow_bc_type = bc_type
        logger.info("Outflow bc type: %s" % params.outflow_bc_type)

    if kwargs.get(Args.OUTLET_FACE_NAMES_INPUT_FILE):
        params.outlet_face_names_file = kwargs.get(Args.OUTLET_FACE_NAMES_INPUT_FILE)
        if not os.path.exists(params.outlet_face_names_file):
            logger.error("The input outlet face names file '%s' was not found." % params.outlet_face_names_file)
            return None
        logger.info("Outlet face names file: '%s'." % params.outlet_face_names_file)

    if kwargs.get(Args.SAVE_DATA_FREQUENCY):
        params.save_data_freq = kwargs.get(Args.SAVE_DATA_FREQUENCY)
    logger.info("Save data frequency: %s" % params.save_data_freq)

    if kwargs.get(Args.SOLVER_OUTPUT_FILE):
        params.solver_output_file = kwargs.get(Args.SOLVER_OUTPUT_FILE)
        logger.info("Solver output file: %s" % params.solver_output_file)

    if kwargs.get(Args.SURFACE_MODEL):
        params.surface_model = kwargs.get(Args.SURFACE_MODEL)
        logger.info("Surface model: %s" % params.surface_model)
        if not os.path.exists(params.surface_model):
            logger.error("The surface model file '%s' was not found." % params.surface_model)
            return None

    if kwargs.get(Args.TIME_STEP):
        params.time_step = (kwargs.get(Args.TIME_STEP) in true_values)
    logger.info("Simulation time step: %f" % params.time_step)

    if kwargs.get(Args.UNIFORM_BC):
        params.uniform_bc = (kwargs.get(Args.UNIFORM_BC) in true_values)
    logger.info("Uniform boundary conditions: %s" % params.uniform_bc)

    if kwargs.get(Args.UNITS):
        units = kwargs.get(Args.UNITS)
        if params.set_units(units):
            params.units = units
        else:
            logger.error("The units value '%s' was not recognized. Valid values are mm or cm." % units)
            return None
    logger.info("Units: %s" % params.units)


    if kwargs.get(Args.WALL_PROPERTIES_INPUT_FILE):
        params.wall_properties_input_file = kwargs.get(Args.WALL_PROPERTIES_INPUT_FILE)
        logger.info("Wall properties input file: %s" % params.wall_properties_input_file)
        if not os.path.exists(params.wall_properties_input_file):
            logger.error("The wall properties input file '%s' was not found." % 
              params.wall_properties_input_file)
            return None
        params.uniform_material = False
        logger.info("Wall properties are not uniform.")
    else:
        logger.info("Wall properties are uniform.")

    if kwargs.get(Args.WALL_PROPERTIES_OUTPUT_FILE):
        params.wall_properties_output_file = kwargs.get(Args.WALL_PROPERTIES_OUTPUT_FILE)
        logger.info("Wall properties output file: %s" % params.wall_properties_output_file)

    # The 'write_mesh_file' parameter is set to True if the WRITE_MESH_FILE
    # argument is given with no value. Otherwise it is set to the value given.
    params.write_mesh_file = (kwargs.get(Args.WRITE_MESH_FILE) == True) or \
      (kwargs.get(Args.WRITE_MESH_FILE) in true_values)
    logger.info("Write mesh file: %s" % params.write_mesh_file)

    # The 'write_solver_file' parameter is set to True if the WRITE_SOLVER_FILE
    # argument is given with no value. Otherwise it is set to the value given.
    params.write_solver_file = (kwargs.get(Args.WRITE_SOLVER_FILE) == True) or \
      (kwargs.get(Args.WRITE_SOLVER_FILE) in true_values)
    logger.info("Write solver file: %s" % params.write_solver_file)


    ## Check for argument consistency.
    #
    if params.compute_centerlines and params.centerlines_input_file: 
        logger.error("Both compute centerlines and read centerlines are given.")
        return None

    if params.compute_centerlines and not params.inlet_face_input_file:
        logger.error("An inlet face file must be given when computing centerlines.")
        return None

    if params.wall_properties_input_file and not params.wall_properties_output_file: 
        logger.error("If a wall properties input file is given then a wall properties output file must also be given.")
        return None

    if params.outflow_bc_type and not params.outflow_bc_file:
        logger.error("If an outflow boundary condition type is given then an input data file for that type must also be given.")
        return None

    if params.write_solver_file: 
        if not params.outlet_face_names_file: 
            logger.error("No outlet face names file was given.")
            return None

    # Uniform / Non-uniform BCs.
    if params.uniform_bc:
        if params.outflow_bc_type != None:
            logger.error("An outflow BC type can't be given with uniform BCs.")
            return None
    else:
        if params.outflow_bc_type == None:
            logger.error("An outflow BC type must be given for non-uniform BCs.")
            return None

    return params 

def read_centerlines(params):
    """ Read centerlines for a surface model from a file.
    
    The centerlines must have had 
    """
    centerlines = Centerlines()
    centerlines.read(params, params.centerlines_input_file)

    logger.info("Read centerlines from the file: %s", params.centerlines_input_file) 
    logger.info("   Number of points: %d ", centerlines.branch_geometry.GetNumberOfPoints())
    logger.info("   Number of cells: %d ", centerlines.branch_geometry.GetNumberOfCells())
    logger.info("   Number of arrays: %d", centerlines.branch_geometry.GetCellData().GetNumberOfArrays())

    return centerlines

def compute_centerlines(params):
    """ Compute the centerlines for a surface model.
    """

    ## Check input parameters.
    #
    if not params.surface_model:
        logger.error("No surface model has been specified.")
        return

    if not params.centerlines_output_file:
        logger.error("No centerlines output file has been specified.")
        return

    # Create Centerlines object that encapsulats centerline calculations. 
    centerlines = Centerlines()

    # Extract centerlines.
    centerlines.extract_center_lines(params)

    # Split and group centerlines along branches. 
    centerlines.extract_branches(params)

    # Write the centerlines branches to a file.
    if centerlines.branch_geometry:
        write_polydata(params.centerlines_output_file, centerlines.branch_geometry)

    return centerlines

def run(**kwargs):
    """ Execute the 1D mesh generation using passed parameters.
    """
    ## Set input parameters.
    params = set_parameters(**kwargs)
    if not params:
        return False

    centerlines = None 

    ## Extract surface centerlines.
    if params.compute_centerlines:
        centerlines = compute_centerlines(params)

    ## Read surface centerlines.
    elif params.centerlines_input_file:
        centerlines = read_centerlines(params)

    if not centerlines:
        logger.error("No centerlines calculated or read in.")
        sys.exit(1)

    ## Generate a 1D mesh.
    mesh = Mesh()
    mesh.generate(params, centerlines)

    return True

def run_from_c(*args, **kwargs):
    """ Execute the 1D mesh generation using passed parameters from c++

    The '*args' argument contains the directory to write the log file.
    """
    output_dir = args[0]
    init_logging(output_dir)
    msg = "status:ok\n"

    try:
        run(**kwargs)
    except Exception as e:
        msg = "status:error\n"
        msg += "exception:" + str(e) + "\n"

    ## Attach log file to returned result.
    #
    msg = "log file:\n"
    log_file_name = path.join(output_dir, get_log_file_name())

    with open(log_file_name, 'r') as file:
        msg += file.read()

    return msg

    if not run(**kwargs):
        sys.exit(1)

if __name__ == '__main__':
    init_logging()
    args, print_help = parse_args()
    if not run(**vars(args)):
        sys.exit(1)

