#!/usr/bin/env python

"""
The module contains utilities used to read files, calculate centers, etc.
"""

import os.path
import vtk

import logging
from manage import get_logger_name
logger = logging.getLogger(get_logger_name())

SurfaceFileFormats = ["vtk", "vtp"]

def get_polydata_centroid(poly_data):
    """ Calculate the centroid of polydata.
    """
    x_list = []
    y_list = []
    z_list = []
    cx = 0.0;
    cy = 0.0;
    cz = 0.0;
    num_pts = poly_data.GetNumberOfPoints()

    for i in range(num_pts): 
        point = poly_data.GetPoints().GetPoint(i)
        cx += point[0]
        cy += point[1]
        cz += point[2]

    return [cx/num_pts, cy/num_pts, cz/num_pts]


def read_surface(file_name, file_format="vtp", datatype=None):
    """
    Read surface geometry from a file.

    Args:
        file_name (str): Path to input file.
        file_format (str): File format (.vtp, .stl, etc.). 
        datatype (str): Additional parameter for vtkIdList objects.

    Returns:
        polyData (vtkSTL/vtkPolyData/vtkXMLStructured/
                    vtkXMLRectilinear/vtkXMLPolydata/vtkXMLUnstructured/
                    vtkXMLImage/Tecplot): Output data.
    """

    # Check if file exists
    if not os.path.exists(file_name):
        raise RuntimeError("Could not find file: %s" % file_name)

    # Get reader
    if file_format == 'stl':
        reader = vtk.vtkSTLReader()
        reader.MergingOn()
    elif file_format == 'vtk':
        reader = vtk.vtkPolyDataReader()
    elif file_format == 'vtp':
        reader = vtk.vtkXMLPolyDataReader()
    elif file_format == 'vts':
        reader = vtk.vtkXMinkorporereLStructuredGridReader()
    elif file_format == 'vtr':
        reader = vtk.vtkXMLRectilinearGridReader()
    elif file_format == 'vtu':
        reader = vtk.vtkXMLUnstructuredGridReader()
    elif file_format == "vti":
        reader = vtk.vtkXMLImageDataReader()
    elif file_format == "np" and datatype == "vtkIdList":
        result = np.load(filename).astype(np.int)
        id_list = vtk.vtkIdList()
        id_list.SetNumberOfIds(result.shape[0])
        for i in range(result.shape[0]):
            id_list.SetId(i, result[i])
        return id_list
    else:
        raise RuntimeError('Unknown file type %s' % file_format)

    # Read surface geometry.
    reader.SetFileName(file_name)
    reader.Update()
    polydata = reader.GetOutput()
    polygons = polydata.GetPolys()
    num_polys = polygons.GetNumberOfCells()
    return polydata

def read_bc(params):
    """ Read BC files.
    
    """
    BClist = []

    with open(BCfile) as file:
        if outflowBC == "RESISTANCE":
            for line in file:
                #print "line=",line
                BClist.append(float(line))
            #__for line in file

            if len(BClist)!=len(outletfacename):
                logger.error("The number of BC values %d  is not consistant with the number of outlets %d",
                  len(BClist), len(outletfacename))
                exit()

        elif outflowBC == "RCR":
            keyword = file.readline()
            # print"keyword=",keyword
            while True:
                tmp = file.readline()
                if tmp == keyword:
                    RCRval=[]
                    RCRval.append(float(file.readline()))
                    RCRval.append(float(file.readline()))
                    RCRval.append(float(file.readline()))
                    BClist.append(RCRval)
                if len(tmp) == 0:
                    break
           #__while True
        #__if outflowBC=="RCR"
    #__with open(BCfile) as file
  
    user_outlet_names = []

    with open(useroutletfile) as file:
        for line in file:
            useroutletname.extend(line.splitlines())

    logger.info("Number of user provided model outlet names: %d" % len(useroutletname))

    if len(useroutletname)!=len(outletfacename):
        logger.error("The number of user provided outlets is not consistant with the number of outlets in mesh-surfaces. Exit.")
        exit()

def write_polydata(file_name, data, datatype=None):

    """ Write the given VTK object to a file.

    Args:
        file_name (str): The name of the file to write. 
        data (vtkDataObject): Data to write.
        datatype (str): Additional parameter for vtkIdList objects.
    """
    # Check filename format.
    file_ext = file_name.split(".")[-1]

    if file_ext == '':
        raise RuntimeError('The file does not have an extension')

    # Get writer.
    if file_ext == 'vtp':
        writer = vtk.vtkXMLPolyDataWriter()
    elif file_ext == 'vtu':
        writer = vtk.vtkXMLUnstructuredGridWriter()
    else:
        raise RuntimeError('Unknown file type %s' % file_ext)

    # Set file name and the data to write.
    writer.SetFileName(file_name)
    writer.SetInputData(data)
    writer.Update()

    # Write the data.
    writer.Write()


def read_polydata(file_name, datatype=None):
    """ Read a vtp file.

    Args:
        file_name (str): The input polydata file. 
        datatype (str): Additional parameter for vtkIdList objects.

    Returns:
        polydata (vtkDataObject): Geometry read in from the input file. 
    """

    # Check if file exists.
    if not os.path.exists(file_name):
        raise RuntimeError("Could not find file: %s" % file_name)

    # Check filename format.
    file_ext = file_name.split(".")[-1]

    if file_ext == '':
        raise RuntimeError('The file does not have an extension')

    if file_ext == 'vtp':
        reader = vtk.vtkXMLPolyDataReader()
    else:
        raise RuntimeError('Unknown file type %s' % file_ext)

    # Read the vtkPolyData object.
    reader.SetFileName(file_name)
    reader.Update()
    polydata = reader.GetOutput()

    return polydata

