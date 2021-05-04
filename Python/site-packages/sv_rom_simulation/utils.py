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
The module contains utilities used to read files, calculate centers, etc.
"""

import os.path
import vtk

import logging
from .manage import get_logger_name
logger = logging.getLogger(get_logger_name())

SurfaceFileFormats = ["vtk", "vtp"]


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

