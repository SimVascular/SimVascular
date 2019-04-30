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
The module is used to extact centerlines from a surface mesh. 
"""

import os
from pathlib import Path
import numpy as np

import logging
from .manage import get_logger_name

import vtk
try:
    from vmtk import vtkvmtk,vmtkscripts
except ImportError:
    print("vmtk not found.")

from .utils import SurfaceFileFormats, read_surface, get_polydata_centroid, read_polydata


class Centerlines(object):
    """ The Centerlines class is used to encapsulate centerline calculations.

    Attributes:
    """
    def __init__(self):

        self.inlet_face_name = None
        self.inlet_center = None
        self.outlet_centers = None
        self.outlet_face_names = None

        self.geometry = None
        self.branch_geometry = None

        self.logger = logging.getLogger(get_logger_name())

    def extract_center_lines(self, params):
        """ Extract the centerlines of a surface.

           The centerline geometry is returned as a vtkPolyData object.
        """
        ## Get the centers of the inlet and outlet surfaces. 
        self.get_inlet_outlet_centers(params)

        ## Read the surface model used for centerline calculation.
        self.logger.info("Read surface model from %s" % params.surface_model)
        surface_mesh = read_surface(params.surface_model)
        #print(surface_mesh)
        self.logger.info("Number of points in params.outlet_centers %d" % len(self.outlet_centers))

        ## Extract centerlines using vmtk.
        self.logger.info("Calculating surface centerlines ...");
        centerlines = vmtkscripts.vmtkCenterlines()
        centerlines.Surface = surface_mesh 
        centerlines.SeedSelectorName = "pointlist"
        centerlines.AppendEndPoints = 1
        centerlines.SourcePoints = self.inlet_center
        centerlines.TargetPoints = self.outlet_centers
        centerlines.Execute()
        self.geometry = centerlines.Centerlines
        self.logger.info("The surface centerlines have been calculated.");

        # Write outlet face names.
        self.write_outlet_face_names(params)

    def extract_branches(self, params):
        """ Split and group centerlines along branches. 
        """
        self.logger.info("Split and group centerlines along branches ...");
        branch_extractor = vmtkscripts.vmtkBranchExtractor()
        branch_extractor.Centerlines = self.geometry 
        branch_extractor.Execute()
        self.branch_geometry = branch_extractor.Centerlines
        #print(self.centerlines_branch_geometry)
        self.logger.info("The centerlines branches have been calculated.");

    def read(self, params, file_name):
        """ Read centerlines from a .vtp file.

        The centerlines must have been split and grouped along branches.
        """
        self.branch_geometry = read_polydata(file_name)

    def get_inlet_outlet_centers(self, params):
        """ Get the centers of the inlet and outlet surface geometry.

            Surface inlet and outlet faces are identifed by their file name. 
        """
        surf_mesh_dir = Path(params.boundary_surfaces_dir)
        inlet_file_name = params.inlet_face_input_file
        self.outlet_face_names = []
        self.outlet_centers = []

        for face_file in surf_mesh_dir.iterdir():
            file_name = face_file.name
            self.logger.debug("Surface file name: %s" % file_name)
            file_suffix = face_file.suffix.lower()[1:]

            if file_suffix not in SurfaceFileFormats or file_name.lower().startswith('wall'):
                continue

            if (file_name == inlet_file_name):
                inlet_path = str(face_file.absolute())
                self.logger.info("Inlet file: %s" % inlet_path)
                polydata = read_surface(inlet_path, file_suffix)
                self.inlet_center = get_polydata_centroid(polydata)
                self.inlet_face_name = face_file.stem 
            else:
                outlet_path = str(face_file.absolute())
                self.outlet_face_names.append(face_file.stem)
                self.logger.info("Outlet: %s" % file_name)
                polydata = read_surface(outlet_path, file_suffix)
                # Use extend because vmtk expects a list of floats.
                self.outlet_centers.extend(get_polydata_centroid(polydata))
        #__for face_file in surf_mesh_dir.iterdir()

        if (not self.inlet_face_name):
            raise RuntimeError("No inlet face found in the boundary surface directory '%s'" % params.boundary_surfaces_dir)

        self.logger.info("Number of outlet faces: %d" % len(self.outlet_centers))

    def write_outlet_face_names(self, params):
        """ Write outlet face names.
        """
        file_name = os.path.join(params.output_directory, params.CENTERLINES_OUTLET_FILE_NAME)
        self.logger.info("Write outlet face names to: %s" % file_name) 
        with open(file_name, "w") as fp:
            for name in self.outlet_face_names:
                fp.write(name+"\n")

