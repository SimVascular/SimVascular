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

class OutflowBoundaryConditionType(object):
    RCR = "rcr"
    RESISTANCE = "resistance"

class MaterialModel(object):
    LINEAR = "LINEAR"
    OLUFSEN = "OLUFSEN"

class Parameters():
    """ The Parameter class stores the input parameters for a 1D mesh generation.
    """
    class Units(object):
        MM = "mm"
        CM = "cm"

    def __init__(self):
        self.boundary_surfaces_dir = None
        self.output_directory = None
        self.centerlines_input_file = None
        self.centerlines_output_file = None
        self.compute_centerlines = True
        self.compute_mesh = False
        self.mesh_output_file = None
        self.model_name = None
        self.reorganize_seqments = False
        self.solver_output_file = None
        self.surface_model = None
        self.uniform_material = True
        self.wall_properties_input_file = None
        self.wall_properties_output_file = None
        self.write_mesh_file = False
        self.write_solver_file = False

        self.outputformat = "TEXT"

        self.inflow_input_file = None
        self.inlet_face_input_file = None 

        self.uniform_bc = True
        self.outflow_bc_type = None 
        self.outflow_bc_file = None
        self.OUTFLOW_BC_TYPES = {OutflowBoundaryConditionType.RCR : "rcrt.dat", 
                                 OutflowBoundaryConditionType.RESISTANCE : "resistance.dat"}

        self.outlet_face_names_file = None
        self.CENTERLINES_OUTLET_FILE_NAME = "centerlines_outlets.dat"

        # Mesh size in a vessel segment.
        self.element_size = 0.1

        # Min number of elements for a segment.
        self.min_num_elems = 10

        self.time_step = 0.000588
        self.num_time_steps = 2000
        self.save_data_freq = 20

        # Units conversion from mm to cgs.
        self.units = self.Units.CM
        self.lcoef = 1.0
        self.Acoef = 1.0

        # Physical parameters.
        self.density = 1.055
        self.material_model = MaterialModel.OLUFSEN
        self.viscosity = 0.04
        self.olufsen_material_k1 = 0.0
        self.olufsen_material_k2 = -22.5267
        self.olufsen_material_k3 = 1.0e7
        self.olufsen_material_exponent = 1.0
        self.olufsen_material_pressure = 0.0
        self.linear_material_ehr = 1e7;
        self.linear_material_pressure = 0.0;

    def set_units(self, units):
        units = units.lower()
        units_ok = True

        if units == self.Units.MM:
            self.lcoef = 0.1
            self.Acoef = 0.01
        elif units == self.Units.CM:
            self.lcoef = 1.0
            self.Acoef = 1.0
        else:
            units_ok = False 

        return units_ok
 

