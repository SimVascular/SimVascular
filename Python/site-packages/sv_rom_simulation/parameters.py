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
    CORONARY = "coronary"
    SV_TO_ONED = {'rcrt.dat': RCR, 'resistance.dat': RESISTANCE, 'cort.dat': CORONARY}
    ONED_TO_SV = {RCR: 'rcrt.dat', RESISTANCE: 'resistance.dat', CORONARY: 'cort.dat'}


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
        self.mesh_output_file = '1d_model.vtp'
        self.model_name = None
        self.model_order = None
        self.solver_output_file = None
        self.surface_model = None
        self.uniform_material = True
        self.wall_properties_input_file = None
        self.wall_properties_output_file = None

        self.outputformat = "TEXT"

        self.inflow_input_file = None
        self.inlet_face_input_file = None
        self.inlet_face_seg_num = 0

        self.uniform_bc = True
        self.outflow_bc_type = None
        self.outflow_bc_file = None

        self.outlet_face_names_file = None
        self.CENTERLINES_OUTLET_FILE_NAME = "centerlines_outlets.dat"

        # Mesh size in a sub-segment.
        self.element_size = 0.01

        # Min number of elements for a sub-segment.
        self.min_num_elems = 5

        # Min number of sub-segments in a vessel master-segment.
        self.seg_min_num = 1

        # Sub-segment size in a vessel-master segment.
        self.seg_size = 9999

        # Adaptive sub-segment size according to centerline area change?
        self.seg_size_adaptive = False

        self.time_step = 0.001
        self.num_time_steps = 1000
        self.save_data_freq = 1

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
        self.linear_material_ehr = 1e7
        self.linear_material_pressure = 0.0

        # Properties for the 0D model
        self.uniform_0d_element_type = True  # True if all 0D elements have the same 0d element type
        self.zerod_element_type = 'R'

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
