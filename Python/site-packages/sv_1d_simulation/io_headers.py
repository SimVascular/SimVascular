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
This file contains i/o headers for the SimVascular 1D solver (https://github.com/SimVascular/oneDSolver).
"""


class Headers(object):
    """
    Collection of headers of all oneDSolver input-file sections
    """
    node = [
        "#",
        "# ==========",
        "# NODE CARD",
        "# ==========",
        "# - Node Name (double)",
        "# - Node X Coordinate (double)",
        "# - Node Y Coordinate (double)",
        "# - Node Z Coordinate (double)",
        ""]

    joint1 = [
        "#",
        "# ==========",
        "# JOINT CARD",
        "# ==========",
        "# - Joint Name (string)",
        "# - Joint Node (double)",
        "# - Joint Inlet Name (string)",
        "# - Joint Outlet Name (string)",
        ""]

    joint2 = [
        "#",
        "# ================================",
        "# JOINTINLET AND JOINTOUTLET CARDS",
        "# ================================",
        "# - Inlet/Outlet Name (string)",
        "# - Total Number of segments (int)",
        "# - List of segments (list of int)",
        ""]

    segment = [
        "# ============",
        "# SEGMENT CARD",
        "# ============",
        "# - Segment Name (string)",
        "# - Segment ID (int)",
        "# - Segment Length (double)",
        "# - Total Finite Elements in Segment (int)",
        "# - Segment Inlet Node (int)",
        "# - Segment Outlet Node (int)",
        "# - Segment Inlet Area (double)",
        "# - Segment Outlet Area (double)",
        "# - Segment Inflow Value (double)",
        "# - Segment Material (string)",
        "# - Type of Loss (string - 'NONE','STENOSIS','BRANCH_THROUGH_DIVIDING','BRANCH_SIDE_DIVIDING','BRANCH_THROUGH_CONVERGING',",
        "#                          'BRANCH_SIDE_CONVERGING','BIFURCATION_BRANCH')",
        "# - Branch Angle (double)",
        "# - Upstream Segment ID (int)",
        "# - Branch Segment ID (int)",
        "# - Boundary Condition Type (string - 'NOBOUND','PRESSURE','AREA','FLOW','RESISTANCE','RESISTANCE_TIME','PRESSURE_WAVE',",
        "#                                     'WAVE','RCR','CORONARY','IMPEDANCE','PULMONARY')",
        "# - Data Table Name (string)",
        ""]

    solveroptions = [
        "# ==================",
        "# SOLVEROPTIONS CARD",
        "# ==================",
        "# - Solver Time Step (double), ",
        "# - Steps Between Saves (int), ",
        "# - Max Number of Steps (int)",
        "# - Number of quadrature points for finite elements (int), ",
        "# - Name of Datatable for inlet conditions (string)",
        "# - Type of boundary condition (string - 'NOBOUND','PRESSURE','AREA','FLOW','RESISTANCE','RESISTANCE_TIME','PRESSURE_WAVE',",
        "#                                        'WAVE','RCR','CORONARY','IMPEDANCE','PULMONARY')",
        "# - Convergence tolerance (double), ",
        "# - Formulation Type (int - 0 Advective, 1 Conservative), ",
        "# - Stabilization (int - 0 No stabilization, 1 With stabilization)",
        ""]

    material = [
        "# =============",
        "# MATERIAL CARD",
        "# =============",
        "# - Material Name (string)",
        "# - Material Type (string - 'LINEAR','OLUFSEN')",
        "# - Material Density (double)",
        "# - Material Viscosity (double)",
        "# - Material PRef (double)",
        "# - Material Exponent (double)",
        "# - Material Parameter 1 (double)",
        "# - Material Parameter 2 (double)",
        "# - Material Parameter 3 (double)",
        ""]

    output = [
        "# ============",
        "# OUTPUT CARD",
        "# ============",
        "#",
        "# 1. Output file format. The following output types are supported:",
        "#\t\tTEXT. The output of every segment is written in separate text files for the flow rate, pressure, area and Reynolds number. The rows contain output values at varying locations along the segment while columns contains results at various time instants.",
        "#\t\tVTK. The results for all time steps are plotted to a 3D-like model using the XML VTK file format.",
        "# 2. VTK export option. Two options are available for VTK file outputs:",
        "#\t\t0 - Multiple files (default). A separate file is written for each saved increment. A pvd file is also provided which contains the time information of the sequence. This is the best option to create animations.",
        "#\t\t1 - The results for all time steps are plotted to a single XML VTK file.",
        ""]

    junction = [
        "#",
        "# ==========",
        "# JUNCTION MODEL CARD",
        "# ==========",
        "# - joint_name",
        "# - junction_type, i.e. NORMAL_JUNCTION, etc.",
        ""]

    element = [
        "# =============",
        "# ELEMENT CARD",
        "# =============",
        "# - 1d_segment_name",
        "# - 1d_segment_number / 0d_element_number",
        "# - average_radius",
        "# - length",
        "# - blood_density",
        "# - blood_dynamic_viscosity",
        "# - Eh/r",
        "# - inlet_node_number",
        "# - outlet_node_number",
        "# - vessel_vector_x_coor",
        "# - vessel_vector_y_coor",
        "# - vessel_vector_z_coor",
        "# - boundary_condition_type",
        "# - boundary_condition_datatable_name",
        "# - 0d_element_type, i.e. R, RC, RL, RCL, L, C, etc.",
        "# - 0d_element_values in format of: R or R C or R L or R C L or L or C or blank for custom 0d elements",
        ""]

    inlet_bc = [
        "# ==============================",
        "# INLET BOUNDARY CONDITION CARD",
        "# ==============================",
        "# - inlet_segment_number",
        "# - inlet_boundary_condition_type",
        "# - inlet_boundary_condition_datatable_name",
        ""]

    solveroptions_0d = [
        "# ==================",
        "# SOLVEROPTIONS_0D CARD",
        "# ==================",
        "# - number_of_time_pts_per_cardiac_cycle (int)",
        "# - number_of_cardiac_cycles (int)",
        ""]

    def model(self, model_name):
        """
        Write model header
        """
        header = ["# ================================",
                  "# " + model_name + " MODEL - UNITS IN CGS",
                  "# ================================",
                  "",
                  "# ==========",
                  "# MODEL CARD",
                  "# ==========",
                  "# - Name of the model (string)",
                  "",
                  "MODEL " + model_name,
                  ""]
        return header
