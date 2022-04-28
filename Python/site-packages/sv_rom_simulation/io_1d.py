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
This module handles file i/o for the SimVascular 1D solver (https://github.com/SimVascular/oneDSolver).
"""
import pdb
from os import path
import logging
import re
from .manage import get_logger_name
from .parameters import OutflowBoundaryConditionType, MaterialModel

import numpy as np
from collections import OrderedDict

import vtk.util.numpy_support as nps
from vtk import vtkIdList
from vtk import vtkPoints, vtkLine, vtkCellArray, vtkPolyData, vtkXMLPolyDataWriter
from .utils import SurfaceFileFormats, read_polydata, write_polydata
from .io_headers import Headers
from collections import namedtuple

# Define flow data tuple.
FlowData = namedtuple('FlowData', 'time flow')


def read_inflow_file(mesh, params):
    """ Read the inflow file.

    The file can be formatted as space- or comma-separated value pairs.

    Example:   
        <time1>  <flow1>
        <time2>  <flow2>
        ...
        <timeN>  <flowN>
    """
    mesh.logger.info("Read inflow BC ...")
    inflow_data = []
    inflow_file = params.inflow_input_file

    try:
        with open(inflow_file, "r") as ofile:
            for line in ofile:
                if line.strip() == '':
                    continue
                values = re.split("[, ]+", line.strip())
                inflow_data.append(FlowData(time=float(values[0]), flow=float(values[1])))

    except Exception as e:
        msg = "The inflow file is in the wrong format, expecting space- or comma-separated value pairs.\n"
        mesh.logger.error(msg)
        raise RuntimeError(str(e))

    mesh.inflow_data = inflow_data


def read_outlet_face_names(mesh, params):
    """ Read in outlet face names file.
    """
    outlet_face_names = []
    mesh.logger.info("Read model outlet face names from: %s" % params.outlet_face_names_file)
    with open(params.outlet_face_names_file) as file:
        for line in file:
            outlet_face_names.extend(line.splitlines())
    mesh.logger.info("Number of model outlet faces names: %d" % len(outlet_face_names))
    return outlet_face_names


def write_mesh(mesh, params):
    """
    Write the 1D mesh to a VTK VTP format file.
    Args:
        mesh: Mesh object
        params: 1d model parameters
    """
    output_dir = params.output_directory
    file_name = path.join(output_dir, params.mesh_output_file)

    # add points and lines
    points = vtkPoints()
    lines = vtkCellArray()
    i = 0
    for branch in mesh.point_data['coord'].values():
        for j, point in enumerate(branch):
            points.InsertNextPoint(params.lcoef * point)
            if j > 0:
                line = vtkLine()
                line.GetPointIds().SetId(0, i + j - 1)
                line.GetPointIds().SetId(1, i + j)
                lines.InsertNextCell(line)
        i += len(branch)

    # create polydata
    polydata = vtkPolyData()
    polydata.SetPoints(points)
    polydata.SetLines(lines)
    polydata.Modified()

    # add point and cell arrays to output
    output = {'point': mesh.point_data, 'cell': mesh.cell_data}
    for field, out in output.items():
        for name, data in out.items():
            array = []
            for branch in data.values():
                for v in branch:
                    array += [v]
            add_array(polydata, field, array, name)

    # write the VTP file
    writer = vtkXMLPolyDataWriter()
    writer.SetFileName(file_name)
    writer.SetInputData(polydata)
    writer.Write()


def write_solver_file(mesh, params):
    """ Write a solver input file.
    """
    # print("Write solver file.")
    mesh.logger.info("Write solver file.")
    output_dir = params.output_directory
    file_name = path.join(output_dir, params.solver_output_file)
    model_name = params.model_name
    sp = mesh.space
    # print("Solver file %s" % file_name)

    # Open file
    ofile = mesh.Open(file_name, "w")

    # write header
    header = Headers().model(model_name)
    write_solver_section_header(mesh, ofile, header)

    # Write node section.
    write_solver_nodes(mesh, ofile, params)

    # Write joint section.
    write_solver_joints(mesh, ofile, params)

    # Write segment section.
    write_solver_segments(mesh, ofile, params)

    # Write SolverOptions section.
    write_solver_options(mesh, ofile, params)

    # Write material section.
    write_solver_material(mesh, ofile, params)

    # Write output section.
    write_solver_output(mesh, ofile, params)

    ofile.close()


def write_solver_nodes(mesh, ofile, params):
    """ Write a solver input file nodes section.
    """
    header = Headers().node

    ofile.writeln(mesh.solver_file_msg)
    write_solver_section_header(mesh, ofile, header)

    lcoef = params.lcoef
    sp = mesh.space

    # loop branches
    for ids, nodes in zip(mesh.point_data['id'].values(), mesh.point_data['coord'].values()):
        # loop nodes
        for i, node in zip(ids, nodes):
            ofile.writeln("NODE " + str(i) + sp + sp.join(str(lcoef * node[j]) for j in range(3)))


def write_solver_joints(mesh, ofile, params):
    """ Write a solver input file joints section.
    """
    header1 = Headers().joint1
    header2 = Headers().joint2

    # Joint header.
    ofile.writeln(mesh.solver_file_msg)
    write_solver_section_header(mesh, ofile, header1)

    # JointInlet and JointOutlet header.
    ofile.writeln(mesh.solver_file_msg)
    write_solver_section_header(mesh, ofile, header2)

    sp = mesh.space
    seg_connectivity = mesh.seg_connectivity
    seg_rear = mesh.seg_rear

    for i in range(0, len(seg_connectivity)):
        joint = "JOINT J" + str(i) + sp + str(seg_rear[i])
        jin = "IN" + str(i)
        jout = "OUT" + str(i)
        ofile.writeln(joint + sp + jin + sp + jout)
        ofile.writeln("JOINTINLET IN" + str(i) + sp + "1 " + str(seg_connectivity[i][0]))
        ofile.write("JOINTOUTLET OUT" + str(i) + sp + str(len(seg_connectivity[i]) - 1))
        for j in range(1, len(seg_connectivity[i])):
            ofile.write(sp + str(seg_connectivity[i][j]))
        ofile.write("\n\n")


def write_solver_segments(mesh, ofile, params):
    """ Write a solver input file joints section.
    """
    header = Headers().segment
    write_solver_section_header(mesh, ofile, header)
    mesh.logger.info("Write solver segment section ...")

    sp = mesh.space

    uniform_bc = params.uniform_bc
    uniform_material = params.uniform_material
    dx = params.element_size
    min_num_elems = params.min_num_elems
    inflow_data = mesh.inflow_data

    mesh.logger.info("Uniform BC: %s" % uniform_bc)
    for t in set(mesh.bc_type.values()):
        mesh.logger.info("Outflow BC: %s" % t)
    mesh.num_elements = 0

    assert uniform_material, 'non-uniform material not implemented'

    # loop branches
    for (branch, ids), lengths, stenosis, seg_1d in zip(mesh.cell_data['id'].items(), mesh.cell_data['length'].values(), mesh.cell_data['stenosis'].values(), mesh.cell_data['1d_seg'].values()):
        # loop nodes
        for i, (j, length, sten, numfe) in enumerate(zip(ids, lengths, stenosis, seg_1d)):
            if uniform_material:
                matname = "MAT1"
            else:
                matname = "MAT_group" + str(j)

            mesh.num_elements += numfe

            p0 = mesh.point_data['id'][branch][i]
            p1 = mesh.point_data['id'][branch][i + 1]
            a0, a1 = mesh.cell_data['area'][branch][i]

            seg_out = "SEGMENT" + sp + "branch" + str(branch) + "_seg" + str(i) + sp + str(j) + sp +\
                      str(length) + sp + str(numfe) + sp + str(p0) + sp +\
                      str(p1) + sp + str(a0) + sp + str(a1) + sp +\
                      "0.0 " + matname

            if (len(stenosis) == 3 and i == 1) or (len(stenosis) > 3 and i > 0):
                seg_out += " STENOSIS 0.0 " + str(j - 1) + " 0 "
            else:
                seg_out += " NONE 0.0 0 0 "

            ofile.write(seg_out)

            if j in mesh.terminal:
                if uniform_bc:
                    outflow_bc = OutflowBoundaryConditionType.RCR.upper()
                    ofile.writeln(outflow_bc + " " + outflow_bc + "_1")
                    msg = "While writing solver segments encountered: group_terminal[seg_list[i]] == 1"
                    mesh.logger.error(msg)
                    raise RuntimeError(msg)
                else:
                    bc_id = mesh.terminal.index(j)
                    bc_name = list(mesh.outlet_face_names_index.keys())[bc_id]
                    bc_type = mesh.bc_type[bc_name].upper()
                    ofile.writeln(bc_type + " " + bc_type + "_" + str(bc_id))
            else:
                ofile.writeln("NOBOUND NONE")

    ofile.writeln("")
    ofile.writeln("")

    if uniform_bc:
        ofile.writeln("DATATABLE " + outflow_bc_uc + "_1 LIST")
        ofile.writeln(sp)
        ofile.writeln("ENDDATATABLE")
        ofile.writeln("")
    else:
        for i, name in enumerate(mesh.outlet_face_names_index.keys()):
            bc_val = mesh.bc_map[name]
            bc_type = mesh.bc_type[name]
            ofile.writeln("DATATABLE " + bc_type.upper() + "_" + str(i) + " LIST")

            if bc_type == OutflowBoundaryConditionType.RCR or bc_type == OutflowBoundaryConditionType.RESISTANCE:
                for j in bc_val:
                    ofile.writeln("0.0 " + str(j))

            elif bc_type == OutflowBoundaryConditionType.CORONARY:
                # write parameters
                for v in ['Ra1', 'Ra2', 'Ca', 'Cc', 'Rv1', 'P_v']:
                    ofile.writeln("-1.0 " + str(bc_val['var'][v]))

                # write time / pressure pairs
                for t, p in zip(bc_val['time'], bc_val['pressure']):
                    ofile.writeln(str(t) + sp + str(p))

            ofile.writeln("ENDDATATABLE")
            ofile.writeln("")

    ofile.writeln("")
    ofile.writeln("")
    ofile.writeln("DATATABLE INFLOW LIST")

    if not inflow_data:
        ofile.writeln("Copy and paste inflow data here.")
    else:
        for value in inflow_data:
            ofile.writeln(" %f %f" % (value.time, value.flow))

    ofile.writeln("ENDDATATABLE")
    ofile.writeln("")
    ofile.writeln("")


def write_solver_options(mesh, ofile, params):
    """ Write a solver input file options section.
    """
    header = Headers().solveroptions
    write_solver_section_header(mesh, ofile, header)

    sp = mesh.space
    time_step = params.time_step
    num_time_steps = params.num_time_steps
    save_data_freq = params.save_data_freq

    ofile.writeln("SOLVEROPTIONS " + str(time_step) + " " + str(save_data_freq) + " " + str(
        num_time_steps) + " 2 INFLOW FLOW 1.0e-5 1 1")
    ofile.writeln("")


def write_solver_material(mesh, ofile, params):
    """ Write a solver input file material section.
    """
    header = Headers().material
    write_solver_section_header(mesh, ofile, header)

    sp = mesh.space
    mattype = params.material_model
    density = params.density
    viscosity = params.viscosity

    if mattype == MaterialModel.OLUFSEN:
        k1 = params.olufsen_material_k1
        k2 = params.olufsen_material_k2
        k3 = params.olufsen_material_k3
        pressure = params.olufsen_material_pressure
        exponent = params.olufsen_material_exponent
    elif mattype == MaterialModel.LINEAR:
        k1 = params.linear_material_ehr
        k2 = 0.0
        k3 = 0.0
        pressure = params.linear_material_pressure
        exponent = 1.0

    uniform_material = params.uniform_material

    if uniform_material:
        ofile.writeln("MATERIAL MAT1 " + mattype + sp + str(density) + sp + str(viscosity) + sp +
                      str(pressure) + sp + str(exponent) + sp + str(k1) + sp + str(k2) + sp + str(k3))
    else:
        msg = 'Non-uniform material not implemented'
        mesh.logger.error(msg)
        raise RuntimeError(msg)


def write_solver_output(mesh, ofile, params):
    """ Write a solver input file output section.
    """
    header = Headers().output
    ofile.writeln("")
    write_solver_section_header(mesh, ofile, header)

    outputformat = params.outputformat
    ofile.writeln("OUTPUT " + outputformat)


def write_solver_section_header(mesh, ofile, header):
    """ Write a solver input file section header.
    """
    endl = mesh.endl
    hdr = endl.join(line for line in header)
    ofile.writeln(hdr)


def add_array(data, identifier, array, name):
    """
    Add array to vtkPolyData
    Args:
        data: vtkPolyData object
        identifier: array association (point or cell)
        array: scalar list or numpy array
        name: display name of array
    """
    out_array = nps.numpy_to_vtk(np.array(array))
    out_array.SetName(name)
    if identifier == 'point':
        data.GetPointData().AddArray(out_array)
    elif identifier == 'cell':
        data.GetCellData().AddArray(out_array)
    else:
        raise ValueError('Unknown identifier ' + identifier + '. Choose point or cell.')


def read_variable_outflow_bcs(params):
    """
    Read in data for variable flow boundary conditions from file (RCR, resistance, coronary)
    """
    bc_map = {}
    bc_type = {}
    for outflow_bc_sv in params.outflow_bc_type:
        # filename for boundary condition type
        bc_file = path.join(params.outflow_bc_file, outflow_bc_sv)
        if not path.exists(bc_file):
            raise RuntimeError('Boundary condition file not found: ' + bc_file)

        # onedsolver name for boundary condition type
        bc_in = path.basename(outflow_bc_sv)
        if bc_in not in OutflowBoundaryConditionType.SV_TO_ONED:
            raise RuntimeError('Unknown bounadry condition type: ' + bc_in)
        outflow_bc = OutflowBoundaryConditionType.SV_TO_ONED[bc_in]

        # switch between boundary condition types resistance, RCR, coronary
        if outflow_bc == OutflowBoundaryConditionType.RESISTANCE:
            try:
                with open(bc_file) as rfile:
                    for line in rfile:
                        split = line.strip().split(' ')
                        face_name = split[0]

                        # resistance
                        value = []
                        value += [split[1]]

                        # check if reference pressure is given
                        if len(split) == 2:
                            value += [0.0]
                        elif len(split) == 3:
                            value += [split[2]]
                        else:
                            msg = "The resistance file is in the wrong format, expecting face name / value pairs."
                            raise RuntimeError(msg)

                        bc_map[face_name] = value
                        bc_type[face_name] = outflow_bc

            except Exception as e:
                msg = "The resistance file is in the wrong format, expecting face name / value pairs.\n"
                raise RuntimeError(msg)

        elif outflow_bc == OutflowBoundaryConditionType.RCR:
            with open(bc_file) as rfile:
                keyword = rfile.readline()
                while True:
                    tmp = rfile.readline()
                    if tmp == keyword:
                        RCRval = []
                        face_name = rfile.readline().strip()
                        RCRval.append(float(rfile.readline()))
                        RCRval.append(float(rfile.readline()))
                        RCRval.append(float(rfile.readline()))

                        # read reference pressure
                        p0 = float(rfile.readline().strip().split(' ')[1])
                        p1 = float(rfile.readline().strip().split(' ')[1])
                        assert p0 == p1, 'Cannot handle time-dependent reference pressure'
                        RCRval.append(float(p1))

                        bc_map[face_name] = RCRval
                        bc_type[face_name] = outflow_bc
                    if len(tmp) == 0:
                        break

        elif outflow_bc == OutflowBoundaryConditionType.CORONARY:
            with open(bc_file) as rfile:
                keyword = rfile.readline()
                tmp = rfile.readline()
                while True:
                    if tmp == keyword:
                        face_name = rfile.readline().strip()

                        # read coronary parameters
                        coronary_var_sv = OrderedDict()
                        var_names = ['q0', 'q1', 'q2', 'p0', 'p1', 'p2', 'b0', 'b1', 'b2', 'dQinidT', 'dPinidT']
                        for name in var_names:
                            try:
                                coronary_var_sv[name] = float(rfile.readline())
                            except Exception:
                                raise RuntimeError('Wrong format of coronary boundary conditions')

                        # convert SimVascular convention to oneD convention
                        coronary_var = coronary_sv_to_oned(coronary_var_sv)

                        # read ventricular time and pressure pairs
                        time = []
                        pressure = []
                        while True:
                            tmp = rfile.readline()
                            # check if end of table
                            if tmp == keyword or len(tmp) == 0:
                                break

                            # split line in time, pressure
                            try:
                                t, p = tmp.split()
                            except Exception:
                                raise RuntimeError('Wrong format of coronary boundary conditions')

                            time += [float(t)]
                            pressure += [float(p)]

                        coronary = {'var': coronary_var, 'time': time, 'pressure': pressure}

                        bc_map[face_name] = coronary
                        bc_type[face_name] = outflow_bc
                    if len(tmp) == 0:
                        break

    return bc_map, bc_type


def coronary_sv_to_oned(bc):
    """
    Convert format of coronary boundary condition parameters from svSimVascular to svOneDSolver
    """
    # unpack constants
    p1, p2, q0, q1, q2, b1 = (bc['p1'], bc['p2'], bc['q0'], bc['q1'], bc['q2'], bc['b1'])
    Rv_micro = 0

    # build system of equations (obtained from analytically solving for constants as defined in paper)
    # see H. J. Kim et al. "Patient-Specific Modeling of Blood Flow and Pressure in Human Coronary Arteries", p. 3198
    Ra = q2 / p2
    Ra_micro = (p1 ** 2 * q2 ** 2 - 2 * p1 * p2 * q1 * q2 + p2 ** 2 * q1 ** 2) / (p2 * (- q2 * p1 ** 2 + q1 * p1 * p2 - q0 * p2 ** 2 + q2 * p2))
    Ca = -p2**2/(p1*q2 - p2*q1)
    Cim = (- q2*p1**2 + q1*p1*p2 - q0*p2**2 + q2*p2)**2 / ((p1*q2 - p2*q1)*(p1**2*q0*q2 - p1*p2*q0*q1 - p1*q1*q2 + p2**2*q0**2 - 2*p2*q0*q2 + p2*q1**2 + q2**2))
    Rv = -(p1**2*q0*q2 - p1*p2*q0*q1 - p1*q1*q2 + p2**2*q0**2 - 2*p2*q0*q2 + p2*q1**2 + q2**2) / (- q2*p1**2 + q1*p1*p2 - q0*p2**2 + q2*p2)

    # check equation residuals
    res = [p1 - (Ra_micro * Ca + (Rv + Rv_micro) * (Ca + Cim)),
           p2 - (Ca * Cim * Ra_micro * (Rv + Rv_micro)),
           q0 - (Ra + Ra_micro + Rv + Rv_micro),
           q1 - (Ra * Ca * (Ra_micro + Rv + Rv_micro) + Cim * (Ra + Ra_micro) * (Rv + Rv_micro)),
           q2 - (Ca * Cim * Ra * Ra_micro * (Rv + Rv_micro)),
           b1 - (Cim * (Rv + Rv_micro))]
    assert np.max(np.abs(res)) < 1e-5, 'SV coronary constants inconsistent'

    # export constants
    return {'Ra1': Ra, 'Ra2': Ra_micro, 'Ca': Ca, 'Cc': Cim, 'Rv1': Rv, 'P_v': 0.0}
