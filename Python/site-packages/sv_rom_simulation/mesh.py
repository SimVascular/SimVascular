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
This module is used to create an input file for the SimVascular 1D solver (https://github.com/SimVascular/oneDSolver).

A 1D mesh is generated from the centerline geometry caculated from a closed polygonal surface.

A centerline consists of m cells, m=number of tract ids, the length of a cell/line is an approximation of a group.
In Cell Data, lines are listed from 0 to m. For each line, the first number is the number of points for this line
followed by 1st point to the last point.

"""
import os
import logging
import re
import numpy as np
from collections import OrderedDict

from scipy.signal import argrelextrema
import vtk
from vtk.util.numpy_support import vtk_to_numpy as v2n
from vtk import vtkIdList
from vtk import vtkPoints, vtkLine, vtkCellArray, vtkPolyData, vtkXMLPolyDataWriter

from .manage import get_logger_name
from .parameters import OutflowBoundaryConditionType, MaterialModel
from .utils import SurfaceFileFormats, read_polydata, write_polydata
from .io_1d import read_inflow_file, read_outlet_face_names, write_mesh, write_solver_file, \
    write_solver_nodes, write_solver_joints, write_solver_segments, write_solver_options, \
    write_solver_material, write_solver_output, write_solver_section_header, coronary_sv_to_oned, \
    read_variable_outflow_bcs
from .models import ZeroD

from .io_0d import write_0d_solver_file

import pdb


class Mesh(object):
    """
    The Mesh class is used to encapsulate 1D mesh calculations.
    """

    class CellDataFields(object):
        """ This class defines the required field cell data field names.
        """
        pass

    class PointDataFields(object):
        """ This class defines the required field point data field names.
        """
        AREA = "CenterlineSectionArea"
        CENT = "CenterlineId"
        PATH = "Path"
        BRANCH = "BranchId"
        BIFURCATION = "BifurcationId"
        NODEID = "GlobalNodeId"
        NORMAL = "CenterlineSectionNormal"

    class Open(object):
        """ This class wraps the 'open' class and adds a method to automatically
            write newlines.
        """

        def __init__(self, *args, **kwds):
            self.args = args
            self.kwds = kwds
            self.file_obj = open(*self.args, **self.kwds)

        def __enter__(self):
            self.file_obj = open(*self.args, **self.kwds)
            return self

        def __exit__(self, *args):
            self.file_obj.close()

        def close(self):
            self.file_obj.close()

        def writeln(self, string):
            self.file_obj.write(string + '\n')

        def write(self, string):
            self.file_obj.write(string)

    def __init__(self):
        self.centerlines = None
        self.logger = logging.getLogger(get_logger_name())

        # centerline
        self.centerline = None
        self.centerlines_outlet_face_names = None

        # caps
        self.inflow_data = None
        self.outlet_face_names = None
        self.outlet_face_names_index = None

        # boundary conditions
        self.bc_map = None
        self.bc_type = None

        # discretization
        self.seg_connectivity = None
        self.cell_data = None
        self.point_data = None
        self.seg_rear = None
        self.terminal = None
        self.junctions = None

        # discretization statistics
        self.num_seg = None
        self.num_bf_geo = None
        self.num_bf_branch = None
        self.num_elements = None

        # Some constansts for writing the solver file.
        self.solver_file_msg = "\n\n### DO NOT CHANGE THIS SECTION - generated automatically"
        self.space = " "
        self.endl = "\n"

    def generate(self, params, centerlines):
        """ Generate a mesh.
        """
        self.logger.info("Generate the 1D mesh ...")
        self.centerline = centerlines.geometry
        self.centerlines_outlet_face_names = centerlines.outlet_face_names

        # Set outlet face names.
        self.set_outlet_face_names(params)
        self.logger.info("Outlet face names: %s" % str(self.outlet_face_names))

        # check that centerline consist of one region
        if not self.check_centerline_region():
            return False

        # Check that centerline geometry has the required data fields.
        if not self.check_centerlines_data():
            return False

        # import module for adaptive meshing
        if params.seg_size_adaptive and params.seg_min_num > 1:
            try:
                import pwlf
            except ImportError:
                params.seg_size_adaptive = False
                self.logger.info('Could not import pwlf for adaptive meshing. Using uniform mesh instead')

        # mesh discretization
        self.discretize(params)
        self.logger.info("Number of centerline branches: %d" % len(self.cell_data['BranchId']))
        self.logger.info("Number of centerline bifurcations: %d" % self.num_bf_geo)
        self.logger.info("Number of svOneDSolver branches: %d" % self.num_seg)
        self.logger.info("Number of svOneDSolver joints: %d" % self.num_bf_branch)

        if not params.uniform_bc:
            self.set_variable_outflow_bcs(params)

        if params.inflow_input_file:
            read_inflow_file(self, params)

        if not params.uniform_material:
            msg = 'Non-uniform material not implemented'
            self.logger.error(msg)
            raise RuntimeError(msg)

        if params.model_order == 0:
            zerod = ZeroD(self, params)
            model = zerod.generate()

            # statistics
            self.num_elements = self.num_seg

            # write svZeroDSolver input file (.json)
            write_0d_solver_file(self, params, model)

        elif params.model_order == 1:
            # write discretization for visualization in ParaView
            write_mesh(self, params)

            # write svOneDSolver input file
            write_solver_file(self, params)

        return True

    def set_outlet_face_names(self, params):
        """ Set outlet face names.
        """
        if self.centerlines_outlet_face_names is not None:
            outlet_face_names = self.centerlines_outlet_face_names
        else:
            outlet_face_names = read_outlet_face_names(self, params)

        # Create a map between outlet face name and path ID.
        self.outlet_face_names_index = OrderedDict()
        self.outlet_face_names = []
        for i, face_name in enumerate(outlet_face_names):
            self.outlet_face_names.append(face_name)
            self.outlet_face_names_index[face_name] = i

    def set_variable_outflow_bcs(self, params):
        """ Read in data for variable flow boundary conditions.

        Parameters for outflow BCs (resistance or RCR) are read from a file.
        """
        self.logger.info("Set variable outflow boundary conditions ...")

        # convert legacy parameters
        if isinstance(params.outflow_bc_type, str):
            params.outflow_bc_type = [OutflowBoundaryConditionType.ONED_TO_SV[params.outflow_bc_type]]
            params.outflow_bc_file = os.path.dirname(params.outflow_bc_file)

        try:
            self.bc_map, self.bc_type = read_variable_outflow_bcs(params)
        except Exception as e:
            self.logger.error('Error while reading variable outflow boundary conditions')
            raise RuntimeError(str(e))

        if len(self.bc_map) != len(self.outlet_face_names):
            msg = "The number of BC values %d do not match the number of outlets %d." % (
                len(self.bc_map), len(self.outlet_face_names))
            raise RuntimeError(msg)

        for s, t in self.bc_type.items():
            self.logger.info('Surface ' + s + ' boundary condition type ' + t)

    def check_centerline_region(self):
        """
        Check if centerline is one connected region
        """
        con = vtk.vtkConnectivityFilter()
        con.SetInputData(self.centerline)
        con.SetExtractionModeToAllRegions()
        con.ColorRegionsOn()
        con.Update()
        n_region = con.GetNumberOfExtractedRegions()

        if n_region == 0:
            self.logger.error('Centerline is empty')
            raise RuntimeError('Centerline is empty')
        if n_region > 1:
            self.logger.error('Centerline consist of more than one region')
            raise RuntimeError('Centerline consist of more than one region')

        return True

    def check_centerlines_data(self):
        """ Check that the centerline data contains all of the required fields.
        """
        field_names = [v for k, v in self.CellDataFields.__dict__.items() if not k.startswith('__')]
        for field in field_names:
            if not self.centerline.GetCellData().GetArray(field):
                self.logger.error("Centerlines do not contain the '%s' data field." % field)
                return False

        field_names = [v for k, v in self.PointDataFields.__dict__.items() if not k.startswith('__')]
        for field in field_names:
            if not self.centerline.GetPointData().GetArray(field):
                self.logger.error("Centerlines do not contain the '%s' data field." % field)
                return False

        return True

    def get_cell_data(self, field):
        """ Get the data for the given cell field names.
        """
        cell_data = self.centerline.GetCellData().GetArray(field)
        return v2n(cell_data)

    def get_point_data(self, field):
        """ Get the data for the given cell field names.
        """
        point_data = self.centerline.GetPointData().GetArray(field)
        return v2n(point_data)

    def discretize(self, params):
        """
        Do the actual discretization
        """
        # step 1: create nodes / areas / segment lengths
        self.discretize_branches(params)

        # step 2: create connectivity
        self.discretize_bifurcations()

    def discretize_branches(self, params):
        """
        Create refined physical quantities: nodes / areas / segment lengths
        """
        # get names of point data fields
        br_name = self.PointDataFields.BRANCH
        p_name = self.PointDataFields.PATH

        # get list of all branches
        branch = self.get_point_data(br_name)
        branch_list = np.unique(branch).tolist()
        if -1 in branch_list:
            branch_list.remove(-1)

        # initialize properties of sub-segments in fine mesh to replace those of original mesh
        self.num_seg = 0
        self.cell_data = {'id': {}, br_name: {}, 'area': {}, 'length': {}, 'stenosis': {}, '1d_seg': {}, 'curv': {}}
        self.point_data = {'id': {}, br_name: {}, 'coord': {}, p_name: {}}

        # global counters for unique point/element-ids
        cell_ids_offset = 0
        point_ids_offset = 0

        # loop all branches
        for br in branch_list:
            # determine sampling of branch in interval [0, 1]
            sample_1d, f_sten, area, seg_num_1d = self.get_sampling(br, params)

            # number of samples
            num_seg = sample_1d.shape[0] - 1

            # interpolate branch attributes at defined sampling points
            length, _, _, points, r_curv = self.sample_branch(br, sample_1d)

            # get cell centers to convert point data to cell data
            cell_centers = (sample_1d[1:] + sample_1d[:-1]) / 2.0

            # store attributes
            self.cell_data['id'][br] = np.arange(num_seg) + cell_ids_offset
            self.cell_data['area'][br] = params.Acoef * area
            self.cell_data[br_name][br] = np.ones(num_seg) * br
            self.cell_data['length'][br] = params.lcoef * length * np.diff(sample_1d)
            self.cell_data['curv'][br] = interp1d(cell_centers, sample_1d, r_curv / params.lcoef)
            self.cell_data['stenosis'][br] = f_sten
            self.cell_data['1d_seg'][br] = seg_num_1d
            self.point_data['id'][br] = np.arange(num_seg + 1) + point_ids_offset
            self.point_data[br_name][br] = np.ones(num_seg + 1) * br
            self.point_data['coord'][br] = params.lcoef * points
            self.point_data[p_name][br] = params.lcoef * sample_1d * length
            self.num_seg += num_seg

            point_ids_offset += num_seg + 1
            cell_ids_offset += num_seg

    def get_sampling(self, br, params):
        """
        Get sampling points along branch br in interval [0, 1]
        """
        # initially sample branch attributes at all centerline points
        length, path_1d, area, _, _ = self.sample_branch(br)

        # determine number of segments in this branch
        num_seg = int(round(np.sum(length) / params.seg_size))
        if num_seg < params.seg_min_num:
            num_seg = params.seg_min_num

        # get branch ids
        branch = self.get_point_data(self.PointDataFields.BRANCH)

        # get all branch points
        point_ids = np.where(branch == br)

        # get point arrays from centerline
        area_br = self.get_point_data(self.PointDataFields.AREA)[point_ids]
        path_br = self.get_point_data(self.PointDataFields.PATH)[point_ids]

        if params.seg_min_num == -1:
            # take high-fidelity sampling from centerline
            return path_1d, np.zeros(len(path_1d) - 1), np.vstack((area[:-1], area[1:])).T, params.min_num_elems
        else:
            if num_seg > 1:
                if params.seg_size_adaptive:
                    # adaptive mesh based on area change
                    import pwlf
                    sample_1d = pwlf.PiecewiseLinFit(path_1d, area).fitfast(num_seg, pop=3)
                else:
                    # uniform sampling along centerline
                    sample_1d = np.linspace(0, 1, num_seg + 1)

                # get segment lengths
                length_1d = np.diff(sample_1d) * path_br[-1]

                # extract segments
                intervals = []
                for i, sp in enumerate(sample_1d):
                    intervals += [np.argmin(np.abs(path_br - sp * path_br[-1]))]

                # assemble segment properties
                sample_br = [0]
                f_sten_br = []
                a_br = []
                seg_num_1d = []
                for i in range(len(intervals) - 1):
                    # select segment
                    seg = range(intervals[i], intervals[i + 1])
                    if len(seg) > 1:
                        a = area_br[seg]
                        a_br += [[a[0], a[-1]]]

                        # stenosis factor
                        a_0 = np.min(a)
                        a_s = np.max(a)
                        f_sten_br += [1 / a_0 ** 2 * (a_0 / a_s - 1) ** 2]
                        snum = get_1d_seg_num(a, params)
                    else:
                        f_sten_br += [0]
                        a_br += [2 * [area[intervals[i]]]]
                        snum = params.seg_min_num
                    sample_br += [sample_br[-1] + length_1d[i]]
                    seg_num_1d += [snum]

                return np.array(sample_br) / sample_br[-1], np.array(f_sten_br), np.array(a_br), np.array(seg_num_1d)
            else:
                # find stenoses over entire branch (yields between 1 and 3 segments)
                return find_stenoses(area_br, path_br, params)

    def sample_branch(self, br, sample=None):
        """
        Sample points and areas of a centerline group along a path
        Args:
            br: BranchId
            sample: optional, sample points in [0, 1] (default: sample at centerline points)

        Returns:
        path of centerline points in [0, 1],sampled points, sampled areas
        """
        # get point arrays from centerline
        area = self.get_point_data(self.PointDataFields.AREA)
        distance = self.get_point_data(self.PointDataFields.PATH)
        branch = self.get_point_data(self.PointDataFields.BRANCH)
        points = v2n(self.centerline.GetPoints().GetData())

        # get all branch points
        point_ids = np.where(branch == br)

        # distance between points
        dist = distance[point_ids]

        # branch length
        length = dist[-1]

        # map path to [0, 1]
        path_1d = dist / dist[-1]

        # get curvature
        curvature = curvature_radius(points)

        if sample is not None:
            # if sampling is given, interpolate along 1d centerline
            f = lambda x: interp1d(sample, path_1d, x[point_ids])
        else:
            # return quantities along original path
            f = lambda x: x[point_ids]

        return length, path_1d, f(area), f(points), f(curvature)

    def get_junction_length(self, br):
        """
        Get the length of the part of a branch that is within the upstream junction
        Args:
            br: BranchId

        Returns:
        path length within junction
        """
        # does not apply to inlet branch
        if br == 0:
            return 0.0

        # get arrays from centerline
        cent = self.get_point_data(self.PointDataFields.CENT)
        branch = self.get_point_data(self.PointDataFields.BRANCH)
        bifurcation = self.get_point_data(self.PointDataFields.BIFURCATION)
        points = v2n(self.centerline.GetPoints().GetData())

        # first point id of branch
        ip = np.where(branch == br)[0][0]

        # id of upstream junction
        jc = bifurcation[ip - 1]

        # pick a centerline that passes through branch (and upstream junction)
        cid = np.where(cent[ip])[0][0]

        # points of centerline within upstream junction
        point_ids = np.where(np.logical_and(bifurcation == jc, cent[:, cid]))[0]

        # calculate path length
        return np.sum(np.linalg.norm(np.diff(points[point_ids], axis=0), axis=1))

    def discretize_bifurcations(self):
        """
        Create refined sub-segment connectivity
        """
        # get mesh connectivity
        mesh = get_connectivity(self.centerline)

        branch_id = self.get_point_data(self.PointDataFields.BRANCH)

        # tangent on centerline = section normals
        tangent = self.get_point_data(self.PointDataFields.NORMAL)

        # cell id list of 1d bifurcations (first is inlet)
        self.seg_connectivity = []

        # point id on bifurcations
        self.seg_rear = []

        # add joints for bifurcations
        for bf, bifurcation in mesh.items():
            # bifurcation inflow cell is at the end of a branch
            assert bifurcation.inflow is not None, 'bifurcation ' + str(bf) + ' has no inlet branch'
            joint = [self.cell_data['id'][bifurcation.inflow][-1]]

            # bifurcation outflow cells are at the beginning of a branch
            for branch in bifurcation.outflow:
                joint += [self.cell_data['id'][branch][0]]

            self.seg_connectivity += [joint]
            self.seg_rear += [self.point_data['id'][bifurcation.inflow][-1]]
        self.num_bf_geo = len(self.seg_connectivity)

        # add joints for cells within branch
        for branch, cells in self.cell_data['id'].items():
            for i in range(cells.shape[0] - 1):
                self.seg_connectivity += [[cells[i], cells[i + 1]]]
                self.seg_rear += [self.point_data['id'][branch][i + 1]]
        self.num_bf_branch = len(self.seg_connectivity)

        # add outlet cells
        self.terminal = []
        for branch in self.get_outlet_branches():
            self.terminal += [self.cell_data['id'][branch][-1]]

        # add junction information
        self.junctions = {}
        for i, seg in enumerate(self.seg_connectivity):
            # only add "real" junctions (not junctions inside branches)
            if len(seg) > 2:
                # find branch of segment
                for br, segments in self.cell_data['id'].items():
                    if seg[0] in segments:
                        break

                junction = {'tangents': [], 'lengths': [], 'areas': [self.cell_data['area'][br][-1][-1]]}

                # inlet tangent vector
                tangent_in = tangent[branch_id == br][-1]
                tangent_in /= np.linalg.norm(tangent_in)
                junction['tangents'] += [tangent_in.tolist()]

                # loop segments attached to junction
                for s in seg[1:]:
                    # find branch of segment
                    for br, segments in self.cell_data['id'].items():
                        if s in segments:
                            break

                    # angle
                    tangent_out = tangent[branch_id == br][0]
                    tangent_out /= np.linalg.norm(tangent_out)
                    junction['tangents'] += [tangent_out.tolist()]
                    junction['areas'] += [self.cell_data['area'][br][0][0]]
                    junction['lengths'] += [self.get_junction_length(br)]
                self.junctions[i] = junction

    def get_outlet_branches(self):
        """
        Get list of branches connected to outlets
        """
        # branch ids
        br_id = self.get_point_data(self.PointDataFields.BRANCH)
        bf_id = self.get_point_data(self.PointDataFields.BIFURCATION)

        # global node id
        gid = self.get_point_data(self.PointDataFields.NODEID)

        # outlet points are only connected to one cell (skip inlet point)
        ids = vtkIdList()
        outlets = []
        for p in range(self.centerline.GetNumberOfPoints()):
            self.centerline.GetPointCells(p, ids)
            if ids.GetNumberOfIds() == 1 and gid[p] != 0:
                assert br_id[p] != -1, 'bifurcation ' + str(bf_id[p]) + ' is connected to an outlet'
                outlets += [br_id[p]]
        return outlets


def find_stenoses(area, path, params):
    """
    Returns stenosis locations and coefficients along a branch
    """
    # find extrema
    i_min = argrelextrema(area, np.less)[0]
    i_max = argrelextrema(area, np.greater)[0]

    # truncate minima and maxima
    a_0 = area[i_min]
    a_s = area[i_max]
    if len(a_0) > len(a_s):
        a_0 = a_0[:-1]
        i_min = i_min[:-1]
    elif len(a_0) < len(a_s):
        a_s = a_s[1:]
        i_max = i_max[1:]

    # stenosis factor
    f_sten = 1 / a_0 ** 2 * (a_0 / a_s - 1) ** 2

    # no stenosis
    sample_seg = np.array([0, 1])
    a_seg = np.array([[area[0], area[-1]]])
    if len(f_sten) == 0:
        return sample_seg, np.array([0]), a_seg, [get_1d_seg_num(a_seg, params)]
    # one stenosis
    elif len(f_sten) == 1:
        return sample_seg, f_sten, a_seg, [get_1d_seg_num(a_seg, params)]
    # two stenoses, lump together
    elif len(f_sten) == 2:
        return sample_seg, np.array([np.sum(f_sten)]), a_seg, [get_1d_seg_num(a_seg, params)]

    # find largest stenosis
    i_sten = np.argmax(f_sten)

    # pick neighbors of stenosis
    if 0 < i_sten < len(f_sten) - 1:
        seg = [i_sten - 1, i_sten + 1]
    # stenosis at the start/end, lump together
    else:
        return sample_seg, np.array([np.sum(f_sten)]), a_seg, [get_1d_seg_num(a_seg, params)]

    # sample path points
    sample_1d = [0]
    area_1d = []
    for s in seg:
        sample_1d += [path[i_min[s]] / path[-1]]
        area_1d += [area[i_min[s]]]
    sample_1d += [1]

    # store stenosis coefficients
    f_sten_br = [np.sum(f_sten[:i_sten]), f_sten[i_sten], np.sum(f_sten[i_sten + 1:])]

    # area for svOneDSolver
    a_br = [[area[0], area_1d[0]],
            [area[i_min[i_sten]], area[i_min[i_sten]]],
            [area_1d[1], area[-1]]]

    seg_1d = []
    for a in a_br:
        seg_1d += [get_1d_seg_num(a, params)]

    return np.array(sample_1d), np.array(f_sten_br), np.array(a_br), seg_1d


def get_1d_seg_num(area, params):
    """
    Get number of segments for 1d solver
    """
    a_diff = np.max(area) - np.min(area)
    num_seg = int(round(np.sqrt(a_diff) / params.element_size))
    return np.max((params.min_num_elems, num_seg))


class Bifurcation(object):
    """
    Simple class to track inlet and outlets of a bifurcation
    """
    def __init__(self):
        self.inflow = None
        self.outflow = []

    def add_inflow(self, i):
        """
        Add inflow branch i (can only be executed once)
        """
        if self.inflow is None:
            self.inflow = i
        elif self.inflow != i:
            raise ValueError('bifurcation already has inflow id ' + repr(self.inflow))

    def add_outflow(self, i):
        """
        Add outflow branch i
        """
        if i not in self.outflow:
            self.outflow += [i]


def get_connectivity(cent):
    """
    Extract the connectivity (which branches are connected to which bifurcation) from a centerline
    """
    # read arrays from centerline
    branch = v2n(cent.GetPointData().GetArray('BranchId'))
    bifurcation = v2n(cent.GetPointData().GetArray('BifurcationId'))
    bifurcation_list = np.unique(bifurcation).tolist()
    bifurcation_list.remove(-1)

    # get centerline connectivity: which branches are attached to which bifurcation?
    connectivity = {}
    for bf in bifurcation_list:
        connectivity[bf] = Bifurcation()

    # loop all cells
    for c in range(cent.GetNumberOfCells()):
        ele = cent.GetCell(c)
        point_ids = np.array([ele.GetPointIds().GetId(i) for i in range(ele.GetPointIds().GetNumberOfIds())])
        br_ids = branch[point_ids].tolist()

        # find cells that are at borders of bifurcations (two unique RegionIds)
        if np.unique(br_ids).shape[0] == 2:
            # should be one branch and one bifurcation
            assert -1 in br_ids, 'No bifurcation in cell'

            # local node ids of branch and bifurcation (0 or 1)
            i_bf_ele = br_ids.index(-1)
            i_br_ele = int(not i_bf_ele)

            # branch and bifurcation id
            bf = bifurcation[point_ids[i_bf_ele]]
            br = branch[point_ids[i_br_ele]]

            assert bf != -1, 'Multiple bifurcations in cell'
            assert br != -1, 'Multiple branches in cell'

            # branch node is upstream in cell?
            if i_br_ele == 0:
                connectivity[bf].add_inflow(br)
            else:
                connectivity[bf].add_outflow(br)

    for bf, bifurcation in connectivity.items():
        assert len(bifurcation.outflow) >= 2, 'bifurcation ' + str(bf) + ' has less then two outlets'

    return connectivity


def curvature_radius(points):
    """
    Calculate curvature radius from list of points with three adjacent points each
    """
    # form 3 point sets
    p1 = points[:-2]
    p2 = points[1:-1]
    p3 = points[2:]

    p1p2 = p1 - p2
    p1p3 = p1 - p3
    p2p3 = p2 - p3

    # area of triangle
    area = np.linalg.norm(np.cross(p1p2, p1p3), axis=1) / 2.0

    # curvature radius
    curvature = (np.linalg.norm(p1p2) * np.linalg.norm(p1p3) * np.linalg.norm(p2p3)) / (4.0 * area)

    # resample to all points
    path_1d = np.append(0, np.cumsum(np.linalg.norm(np.diff(points, axis=0), axis=1)))
    return interp1d(path_1d, path_1d[1:-1], curvature)


def interp1d(x, xp, fp):
    """
    Mimic behavior of scipy.interpolate.interp1d (extend np.interp to multi-dimensional function values)
    Args:
        x: sample points
        xp: data points
        fp: data values (xD)

    Returns:
        y: interpolated values (xD)
    """
    if len(fp.shape) > 1:
        y = np.zeros((x.shape[0], fp.shape[1]))
        for d in range(fp.shape[1]):
            y[:, d] = np.interp(x, xp, fp[:, d])
    else:
        y = np.interp(x, xp, fp)
    return y
