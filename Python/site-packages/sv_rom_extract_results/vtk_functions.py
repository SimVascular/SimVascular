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
Collection of useful functions using vtk geometries
"""

import os
import vtk
import numpy as np

from vtk.util.numpy_support import numpy_to_vtk as n2v
from vtk.util.numpy_support import vtk_to_numpy as v2n


def read_geo(fname):
    """
    Read geometry from file, chose corresponding vtk reader
    Args:
        fname: vtp surface or vtu volume mesh

    Returns:
        vtk reader, point data, cell data
    """
    _, ext = os.path.splitext(fname)
    if ext == '.vtp':
        reader = vtk.vtkXMLPolyDataReader()
    elif ext == '.vtu':
        reader = vtk.vtkXMLUnstructuredGridReader()
    else:
        raise ValueError('File extension ' + ext + ' unknown.')
    reader.SetFileName(fname)
    reader.Update()

    return reader.GetOutput()


def write_geo(fname, input):
    """
    Write geometry to file
    Args:
        fname: file name
    """
    _, ext = os.path.splitext(fname)
    if ext == '.vtp':
        writer = vtk.vtkXMLPolyDataWriter()
    elif ext == '.vtu':
        writer = vtk.vtkXMLUnstructuredGridWriter()
    else:
        raise ValueError('File extension ' + ext + ' unknown.')
    writer.SetFileName(fname)
    writer.SetInputData(input)
    writer.Update()
    writer.Write()


def collect_arrays(output):
    res = {}
    for i in range(output.GetNumberOfArrays()):
        name = output.GetArrayName(i)
        data = output.GetArray(i)
        res[name] = v2n(data)
    return res


class ClosestPoints:
    """
    Find closest points within a geometry
    """
    def __init__(self, inp):
        if isinstance(inp, str):
            geo = read_geo(inp)
            inp = geo.GetOutput()
        dataset = vtk.vtkPolyData()
        dataset.SetPoints(inp.GetPoints())

        locator = vtk.vtkPointLocator()
        locator.Initialize()
        locator.SetDataSet(dataset)
        locator.BuildLocator()

        self.locator = locator

    def search(self, points, radius=None):
        """
        Get ids of points in geometry closest to input points
        Args:
            points: list of points to be searched
            radius: optional, search radius
        Returns:
            Id list
        """
        ids = []
        for p in points:
            if radius is not None:
                result = vtk.vtkIdList()
                self.locator.FindPointsWithinRadius(radius, p, result)
                ids += [result.GetId(k) for k in range(result.GetNumberOfIds())]
            else:
                ids += [self.locator.FindClosestPoint(p)]
        return ids


def region_grow(geo, seed_points, seed_ids, logger, n_max=99):
    # initialize output arrays
    array_ids = -1 * np.ones(geo.GetNumberOfPoints(), dtype=int)
    array_rad = np.zeros(geo.GetNumberOfPoints())
    array_dist = -1 * np.ones(geo.GetNumberOfPoints(), dtype=int)
    array_ids[seed_points] = seed_ids

    # initialize ids
    cids_all = set()
    pids_all = set(seed_points.tolist())
    pids_new = set(seed_points.tolist())

    # get points
    pts = v2n(geo.GetPoints().GetData())

    # loop until region stops growing or reaches maximum number of iterations
    i = 0
    while len(pids_new) > 0 and i < n_max:
        # update
        pids_old = pids_new

        # print progress
        print_str = 'Iteration ' + str(i)
        print_str += '\tNew points ' + str(len(pids_old)) + '     '
        print_str += '\tTotal points ' + str(len(pids_all))
        logger.info(print_str)

        # grow region one step
        pids_new = grow(geo, array_ids, pids_old, pids_all, cids_all)

        # convert to array
        pids_old_arr = list(pids_old)

        # create point locator with old wave front
        points = vtk.vtkPoints()
        points.Initialize()
        for i_old in pids_old:
            points.InsertNextPoint(geo.GetPoint(i_old))

        dataset = vtk.vtkPolyData()
        dataset.SetPoints(points)

        locator = vtk.vtkPointLocator()
        locator.Initialize()
        locator.SetDataSet(dataset)
        locator.BuildLocator()

        # find closest point in new wave front
        for i_new in pids_new:
            i_old = pids_old_arr[locator.FindClosestPoint(geo.GetPoint(i_new))]
            array_ids[i_new] = array_ids[i_old]
            array_rad[i_new] = array_rad[i_old] + np.linalg.norm(pts[i_new] - pts[i_old])
            array_dist[i_new] = i

        # count grow iterations
        i += 1

    return array_ids, array_dist + 1, array_rad


def grow(geo, array, pids_in, pids_all, cids_all):
    # ids of propagating wave-front
    pids_out = set()

    # loop all points in wave-front
    for pi_old in pids_in:
        cids = vtk.vtkIdList()
        geo.GetPointCells(pi_old, cids)

        # get all connected cells in wave-front
        for j in range(cids.GetNumberOfIds()):
            # get cell id
            ci = cids.GetId(j)

            # skip cells that are already in region
            if ci in cids_all:
                continue
            else:
                cids_all.add(ci)

            pids = vtk.vtkIdList()
            geo.GetCellPoints(ci, pids)

            # loop all points in cell
            for k in range(pids.GetNumberOfIds()):
                # get point id
                pi_new = pids.GetId(k)

                # add point only if it's new and doesn't fullfill stopping criterion
                if array[pi_new] == -1 and pi_new not in pids_in:
                    pids_out.add(pi_new)
                    pids_all.add(pi_new)

    return pids_out


def add_array(geo, name, array):
    """
    Add array to geometry
    """
    arr = n2v(array)
    arr.SetName(name)
    geo.GetPointData().AddArray(arr)
