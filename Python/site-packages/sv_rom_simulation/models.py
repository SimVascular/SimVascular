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
Contains all ROM building blocks (currently onky 1D)
"""
import numpy as np
from collections import defaultdict
from scipy.signal import argrelextrema


# todo: move modeling stuff from Mesh class and io_1d in ROM base class. ZeroD, OneD inherit from base class


class ZeroD(object):
    """
    Contains all 0D building blocks
    """

    def __init__(self, mesh, params):
        self.mesh = mesh
        self.params = params
        self.model = {}

    def generate(self):
        """
        bla
        """
        for k in ['branches', 'junctions']:
            self.model[k] = {'R_poiseuille': defaultdict(list),
                             'L': defaultdict(list),
                             'C': defaultdict(list)}
            if k == 'branches':
                self.model[k]['stenosis_coefficient'] = defaultdict(list)

        # loop branches
        for (br, ids) in self.mesh.cell_data['id'].items():

            # loop segments
            for seg, _ in enumerate(ids):
                self.model['branches']['stenosis_coefficient'][br] += [self.get_factor_stenosis(br, seg)]

                # segment length
                length = self.mesh.cell_data['length'][br][seg]

                # segment average radius
                radius = self.get_segment_avg_radius(br, seg)

                # calculate 0D elements
                self.model['branches']['R_poiseuille'][br] += [self.get_resistance_viscous(length, radius)]
                self.model['branches']['L'][br] += [self.get_inductance(length, radius)]
                self.model['branches']['C'][br] += [self.get_capacitance(length, radius)]

        return self.model

    def get_factor_stenosis(self, br, seg):
        """
        See (7) in Mirramezani and Shadden, 2020
        """
        return 1.52 * self.params.density / 2 * self.mesh.cell_data['stenosis'][br][seg]

    def get_resistance_viscous(self, length, avg_radius):
        """
        Calculate viscous resistance using Poiseuille flow
        """
        return 8.0 * self.params.viscosity * length / (np.pi * (avg_radius ** 4))

    def get_capacitance(self, length, avg_radius):
        """
        Compute capacitance using Poiseuille flow
        """
        return 3.0 * length * np.pi * (avg_radius ** 2) / (2 * self.get_vessel_stiffness(avg_radius))

    def get_inductance(self, length, avg_radius):
        """
        Compute inductance using Poiseuille flow
        """
        return length * self.params.density / (np.pi * (avg_radius ** 2))

    def get_segment_avg_radius(self, br, seg):
        """
        Compute average segment radius from inlet and outlet area
        """
        a0, a1 = self.mesh.cell_data['area'][br][seg]
        return (np.sqrt(a0 / np.pi) + np.sqrt(a1 / np.pi)) / 2.0

    def get_vessel_stiffness(self, r):
        """
        Get vessel stiffness depending on constitutive law
        """
        if self.params.material_model == "OLUFSEN":
            k1 = self.params.olufsen_material_k1
            k2 = self.params.olufsen_material_k2
            k3 = self.params.olufsen_material_k3
            return k1 * np.exp(k2 * r) + k3
        elif self.params.material_model == "LINEAR":
            return self.params.linear_material_ehr
        else:
            raise ValueError("Material type " + self.params.material_type + " unknown (choose OLUFSEN or LINEAR).")
