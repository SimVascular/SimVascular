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

import os
import json
import pdb
import numpy as np

from .parameters import OutflowBoundaryConditionType


def write_0d_solver_file(mesh, params, model):
    """
    Generate 0d solver input file (.json)
    """
    # create input dictionary
    inp = {'simulation_parameters': {},
           'boundary_conditions': [],
           'junctions': [],
           'vessels': []}

    # general
    inp['simulation_parameters']['model_name'] = params.model_name

    # time
    dt = params.time_step
    n_step = params.num_time_steps
    t_cycle = mesh.inflow_data[-1][0]
    inp['simulation_parameters']['number_of_time_pts_per_cardiac_cycle'] = int(t_cycle / dt)
    inp['simulation_parameters']['number_of_cardiac_cycles'] = int(n_step / t_cycle * dt) + 1

    # fluid
    inp['simulation_parameters']['density'] = params.density
    inp['simulation_parameters']['viscosity'] = params.viscosity

    # vessels
    for (branch, ids), lengths in zip(mesh.cell_data['id'].items(), mesh.cell_data['length'].values()):
        for i, (j, length) in enumerate(zip(ids, lengths)):
            vessel = {'vessel_id': int(j),
                      'vessel_name': 'branch' + str(branch) + '_seg' + str(i),
                      'vessel_length': length,
                      'zero_d_element_type': 'BloodVessel',
                      'zero_d_element_values': {}}
            # zerod values
            for zerod in model['branches'].keys():
                vessel['zero_d_element_values'][zerod] = model['branches'][zerod][branch][i]
            # inlet bc
            if branch == 0 and i == 0:
                vessel['boundary_conditions'] = {'inlet': 'INFLOW'}
            # outlet bc
            if j in mesh.terminal:
                bc_name = list(mesh.outlet_face_names_index.keys())[mesh.terminal.index(j)]
                bc_str = mesh.bc_type[bc_name].upper() + '_' + str(mesh.outlet_face_names_index[bc_name])
                vessel['boundary_conditions'] = {'outlet': bc_str}
            inp['vessels'] += [vessel]

    # junctions
    for i, j_branches in enumerate(mesh.seg_connectivity):
        # general junction properties
        junction = {'junction_name': 'J' + str(i),
                    'junction_type': 'internal_junction',
                    'inlet_vessels': [int(j_branches[0])],
                    'outlet_vessels': []}
        for j in j_branches[1:]:
            junction['outlet_vessels'] += [int(j)]
        # branching vessels
        if len(j_branches) > 2:
            junction['junction_type'] = 'BloodVesselJunction'
            junction['tangents'] = mesh.junctions[i]['tangents']
            for n in ['areas', 'lengths']:
                junction[n] = [float(v) for v in mesh.junctions[i][n]]

        inp['junctions'] += [junction]

    # inlet bc
    inflow_q = []
    inflow_t = []
    for value in mesh.inflow_data:
        inflow_t += [value.time]
        inflow_q += [value.flow]
    inflow = {'bc_name': 'INFLOW',
              'bc_type': 'FLOW',
              'bc_values': {'t': inflow_t, 'Q': inflow_q}}
    inp['boundary_conditions'] += [inflow]

    # outlet bcs
    for bc_name, i in mesh.outlet_face_names_index.items():
        bc_type = mesh.bc_type[bc_name]
        bc_str = mesh.bc_type[bc_name].upper() + '_' + str(i)
        bc_val = mesh.bc_map[bc_name]

        outflow = {'bc_name': bc_str,
                   'bc_type': bc_type.upper(),
                   'bc_values': {}}
        if bc_type == OutflowBoundaryConditionType.RCR:
            seq = ['Rp', 'C', 'Rd', 'Pd']
            for name, val in zip(seq, bc_val):
                outflow['bc_values'][name] = float(val)
        elif bc_type == OutflowBoundaryConditionType.RESISTANCE:
            seq = ['R', 'Pd']
            for name, val in zip(seq, bc_val):
                outflow['bc_values'][name] = float(val)
        elif bc_type == OutflowBoundaryConditionType.CORONARY:
            for name, val in bc_val['var'].items():
                outflow['bc_values'][name] = val
            outflow['bc_values']['t'] = bc_val['time']
            outflow['bc_values']['Pim'] = bc_val['pressure']
            outflow['bc_values']['Pv'] = 0.0
        inp['boundary_conditions'] += [outflow]

    # write to file
    file_name = os.path.join(params.output_directory, params.solver_output_file)
    with open(file_name, 'w') as file:
        json.dump(inp, file, indent=4, sort_keys=True)
