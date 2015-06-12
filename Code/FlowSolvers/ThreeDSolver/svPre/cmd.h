/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including 
 * without limitation the rights to use, copy, modify, merge, publish, 
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *=========================================================================*/

#include "cvFlowsolverOptions.h"

#ifndef CMD_H
#define CMD_H

#define MAXSTRINGLENGTH 1024
#define MAXCMDLINELENGTH 1024
#define MAXPATHLEN 1024
#define DATA_MAX_SIZE 100
#define CV_OK 1
#define CV_ERROR 0

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

//#include <vector>
//using namespace std;

#include "vtkPolyData.h"
#include "vtkDoubleArray.h"

#ifdef WIN32
#define CALLTYPE __cdecl
#else
#define CALLTYPE
#endif

typedef struct Cmd {
  char *name;
  int (*pt2Function)(char*);
} Cmd;

typedef enum {WOMERSLEY=0,PARABOLIC=1,PLUG=2} BCTShapeType;

typedef struct{
    int pointNum;
    double* t;
    vtkPolyData* pd;
    vtkDoubleArray** mapped_data;
} BCTData;

extern int cmd_input; 
extern FILE *fp_input;
extern "C" FILE *stddbg;
extern int verbose_;
extern int cvsolver_node_order_;

// parse script file
int cmd_proc (char *cmd, int *ok);
int cmd_token_get (int *p_n, char *string, char *token, int *end);
int cmd_set_input (int p_cmd_input, FILE *p_fp);

// general utility
extern "C" {
    int debugprint(FILE* ,char* , ...);
}

// actual commands

//
// can use VTU to replace all of these calls
//
int CALLTYPE cmd_number_of_nodes(char*);
int CALLTYPE cmd_number_of_elements(char*);
int CALLTYPE cmd_number_of_mesh_edges(char*);
int CALLTYPE cmd_number_of_mesh_faces(char*);
int CALLTYPE cmd_nodes(char*);
int CALLTYPE cmd_elements(char*);
int CALLTYPE cmd_boundary_faces(char*);

// can use VTU instead of functions above

// still needed, but should come from VTU
int CALLTYPE cmd_adjacency(char*);

//
// Commands using Spectrum formatted files
//

int CALLTYPE cmd_noslip(char*);
int CALLTYPE cmd_prescribed_velocities(char*);
int CALLTYPE cmd_zero_pressure(char*);
int CALLTYPE cmd_pressure(char*);
int CALLTYPE cmd_set_surface_id(char*);

//
// Commands using VTK formatted files
//

int CALLTYPE cmd_mesh_vtu(char*);
int CALLTYPE cmd_mesh_and_adjncy_vtu(char*);
int CALLTYPE cmd_read_all_variables_vtu(char*);
int CALLTYPE cmd_read_pressure_velocity_vtu(char*);
int CALLTYPE cmd_read_pressure_vtu(char*);
int CALLTYPE cmd_read_velocity_vtu(char*);
int CALLTYPE cmd_read_displacements_vtu(char*);
int CALLTYPE cmd_read_accelerations_vtu(char*);
int CALLTYPE cmd_read_varwallprop_vtu(char*);
int CALLTYPE cmd_set_surface_id_vtp(char*);
int CALLTYPE cmd_noslip_vtp(char*);
int CALLTYPE cmd_prescribed_velocities_vtp(char*);
int CALLTYPE cmd_zero_pressure_vtp(char*);
int CALLTYPE cmd_pressure_vtp(char*);
int CALLTYPE cmd_deformable_wall_vtp(char*);
int CALLTYPE cmd_deformable_wall_vtp_simple(char*);
int CALLTYPE cmd_fix_free_edge_nodes_vtp(char*);
int CALLTYPE cmd_create_mesh_deformable_vtp(char*);
int CALLTYPE cmd_wall_displacements_write_vtp(char*);
//
//  General commands
//
int CALLTYPE cmd_number_of_solnvars(char*);
#if(VER_VARWALL == 1)
int CALLTYPE cmd_deformable_wall(char*);
#endif
int CALLTYPE cmd_ascii_format(char*);
int CALLTYPE cmd_verbose(char*);
int CALLTYPE cmd_cvsolver_node_order(char*);
int CALLTYPE cmd_initial_pressure(char*);
int CALLTYPE cmd_initial_velocity(char*);

//
//  Deformable functions not updated for VTK
//
int CALLTYPE cmd_deformable_wall(char*);
int CALLTYPE cmd_fix_free_edge_nodes(char*);
int CALLTYPE cmd_create_mesh_deformable(char*);
int CALLTYPE cmd_deformable_direct_solve(char*);
int CALLTYPE cmd_deformable_iterative_solve(char*);
#if(VER_VARWALL == 1)
int CALLTYPE cmd_deformable_iterative_solve_var_prop(char*);
#endif
int CALLTYPE cmd_deformable_write_vtk_mesh(char*);
#if(VER_VARWALL == 1)
int CALLTYPE cmd_varwallprop_write_vtk(char*);
#endif
int CALLTYPE cmd_deformable_write_feap(char*);
int CALLTYPE cmd_deformable_Evw(char*);
int CALLTYPE cmd_deformable_nuvw(char*);
int CALLTYPE cmd_deformable_thickness(char*);
int CALLTYPE cmd_deformable_pressure(char*);
int CALLTYPE cmd_deformable_kcons(char*);

//
// read from solver formatted files
//
#if(VER_VARWALL == 1)
int CALLTYPE cmd_append_varwallprop(char*);
#endif
int CALLTYPE cmd_read_restart_solution(char*);
int CALLTYPE cmd_read_restart_displacements(char*);
int CALLTYPE cmd_read_restart_accelerations(char*);
#if(VER_VARWALL == 1)
int CALLTYPE cmd_read_restart_varwallprop(char*);
int CALLTYPE cmd_read_geombc_varwallprop(char*);
#endif
int CALLTYPE cmd_read_displacements(char*);

//
// output to solver formatted files
//
int CALLTYPE cmd_write_restartdat(char*);
int CALLTYPE cmd_write_geombcdat(char*);
int CALLTYPE cmd_write_numstartdat(char*);
int CALLTYPE cmd_append_displacements(char*);

#if(VER_VARWALL == 1)
int CALLTYPE cmd_Laplace_Thickness(char*);
int CALLTYPE cmd_Laplace_Evw(char*);
int CALLTYPE cmd_Transient_Laplace_Evw(char*);
int CALLTYPE cmd_set_scalar_BCs(char*);
int CALLTYPE cmd_set_thickness_BCs(char*);
int CALLTYPE cmd_set_thickness_BCs_vtp(char*);
int CALLTYPE cmd_set_Evw_BCs(char*);
int CALLTYPE cmd_set_Evw_BCs_vtp(char*);
int CALLTYPE cmd_set_Initial_Evw(char*);
int CALLTYPE cmd_set_Initial_Evw_vtp(char*);
int CALLTYPE cmd_varwallprop_write_vtp(char*);
#endif

int CALLTYPE cmd_fluid_density(char*);
int CALLTYPE cmd_fluid_viscosity(char*);
int CALLTYPE cmd_bct_analytical_shape(char*);
int CALLTYPE cmd_bct_period(char*);
int CALLTYPE cmd_bct_point_number(char*);
int CALLTYPE cmd_bct_fourier_mode_number(char*);
//int CALLTYPE cmd_bct_preserve_flow(char*);
int CALLTYPE cmd_bct_flip(char*);
int CALLTYPE cmd_bct_create(char*);
int CALLTYPE cmd_bct_merge_on(char*);
int CALLTYPE cmd_bct_write_dat(char*);
int CALLTYPE cmd_bct_write_vtp(char*);

#endif // CMD_H
