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

/*------------------------------------------------------------*
 *                                                            *
 *                ****  command processor  ****               *
 *                                                            *
 *------------------------------------------------------------*/

#include "cvFlowsolverOptions.h"

#include "cmd.h"
 
FILE* stddbg;
int cmd_input = 1;
FILE *fp_input = stdin;

static Cmd cmd_table[] = {
  {"ascii_format", cmd_ascii_format},
  {"number_of_nodes", cmd_number_of_nodes},
  {"number_of_elements", cmd_number_of_elements},
  {"number_of_mesh_edges", cmd_number_of_mesh_edges},
  {"number_of_mesh_faces", cmd_number_of_mesh_faces},
  {"number_of_variables", cmd_number_of_solnvars},
  {"nodes",      cmd_nodes},
  {"elements",      cmd_elements},
  {"noslip", cmd_noslip},
  {"prescribed_velocities", cmd_prescribed_velocities},
  {"zero_pressure", cmd_zero_pressure},
  {"pressure", cmd_pressure},
  {"read_displacements",cmd_read_displacements},
  {"boundary_faces", cmd_boundary_faces},
  {"adjacency", cmd_adjacency},
  {"set_surface_id", cmd_set_surface_id},
  {"deformable_wall", cmd_deformable_wall},
  {"verbose", cmd_verbose},
  {"cvsolver_node_order",cmd_cvsolver_node_order},
  {"initial_pressure",cmd_initial_pressure},
  {"initial_velocity",cmd_initial_velocity},
  {"fix_free_edge_nodes",cmd_fix_free_edge_nodes},
  {"deformable_create_mesh", cmd_create_mesh_deformable},
  {"deformable_write_vtk_mesh", cmd_deformable_write_vtk_mesh},
  {"deformable_write_feap", cmd_deformable_write_feap},
#if(VER_VARWALL == 1)
  {"set_surface_thickness", cmd_set_thickness_BCs},
  {"set_surface_Evw", cmd_set_Evw_BCs},
  {"set_surface_initial_Evw", cmd_set_Initial_Evw},
  {"varwallprop_write_vtk", cmd_varwallprop_write_vtk},
#endif
  {"mesh_vtu",      cmd_mesh_vtu},
  {"mesh_and_adjncy_vtu",      cmd_mesh_and_adjncy_vtu},
  {"read_all_variables_vtu", cmd_read_all_variables_vtu},
  {"read_pressure_velocity_vtu", cmd_read_pressure_velocity_vtu},
  {"read_pressure_vtu", cmd_read_pressure_vtu},
  {"read_velocity_vtu", cmd_read_velocity_vtu},
  {"read_displacements_vtu", cmd_read_displacements_vtu},
  {"read_accelerations_vtu", cmd_read_accelerations_vtu},
  {"read_varwallprop_vtu", cmd_read_varwallprop_vtu},
  {"read_solution_restart", cmd_read_restart_solution},
  {"read_displacements_restart", cmd_read_restart_displacements},
  {"read_accelerations_restart", cmd_read_restart_accelerations},
  {"set_surface_id_vtp", cmd_set_surface_id_vtp},
  {"noslip_vtp", cmd_noslip_vtp},
  {"prescribed_velocities_vtp", cmd_prescribed_velocities_vtp},
  {"zero_pressure_vtp", cmd_zero_pressure_vtp},
  {"pressure_vtp", cmd_pressure_vtp},
  {"write_restart", cmd_write_restartdat},
  {"write_geombc", cmd_write_geombcdat},
  {"write_numstart", cmd_write_numstartdat},
  {"deformable_wall_vtp_simple", cmd_deformable_wall_vtp_simple},
  {"fix_free_edge_nodes_vtp", cmd_fix_free_edge_nodes_vtp},
  {"deformable_create_mesh_vtp", cmd_create_mesh_deformable_vtp},
  {"deformable_wall_vtp", cmd_deformable_wall_vtp},
  {"deformable_E",cmd_deformable_Evw},
  {"deformable_nu",cmd_deformable_nuvw},
  {"deformable_thickness",cmd_deformable_thickness},
  {"deformable_pressure",cmd_deformable_pressure},
  {"deformable_kcons",cmd_deformable_kcons},
  {"deformable_direct_solve_displacements", cmd_deformable_direct_solve},
  {"deformable_solve_displacements", cmd_deformable_iterative_solve},
  {"wall_displacements_write_vtp", cmd_wall_displacements_write_vtp},
  {"append_displacements",cmd_append_displacements},
  #if(VER_VARWALL == 1)
  {"read_varwallprop_restart",cmd_read_restart_varwallprop},
  {"read_varwallprop_geombc",cmd_read_geombc_varwallprop},
  {"set_surface_thickness_vtp", cmd_set_thickness_BCs_vtp},
  {"set_surface_E_vtp", cmd_set_Evw_BCs_vtp},
  {"set_surface_initial_E_vtp", cmd_set_Initial_Evw_vtp},
  {"solve_varwall_thickness",cmd_Laplace_Thickness},
  {"solve_varwall_E",cmd_Laplace_Evw},
  {"solve_transient_varwall_E",cmd_Transient_Laplace_Evw},
  {"deformable_solve_varwall_displacements", cmd_deformable_iterative_solve_var_prop},
  {"varwallprop_write_vtp", cmd_varwallprop_write_vtp},
  {"append_varwallprop",cmd_append_varwallprop},
#endif
  {"fluid_density", cmd_fluid_density},
  {"fluid_viscosity", cmd_fluid_viscosity},
  {"bct_analytical_shape", cmd_bct_analytical_shape},
  {"bct_period", cmd_bct_period},
  {"bct_point_number", cmd_bct_point_number},
  {"bct_fourier_mode_number", cmd_bct_fourier_mode_number},
//  {"bct_preserve_flow", cmd_bct_preserve_flow},
  {"bct_flip", cmd_bct_flip},
  {"bct_create", cmd_bct_create},
  {"bct_merge_on", cmd_bct_merge_on},
  {"bct_write_dat", cmd_bct_write_dat},
  {"bct_write_vtp", cmd_bct_write_vtp},
  {NULL,       NULL}};

int debugprint(FILE* fp,char *fmt, ...)
{
   if (fp == NULL) return CV_OK;
   if (!verbose_) return CV_OK;

   va_list argp;
   va_start(argp, fmt);
   vfprintf(stdout, fmt, argp);
   va_end(argp);
   fflush(stdout);
   return CV_OK;
}


/*------------------------------------------------------------*
 *                                                            *
 *             ****  cmd_proc  ****                           *
 *                                                            *
 * process a command.                                         *
 *------------------------------------------------------------*/

int cmd_proc (char *cmd, int *ok) {

  int i, j;

  int (*pt2Function)(char*);

  char *s;

  char name[MAXSTRINGLENGTH];

 /**************
  ***  body  ***
  **************/

  if (cmd[0] == '#' || cmd[0] == '\n' || cmd[0] == '\r') {
    return CV_OK;
    }

  sscanf(cmd, "%s", name);
 
  debugprint(stddbg,"command being processed is %s. \n",name);
  
  for (i = 0; cmd_table[i].name != NULL; i++) {
    if (!strcmp(name, cmd_table[i].name)) {
      pt2Function = cmd_table[i].pt2Function;
      return (*pt2Function)(cmd);
      //return CV_OK;
      }
    }
  
  for (i = 0; i < strlen(cmd); i++) {
    if ((cmd[i] != ' ') && (cmd[i] != '\n')) {
      fprintf(stderr, "\n  **** error: unknown command: %s", cmd);
      return CV_ERROR;
      }
    }

  return CV_OK;

}


/*------------------------------------------------------------* 
 *                                                            *  
 *              ****  cmd_token_get  ****                     *  
 *                                                            *  
 * get the next blank separated string.                       *  
 *------------------------------------------------------------*/

int cmd_token_get (int *p_n, char *string, char *token, int *end) {

  int i, j;

  int quote;

  int n;

  int len;

  static int c, state = 0;

 /**************
  ***  body  ***
  **************/

  len = strlen(string);
  n = *p_n;

  i = 0;
  quote = 0;
  token[0] = '\0';
  *end = 0;

  if (n >= len)
    return CV_ERROR;

  while ((c = string[n++]) != '\0') { 
    if ((c == ' ') || (c == '\t')) {
      if (i != 0) {
        token[i] = '\0';
	*p_n = n;
	return CV_OK;
	}
      }
    else if (c == '"') {
      while ((c = string[n++]) != '\0') {
        if (c == '"') {
          token[i] = '\0';
	  *p_n = n;
	  return CV_OK;
	  }
        else if (c == '\n') { 
          token[i] = '\0';
 	  *p_n = n;
          *end = 1;
	  return CV_OK;
	  }

        token[i++] = c;
        token[i] = '\0';
	}
      }
    else if (c == '\n') {
      *end = 1;

      if (i != 0) {
        token[i] = '\0';
	*p_n = n;
        return CV_OK;
	}
      else {
        token[i] = '\0';
	*p_n = n;
        return CV_OK;
	}
      }
    else {
      token[i++] = c;
      token[i] = '\0';
      }
    }

  *p_n = n;
  *end = 1;

  if (i == 0) 
    return CV_ERROR;

  return CV_OK;
}


/*------------------------------------------------------------*
 *                                                            *
 *                  ****  cmd_set_input  ****                 *
 *                                                            *
 *------------------------------------------------------------*/

int cmd_set_input (int p_cmd_input, FILE *p_fp) {

 /**************
  ***  body  ***
  **************/

  cmd_input = p_cmd_input;

  if (!p_cmd_input) {
    fp_input = p_fp;
    }
  else {
    fp_input = stdin;
    }

  return CV_OK;
  
}



