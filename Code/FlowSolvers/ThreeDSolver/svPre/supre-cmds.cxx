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

#include "cmd.h"
#include "cvSolverIO.h"

#ifdef SV_USE_ZLIB
#include "simvascular_zlib.h"
#else
#include <stdlib.h>
#define gzopen fopen
#define gzprintf fprintf
#define gzFile FILE*
#define gzclose fclose
#define gzgets fgets
#define gzeof feof
#endif

#ifndef WIN32
#include <strings.h>
#else
void  bzero(void* ptr, size_t sz) {
    int i;
    char *cptr;
    cptr = (char*) ptr;
    for (i=0; i < sz; i++) {
        cptr[i]=0;
    }
    return;
}
#endif
#include <time.h>

#include <vector>
using namespace std;

#include "vtkPointData.h"

// =========
//   Cross
// =========

inline
void Cross( double ax, double ay, double az,
            double bx, double by, double bz,
            double *prodx, double *prody, double *prodz )
{
  (*prodx) = ay * bz - az * by;
  (*prody) = -( ax * bz - az * bx );
  (*prodz) = ax * by - ay * bx;
  return;
}


// =======
//   Dot
// =======

inline
double Dot( double ax, double ay, double az,
            double bx, double by, double bz )
{
  double product;

  product = ax * bx + ay * by + az * bz;
  return product;
}

//
// some helpful global variables
//
extern char* oformat;

extern int numNodes_;
extern int numElements_;
extern int numMeshEdges_;
extern int numMeshFaces_;
extern int numSolnVars_;
extern int numBoundaryFaces_;
extern int** boundaryElements_;
extern double* nodes_;
extern int* elements_;
extern int* boundaryElementsIds_;
extern int* xadj_;
extern int xadjSize_;
extern int* adjncy_;
extern int adjncySize_;
extern int* iBC_;
extern int* iBCB_;
extern double* BCB_;
extern double init_p_;
extern double init_v_[3];
extern double* soln_;
extern double* dispsoln_;
#if(VER_VARWALL == 1)
//variable wall thickness and Young Mod
extern double* wallpropsoln_;
//dirichlet BC for nodes, Laplace Eqn for wallthickness
extern double* gBC_;
#endif

extern double* acc_;

extern int   DisplacementNumElements_;
extern int*  DisplacementConn_[3];
extern int   DisplacementNumNodes_;
extern int*  DisplacementNodeMap_;
extern double* DisplacementSolution_;
#if(VER_VARWALL == 1)
extern double* ThicknessSolution_;
extern double* EvwSolution_;
#endif
extern double  Displacement_Evw_;
extern double  Displacement_nuvw_;
extern double  Displacement_thickness_;
extern double  Displacement_kcons_;
extern double  Displacement_pressure_;

int writeGEOMBCDAT(char* filename);
int writeRESTARTDAT(char* filename);
int readRESTARTDAT(char* infile,int readSoln, int readDisp, int readAcc);

int parseNum(char *cmd, int *num);
int parseFile(char *cmd);
int NWgetNextNonBlankLine(int *eof);
int NWcloseFile();
int setNodesWithCode(char *cmd, int val);
int setBoundaryFacesWithCode(char *cmd,int setSurfID, int surfID,
                             int setCode, int code, double value);
int parseDouble(char *cmd, double *num);
int parseDouble2(char *cmd, double *num);
int parseDouble3(char *cmd, double *v1, double *v2, double *v3);
int parseCmdStr(char *cmd, char *mystr);
int parseCmdStr2(char *cmd, char *mystr);
int parseNum2(char *cmd, int *num);
int check_node_order(int n0, int n1, int n2, int n3, int elementId,
                     int *k0,int *k1, int *k2, int *k3);
int fixFreeEdgeNodes(char *cmd);
int createMeshForDispCalc(char *cmd);

extern gzFile fp_;
extern char buffer_[MAXCMDLINELENGTH];

extern double rho_;
extern double mu_;
extern int bctShape_;
extern double bctPeriod_;
extern int bctPointNum_;
extern int bctModeNum_;
extern int bctPreserve_;
extern int bctFlip_;
extern int bctMerge_;
extern int bctNodeNumTotal_;
extern int bctPointNumMax_;
extern vector<BCTData> vbct;

int cmd_ascii_format(char*) {

    // enter
    debugprint(stddbg,"Entering cmd_ascii_format.\n");

    static char* newformat = "ascii";
    oformat = newformat;

    // cleanup
    debugprint(stddbg,"Exiting cmd_ascii_format.\n");
    return CV_OK;
}

int cmd_verbose(char*) {

    verbose_ = 1;

    // enter
    debugprint(stddbg,"Entering cmd_verbose_format.\n");
    // cleanup
    debugprint(stddbg,"Exiting cmd_verbose_format.\n");
    return CV_OK;
}

int cmd_cvsolver_node_order(char*) {
    // enter
    debugprint(stddbg,"Entering cmd_cvsolver_node_order.\n");
    cvsolver_node_order_ = 1;
    // cleanup
    debugprint(stddbg,"Exiting cmd_cvsolver_node_order.\n");
    return CV_OK;
}

int cmd_number_of_nodes(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_number_of_nodes.\n");

    // do work
    numNodes_ = 0;
    if (parseNum(cmd, &numNodes_) == CV_ERROR) {
        return CV_ERROR;
    }
    debugprint(stddbg,"  Number of Nodes = %i\n",numNodes_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_number_of_nodes.\n");
    return CV_OK;

}

int cmd_number_of_elements(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_number_of_elements.\n");

    // do work
    numElements_ = 0;
    if (parseNum(cmd, &numElements_) == CV_ERROR) {
        return CV_ERROR;
    }
    debugprint(stddbg,"  Number of Elements = %i\n",numElements_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_number_of_elements.\n");
    return CV_OK;
}

int cmd_number_of_mesh_faces(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_number_of_mesh_faces.\n");

    // do work
    numMeshFaces_ = 0;
    if (parseNum(cmd, &numMeshFaces_) == CV_ERROR) {
        return CV_ERROR;
    }
    debugprint(stddbg,"  Number of Mesh Faces = %i\n",numMeshFaces_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_number_of_mesh_faces.\n");
    return CV_OK;
}

int cmd_number_of_mesh_edges(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_number_of_mesh_edges.\n");

    // do work
    numMeshEdges_ = 0;
    if (parseNum(cmd, &numMeshEdges_) == CV_ERROR) {
        return CV_ERROR;
    }
    debugprint(stddbg,"number of mesh edges = %i\n",numMeshEdges_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_number_of_mesh_edges.\n");
    return CV_OK;
}

int cmd_number_of_solnvars(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_number_of_solnvars.\n");

    // do work
    numSolnVars_ = 0;
    if (parseNum(cmd, &numSolnVars_) == CV_ERROR) {
        return CV_ERROR;
    }
    debugprint(stddbg,"  Number of Soln Vars = %i\n",numSolnVars_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_number_of_solnvars.\n");
    return CV_OK;
}


int cmd_initial_pressure(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_initial_pressure.\n");

    // do work
    double p = 0;
    init_p_ = 0.0;
    if (parseDouble(cmd, &p) == CV_ERROR) {
        return CV_ERROR;
    }
    init_p_ = p;
    debugprint(stddbg,"  Initial Pressure = %lf\n",init_p_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_initial_pressure.\n");
    return CV_OK;

}


int cmd_initial_velocity(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_initial_velocity.\n");

    // do work
    init_v_[0] = 0.0;
    init_v_[1] = 0.0;
    init_v_[2] = 0.0;
    if (parseDouble3(cmd, &init_v_[0], &init_v_[1], &init_v_[2]) == CV_ERROR) {
        return CV_ERROR;
    }
    debugprint(stddbg,"  Initial Velocity = <%lf,%lf,%lf>\n",init_v_[0],init_v_[1],init_v_[2]);

    // cleanup
    debugprint(stddbg,"Exiting cmd_initial_velocity.\n");
    return CV_OK;

}


int cmd_nodes(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_nodes.\n");

    // do work
    if (numNodes_ == 0) {
      fprintf(stderr,"ERROR:  Must specify number of nodes before you read them in!\n");
      return CV_ERROR;
    }

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    nodes_ = new double [3*numNodes_];
    double x,y,z;

    // NOTE: currently node id is ignored,
    // nodes must be in sequential order!
    int nodeId;

    int eof = 0;

    for (int i = 0; i < numNodes_ ; i++) {
        if (NWgetNextNonBlankLine(&eof) == CV_ERROR) {
            delete nodes_;
            nodes_ = NULL;
            return CV_ERROR;
        }
        if (sscanf(buffer_,"%i %lf %lf %lf",&nodeId,&x,&y,&z) != 4) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
            delete nodes_;
            nodes_ = NULL;
            return CV_ERROR;
        }
        nodes_[0*numNodes_+i] = x;
        nodes_[1*numNodes_+i] = y;
        nodes_[2*numNodes_+i] = z;
    }
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting cmd_nodes.\n");
    return CV_OK;

}

int cmd_elements(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_elements.\n");

    // do work
    if (numElements_ == 0) {
      fprintf(stderr,"ERROR:  Must specify number of elements before you read them in!\n");
      return CV_ERROR;
    }

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    elements_ = new int [4*numElements_];

    int n0,n1,n2,n3;

    // NOTE: currently element id is ignored,
    // elements must be in sequential order!
    int elementId;

    int eof = 0;

    for (int i = 0; i < numElements_ ; i++) {
        NWgetNextNonBlankLine(&eof);
        if (sscanf(buffer_,"%i %i %i %i %i",&elementId,&n0,&n1,&n2,&n3) != 5) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
            NWcloseFile();
            delete elements_;
            elements_ = NULL;
            return CV_ERROR;
        }

        int j0 = 0;
        int j1 = 0;
        int j2 = 0;
        int j3 = 0;

        check_node_order(n0, n1, n2, n3, elementId,
                         &j0,&j1,&j2,&j3);

        elements_[0*numElements_+i] = j0;
        elements_[1*numElements_+i] = j1;
        elements_[2*numElements_+i] = j2;
        elements_[3*numElements_+i] = j3;
    }
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting cmd_elements.\n");
    return CV_OK;

}

int cmd_boundary_faces(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_boundary_faces.\n");

    // do work
    if (numElements_ == 0) {
      fprintf(stderr,"ERROR:  Must specify number of elements before boundary faces!\n");
      return CV_ERROR;
    }
    if (numMeshFaces_ == 0) {
      fprintf(stderr,"ERROR:  Must specify number of mesh faces before boundary faces!\n");
      return CV_ERROR;
    }

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    // init data fort time this function is called
    if (boundaryElements_ == NULL) {
        boundaryElements_ = new int* [4];
        boundaryElements_[0] = new int [numMeshFaces_];
        boundaryElements_[1] = new int [numMeshFaces_];
        boundaryElements_[2] = new int [numMeshFaces_];
        boundaryElements_[3] = new int [numMeshFaces_];
        boundaryElementsIds_ = new int [numMeshFaces_];
    }

    int n0,n1,n2,n3;

    // NOTE: currently element id is ignored,
    // elements must be in sequential order!
    int elementId,matId;

    int eof = 0;

    int i = 0;
    for (i = 0; i < numMeshFaces_ ; i++) {
        if (NWgetNextNonBlankLine(&eof) == CV_ERROR) {
            if (eof != 0) break;
            return CV_ERROR;
        }
        if (sscanf(buffer_,"%i %i %i %i %i",&elementId,&matId,&n0,&n1,&n2) != 5) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
            continue;
        }

        n3 = -1;
        int j0 = 0;
        int j1 = 0;
        int j2 = 0;
        int j3 = 0;

        check_node_order(n0, n1, n2, n3, elementId,
                         &j0,&j1,&j2,&j3);

        boundaryElements_[0][numBoundaryFaces_] = j0;
        boundaryElements_[1][numBoundaryFaces_] = j1;
        boundaryElements_[2][numBoundaryFaces_] = j2;
        boundaryElements_[3][numBoundaryFaces_] = j3;

        // note that I assume element numbering starts at 1,
        // whereas flow solver assumes it started at zero!!
        boundaryElementsIds_[numBoundaryFaces_] = elementId - 1;

        numBoundaryFaces_++;

    }
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting cmd_boundary_elements.\n");
    return CV_OK;

}

int cmd_noslip(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_noslip.\n");

    // do work

    // no-slip code is 56
    setNodesWithCode(cmd,56);

    // cleanup
    debugprint(stddbg,"Exiting cmd_noslip.\n");
    return CV_OK;
}

int cmd_prescribed_velocities(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_prescribed_velocities.\n");

    // do work

    // no-slip code is 56
    setNodesWithCode(cmd,56);

    // cleanup
    debugprint(stddbg,"Exiting cmd_prescribed_velocities.\n");
    return CV_OK;
}

int cmd_fix_free_edge_nodes(char *cmd) {

    // enter
    debugprint(stddbg,"Entering fix_free_edge_nodes.\n");

    // do work
    if (fixFreeEdgeNodes(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    // cleanup
    debugprint(stddbg,"Exiting cmd_fix_free_edge_nodes.\n");
    return CV_OK;
}

int cmd_create_mesh_deformable(char *cmd) {

    // enter
    debugprint(stddbg,"Entering create_mesh_deformable.\n");

    // do work
    if (createMeshForDispCalc(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    // cleanup
    debugprint(stddbg,"Exiting create_mesh_deformable.\n");
    return CV_OK;
}

int cmd_zero_pressure(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_zero_pressure.\n");

    // do work
    double pressure = 0.0;
    int setSurfID = 0;
    int surfID = 0;
    int setPressure = 1;

    // should use bits and not ints here!!
    int code = 2;

    if (setBoundaryFacesWithCode(cmd,setSurfID,surfID,
                                 setPressure,code,pressure) == CV_ERROR) {
        return CV_ERROR;
    }

    // cleanup
    debugprint(stddbg,"Exiting cmd_zero_pressure.\n");
    return CV_OK;
}


int cmd_pressure(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_pressure.\n");

    // do work
    double pressure = 0.0;

    if (parseDouble2(cmd,&pressure) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  Pressure = %lf\n",pressure);

    int setSurfID = 0;
    int surfID = 0;
    int setPressure = 1;
    // should use bits and not ints here!!
    int code = 2;

    if (setBoundaryFacesWithCode(cmd,setSurfID,surfID,
                                 setPressure,code,pressure) == CV_ERROR) {
        return CV_ERROR;
    }

    // cleanup
    debugprint(stddbg,"Exiting cmd_pressure.\n");
    return CV_OK;
}


int cmd_deformable_wall(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_wall.\n");

    // do work
    double value = 0.0;
    int setSurfID = 0;
    int surfID = 0;
    int setCode = 1;
    // should use bits and not ints here!!
    int code = 16;

    if (setBoundaryFacesWithCode(cmd,setSurfID,surfID,
                                 setCode,code,value) == CV_ERROR) {
        return CV_ERROR;
    }

    dispsoln_ = new double[3*numNodes_]();

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_wall.\n");
    return CV_OK;
}


int cmd_set_surface_id(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_set_surface_id.\n");

    // do work
    int surfID = 0;
    if (parseNum2(cmd,&surfID) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  Setting surfID to [%i]\n",surfID);

    double value = 0.0;
    int setSurfID = 1;
    int setCode = 0;
    // should use bits and not ints here!!
    int code = 0;

    if (setBoundaryFacesWithCode(cmd,setSurfID,surfID,
                                 setCode,code,value) == CV_ERROR) {
        return CV_ERROR;
    }

    // cleanup
    debugprint(stddbg,"Exiting cmd_set_surface_id.\n");
    return CV_OK;
}

int cmd_adjacency(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_adjacency.\n");

    // do work
    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    xadjSize_ = 0;

    int eof = 0;

    if (NWgetNextNonBlankLine(&eof) == CV_ERROR) {
        return CV_ERROR;
    }

    if (sscanf(buffer_,"xadj: %i\n",&xadjSize_) != 1) {
        fprintf(stderr,"ERROR parsing line (%s)\n",buffer_);
        return CV_ERROR;
    }

   adjncySize_ = 0;

    if (NWgetNextNonBlankLine(&eof) == CV_ERROR) {
        return CV_ERROR;
    }

   if (sscanf(buffer_,"adjncy: %i\n",&adjncySize_) != 1) {
        fprintf(stderr,"ERROR parsing line (%s)\n",buffer_);
        return CV_ERROR;
   }

   xadj_ = new int [xadjSize_];
   adjncy_ = new int [adjncySize_];

   for (int i = 0; i < xadjSize_ ; i++) {
      if (NWgetNextNonBlankLine(&eof) == CV_ERROR) {
        return CV_ERROR;
      }
      if (sscanf(buffer_,"%i",&xadj_[i]) != 1) {
            fprintf(stderr,"ERROR:  line not of correct format (%s)\n",buffer_);
            delete xadj_;
            delete adjncy_;
            return CV_ERROR;
      }
   }

   for (int i = 0; i < adjncySize_ ; i++) {
      if (NWgetNextNonBlankLine(&eof) == CV_ERROR) {
        return CV_ERROR;
      }
      if (sscanf(buffer_,"%i",&adjncy_[i]) != 1) {
            fprintf(stderr,"ERROR:  line not of correct format (%s)\n",buffer_);
            delete xadj_;
            delete adjncy_;
            return CV_ERROR;
      }
   }

   NWcloseFile();
   return CV_OK;

}

int cmd_deformable_Evw(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_Evw.\n");

    // do work
    double value = 0;
    Displacement_Evw_ = 0.0;
    if (parseDouble(cmd, &value) == CV_ERROR) {
        return CV_ERROR;
    }
    Displacement_Evw_ = value;
    debugprint(stddbg,"  Evw = %lf\n",Displacement_Evw_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_Evw.\n");
    return CV_OK;

}

int cmd_deformable_nuvw(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_nuvw.\n");

    // do work
    double value = 0;
    Displacement_nuvw_ = 0.0;
    if (parseDouble(cmd, &value) == CV_ERROR) {
        return CV_ERROR;
    }
    Displacement_nuvw_ = value;
    debugprint(stddbg,"  nuvw = %lf\n",Displacement_nuvw_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_nuvw.\n");
    return CV_OK;

}

int cmd_deformable_thickness(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_thickness.\n");

    // do work
    double value = 0;
    Displacement_thickness_ = 0.0;
    if (parseDouble(cmd, &value) == CV_ERROR) {
        return CV_ERROR;
    }
    Displacement_thickness_ = value;
    debugprint(stddbg,"  thickness = %lf\n",Displacement_thickness_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_thickness.\n");
    return CV_OK;

}

int cmd_deformable_kcons(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_kcons.\n");

    // do work
    double value = 0;
    Displacement_kcons_ = 0.0;
    if (parseDouble(cmd, &value) == CV_ERROR) {
        return CV_ERROR;
    }
    Displacement_kcons_ = value;
    debugprint(stddbg,"  kcons = %lf\n",Displacement_kcons_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_kcons.\n");
    return CV_OK;

}

int cmd_deformable_pressure(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_pressure.\n");

    // do work
    double value = 0;
    Displacement_pressure_ = 0.0;
    if (parseDouble(cmd, &value) == CV_ERROR) {
        return CV_ERROR;
    }
    Displacement_pressure_ = value;
    debugprint(stddbg,"  pressure = %lf\n",Displacement_pressure_);

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_pressure.\n");
    return CV_OK;

}

int calcInitDisplacements(double Evw,double nuvw,
                          double thickness,double pressure,double kcons,
                          int use_direct_solve);

int cmd_deformable_solve(char *cmd,int use_direct_solve) {

  // enter
  debugprint(stddbg,"Entering cmd_deformable_solve.\n");

  debugprint(stddbg,"  Solver Params:\n");
  debugprint(stddbg,"    Use Direct Solver = %i\n",use_direct_solve);
  debugprint(stddbg,"    Evw               = %lf\n",Displacement_Evw_);
  debugprint(stddbg,"    nuvw              = %lf\n",Displacement_nuvw_);
  debugprint(stddbg,"    thickness         = %lf\n",Displacement_thickness_);
  debugprint(stddbg,"    kcons             = %lf\n",Displacement_kcons_);
  debugprint(stddbg,"    pressure          = %lf\n",Displacement_pressure_);

  calcInitDisplacements(Displacement_Evw_,
                        Displacement_nuvw_,
                        Displacement_thickness_,
                        Displacement_pressure_,
                        Displacement_kcons_,
                        use_direct_solve);

  int i,size,nsd,nshg;

  if (dispsoln_ == NULL) {

    nsd = 3;
    nshg = numNodes_;
    size = nsd*nshg;

    dispsoln_ = new double[size];

    // zeros everywhere
    for (i = 0; i < size; i++) {
      dispsoln_[i] = 0.0;
    }

  }

  // stick in displacements
  for (i = 0; i < DisplacementNumNodes_; i++) {
    int nid = DisplacementNodeMap_[i];
    dispsoln_[numNodes_*0+nid-1] = DisplacementSolution_[3*i+0];
    dispsoln_[numNodes_*1+nid-1] = DisplacementSolution_[3*i+1];
    dispsoln_[numNodes_*2+nid-1] = DisplacementSolution_[3*i+2];
  }

  // exit
  debugprint(stddbg,"Exiting cmd_deformable_solve.\n");
  return CV_OK;

}

#if(VER_VARWALL == 1)
int calcInitDisplacements_var_prop(double Evw,double nuvw,
                          double thickness,double pressure,double kcons,
                          int use_direct_solve);

int cmd_deformable_solve_var_prop(char *cmd,int use_direct_solve) {

  // enter
  debugprint(stddbg,"Entering cmd_deformable_solve_var_prop.\n");

  debugprint(stddbg,"  Solver Params:\n");
  debugprint(stddbg,"    Use Direct Solver = %i\n",use_direct_solve);
//  debugprint(stddbg,"    Evw               = %lf\n",Displacement_Evw_);
  debugprint(stddbg,"    nuvw              = %lf\n",Displacement_nuvw_);
//  debugprint(stddbg,"    thickness         = %lf\n",Displacement_thickness_);
  debugprint(stddbg,"    kcons             = %lf\n",Displacement_kcons_);
  debugprint(stddbg,"    pressure          = %lf\n",Displacement_pressure_);

  calcInitDisplacements_var_prop(Displacement_Evw_,
                        Displacement_nuvw_,
                        Displacement_thickness_,
                        Displacement_pressure_,
                        Displacement_kcons_,
                        use_direct_solve);

  int i,size,nsd,nshg;

  if (dispsoln_ == NULL) {

    nsd = 3;
    nshg = numNodes_;
    size = nsd*nshg;

    dispsoln_ = new double[size];

    // zeros everywhere
    for (i = 0; i < size; i++) {
      dispsoln_[i] = 0.0;
    }

  }

  // stick in displacements
  for (i = 0; i < DisplacementNumNodes_; i++) {
    int nid = DisplacementNodeMap_[i];
    dispsoln_[numNodes_*0+nid-1] = DisplacementSolution_[3*i+0];
    dispsoln_[numNodes_*1+nid-1] = DisplacementSolution_[3*i+1];
    dispsoln_[numNodes_*2+nid-1] = DisplacementSolution_[3*i+2];
  }

  // exit
  debugprint(stddbg,"Exiting cmd_deformable_solve_var_prop.\n");
  return CV_OK;

}

// SET THICKNESS BC
int cmd_set_thickness_BCs(char *cmd){
  return cmd_set_scalar_BCs(cmd);
}

// SET ELASTIC MODULUS BC
int cmd_set_Evw_BCs(char *cmd){
  return cmd_set_scalar_BCs(cmd);
}

int cmd_set_scalar_BCs(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_set_scalar_BCs.\n");

    // do work
    double value = 0;
    if (parseDouble2(cmd,&value) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"Setting surface thickness or Evw to [%lf] \n",value);

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    if (gBC_ == NULL) {
     gBC_ = new double [numNodes_];
     for (int i  = 0; i < numNodes_; i++) {
         gBC_[i] = -1.0;
     }
    }

    int eof = 0;
    int nodeId = 0;

    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
        if (sscanf(buffer_,"%i",&nodeId) != 1) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
        } else {
          // this should be a bit set instead of an int!!
          gBC_[nodeId - 1] = value;
        //  printf("set nodesID %d thickness %lf \n",nodeId,value);
        }
    }
    if (eof == 0) return CV_ERROR;
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting cmd_set_scalar_BCs.\n");
    return CV_OK;
}

int calcThicknessEvwDistribution(int Laplacetype);
//int openmptest();

// SOLVE LAPLACE EQUATION WITH THICKNESS
int cmd_Laplace_Thickness(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_Laplace_Thickness.\n");

    if (ThicknessSolution_ == NULL) {
      ThicknessSolution_ = new double [numNodes_];
    }

    int Laplacetype  = 0; // THICKNESS
    calcThicknessEvwDistribution(Laplacetype);

    // cleanup
    debugprint(stddbg,"Exiting cmd_Laplace_Thickness.\n");
    return CV_OK;
}

// SOLVE LAPLACE EQUATION WITH EVW
int cmd_Laplace_Evw(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_Laplace_Evw.\n");

    if (EvwSolution_ == NULL) {
      EvwSolution_ = new double [numNodes_];
    }

    int Laplacetype  = 1; // EVW
    calcThicknessEvwDistribution(Laplacetype);

    // cleanup
    debugprint(stddbg,"Exiting cmd_Laplace_Evw.\n");
    return CV_OK;
}

int cmd_set_Initial_Evw(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_set_Initial_Evw.\n");

    // do work
    double surfevw = 0;
    if (parseDouble2(cmd,&surfevw) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  Setting initial value to [%lf]\n",surfevw);

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    if (gBC_ == NULL) {
     gBC_ = new double [numNodes_];
     for (int i  = 0; i < numNodes_; i++) {
         gBC_[i] = -1.0;
     }
    }

    if (EvwSolution_ == NULL) {
     EvwSolution_ = new double [numNodes_];
     for (int i  = 0; i < numNodes_; i++) {
         EvwSolution_[i] = 0.0;
     }
    }

    int eof = 0;
    int nodeId = 0;

    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
        if (sscanf(buffer_,"%i",&nodeId) != 1) {
            fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
        } else {

          EvwSolution_[nodeId - 1] = surfevw;
        }
    }
    if (eof == 0) return CV_ERROR;
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting setNodesWithCode.\n");

    // cleanup
    debugprint(stddbg,"Exiting cmd_set_Initial_Evw.\n");
    return CV_OK;
}


int calcEvwDistribution();
int cmd_Transient_Laplace_Evw(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_Transient_Laplace_Evw.\n");

      if (EvwSolution_ == NULL) {
     EvwSolution_ = new double [numNodes_];
    }

  calcEvwDistribution();

   //  printf("ThicknessSolution_[100]=%lf \n",ThicknessSolution_[100]);
    // cleanup
    debugprint(stddbg,"Exiting cmd_Transient_Laplace_Evw.\n");
    return CV_OK;
}

//int cmd_append_varwallprop(char *cmd) {
//
//  // enter
//  debugprint(stddbg,"Entering cmd_append_varwallprop.\n");
//
//
//  int i,size,nsd,nshg;
//
//  if (wallpropsoln_ == NULL) {
//
//    nsd = 2;
//    nshg = numNodes_;
//    size = nsd*nshg;
//
//    wallpropsoln_ = new double[size];
//
//    // zeros everywhere
//    for (i = 0; i < size; i++) {
//      wallpropsoln_[i] = 0.0;
//    }
//
//  }
//
//  // stick in displacements
//  for (i = 0; i < DisplacementNumNodes_; i++) {
//    int nid = DisplacementNodeMap_[i];
//  //  wallpropsoln_[numNodes_*0+nid-1] = WallpropSolution_[2*i+0];
//  //  wallpropsoln_[numNodes_*1+nid-1] = WallpropSolution_[2*i+1];
//    //test! temporary
////    wallpropsoln_[numNodes_*0+nid-1] = Displacement_thickness_ ;
//    wallpropsoln_[numNodes_*0+nid-1] = ThicknessSolution_[nid-1];
//    wallpropsoln_[numNodes_*1+nid-1] = EvwSolution_[nid-1];
//
//  }
//
//
//      // enter
//    debugprint(stddbg,"Entering append_wallprops.\n");
//
//    char filename[MAXPATHLEN];
//
//    // do work
//    parseCmdStr(cmd,filename);
//
//    // some simple validity checks
//    if (numNodes_ == 0 || DisplacementNumNodes_ == 0 || wallpropsoln_ == NULL) {
//        fprintf(stderr,"ERROR:  Not all required info set!\n");
//        return CV_ERROR;
//    }
//
//    int filenum = -1;
//
//    if(filename[0]=='\0'){
//        openfile_ ("geombc.dat.1", "append", &filenum);
//    }else{
//        openfile_ (filename, "append", &filenum);
//    }
//
//    if (filenum < 0) {
//        fprintf(stderr,"ERROR:  could not open file (%s)\n",filename);
//        return CV_ERROR;
//    }
//
//
//    int lstep = 0;
//
//
//    // fprintf(stdout,"check nsd nshg numNodes %i %i %i \n",nsd,nshg,numNodes_ );
//    // append to file
//    int nitems = 3;
//
//    int iarray[3];
//    iarray[ 0 ] = nshg;
//    iarray[ 1 ] = nsd;
//    iarray[ 2 ] = lstep;
//
//    writeheader_( &filenum, "varwallprop ",
//                  ( void* )iarray, &nitems, &size,"double", oformat );
//
//
//    nitems = size;
//    writedatablock_( &filenum, "varwallprop ",
//                     ( void* )(wallpropsoln_), &nitems, "double", oformat );
//
//    closefile_( &filenum,"append");
//
//    delete wallpropsoln_;
//
//    // cleanup
//    debugprint(stddbg,"Exiting cmd_append_varwallprop.\n");
//    return CV_OK;
//
//}

int append_varwallprop_to_file(char *filename) {

  // enter
  debugprint(stddbg,"Entering append_varwallprop_to_file.\n");


  int i,size,nsd,nshg;

  // some simple validity checks
  if (numNodes_ == 0 || DisplacementNumNodes_ == 0 ) {
      fprintf(stderr,"ERROR:  Not all required info set!\n");
      return CV_ERROR;
  }

  if (wallpropsoln_ == NULL) {

    nsd = 2;
    nshg = numNodes_;
    size = nsd*nshg;

    wallpropsoln_ = new double[size];

    // zeros everywhere
    for (i = 0; i < size; i++) {
      wallpropsoln_[i] = 0.0;
    }

  }

  // stick in displacements
  for (i = 0; i < DisplacementNumNodes_; i++) {
    int nid = DisplacementNodeMap_[i];
  //  wallpropsoln_[numNodes_*0+nid-1] = WallpropSolution_[2*i+0];
  //  wallpropsoln_[numNodes_*1+nid-1] = WallpropSolution_[2*i+1];
    //test! temporary
//    wallpropsoln_[numNodes_*0+nid-1] = Displacement_thickness_ ;
    wallpropsoln_[numNodes_*0+nid-1] = ThicknessSolution_[nid-1];
    wallpropsoln_[numNodes_*1+nid-1] = EvwSolution_[nid-1];

  }

    int filenum = -1;
    openfile_ (filename, "append", &filenum);

    if (filenum < 0) {
        fprintf(stderr,"ERROR:  could not open file (%s)\n",filename);
        return CV_ERROR;
    }


    int lstep = 0;


    // fprintf(stdout,"check nsd nshg numNodes %i %i %i \n",nsd,nshg,numNodes_ );
    // append to file
    int nitems = 3;

    int iarray[3];
    iarray[ 0 ] = nshg;
    iarray[ 1 ] = nsd;
    iarray[ 2 ] = lstep;

    writeheader_( &filenum, "varwallprop ",
                  ( void* )iarray, &nitems, &size,"double", oformat );


    nitems = size;
    writedatablock_( &filenum, "varwallprop ",
                     ( void* )(wallpropsoln_), &nitems, "double", oformat );

    closefile_( &filenum,"append");

    delete wallpropsoln_;

    // cleanup
    debugprint(stddbg,"Exiting append_varwallprop_to_file.\n");
    return CV_OK;

}

int cmd_append_varwallprop(char *cmd) {

    if (ThicknessSolution_== NULL || EvwSolution_ == NULL ) {
         fprintf(stderr,"ERROR: Both ThicknessSolution_ and EvwSolution_ have not been computed.\n");
         return CV_ERROR;
     }

    char filename[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,filename);


    if(filename[0]=='\0'){
        return append_varwallprop_to_file("geombc.dat.1");
    }else{
        return append_varwallprop_to_file(filename);
    }

}
#endif

int cmd_deformable_direct_solve(char *cmd) {
    return cmd_deformable_solve(cmd,1);
}
int cmd_deformable_iterative_solve(char *cmd) {
    return cmd_deformable_solve(cmd,0);
}

#if(VER_VARWALL == 1)
int cmd_deformable_iterative_solve_var_prop(char *cmd) {
    return cmd_deformable_solve_var_prop(cmd,0);
}
#endif

int cmd_deformable_write_vtk_mesh(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_write_vtk_mesh.\n");

    char outfile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,outfile);

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    int i,j;

    FILE *fp = NULL;
    fp = fopen(outfile,"w");
    if (fp == NULL) {
        fprintf(stderr,"ERROR: could not open file (%s)\n",outfile);
        return CV_ERROR;
    }

    int numPts      = DisplacementNumNodes_;
    int numElem     = DisplacementNumElements_;
    int *map        = DisplacementNodeMap_;
    int *conn[3];
    conn[0] = DisplacementConn_[0];
    conn[1] = DisplacementConn_[1];
    conn[2] = DisplacementConn_[2];

    fprintf(fp,"# vtk DataFile Version 3.0\n");
    fprintf(fp,"vtk output\n");
    fprintf(fp,"ASCII\n");
    fprintf(fp,"DATASET POLYDATA\n");
    fprintf(fp,"POINTS %i double\n",numPts);
    for (i = 0; i < numPts; i++) {
        j = map[i];
        double x = nodes_[0*numNodes_+j-1];
        double y = nodes_[1*numNodes_+j-1];
        double z = nodes_[2*numNodes_+j-1];
        fprintf(fp,"%lf %lf %lf\n",x,y,z);
    }
    fprintf(fp,"POLYGONS %i %i\n",numElem,4*numElem);
    for (i = 0; i < numElem; i++) {
        fprintf(fp,"3 %i %i %i\n",conn[0][i],conn[1][i],conn[2][i]);
    }
    fprintf(fp,"CELL_DATA %i\n",numElem);
    fprintf(fp,"POINT_DATA %i\n",numPts);
    fprintf(fp,"SCALARS scalars float\n");
    fprintf(fp,"LOOKUP_TABLE default\n");
    for (i = 0; i < numPts; i++) {
        j = map[i];
        fprintf(fp,"%i\n",j);
    }
    fclose(fp);

    // cleanup
    debugprint(stddbg,"Exiting cmd_write_vtk_mesh.\n");

    return CV_OK;

}

#if(VER_VARWALL == 1)
int cmd_varwallprop_write_vtk(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_varwallprop_write_vtk.\n");

    char outfile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,outfile);

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    int i,j;
    double scalarval;

    FILE *fp = NULL;
    if(outfile[0]=='\0'){
        fp = fopen("varwallprop.vtk","w");
    }else{
        fp = fopen(outfile,"w");
    }

    if (fp == NULL) {
        fprintf(stderr,"ERROR: could not open file (%s)\n",outfile);
        return CV_ERROR;
    }

   if (ThicknessSolution_== NULL && EvwSolution_ == NULL ) {
        fprintf(stderr,"ERROR: Both ThicknessSolution_ and EvwSolution_ are not computed  (%s)\n",outfile);
        return CV_ERROR;
    }

    fprintf(fp,"# vtk DataFile Version 3.0\n");
    fprintf(fp,"vtk output\n");
    fprintf(fp,"ASCII\n");
    fprintf(fp,"DATASET UNSTRUCTURED_GRID\n");
    fprintf(fp,"POINTS %i float\n",numNodes_);
    for (i = 0; i < numNodes_; i++) {
        double x = nodes_[0*numNodes_+i];
        double y = nodes_[1*numNodes_+i];
        double z = nodes_[2*numNodes_+i];
        fprintf(fp,"%lf %lf %lf\n",x,y,z);
    }
    fprintf(fp,"CELLS %i %i\n",numElements_,5*numElements_);
    for (i = 0; i < numElements_; i++) {
    //global id -1 to get correct index
    fprintf(fp,"4 %i %i %i %i\n",elements_[numElements_*0+i]-1,elements_[numElements_*1+i]-1,
            elements_[numElements_*2+i]-1,elements_[numElements_*3+i]-1);

    }

    fprintf(fp,"CELL_TYPES %i \n",numElements_);
    for (i = 0; i < numElements_; i++) {
    fprintf(fp,"%i \n",10);
    }
    fprintf(fp,"POINT_DATA %i\n",numNodes_);


    if (ThicknessSolution_ != NULL) {
     fprintf(fp,"SCALARS thickness double\n");
     fprintf(fp,"LOOKUP_TABLE default\n");
    for (i = 0; i < numNodes_; i++) {
       scalarval=ThicknessSolution_[i];
      // printf("%lf\n",scalarval);
        fprintf(fp,"%lf\n",scalarval);
    }

    }

   if (EvwSolution_ != NULL) {
    fprintf(fp,"SCALARS Young_Mod double\n");
    fprintf(fp,"LOOKUP_TABLE default\n");
    for (i = 0; i < numNodes_; i++) {

       scalarval=EvwSolution_[i];
      // printf("%lf\n",scalarval);
        fprintf(fp,"%lf\n",scalarval);
    }

    }

    fclose(fp);

    // cleanup
    debugprint(stddbg,"Exiting cmd_varwallprop_write_vtk.\n");

    return CV_OK;

}
#endif

int cmd_deformable_write_feap(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_deformable_write_feap.\n");

    char outfile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,outfile);

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    int i,j;

    FILE *fp = NULL;
    fp = fopen(outfile,"w");
    if (fp == NULL) {
        fprintf(stderr,"ERROR: could not open file (%s)\n",outfile);
        return CV_ERROR;
    }

    int numPts      = DisplacementNumNodes_;
    int numElem     = DisplacementNumElements_;
    int *map        = DisplacementNodeMap_;
    int *conn[3];
    conn[0] = DisplacementConn_[0];
    conn[1] = DisplacementConn_[1];
    conn[2] = DisplacementConn_[2];

    fprintf(fp,"FEAP - output from supre\n");
    fprintf(fp,"%i %i %i %i %i\n",numPts,numElem,3,3,3);
    fprintf(fp," ! blank termination record\n");
    fprintf(fp,"COORdinates\n");
    for (i = 0; i < numPts; i++) {
        j = map[i];
        double x = nodes_[0*numNodes_+j-1];
        double y = nodes_[1*numNodes_+j-1];
        double z = nodes_[2*numNodes_+j-1];
        fprintf(fp,"%lf %lf %lf\n",x,y,z);
    }
    fprintf(fp," ! blank termination record\n");
    fprintf(fp,"ELEMent\n");
    for (i = 0; i < numElem; i++) {
        // feap node numbering starts at 1!!
        fprintf(fp,"%i %i %i\n",conn[0][i]+1,conn[1][i]+1,conn[2][i]+1);
    }
    fprintf(fp," ! blank termination record\n");

    // this is the same loop we use when solving for the initial conditions
    int numbcs = 0;

    for (i = 0; i < numPts; i++) {
      // should use bit test instead of an int
      if (iBC_[map[i]-1] == 56) {
        for (int idegree=0; idegree < 3; idegree++) {
            numbcs++;
        }
      }
    }

    fprintf(fp,"TDOFprescribed\n");
    fprintf(fp,"%i\n",numbcs);
    fprintf(fp,"DISPlacement\n");

    for (i = 0; i < numPts; i++) {
      // should use bit test instead of an int
      if (iBC_[map[i]-1] == 56) {
        for (int idegree=0; idegree < 3; idegree++) {
            fprintf(fp,"%i %i 0.0\n",i+1,idegree+1);
        }
      }
    }

    // final blank not needed I guess
    //fprintf(fp," ! blank termination record\n");

    fclose(fp);

    // cleanup
    debugprint(stddbg,"Exiting cmd_deformable_write_feap.\n");

    return CV_OK;

}

int cmd_write_restartdat(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_write_restart.\n");

    char infile[MAXPATHLEN];
    parseCmdStr(cmd,infile);

    // do work
    if(infile[0]=='\0'){
    	writeRESTARTDAT("restart.0.1");
    }else{
    	writeRESTARTDAT(infile);
    }

    // cleanup
    debugprint(stddbg,"Exiting cmd_write_restart.\n");
    return CV_OK;
}


int cmd_read_restart_solution(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_read_restart_solution.\n");

    char infile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,infile);

    int readSoln = 1;
    int readDisp = 0;
    int readAcc  = 0;
    readRESTARTDAT(infile,readSoln,readDisp,readAcc);

    // cleanup
    debugprint(stddbg,"Exiting cmd_read_restart_solution.\n");
    return CV_OK;
}


int cmd_read_restart_displacements(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_read_restart_displacements.\n");

    char infile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,infile);

    int readSoln = 0;
    int readDisp = 1;
    int readAcc  = 0;
    readRESTARTDAT(infile,readSoln,readDisp,readAcc);

    // cleanup
    debugprint(stddbg,"Exiting cmd_read_restart_displacements.\n");
    return CV_OK;
}


int cmd_read_restart_accelerations(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_read_accelerations.\n");

    char infile[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,infile);

    int readSoln = 0;
    int readDisp = 0;
    int readAcc  = 1;
    readRESTARTDAT(infile,readSoln,readDisp,readAcc);

    // cleanup
    debugprint(stddbg,"Exiting cmd_read_restart_accelerations.\n");
    return CV_OK;
}

#if(VER_VARWALL == 1)
int cmd_read_varwallprop(char *cmd) {

  char infile[MAXPATHLEN];

  // do work
  parseCmdStr(cmd,infile);

  debugprint(stddbg,"Entering cmd_read_varwallprop from %s.\n", infile);

  int intfromfile[50];
  int irstin;
  int ione=1, itwo=2, ithree=3,  iseven=7;
  int nshgl,numvar,lstep,iqsiz;

  openfile_(infile, "read", &irstin   );

  readheader_(&irstin,"varwallprop",(void*)intfromfile,&ithree,"double",oformat);
  nshgl=intfromfile[0];
  numvar=intfromfile[1];
  lstep=intfromfile[2];
  iqsiz=nshgl*numvar;
  wallpropsoln_ = new double [iqsiz];
  readdatablock_(&irstin,"varwallprop",(void*)wallpropsoln_, &iqsiz, "double", oformat);

  closefile_( &irstin, "read" );

  // fprintf(stdout,"readwallprop %lf %lf \n",wallpropsoln_[78],wallpropsoln_[1000]);
  // fprintf(stdout,"readwallprop %lf %lf \n",wallpropsoln_[200],wallpropsoln_[300]);
  // fprintf(stdout,"readwallprop %lf %lf \n",wallpropsoln_[4000],wallpropsoln_[500]);
  // fprintf(stdout,"readwallprop %lf %lf \n",wallpropsoln_[6000],wallpropsoln_[700]);
  // fprintf(stdout,"readwallprop %lf %lf \n",wallpropsoln_[8080],wallpropsoln_[9870]);
  // fprintf(stdout,"readwallprop %lf %lf \n",wallpropsoln_[9000],wallpropsoln_[1100]);
  // cleanup
  debugprint(stddbg,"Exiting cmd_read_varwallprop from %s.\n", infile);
  return CV_OK;
}

int cmd_read_restart_varwallprop(char *cmd){
	return cmd_read_varwallprop(cmd);
}

int cmd_read_geombc_varwallprop(char *cmd){
	return cmd_read_varwallprop(cmd);
}

#endif

int cmd_write_geombcdat(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_write_geombcdat.\n");

    char infile[MAXPATHLEN];
    parseCmdStr(cmd,infile);

    // do work
    if(infile[0]=='\0'){
    	writeGEOMBCDAT("geombc.dat.1");
    }else{
    	writeGEOMBCDAT(infile);
    }

    // cleanup
    debugprint(stddbg,"Exiting cmd_write_geombcdat.\n");
    return CV_OK;
}

int cmd_write_numstartdat(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_write_numstartdat.\n");

    char str[MAXSTRINGLENGTH];
    parseCmdStr(cmd,str);
    int start_time_step=0;

    if(str[0]!='\0'){
        if (parseNum(cmd, &start_time_step) == CV_ERROR) {
            return CV_ERROR;
        }
    }

    char filename[MAXPATHLEN];

    parseCmdStr2(cmd,filename);
    if(filename[0]=='\0'){
        strcpy(filename, "numstart.dat");
    }

    FILE* fp = NULL;
    fp = fopen(filename,"w");
    if(fp!=NULL){
        fprintf(fp,"%d\n",start_time_step);
        fclose(fp);
    }else{
        fprintf(stderr,"ERROR:  could not open numstart.dat!\n");
        return CV_ERROR;
    }

    debugprint(stddbg,"Exiting cmd_write_numstartdat.\n");
    return CV_OK;
}

int writeCommonHeader(int *filenum) {

    int magic_number = 362436;
    int* mptr = &magic_number;

    char wrtstr[255];
    int iarray[10];
    int size, nitems;

    /* before anything we put in the standard headers */

    writestring_( filenum, "# SimVascular Input File Version 2.0\n");
    writestring_( filenum, "# Byte Order Magic Number : 362436 \n");
    writestring_( filenum, "# Created by supre version 0.1\n");
    writestring_( filenum,"# CmdLineArgs : \n");

    // write out time
    time_t timenow = time ( &timenow);
    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"# %s\n", ctime( &timenow ));
    writestring_( filenum, wrtstr );

    int one=1;

    size = 1;
    nitems = 1;
    iarray[ 0 ] = 1;
    writeheader_( filenum, "byteorder magic number ",
                  (void*)iarray, &nitems, &size, "integer", oformat );

    writedatablock_( filenum, "byteorder magic number ",
                     (void*)mptr, &nitems, "integer", oformat );

    return CV_OK;

}


int cmd_append_displacements(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_append_displacements.\n");

    char filename[MAXPATHLEN];

    // do work
    parseCmdStr(cmd,filename);

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0 || dispsoln_ == NULL) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    int i;
    int filenum = -1;

    if(filename[0]=='\0'){
        openfile_ ("restart.0.1", "append", &filenum);
    }else{
        openfile_ (filename, "append", &filenum);
    }

    if (filenum < 0) {
        fprintf(stderr,"ERROR:  could not open file (%s)\n",filename);
        return CV_ERROR;
    }

    int nsd = 3;
    int lstep = 0;
    int nshg = numNodes_;
    int size = nsd*nshg;

    // append to file
    int nitems = 3;

    int iarray[3];
    iarray[ 0 ] = nshg;
    iarray[ 1 ] = nsd;
    iarray[ 2 ] = lstep;

    writeheader_( &filenum, "displacement ",
                  ( void* )iarray, &nitems, &size,"double", oformat );

    nitems = size;
    writedatablock_( &filenum, "displacement ",
                     ( void* )(dispsoln_), &nitems, "double", oformat );

    closefile_( &filenum,"append");

    delete dispsoln_;

    // cleanup
    debugprint(stddbg,"Exiting cmd_append_displacements.\n");
    return CV_OK;
}

int cmd_read_displacements(char *cmd) {

    // enter
    debugprint(stddbg,"Entering cmd_read_displacements.\n");

    // do work
    if (numNodes_ == 0) {
      fprintf(stderr,"ERROR:  Must specify number of nodes before you read in displacements!\n");
      return CV_ERROR;
    }

    if (parseFile(cmd) == CV_ERROR) {
        return CV_ERROR;
    }

    int i;

    if (dispsoln_ == NULL) {

      int nsd = 3;
      int nshg = numNodes_;
      int size = nsd*nshg;

      dispsoln_ = new double[size];

      // zeros everywhere
      for (i = 0; i < size; i++) {
        dispsoln_[i] = 0.0;
      }

    }

    double vx,vy,vz;
    int nodeId;
    int eof = 0;

    while (NWgetNextNonBlankLine(&eof) == CV_OK) {
      if (sscanf(buffer_,"%i %lf %lf %lf",&nodeId,&vx,&vy,&vz) != 4) {
        fprintf(stderr,"WARNING:  line not of correct format (%s)\n",buffer_);
        return CV_ERROR;
      }
      dispsoln_[numNodes_*0+nodeId-1] = vx;
      dispsoln_[numNodes_*1+nodeId-1] = vy;
      dispsoln_[numNodes_*2+nodeId-1] = vz;
    }
    NWcloseFile();

    // cleanup
    debugprint(stddbg,"Exiting cmd_read_displacements.\n");
    return CV_OK;

}


int readRESTARTDAT(char* filename, int readSoln, int readDisp, int readAcc) {

  int intfromfile[50];
  int irstin;
  int ione=1, itwo=2, ithree=3,  iseven=7;
  int nshgl,numvar,lstep,iqsiz;

  openfile_(filename, "read", &irstin   );

  // read solution if desired
  if (readSoln) {
    readheader_(&irstin,"solution",(void*)intfromfile,&ithree,"double",oformat);
    nshgl=intfromfile[0];
    numvar=intfromfile[1];
    lstep=intfromfile[2];
    iqsiz=nshgl*numvar;
    soln_ = new double [iqsiz];
    readdatablock_(&irstin,"solution",(void*)soln_, &iqsiz, "double", oformat);
  }

  // read displacements if desired
  if (readDisp) {
    readheader_(&irstin,"displacement",(void*)intfromfile,&ithree,"double",oformat);
    nshgl=intfromfile[0];
    numvar=intfromfile[1];
    lstep=intfromfile[2];
    iqsiz=nshgl*numvar;
    dispsoln_ = new double [iqsiz];
    readdatablock_(&irstin,"displacement",(void*)dispsoln_, &iqsiz, "double", oformat);
  }

  // read accelerations if desired
  if (readAcc) {
    readheader_(&irstin,"time derivative of solution",(void*)intfromfile,&ithree,"double",oformat);
    nshgl=intfromfile[0];
    numvar=intfromfile[1];
    lstep=intfromfile[2];
    iqsiz=nshgl*numvar;
    acc_ = new double [iqsiz];
    readdatablock_(&irstin,"time derivative of solution",(void*)acc_, &iqsiz, "double", oformat);
  }

  closefile_( &irstin, "read" );

  return CV_OK;

}


int writeRESTARTDAT(char* filename) {

    // some simple validity checks
    if (numNodes_ == 0 || numSolnVars_ == 0) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    int i;

    int filenum = -1;
    openfile_ (filename, "write", &filenum);
    if (filenum < 0) {
        fprintf(stderr,"ERROR:  could not open file (%s)\n",filename);
        return CV_ERROR;
    }

    // write out the commmon header
    writeCommonHeader(&filenum);

    char wrtstr[255];
    int iarray[10];
    int size, nitems;

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of modes : < 0 > %d \n", numNodes_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of variables : < 0 > %d \n", numSolnVars_);
    writestring_( &filenum, wrtstr );

    iarray[0] = numNodes_;
    iarray[1] = numSolnVars_;
    iarray[2] = 0;

    size = numNodes_*numSolnVars_;
    nitems = 3;
    writeheader_( &filenum, "solution ",
                  (void*)iarray, &nitems, &size,"double", oformat );

    //
    //  zero all initial fields if solution has not
    //  been read from file
    //

    if (soln_ == NULL) {

      soln_ = new double [size]();
      for (i = 0; i < numNodes_; i++) {
        soln_[i] = init_p_;
      }

      // use small non-zero initial velocity
      for (i = 0; i < numNodes_; i++) {
        soln_[1*numNodes_+i] = init_v_[0];
        soln_[2*numNodes_+i] = init_v_[1];
        soln_[3*numNodes_+i] = init_v_[2];
      }

    }

    writedatablock_( &filenum, "solution ",
                       (void*)soln_, &size,
                         "double", oformat );

    delete [] soln_;

    int nsd, lstep, nshg;

    if (dispsoln_ != NULL) {

      nsd = 3;
      lstep = 0;
      nshg = numNodes_;
      size = nsd*nshg;
      nitems = 3;

      iarray[ 0 ] = nshg;
      iarray[ 1 ] = nsd;
      iarray[ 2 ] = lstep;

      writeheader_( &filenum, "displacement ",
                  ( void* )iarray, &nitems, &size,"double", oformat );

      nitems = size;
      writedatablock_( &filenum, "displacement ",
                     ( void* )(dispsoln_), &nitems, "double", oformat );

      delete dispsoln_;
    }

    if (acc_ != NULL) {
      nsd = 4;
      lstep = 0;
      nshg = numNodes_;
      size = nsd*nshg;
      nitems = 3;

      iarray[ 0 ] = nshg;
      iarray[ 1 ] = nsd;
      iarray[ 2 ] = lstep;

      writeheader_( &filenum, "time derivative of solution ",
                  ( void* )iarray, &nitems, &size,"double", oformat );

      nitems = size;
      writedatablock_( &filenum, "time derivative of solution ",
                     ( void* )(acc_), &nitems, "double", oformat );

      delete acc_;
    }

    closefile_ (&filenum,"write");

    return CV_OK;

}

int writeGEOMBCDAT(char* filename) {

    // some simple validity checks
    if (numNodes_ == 0 || numElements_ == 0 ||
        numSolnVars_ == 0 || numMeshEdges_ == 0 ||
        numMeshFaces_ == 0 || numBoundaryFaces_ == 0 ||
        nodes_ == NULL || elements_ == NULL ||
        xadjSize_ == 0 || adjncySize_ == 0 ||
        iBCB_ == NULL || BCB_ == NULL || iBC_ == NULL) {
        fprintf(stderr,"ERROR:  Not all required info set!\n");
        return CV_ERROR;
    }

    int i;

    int filenum = -1;
    openfile_ (filename, "write", &filenum);
    if (filenum < 0) {
        fprintf(stderr,"ERROR:  could not open file (%s)\n",filename);
        return CV_ERROR;
    }

    // write out the commmon header
    writeCommonHeader(&filenum);

    char wrtstr[255];
    int iarray[10];
    int size, nitems;

    bzero((void*)wrtstr, 255 );
    sprintf(wrtstr,"number of processors : < 0 > 1 \n");
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of variables : < 0 > %d \n", numSolnVars_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of nodes : < 0 > %d \n", numNodes_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf( wrtstr,"number of nodes in the mesh : < 0 > %d \n",numNodes_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf( wrtstr,"number of edges in the mesh : < 0 > %d \n",numMeshEdges_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf( wrtstr,"number of faces in the mesh : < 0 > %d \n",numMeshFaces_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of modes : < 0 > %d \n", numNodes_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of shapefunctions solved on processor : < 0 > %d \n", numNodes_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of global modes : < 0 > %d \n", numNodes_);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of interior elements : < 0 > %d \n", numElements_);
    writestring_( &filenum, wrtstr );

    int numelb = numBoundaryFaces_;

    // count the number of non-zero entries for dirichlet
    int numEBC = 0;
    for (i = 0; i < numNodes_; i++) {
        if (iBC_[i] != 0) numEBC++;
    }

    int numpbc = numEBC;

    int nen = 4;
    int tmpblk = 1;
    int tmpblkb = 1;

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of boundary elements : < 0 > %d \n", numelb);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"maximum number of element nodes  : < 0 > %d \n", nen);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of interior tpblocks : < 0 > %d \n", tmpblk);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of boundary tpblocks : < 0 > %d \n", tmpblkb);
    writestring_( &filenum, wrtstr );

    bzero( (void*)wrtstr, 255 );
    sprintf(wrtstr,"number of nodes with Dirichlet BCs : < 0 > %d \n", numpbc);
    writestring_( &filenum, wrtstr );


    //
    //  write nodes
    //

    size = 3*numNodes_;
    nitems = 2 ;
    iarray[ 0 ] = numNodes_;
    iarray[ 1 ] = 3;

    writeheader_( &filenum, "co-ordinates ", (void*)iarray, &nitems, &size,
                  "double", oformat  );

    nitems = 3*numNodes_;
    writedatablock_( &filenum, "co-ordinates ", (void*)nodes_, &nitems,
                     "double", oformat );

    //
    // write elements
    //

    /* for interior each block */
    int bnen    = 4;                 /* nen of the block -- topology */
    int bpoly   = 1;                 /* polynomial order of the block */
    int nelblk  = numElements_;      /* numel of this block */
    int bnsh    = 4;                 /* nshape of this block */
    int bnshlb  = 3;
    int bnenbl  = 3;
    int blcsyst = 1;

    size = nelblk*bnsh;
    nitems = 7;
    iarray[ 0 ] = nelblk;
    iarray[ 1 ] = bnen;
    iarray[ 2 ] = bpoly;
    iarray[ 3 ] = bnsh;
    iarray[ 4 ] = bnshlb;
    iarray[ 5 ] = bnenbl;
    iarray[ 6 ] = blcsyst;

    writeheader_( &filenum, "connectivity interior linear tetrahedron ",
                  (void*)iarray, &nitems, &size,
                  "integer", oformat );

    nitems = nelblk*bnsh;
    writedatablock_( &filenum, "connectivity interior linear tetrahedron ",
                   (void*)elements_, &nitems,
                   "integer", oformat );

    //  ien to sms
    //

    size = nelblk;
    nitems = 1;
    iarray[ 0 ] = nelblk;
    writeheader_( &filenum, "ien to sms linear tetrahedron ",
                  (void*)iarray, &nitems, &size,"integer", oformat );

    nitems = nelblk;
    int* ien_sms = new int [numElements_];
    for (i = 0; i < numElements_ ; i++) {
        ien_sms[i] = i;
    }
    writedatablock_( &filenum, "ien to sms linear tetrahedron ",
                     (void*)ien_sms, &nitems,
                         "integer", oformat );
    delete ien_sms;

    //  boundary elements
    //

    // ??
    int numNBC = 6;

    iarray[ 0 ] = numBoundaryFaces_;
    iarray[ 1 ] = bnen;
    iarray[ 2 ] = bpoly;
    iarray[ 3 ] = bnsh;
    iarray[ 4 ] = bnshlb;
    iarray[ 5 ] = bnenbl;
    iarray[ 6 ] = blcsyst;
    iarray[ 7 ] = numNBC;

    size = numBoundaryFaces_*bnsh;
    nitems = 8;
    writeheader_( &filenum, "connectivity boundary linear tetrahedron ",
                      (void*)iarray, &nitems, &size,
                      "integer", oformat );

    int* ienb = new int [4*numBoundaryFaces_];
    for (i = 0; i < numBoundaryFaces_; i++) {
        ienb[0*numBoundaryFaces_+i] = boundaryElements_[0][i];
        ienb[1*numBoundaryFaces_+i] = boundaryElements_[1][i];
        ienb[2*numBoundaryFaces_+i] = boundaryElements_[2][i];
        ienb[3*numBoundaryFaces_+i] = boundaryElements_[3][i];
    }

    nitems = numBoundaryFaces_*bnsh;
    writedatablock_( &filenum, "connectivity boundary linear tetrahedron ",
                      (void*)ienb, &nitems,"integer", oformat );

    delete ienb;

    //
    // ienb to sms linear tetrahedron
    //

    iarray [ 0 ] = numBoundaryFaces_;
    nitems = 1;
    size = numBoundaryFaces_ ;

    writeheader_( &filenum, "ienb to sms linear tetrahedron ",
                      (void*)iarray, &nitems, &size,
                      "integer", oformat );

    writedatablock_( &filenum, "ienb to sms linear tetrahedron ",
                      (void*)boundaryElementsIds_, &numBoundaryFaces_,"integer", oformat );


    // nbc codes linear tetrahedron  : < 5132 > 2566 4 1 4 3 3 1 6

    iarray[ 0 ] = numBoundaryFaces_;
    iarray[ 1 ] = bnen;
    iarray[ 2 ] = bpoly;
    iarray[ 3 ] = bnsh;
    iarray[ 4 ] = bnshlb;
    iarray[ 5 ] = bnenbl;
    iarray[ 6 ] = blcsyst;
    iarray[ 7 ] = numNBC;
    nitems  = 8;
    size = numBoundaryFaces_*2;

    writeheader_( &filenum, "nbc codes linear tetrahedron " , (void*)iarray, &nitems, &size,
                      "integer", oformat );

    // ???
    // ???
    // ???
    //nitems = numBoundaryFaces_*bnsh;
    nitems  = numBoundaryFaces_*2;

    writedatablock_( &filenum, "nbc codes linear tetrahedron ", (void*)iBCB_, &nitems,
                         "integer", oformat );

    // nbc values linear tetrahedron  : < 15396 > 2566 4 1 4 3 3 1 6

    size = numBoundaryFaces_*6;
    nitems = 8;

    writeheader_( &filenum, "nbc values linear tetrahedron ", (void*)iarray, &nitems, &size,
                      "double", oformat );

    nitems = numBoundaryFaces_*6;
    writedatablock_( &filenum, "nbc values linear tetrahedron ", (void*)BCB_, &nitems,
                         "double", oformat );

    // bc mapping array  : < 3636 > 3636
    // bc codes array  : < 26 > 26

    int* iBC = new int [numEBC];
    int* iBCmap = new int[numNodes_];
    int count = 0;
    for (i = 0; i < numNodes_; i++) {
        iBCmap[i]=0;
        if (iBC_[i] != 0) {
            iBC[count] = iBC_[i];
            count++;
            iBCmap[i] = count;
        }
    }

    iarray [ 0 ] = numNodes_;
    nitems = 1;
    size = numNodes_;

    writeheader_( &filenum, "bc mapping array ",
                      (void*)iarray, &nitems, &size,
                      "integer", oformat );

    writedatablock_( &filenum, "bc mapping array ",
                      (void*)iBCmap, &numNodes_,"integer", oformat );

    delete [] iBCmap;

    iarray [ 0 ] = numEBC;
    nitems = 1;
    size = numEBC ;

    writeheader_( &filenum, "bc codes array ",
                      (void*)iarray, &nitems, &size,
                      "integer", oformat );

    writedatablock_( &filenum, "bc codes array ",
                      (void*)iBC, &numEBC,"integer", oformat );

    delete iBC;

    // boundary condition array : < 312 > 312

    int numVars = 0;
    size = numEBC*(numVars+12);
    nitems = 1;
    iarray[ 0 ] = numEBC*(numVars+12);
    writeheader_( &filenum , "boundary condition array ", (void *)iarray, &nitems,
                  &size, "double", oformat );

    nitems = numEBC*(numVars+12);

    double *BCf = new double [nitems];

    // this code creates only no-slip b.c.
    // for all nodes
    for (i = 0; i < nitems; i++) {
        BCf[i] = 0.0;
    }
    for (i = 0; i <numEBC; i++) {
        BCf[3*numEBC+i]=1.0;
    }

    writedatablock_( &filenum, "boundary condition array ", (void*)(BCf),
                     &nitems , "double", oformat );

    delete BCf;

    //
    // periodic masters array  : < 3636 > 3636
    //

    iarray [ 0 ] = numNodes_;
    size = numNodes_;
    nitems = 1;
    writeheader_( &filenum, "periodic masters array ",
                      (void*)iarray, &nitems, &size,
                      "integer", oformat );
    int* periodic = new int [numNodes_];
    for ( i = 0; i < numNodes_; i++) {
        periodic[i] = 0;
    }

    nitems  = numNodes_;
    writedatablock_( &filenum, "periodic masters array ",
                      (void*)periodic, &nitems,"integer", oformat );

    delete periodic;

    //
    // keyword xadj  : < 17609 > 17608 0
    // keyword adjncy  : < 67866 > 33933
    // keyword vwgt  : < 17608 > 17608
    //

    nitems = 2;
    iarray[0] = numElements_;
    int sspebc = 0;
    iarray[1] = sspebc;
    writeheader_( &filenum, "keyword xadj ", (void*)iarray, &nitems,
                  &xadjSize_, "integer", oformat );

    writedatablock_( &filenum, "keyword xadj ",
                     (void*)xadj_, &xadjSize_,"integer", oformat );

    nitems = 1;
    iarray[0] = numMeshFaces_ - numBoundaryFaces_;
    writeheader_( &filenum, "keyword adjncy ", (void*)iarray, &nitems,
                  &adjncySize_, "integer", oformat );

    writedatablock_( &filenum, "keyword adjncy ", (void*)adjncy_, &adjncySize_,
                     "integer", oformat );

    nitems = 1;
    iarray[0] = numElements_;
    writeheader_( &filenum, "keyword vwgt ", (void*)iarray, &nitems,
                  &numElements_, "integer", oformat );

    int* vwgt = new int [numElements_];
    for (i = 0; i < numElements_; i++) {
        vwgt[i] = 4;
    }

    writedatablock_( &filenum, "keyword vwgt ", (void*)vwgt, &numElements_,
                     "integer", oformat );

    delete vwgt;

    closefile_ (&filenum,"write");

#if(VER_VARWALL == 1)
    if (ThicknessSolution_!= NULL && EvwSolution_ != NULL ) {
        return append_varwallprop_to_file(filename);
    }
#endif

    return CV_OK;

}

int cmd_fluid_density(char *cmd) {

    debugprint(stddbg,"Entering cmd_fluid_density.\n");

    if (parseDouble(cmd, &rho_) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  Fluid Density = %lf\n",rho_);

    debugprint(stddbg,"Exiting cmd_fluid_density.\n");
    return CV_OK;
}

int cmd_fluid_viscosity(char *cmd) {

    debugprint(stddbg,"Entering cmd_fluid_viscosity.\n");

    if (parseDouble(cmd, &mu_) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  Fluid Viscosity = %lf\n",mu_);

    debugprint(stddbg,"Exiting cmd_fluid_viscosity.\n");
    return CV_OK;
}

int cmd_bct_analytical_shape(char *cmd){

    debugprint(stddbg,"Entering cmd_bct_analytical_shape.\n");

    char shapeType[MAXSTRINGLENGTH];
    parseCmdStr(cmd,shapeType);

    debugprint(stddbg,"111.\n");

    if(strcmp(shapeType,"womersley")==0){
        bctShape_=WOMERSLEY; debugprint(stddbg,"222.\n");
    }else if(strcmp(shapeType,"parabolic")==0){
        bctShape_=PARABOLIC; debugprint(stddbg,"333.\n");
    }else if(strcmp(shapeType,"plug")==0){
        bctShape_=PLUG; debugprint(stddbg,"444.\n");
    }else{
        fprintf(stderr,"ERROR: No such BCT Analytical Shape: (%s)\n",shapeType);
        return CV_ERROR;
    }

    debugprint(stddbg,"  BCT Analytical Shape is %s\n",shapeType);

    debugprint(stddbg,"Exiting cmd_bct_analytical_shape.\n");
    return CV_OK;
}

int cmd_bct_period(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_period.\n");

    if (parseDouble(cmd, &bctPeriod_) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  BCT Period = %lf\n",bctPeriod_);

    debugprint(stddbg,"Exiting cmd_bct_period.\n");
    return CV_OK;
}

int cmd_bct_point_number(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_point_number.\n");

    if (parseNum(cmd, &bctPointNum_) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  BCT point number in period: %i\n",bctPointNum_);

    debugprint(stddbg,"Exiting cmd_bct_point_number.\n");
    return CV_OK;

}

int cmd_bct_fourier_mode_number(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_fourier_mode_number.\n");

    if (parseNum(cmd, &bctModeNum_) == CV_ERROR) {
        return CV_ERROR;
    }

    debugprint(stddbg,"  BCT Fourier mode number: %i\n",bctModeNum_);

    debugprint(stddbg,"Exiting cmd_bct_fourier_mode_number.\n");
    return CV_OK;

}

//int cmd_bct_preserve_flow(char *cmd) {
//
//    debugprint(stddbg,"Entering cmd_bct_preserve_flow.\n");
//
//    bctPreserve_=1;
//
//    debugprint(stddbg,"  BCT preserves flow\n");
//
//    debugprint(stddbg,"Exiting cmd_bct_preserve_flow.\n");
//    return CV_OK;
//
//}

int cmd_bct_flip(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_flip.\n");

    bctFlip_=1;

    debugprint(stddbg,"  BCT normal flipped.\n");

    debugprint(stddbg,"Exiting cmd_bct_flip.\n");
    return CV_OK;

}

int cmd_bct_merge_on(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_merge_on.\n");

    bctMerge_=1;

    debugprint(stddbg,"  Will merge all BCT files to one.\n");

    debugprint(stddbg,"Exiting cmd_bct_merge_on.\n");
    return CV_OK;

}

int cmd_bct_write_dat(char *cmd) {

    debugprint(stddbg,"Entering cmd_bct_write_dat.\n");

    char filename[MAXPATHLEN];

    parseCmdStr(cmd,filename);

    FILE* fp = NULL;

    if(vbct.size()==1){
        bctMerge_=1;
    }

    if(bctMerge_==1){
        //char bct_name[]="bct.dat";

        if(filename[0]=='\0'){
            strcpy(filename, "bct.dat");
        }

        //fp=fopen(bct_name,"w");
        fp=fopen(filename,"w");
        if(fp==NULL){
            fprintf(stderr, "ERROR: could not open file (%s).\n", filename);
            return CV_ERROR;
        }
        fprintf(fp,"%d %d\n",bctNodeNumTotal_,bctPointNumMax_+1);

    }

    for(int n=0;n<vbct.size();n++){
        BCTData bct=vbct[n];
        vtkPolyData* pd=bct.pd;
        double* t=bct.t;
        int pointNum=bct.pointNum;
        int nodeNum=pd->GetNumberOfPoints();

        if(bctMerge_!=1){
            if(filename[0]=='\0'){
                strcpy(filename, "bct.dat");
            }

            char newfilename[MAXPATHLEN];

            strncpy (newfilename, filename, strlen(filename)-4 );
            newfilename[strlen(filename)-4]='\0';

            char rmdr[40];
            sprintf(rmdr,"%d.dat",n+1);
            strcat(newfilename,rmdr);

            fp=fopen(newfilename,"w");
            if(fp==NULL){
                fprintf(stderr, "ERROR: could not open file (%s).\n", filename);
                return CV_ERROR;
            }
            fprintf(fp,"%d %d\n",nodeNum,pointNum+1);
        }

        for(int i=0;i<nodeNum;i++){
            double pt[3];
            pd->GetPoint(i,pt);
            int nodeID=pd->GetPointData()->GetScalars()->GetTuple1(i);

//            fprintf(stdout,"working on node %d of %d.\n",i,nodeNum);

            fprintf(fp,"%e %e %e %d %d\n",pt[0],pt[1],pt[2],pointNum+1,nodeID);

            double vel[3];
            for(int j=0;j<pointNum;j++){

                bct.mapped_data[j]->GetTuple(i,vel);
                fprintf(fp,"%e %e %e %e\n",vel[0],vel[1],vel[2],t[j]);

            }
            bct.mapped_data[0]->GetTuple(i,vel);
            fprintf(fp,"%e %e %e %e\n",vel[0],vel[1],vel[2],t[pointNum]);

        }

        if(bctMerge_!=1){
            fclose(fp);
            fp=NULL;
        }

    }


    if(bctMerge_==1){
        fclose(fp);
    }

    debugprint(stddbg,"Exiting cmd_bct_write_dat.\n");
    return CV_OK;
}
