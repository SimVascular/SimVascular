/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
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

#include "SimVascular.h" 

#include "cv_globals.h"

#include "cv_discrete_utils.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

#include "tcl.h"
#include "cvVTK.h"
#include "vtkSTLWriter.h"
#include "vtkIntArray.h"

#include "MeshSim.h"
#include "SimDiscrete.h"

// these should probably match those found in gdscMeshSimObject.h
#define MY_MESHSIM_VERTEX_ORDERING 1
#define MY_MESHSIM_EDGE_ORDERING 1
#define MY_MESHSIM_FACE_ORDERING 1


/* -------------------- */
/*  DiscreteUtils_Init  */
/* -------------------- */

int DiscreteUtils_Init()
{

  // assume that code is build with MeshSimMeshObject
  // also, and that initialization is done in that layer

  try
    {
      SimDiscrete_start(0);
    }
  catch (...)
    {
      fprintf(stdout,"  ERROR in SimDiscrete_start.  Not available.\n");
      return CV_ERROR;
    }
  return CV_OK;
}

 
/* ---------------------------------- */
/*  DiscreteUtils_MakePoly3dSolidVtk  */
/* ---------------------------------- */

#ifdef USE_STL_TO_READ_DISCRETE_MODEL
int DiscreteUtils_MakePoly3dSolidVtk( vtkPolyData *inputPolyData, 
				      int faceCode, double angle,
				      pDiscreteModel *result) {

  
    // first need to write out an stl file to read in the
    // discrete model
    Tcl_Interp *myinterp = Tcl_CreateInterp();
    Tcl_Eval(myinterp, "file delete MakePoly3dSolid.stl");
    Tcl_DeleteInterp(myinterp);

  vtkSTLWriter *writer = vtkSTLWriter::New();
  writer->SetInput(inputPolyData);
  writer->SetFileName("MakePoly3dSolid.stl");
  writer->SetFileTypeToASCII();
  writer->Update();

  // the size we want our global size to be set to
  //double target_size = 0.01;

  pMesh mesh = M_new(0,0); // create the mesh
  
  M_importFromSTLFile(mesh,"MakePoly3dSolid.stl"); // import the stl file
  //M_importFromSTLBinaryFile(mesh, argv[1]); // if binary STL

  // create a DiscreteModel object from the mesh
  pDiscreteModel model = DM_createFromMesh(mesh,0);
 
  // get the initial extracted mesh and write it out
  //M_writeSMS(mesh,"init.sms", 2);

  //  DM_findPlanarFaces(model, 0, 0);
  DM_findEdgesByFaceNormalsDegrees(model,0);
  DM_eliminateDanglingEdges(model);
  DM_completeTopology(model); // finish up construction

  // memory leak!!!!!!
  mesh = DM_getMesh(model);
  
  *result = model;
 
  /*
  DM_save (model, "discrete-init.sdm");

  // set up the mesh to allow for modifications
  MS_setupMesh(mesh);

  MS_setGlobalMeshSize(mesh,1,target_size);
  
  // initialize the surface mesher
  MS_initSurfaceMesher(mesh,0);
  
  // run surface mesher in mode to manipulate existing surface mesh
  refDerefSurfMesh(mesh, 15, 90.01, 15, 165, 15, 15, 0, 0, 0);
  */

  return CV_OK;
}

#else

int discreteCalcJacDet(double xx[2][4], double r, double s, double *determinant) {

  /**************************************************************
   *  The code below was adapted from Finite Element Procedures *
   *  by K. Bathe, 1996, pages 482-3.                           *
   **************************************************************/

  *determinant = 0.0;

  int i,j,k;
  double rp,sp,rm,sm;
  double h[4],p[2][4],xj[2][2];
  double dum,det;

  rp = 1.0 + r;
  sp = 1.0 + s;
  rm = 1.0 - r;
  sm = 1.0 - s;
  
  // interpolation functions

  h[0] = 0.25 * rp * sp;
  h[1] = 0.25 * rm * sp;
  h[2] = 0.25 * rm * sm;
  h[3] = 0.25 * rp * sm;

  //
  // natural coordinate derviatives of the interpolation functions
  //

  // with respect to r

  p[0][0] = 0.25 * sp;
  p[0][1] = -p[0][0];
  p[0][2] = -0.25 * sm;
  p[0][3] = -p[0][2];

  // with respect to s

  p[1][0] = 0.25 * rp;
  p[1][1] = 0.25 * rm;
  p[1][2] = -p[1][1];
  p[1][3] = -p[1][0];

  // evaluate the jacobian at point (r,s)

  for (i = 0; i <= 1; i++) {
    for (j = 0; j <= 1; j++) {
      dum = 0;
      for (k = 0; k <= 3; k++) {
          dum = dum + p[i][k]*xx[j][k];
      }
      xj[i][j]=dum;
    }
  }

  // compute the determinant of the jacobian at point (r,s)
  det = xj[0][0]*xj[1][1] - xj[1][0]*xj[0][1];

  *determinant = det;

  if (det < 0.0) {
    //fprintf(stderr,"ERROR: Jacobian determinant negative! (%lf)\n"
    //     ,det);
    return CV_ERROR;
  }  

  return CV_OK;

}

int DiscreteUtils_MakePoly3dSolidVtk( vtkPolyData *inputPD, 
				      int facetCode, double angle,
				      pDiscreteModel *result) {
 
   double *coords = NULL;
   int numVerts = 0;
   vtkCellArray *pdPgns;
   int size, i;
   vtkIdType npts, *pts;
   int pos = 0;
   pProgress progress = NULL;

   double rad_angle = CV_PI / 180.0 * angle;

   VtkUtils_GetPoints(inputPD,&coords,&numVerts);

   int numElems = inputPD->GetNumberOfPolys();
   pdPgns = inputPD->GetPolys();
   size = pdPgns->GetNumberOfConnectivityEntries();

   int* elementData = new int [size];
   int* elementType = new int [numElems];

   pdPgns->InitTraversal();
   int currentCell = -1;
   int numNegDet = 0;
   int numSwitched = 0;

   while ( pdPgns->GetNextCell( npts, pts ) ) {

     currentCell++;
     if (npts != 3) {
       fprintf(stdout,"WARNING: ignoring poly (%i)\n", npts);
       continue;
     } else {
       // Hard coded value for a Triangle Face
       elementType[currentCell] = 5;
     }
     for (i = 0; i < npts; i++) {
       elementData[pos+i] = pts[i];
     }

     // check orientation?
     double x1[3],x2[3],x3[3];
     double v1[2],v2[2],v3[2];
     inputPD->GetPoints()->GetPoint(pts[0],x1);
     inputPD->GetPoints()->GetPoint(pts[1],x2);     
     inputPD->GetPoints()->GetPoint(pts[2],x3);
     vtkTriangle::ProjectTo2D(x1,x2,x3,v1,v2,v3);

     double xx[2][4];
     double det;

     // 2-point gaussian integration point locations
     // note that for 2 pt gauss integration the weights are 1.0
     double a[3];
     a[0] = 0.0;
     a[1] = 0.577350269189626;
     a[2] = -a[1];

     xx[0][0]=v1[0] ; xx[1][0]=v1[1] ;
     xx[0][1]=v2[0] ; xx[1][1]=v2[1] ;
     xx[0][2]=v3[0] ; xx[1][2]=v3[1] ;
     xx[0][3]=v3[0] ; xx[1][3]=v3[1] ;

     // calculate a sample determinant, and rearrange points 
     // if det. is negative.
     if (discreteCalcJacDet(xx,a[1],a[1],&det) == CV_ERROR) {
       numSwitched++;
       elementData[pos+0]=pts[2];
       elementData[pos+1]=pts[1];
       elementData[pos+2]=pts[0];
       inputPD->GetPoints()->GetPoint(pts[2],x1);
       inputPD->GetPoints()->GetPoint(pts[1],x2);     
       inputPD->GetPoints()->GetPoint(pts[0],x3);
       vtkTriangle::ProjectTo2D(x1,x2,x3,v1,v2,v3);
       xx[0][0]=v1[0] ; xx[1][0]=v1[1] ;
       xx[0][1]=v2[0] ; xx[1][1]=v2[1] ;
       xx[0][2]=v3[0] ; xx[1][2]=v3[1] ;
       xx[0][3]=v3[0] ; xx[1][3]=v3[1] ;
       if (discreteCalcJacDet(xx,a[1],a[1],&det) == CV_ERROR) {
         numNegDet++;
       }
     }

     pos += npts;

     if ( pos > size ) {
        printf("ERR [VtkUtils_GetAllPolys]: unexpected vtkCellArray result\n");
        delete [] elementData;
        delete [] elementType;
        delete [] coords;
        return CV_ERROR;
     }

   }

  fprintf(stdout,"num switched: %i\n",numSwitched);
  fprintf(stdout,"num neg det: %i\n",numNegDet);

  pMesh mesh = M_new(0,NULL); // create the mesh
  pVertex *vReturn = NULL;
  pEntity *eReturn = new pEntity [numElems];

  progress = Progress_new();
  if (M_importFromData(mesh,numVerts,coords,numElems,elementType,
                       elementData,vReturn,eReturn,progress) != 0) {
        delete [] elementData;
        delete [] elementType;
        delete [] coords;
	delete [] eReturn;
        Progress_delete(progress);
        return CV_ERROR;
  }
  Progress_delete(progress);

  delete [] elementData;
  delete [] elementType;
  delete [] coords;

  pPList intersectingEnts = PList_new();

  progress = Progress_new();
  if (MS_checkMeshIntersections(mesh,intersectingEnts,progress) != 0) {
    fprintf(stderr,"ERROR:  mesh self-intersects!\n");
    fprintf(stdout,"size of list: %i\n",PList_size(intersectingEnts));
       void *iter = 0; // must initialize to 0
       void *ent;
       int numVerts = 0;
       int numEdges = 0;
       int numFaces = 0;
       while(ent = PList_next(intersectingEnts,&iter)){
       // process each item in list
       int t = EN_type((pEntity)ent);
       if (t == 0) {
	 numVerts++;
       } else if (t == 1) {
         numEdges++;
       } else if (t == 2) {
         numFaces++;
       }
      }
       fprintf(stdout,"nums: %i %i %i\n",numVerts,numEdges,numFaces);
       Progress_delete(progress);
       return CV_ERROR;    
  }
  Progress_delete(progress);

  pDiscreteModel model;

  // now set the ids using the cell data
  vtkAbstractArray* scalars = NULL;
  scalars = inputPD->GetCellData()->GetAbstractArray("ModelFaceID");

  if (scalars == NULL) {

    fprintf(stdout,"NOTE:  no ModelFaceID data for discrete model\n");
    
    //return CV_ERROR;

    progress = Progress_new();
    model = DM_createFromMesh(mesh,0,progress);
    Progress_delete(progress);

    //  DM_findPlanarFaces(model, 0, 0);
    progress = Progress_new();
    DM_findEdgesByFaceNormalsDegrees(model,angle,progress);
    Progress_delete(progress);
    progress = Progress_new();
    DM_eliminateDanglingEdges(model,progress);
    Progress_delete(progress);

  } else {

    vtkIntArray* iscalars = dynamic_cast<vtkIntArray*>(scalars);

    if (iscalars->GetNumberOfTuples() != numElems) {

       fprintf(stdout,"ERROR:  not enough cell data (%i != %i).  Data ignored\n",
              iscalars->GetNumberOfTuples(),numElems);
       progress = Progress_new();
       model = DM_createFromMesh(mesh,0,progress);
       Progress_delete(progress);
       progress = Progress_new();
       DM_findEdgesByFaceNormalsDegrees(model,angle,progress);
       Progress_delete(progress);

    } else {
 
      fprintf(stdout,"NOTE:  Using ModelFaceID data to tag faces for discrete model\n");
 
      for (i = 0; i < numElems; i++) {
        int val = iscalars->GetTuple1(i);
        F_markAsBoundary(static_cast<pFace>(eReturn[i]),val);
      }
      progress = Progress_new();   
      model = DM_createFromMesh(mesh,0,progress);
      Progress_delete(progress);
    }
    progress = Progress_new();
    DM_eliminateDanglingEdges(model,progress);
    Progress_delete(progress);

  }

  delete [] eReturn;

  // create a DiscreteModel object from the mesh
  progress = Progress_new();
  DM_completeTopology(model,progress); // finish up construction
  Progress_delete(progress);

  // memory leak!!!!!!
  mesh = DM_getMesh(model);
  
  *result = model;
 
  return CV_OK;
}

#endif


/* -------------- */
/* GetVtkPolyData */
/* -------------- */

int DiscreteUtils_GetVtkPolyData( pDiscreteModel model, int useMaxDist, double max_dist, vtkPolyData **out )
{

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.

  vtkPolyData *mycvPolyData;

  int i,j;

  //pMesh mesh = M_new(0,0);
  //mesh = DM_getMesh(model);

  pMesh mesh = DM_getMeshCopy(model,0);

  pACase mcase = MS_newMeshCase(model);
  pModelItem mdomain = GM_domain(model);
 
  if (useMaxDist != 0) {
      // set up the mesh to allow for modifications
      //MS_setupMesh(mesh);

      //pMesh mesh = M_new(0,model);  

      double target_size = max_dist;
      MS_setMeshSize(mcase,mdomain,1,target_size,NULL);
  
      // initialize the surface mesher
      //MS_initSurfaceMesher(mcase,0);
      //pProcess sp = MS_newSurfaceMesher(mcase,mesh,0);
      //Process_execute(sp);
      //Process_delete(sp);

      // run surface mesher in mode to manipulate existing surface mesh
      ///////  MUST FIX 2013-11-05
      ///////     DM_modifySurfaceMesh(mesh, mcase, 15, 90.01, 15, 165, 15, 15, 0, 0, 1);
  }

  int num_verts = M_numVertices (mesh);
  fprintf(stdout,"num_verts: %i\n",num_verts);
  // only want the linear tets, surfaces, and nodes

  // vtk requires single precision
  vtkFloatingPointType p[3];

  // create a list of the nodes used in the linear elements

  // Add node numbers to the points
  // NOTE: node numbers start at 1
  VIter myViter = M_vertexIter(mesh);
  for (i = 0; i < num_verts; i++){
    P_setID (V_point (VIter_next(myViter)), i+1);
  }
  VIter_delete(myViter);

  vtkPoints* myPoints = vtkPoints::New();
  myPoints->SetNumberOfPoints(num_verts);
  myViter = M_vertexIter(mesh);
  for (i = 0; i < num_verts; i++) {
    pPoint point = V_point (VIter_next(myViter));
    int nodenumber = P_id (point);
    //fprintf(stdout,"nodenumber: %i\n",nodenumber);  
    double x = P_x (point);
    double y = P_y (point);
    double z = P_z (point);  
    p[0] = x; p[1] = y; p[2] = z;
    myPoints->InsertPoint(nodenumber-1,p);
  } // i 
  VIter_delete(myViter);

  mycvPolyData = vtkPolyData::New();
  mycvPolyData->Allocate(1000,1000);
  mycvPolyData->SetPoints(myPoints);

  // if a surface mesh, insert facets
  vtkIdList* PointIds = vtkIdList::New();
  PointIds->Allocate(10,10);  
 
  // insert the triangles

  int num_faces = M_numFaces (mesh);
  fprintf(stdout,"num_faces: %i\n",num_faces);

  FIter myFiter = M_faceIter(mesh);
  pFace myface; 
  while (myface = FIter_next(myFiter)) {
      pPList vert_list = F_vertices (myface,MY_MESHSIM_VERTEX_ORDERING);
      for (j = 0; j < PList_size (vert_list); j++) {
        int nodenum = P_id (V_point ((pVertex)PList_item (vert_list, j)));
        PointIds->InsertNextId(nodenum-1);
      }      
      mycvPolyData->InsertNextCell(VTK_POLYGON,PointIds);
      PList_delete(vert_list);  
      PointIds->Reset();
  }  
  FIter_delete(myFiter);

  PointIds->Delete();

  *out = mycvPolyData;

  return CV_OK;

}









