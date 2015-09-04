/*=========================================================================
 *
 * Copyright (c) 2014 The Regents of the University of California.
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
#include "cvMeshSimMeshObject.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"

#include "cv_sys_geom.h"

#include "vtkGeometryFilter.h"
#include "vtkCleanPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkPoints.h"
#include "vtkIdList.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#endif

// -----------
// cvMeshSimMeshObject
// -----------

cvMeshSimMeshObject::cvMeshSimMeshObject(Tcl_Interp *interp)
  : cvMeshObject()
{
  interp_ = interp;
  mesh = NULL;
  model = NULL;
  meshloaded_ = 0;
  loadedVolumeMesh_ = 0;
  quadElem_ = 0; 
  //nodemap_ = NULL;
  pts_ = NULL;

  meshFileName_[0] = '\0';
  solidFileName_[0] = '\0';

  ptrNodes_ = NULL;
  ptrEdges_ = NULL;
  ptrElements_ = NULL;
  ptrModelRegions_ = NULL;

  case_ = NULL;
  manager_ = AMAN_new();

#ifdef USE_DISCRETE_MODEL
  discreteModel_ = NULL;
#endif

  solidmodeling_kernel_ = SM_KT_PARASOLID;

  meshoptions_.surface = 0;
  meshoptions_.volume = 0;
  meshoptions_.surface_optimization = 1;
  meshoptions_.surface_smoothing = 3;
  meshoptions_.volume_optimization = 1;
  meshoptions_.volume_smoothing = 1;
  meshoptions_.gsize_type = 0;
  meshoptions_.gsize = 0.0;
  meshoptions_.gcurv_type = 0;
  meshoptions_.gcurv = 0.0;
  meshoptions_.gmincurv_type = 0;
  meshoptions_.gmincurv = 0.0;

#ifdef USE_MESHSIM_ADAPTOR
  errorIndicatorID = NULL;
  modes            = NULL;
  nodalhessianID   = NULL;
  nodalgradientID  = NULL;
  phasta_solution  = NULL;
#endif
}

// -----------
// cvMeshSimMeshObject
// -----------

cvMeshSimMeshObject::cvMeshSimMeshObject( const cvMeshSimMeshObject& sm )
  : cvMeshObject()
{

  // Copy( sm );   // relying on automatic upcast to cvMeshObject*
  
}


// ------------
// ~cvMeshSimMeshObject
// ------------

cvMeshSimMeshObject::~cvMeshSimMeshObject()
{

    //delete mesh;
    //delete model;
   delete regionID_;
   //if (nodemap_ != NULL) delete nodemap_;

   if (pts_ != NULL) delete pts_;

   if (ptrNodes_ != NULL) {
     VIter_delete(ptrNodes_);
     EIter_delete(ptrEdges_);
     RIter_delete(ptrElements_);
     GRIter_delete(ptrModelRegions_);
   }

   if (case_ != NULL) AttCase_unassociate(case_);
   if (manager_ != NULL) AMAN_release(manager_);
#ifdef USE_DISCRETE_MODEL
   if (discreteModel_ != NULL) {
     delete discreteModel_;
   }
#endif

   if (mesh != NULL) 
     M_release(mesh);
   if (model != NULL) GM_release(model);

}

// This methods are added so they aren't bundled into default constructor.

int cvMeshSimMeshObject::SetMeshFileName( const char* meshFileName )
{
  if (meshFileName != NULL)
    sprintf(meshFileName_, "%s", meshFileName);
  else
    meshFileName_[0] = '\0';

  return CV_OK;
}

int cvMeshSimMeshObject::SetSolidFileName( const char* solidFileName )
{
  if (solidFileName != NULL)
    sprintf(solidFileName_, "%s", solidFileName);
  else
    solidFileName_[0] = '\0';

  return CV_OK;
}

int cvMeshSimMeshObject::Logon( const char* logFileName )
{
  Sim_logOn( logFileName );
  return CV_OK;
}

int cvMeshSimMeshObject::Logoff()
{
  Sim_logOff();
  return CV_OK;
}

// -----
// Print
// -----

int cvMeshSimMeshObject::Print() 
{
   /* output the statistics */
  int num_verts = M_numVertices (mesh);
  int num_elems = M_numRegions (mesh);
  int nMeshEdges = M_numEdges (mesh);
  int nMeshFaces = M_numFaces (mesh);

  int nRegion = GM_numRegions (model);
  int nFace = GM_numFaces (model);
  int nEdge = GM_numEdges (model);
  int nVertex = GM_numVertices (model);

  // vertices are the nodes for the linear elements,
  // for quadratic elements we need to count the edges
  // as well.
  int num_nodes = num_verts;
  if (quadElem_ == 1) {
    num_nodes += nMeshEdges;
  }

  fprintf(stdout,"\nMESH STATISTICS:\n");
  fprintf(stdout,"  elements         = %i\n",num_elems);
  fprintf(stdout,"  nodes            = %i\n",num_nodes);
  fprintf(stdout,"  mesh edges       = %i\n",nMeshEdges);
  fprintf(stdout,"  mesh faces       = %i\n",nMeshFaces);
  fprintf(stdout,"\nMODEL STATISTICS:\n");
  fprintf(stdout,"  material regions = %i\n",nRegion);
  fprintf(stdout,"  edges            = %i\n",nEdge);
  fprintf(stdout,"  vertices         = %i\n\n",nVertex);

  for (int i=0; i < numModelRegions_ ; i++) {
    fprintf(stdout,"regionID_[%i]: %i\n",i,regionID_[i]);
  }

  char rtnstr[2048];
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_nodes %i",num_nodes);
  Tcl_AppendElement(interp_, rtnstr);
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_elements %i",num_elems);
  Tcl_AppendElement(interp_, rtnstr);
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_mesh_edges %i",nMeshEdges);
  Tcl_AppendElement(interp_, rtnstr);
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_mesh_faces %i",nMeshFaces);
  Tcl_AppendElement(interp_, rtnstr);

  return CV_OK;
}


// ----
// Copy
// ----

cvMeshObject *cvMeshSimMeshObject::Copy() const
{
  cvMeshSimMeshObject *result = new cvMeshSimMeshObject( *this );
  return result;
}


// -------------
// InitTraversal
// -------------

int cvMeshSimMeshObject::InitTraversal() {

  int i;

  /* set flag indicating mesh has been loaded for object */
  meshloaded_ = 1;
  // NOTE: for now assume volume mesh
  loadedVolumeMesh_ = 1;

  numNodes_ = M_numVertices (mesh);
  numLinearNodes_ = numNodes_; 
  numElements_ = M_numRegions (mesh);
  numModelRegions_ = GM_numRegions (model);

  ptrNodes_ = M_vertexIter(mesh);
  ptrEdges_ = M_edgeIter(mesh);
  ptrElements_ = M_regionIter(mesh);
  ptrModelRegions_ = GM_regionIter(model);

  regionID_ = new int[numModelRegions_];

  // create a lookup table for the material ids for the mesh regions
  GRIter myGRiter = GM_regionIter(model);
  initRegionTraversal();
  for (i=0; i < numModelRegions_;i++) {
    //fprintf(stdout,"region %i\n",i);
    regionID_[i] = GEN_tag((GEntity*)GRIter_next(myGRiter));
  }
  GRIter_delete(myGRiter);

  int num_verts = M_numVertices (mesh);
 
  // Add node numbers to the points
  // NOTE: node numbers start at 1
  VIter myViter = M_vertexIter(mesh);
  for (i = 0; i < num_verts; i++){
    //if (!(i % 1000)) fprintf(stdout,"add node numbers (%i)\n",i);
    P_setID (V_point (VIter_next(myViter)), i+1);
  }
  VIter_delete(myViter);

  // Since M_nextVertex and M_vertex don't appear to map
  // nicely, we create an explicit array to map between
  // them

  /* this is amazing slow, so we (unfortunately) store the
   * explicit nodes instead
  nodemap_ = new int[num_verts+1];
  for (i = 0; i < num_verts; i++) {
      if (!(i % 1000)) fprintf(stdout,"creating map (%i)\n",i);
      int nodenumber = P_id (V_point(M_vertex(mesh,i)));
      nodemap_ [nodenumber] = i;
  }
  */

  double xyz[3];
  pts_ = new double[(num_verts+1)*3];
  myViter = M_vertexIter(mesh);
  for (i = 0; i < num_verts; i++){
    //if (!(i % 1000)) fprintf(stdout,"add node numbers (%i)\n",i);
    V_coord (VIter_next(myViter), xyz);
    pts_[i] = xyz[0];
    pts_[num_verts+i] = xyz[1];
    pts_[2*num_verts+i] = xyz[2];
  }
  VIter_delete(myViter);

  // this code doesn't seem to work changing the numbers
  int eid = 1;
  // need to tag the element numbers
  initRegionTraversal();
  while (getNextRegion() == 1) {
    initElementTraversal();
    while (getNextElement() == 1) {
      //EN_setID(pCurrentMeshRegion_,eid);
      eid++;
    }
  }
  initRegionTraversal();

  return CV_OK;
  
}


// -----------
// Update
// -----------

int cvMeshSimMeshObject::Update() {

  int i;

  // load the solid model
  if (model == NULL) {
    if (LoadModel(solidFileName_) == CV_ERROR) {
      fprintf(stderr,"ERROR: could not load solid model!\n");
      return CV_ERROR;
    }
  }

  // load the mesh
  if (!meshloaded_) {
    if (LoadMesh(meshFileName_,"dummy") == CV_ERROR) {
      fprintf(stderr,"ERROR: could not mesh!\n");
      return CV_ERROR;
    }
  }

  return CV_OK;
  
}

int cvMeshSimMeshObject::GetNodeCoords(int node) {

  nodeID_ = 0;
  nodeX_ = 0; nodeY_ = 0; nodeZ_ = 0;

  // NOTE: node numbers start at 1
  int i;
  pPoint point;

  int num_verts = M_numVertices (mesh);
  int nMeshEdges = M_numEdges (mesh);
  double x,y,z;

  int nodenumber;
  char coordstr[255];

  if (node < 1) {
    fprintf(stderr,"Error: Node %i out of range.\n",node);
    return CV_ERROR;
  }

  if (node > (num_verts+nMeshEdges)) {
    fprintf(stderr,"Error: Node %i out of range.\n",node);
    return CV_ERROR;
  }

  if ((node > num_verts) && quadElem_ == 0) {
    fprintf(stderr,"Error: Node %i out of range.\n",node);
    return CV_ERROR;
  }

  // The following code does not work since the iterators
  // are unimplemented for the point object type. 
  //
  // search the point id's for the appropriate P_id
  //
  //void* temp = 0;
  //for (i = 0;i < (num_verts+nMeshEdges);i++) {
  //   point = M_nextPoint(mesh, &temp);
  //   nodenumber = P_id (point);  
  //   if (nodenumber == node) {
  //      x = P_x (point);
  //      y = P_y (point);
  //      z = P_z (point);  
  //      coordstr[0] = '\0';
  //      sprintf(coordstr,"%lf %lf %lf",x,y,z);
  //      Tcl_AppendElement(interp_, coordstr);
  //      return CV_OK;
  //   }
  //}
  //
  //return CV_ERROR;

  void* temp = 0;

  if (node <= num_verts) {
    nodeID_ = node;
    nodeX_ = pts_[node-1];
    nodeY_ = pts_[num_verts+node-1];
    nodeZ_ = pts_[2*num_verts+node-1];
    return CV_OK;
  } else {
    MEdge *edge;
    EIter myEiter = M_edgeIter(mesh);
    while (edge = EIter_next(myEiter)) { 
      if (E_numPoints(edge) != 1) {
         fprintf(stderr,"ERROR:  no interior point!\n");
         EIter_delete(myEiter);
         return CV_ERROR;
      }
      point = E_point(edge,0);
      nodenumber = P_id (point);  
      if (nodenumber == node) {
        nodeID_ = nodenumber;
        nodeX_ = P_x (point);
        nodeY_ = P_y (point);
        nodeZ_ = P_z (point);  
        //coordstr[0] = '\0';
        //sprintf(coordstr,"%lf %lf %lf",x,y,z);
        //Tcl_AppendElement(interp_, coordstr);
        EIter_delete(myEiter);
        return CV_OK;
      }
    }  // edge
    EIter_delete(myEiter);
  }

  return CV_ERROR;

}
 
 
cvPolyData* cvMeshSimMeshObject::GetPolyData() {

  cvPolyData* result =NULL;

  if(loadedVolumeMesh_ == 1) {
 
    cvUnstructuredGrid* ug = this->GetUnstructuredGrid();
    vtkUnstructuredGrid* vtkug = ug->GetVtkUnstructuredGrid();
     
    vtkGeometryFilter* surfFilt = vtkGeometryFilter::New();
    surfFilt->MergingOff();
    surfFilt->SetInputDataObject(vtkug);
    surfFilt->Update();
    vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
    cleaner->PointMergingOff();
    cleaner->PieceInvariantOff();
    cleaner->SetInputDataObject(surfFilt->GetOutput());
    cleaner->Update();

    result = new cvPolyData( cleaner->GetOutput() );

    cleaner->Delete();
    surfFilt->Delete();
    delete ug;

  } else {

    // recall the node numbers start at 1 in the P_id,
    // but in vtkPolyData file they start at 0.

    vtkPolyData *mypd = NULL;
    vtkPoints* pts = NULL;

    int i,j;
    void* temp = 0;

    int nshgtot = M_numVertices (mesh);
    int neltot = M_numRegions(mesh);

    pts = vtkPoints::New();
    pts->Allocate(nshgtot,1000);
    pts->SetNumberOfPoints(nshgtot);

    mypd = vtkPolyData::New();
    mypd->Allocate(nshgtot,1000);
    mypd->SetPoints(pts);

    vtkIntArray* gid = vtkIntArray::New();
    gid->SetNumberOfComponents(1);
    gid->Allocate(nshgtot,1000);
    gid->SetNumberOfTuples(nshgtot);
    gid->SetName("GlobalNodeID");
    mypd->GetPointData()->SetActiveScalars("GlobalNodeID");

    // create a list of the nodes

    VIter myViter = M_vertexIter(mesh);
    for (i = 0; i < nshgtot; i++) {
      pPoint point = V_point (VIter_next(myViter));
      int nodenumber = P_id (point);  
      double x = P_x (point);
      double y = P_y (point);
      double z = P_z (point);  
      pts->SetPoint(nodenumber-1,x,y,z);
      gid->SetTuple1(nodenumber-1,nodenumber);
    } // i 
    VIter_delete(myViter);

    mypd->SetPoints(pts);
    mypd->GetPointData()->AddArray(gid);

    pts->Delete();
    gid->Delete();

    // if a tet mesh, insert tets
    // if a surface mesh, insert facets
    vtkIdList* PointIds = vtkIdList::New();
    PointIds->Allocate(10,10);  
    temp  = 0;

    // insert the triangles

    int num_faces = M_numFaces (mesh);

    FIter myFiter = M_faceIter(mesh);
    pFace myface; 
    while (myface = FIter_next(myFiter)) {
      pPList vert_list = F_vertices (myface,MY_MESHSIM_VERTEX_ORDERING);
      for (j = 0; j < PList_size (vert_list); j++) {
        int nodenum = P_id (V_point ((pVertex)PList_item (vert_list, j)));
        PointIds->InsertNextId(nodenum-1);
      }      
      mypd->InsertNextCell(VTK_POLYGON,PointIds);
      PList_delete(vert_list);  
      PointIds->Reset();
    }  
    FIter_delete(myFiter);

    PointIds->Delete();

    result = new cvPolyData( mypd );
    mypd->Delete();  // cvPolyData uses ref-counting

  }

  return result;

}


//
//  GetUnstructuredGrid
//
 
cvUnstructuredGrid* cvMeshSimMeshObject::GetUnstructuredGrid() {

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.

  cvUnstructuredGrid *result = NULL; 

  vtkPoints* pts = NULL;
  vtkUnstructuredGrid* grid = NULL;
 
  int i,j;
  void* temp = 0;

  int nshgtot = M_numVertices (mesh);
  int neltot = M_numRegions(mesh);

  grid = vtkUnstructuredGrid::New();
  grid->Allocate(neltot,1000);

  pts = vtkPoints::New();
  pts->Allocate(nshgtot,1000);
  pts->SetNumberOfPoints(nshgtot);

  vtkIntArray* gid = vtkIntArray::New();
  gid->SetNumberOfComponents(1);
  gid->Allocate(nshgtot,1000);
  gid->SetNumberOfTuples(nshgtot);
  gid->SetName("GlobalNodeID");

  // only want the linear tets

  // create a list of the nodes used in the linear elements

  VIter myViter = M_vertexIter(mesh);
  for (i = 0; i < nshgtot; i++) {
    pPoint point = V_point (VIter_next(myViter));
    int nodenumber = P_id (point);  
    double x = P_x (point);
    double y = P_y (point);
    double z = P_z (point);  
    pts->SetPoint(nodenumber-1,x,y,z);
    gid->SetTuple1(nodenumber-1,nodenumber);
  } // i 
  VIter_delete(myViter);

  grid->SetPoints(pts);
  grid->GetPointData()->AddArray(gid);
  grid->GetPointData()->SetActiveScalars("GlobalNodeID");

  pts->Delete();
  gid->Delete();

  vtkIdList* ptids = vtkIdList::New();
  ptids->Allocate(10,10);
  ptids->Initialize();
  ptids->SetNumberOfIds(4);

  vtkIntArray* eid = vtkIntArray::New();
  eid->SetNumberOfComponents(1);
  eid->Allocate(neltot,1000);
  eid->Initialize();
  eid->SetName("GlobalElementID");

  vtkIntArray* rid = vtkIntArray::New();
  rid->SetNumberOfComponents(1);
  rid->Allocate(neltot,1000);
  rid->Initialize();
  rid->SetName("ModelRegionID");

  // only for linear tets
  //initRegionTraversal();
  //while (getNextRegion() == 1) {
  //  initElementTraversal();
  //  while (getNextElement() == 1) {
  //      ptids->SetId(0,connID_[0]-1);ptids->SetId(1,connID_[1]-1);
  //      ptids->SetId(2,connID_[2]-1);ptids->SetId(3,connID_[3]-1);
  //      grid->InsertNextCell(VTK_TETRA,ptids);
  //      eid->InsertNextTuple1(curElemID_);
  //      rid->InsertNextTuple1(curMdlRegID_);
  //  }
  //}
  
  pRegion myelement = NULL;
  RIter myRIter = M_regionIter(mesh);
  int curMdlRegID = 1;
  while ((myelement = RIter_next(myRIter)) != NULL) {
    // the elements are numbered from 1 to N.
    int curElemID = EN_id((pEntity)myelement)+1;
    pPList vert_list = R_vertices (myelement,MY_MESHSIM_VERTEX_ORDERING);
    int num_elem_verts = PList_size (vert_list);
    // must be linear
    if (num_elem_verts != 4) {
      exit(-1);
    }
    for (i = 0; i < num_elem_verts; i++) {
        pVertex vertex = (pVertex)PList_item (vert_list, i);
        // vtk nodes must start at zero
        ptids->SetId(i,P_id(V_point(vertex))-1);
    } // i
    PList_delete(vert_list);      
    grid->InsertNextCell(VTK_TETRA,ptids);
    eid->InsertNextTuple1(curElemID);
    rid->InsertNextTuple1(curMdlRegID);
  }

  ptids->Delete();

  grid->GetCellData()->AddArray(rid);
  grid->GetCellData()->SetScalars(eid);
  grid->GetCellData()->SetActiveScalars("GlobalElementID");
  grid->GetCellData()->CopyAllOn();

  eid->Delete();

  result = new cvUnstructuredGrid( grid );

  grid->Delete();  // UnstructuredGrid uses ref-counting

  return result;

}


//
//  getNextNode
//

int cvMeshSimMeshObject::getNextNode () {
 
  pVertex vertex;

  pPoint point;

  if (iterNodes_ >= numNodes_) return 0;
 
  if (iterNodes_ >= numLinearNodes_ && quadElem_ == 0) return 0;

  if (iterNodes_ < numLinearNodes_) {
    vertex = VIter_next(ptrNodes_);
    point = V_point (vertex);
  }

  if (iterNodes_ == numLinearNodes_) { 
    ptrNodes_=NULL;
  }

  if (iterNodes_ >= numLinearNodes_) {
    //For each edge - output the midpoint node  
    MEdge* edge = EIter_next(ptrEdges_);
  
    if (E_numPoints(edge) != 1) {
      fprintf(stderr,"ERROR:  no interior point!\n");
      return 0;
    }

    point = E_point(edge,0);
  }

  nodeX_ = P_x (point);
  nodeY_ = P_y (point);
  nodeZ_ = P_z (point);
  nodeID_ = P_id (point);

  iterNodes_++;

  return 1;

}  


//
//  getNextRegion
//

int cvMeshSimMeshObject::getNextRegion () {

  fprintf(stdout,"iterModelRegions_ %i\n",iterModelRegions_);
  if (iterModelRegions_ >= numModelRegions_) return 0;

  curMdlRegID_ = regionID_[iterModelRegions_++];
  fprintf(stdout,"curMdlRegID_ %i\n",curMdlRegID_);
  return 1;
}


//
//  getNextElement
//

int cvMeshSimMeshObject::getNextElement () {

  int i;

  while (iterElements_ < numElements_) {

    pRegion region = RIter_next(ptrElements_);
    pCurrentMeshRegion_ = region;

    // the elements are numbered from 1 to N.
    curElemID_ = EN_id((pEntity)pCurrentMeshRegion_)+1;

    iterElements_++;

    /*  only output element if it is in the region */
    if (GEN_tag((GEntity*)R_whatIn(region)) == curMdlRegID_) {

      // linear element stuff
      pPList vert_list = R_vertices (region,MY_MESHSIM_VERTEX_ORDERING);
      int num_elem_verts = PList_size (vert_list);
      for (i = 0; i < num_elem_verts; i++) {
        pVertex vertex = (pVertex)PList_item (vert_list, i);
        pPoint point = V_point (vertex);
        connID_[i] = P_id (point);
      } // i
      PList_delete(vert_list);      

      // quadratic element stuff
      if (quadElem_ == 1) {
  
        pPList edge_list = R_edges(region,MY_MESHSIM_VERTEX_ORDERING);
        int num_edges = PList_size (edge_list);

        for (i = 0; i < num_edges; i++) {
          pEdge xedge = (pEdge)PList_item (edge_list, i);
          if (E_numPoints(xedge) != 1) {
            fprintf(stderr,"ERROR:  no interior point!\n");
            return 0;
  	  }
          connID_[num_elem_verts+i] = P_id(E_point(xedge,0)); 
        } 
        PList_delete(edge_list);
      } // quadElem_

      return 1;
   
    } // if mytag

  } // end-while
 
  return 0;
  
}
 

//
//  getNeighborMdlRegIds
//

//  returns 1 if element is on the interface,
//  0 if it is in the interior

int cvMeshSimMeshObject::getNeighborMdlRegIds () {

  // figure out if the element is on a material interface

  // In case mesh is just a surface, then its on the interface
  if (numModelRegions_ == 0) {
          return 1;
  }

  pPList face_list = R_faces(pCurrentMeshRegion_,MY_MESHSIM_FACE_ORDERING);
  int num_faces = PList_size (face_list);

  if (num_faces != 4) {
      fprintf(stderr,"ERROR:  Invalid number of faces on element!\n");
      num_faces = 4;
  }

  int returnValue = 0;

  for (int i = 0;i < num_faces; i++) {

    pFace face = (pFace)PList_item (face_list, i);

    // Get the elements on both sides of the face
    pRegion reg0 = F_region(face,0);
    pRegion reg1 = F_region(face,1);

    // Get the material regions corresponding to each element
    int tag0;
    int tag1;
        
    if (reg0 != NULL) {tag0=GEN_tag((GEntity*)R_whatIn(reg0));} else {tag0 = -1;}
    if (reg1 != NULL) {tag1=GEN_tag((GEntity*)R_whatIn(reg1));} else {tag1 = -1;}

    // store the material on each side of an element face
    curElemNe_[i][0]=tag0;
    curElemNe_[i][1]=tag1;

    if (tag0 != tag1 || (tag0 == -1 && tag1 == -1)) {
        returnValue = 1;
    }

  }

  return returnValue;
}

void cvMeshSimMeshObject::initNodeTraversal() {iterNodes_ = 0;VIter_reset(ptrNodes_);EIter_reset(ptrEdges_);}
void cvMeshSimMeshObject::initElementTraversal() {iterElements_ = 0;RIter_reset(ptrElements_);}
void cvMeshSimMeshObject::initRegionTraversal() {iterModelRegions_ = 0;GRIter_reset(ptrModelRegions_);}

int cvMeshSimMeshObject::getIdentForFaceId(int orgfaceid, int *faceID) {

  *faceID = 0;

  if (solidmodeling_kernel_ == SM_KT_PARASOLID) {

#ifdef USE_PARASOLID
    if (PsdUtils_CheckIdentForFaceId(orgfaceid) != CV_OK) {
      fprintf(stderr,"ERROR: PsdUtils_CheckIdentForFaceId(%i)\n",orgfaceid);
      fflush(stderr);
      return CV_ERROR;
    }
    pGEntity msentity = NULL;
    msentity = GEN_getGEntityFromParasolidEntity(model,(PK_ENTITY_t)orgfaceid);
    if (msentity == NULL) {
      fprintf(stderr,"ERROR: GEN_getGEntityFromParasolidEntity (entity: %i)\n",orgfaceid);
      fflush(stderr);
      return CV_ERROR;
    }
    *faceID =GEN_tag(msentity);
    if (*faceID == 0) {
      fprintf(stderr,"ERROR: could not find GEN_tag (entity: %i)\n",msentity);
      fflush(stderr);
      return CV_ERROR;
    }
#else
    return CV_ERROR;
#endif

  } else {

    if (solidmodeling_kernel_ != SM_KT_DISCRETE) {
     return CV_ERROR;
    }
    *faceID = orgfaceid;

  }

  return CV_OK;

}

int cvMeshSimMeshObject::WriteMetisAdjacency(char *filename) {

  if (filename == NULL) {
        return CV_ERROR;
  }

  // open the output file
  if (openOutputFile(filename) != CV_OK) return CV_ERROR;

    pRegion other;
    pRegion region;
    pPList faces;

    int nIfaces=0;  
    int iel,i;

    int  *xadj, *adjncy;

    int numel=M_numRegions(mesh);
    RIter rIter = M_regionIter(mesh);

    /* counting interior faces */

    FIter fIter = M_faceIter(mesh);
    pFace face;
      
    while (face = FIter_next(fIter)) {
        pPList regions = F_regions(face);
        if(PList_size(regions)==2) nIfaces++;
        PList_delete(regions);
    }
    FIter_reset(fIter);

    /* Allocating the necessary memory */

    xadj = new int [numel+1];
    adjncy = new int [2*nIfaces];

    int Sregion=0;
    iel =0;
    int adj=0;
    RIter_reset(rIter);
    xadj[0] = 0;
    
    while(region=RIter_next(rIter)){
        faces = R_faces(region,MY_MESHSIM_FACE_ORDERING);
        for (i =0; i<PList_size(faces);i++){
            face = (pFace)PList_item(faces,i);
            pPList fregions = F_regions(face);
            if (PList_size(fregions) == 2) {
                other = (pRegion)PList_item(fregions,0);
                if (other == region) other = (pRegion)PList_item(fregions,1);
                adjncy[adj++]=EN_id((pEntity)other);
            }
            PList_delete(fregions);
        }
        xadj[++iel]=adj;
        PList_delete(faces);
    }

    gzprintf(fp_,"xadj: %i\n",numel+1);
    gzprintf(fp_,"adjncy: %i\n",2*nIfaces);
    for (i = 0; i < numel+1; i++) {
        gzprintf(fp_,"%i\n",xadj[i]);
    }
    for (i = 0; i < 2*nIfaces; i++) {
        gzprintf(fp_,"%i\n",adjncy[i]);
    }
    
    delete xadj;
    delete adjncy;

    return closeOutputFile();

}

int cvMeshSimMeshObject::LoadModel(char *filename) {

  if (filename == NULL) {
    return CV_ERROR;
  }

  // must load model before mesh!
  if (mesh != NULL) {
    return CV_ERROR;
  }

  fprintf(stderr,"Solid Kernel: %s\n",SolidModel_KernelT_EnumToStr(solidmodeling_kernel_));
  if (solidmodeling_kernel_ == SM_KT_PARASOLID) {

#ifdef USE_PARASOLID
    PK_PART_t firstPart;
    int isAssembly;

    if (PsdUtils_ReadNative(filename, NULL, &firstPart, &isAssembly) != CV_OK) {
      return CV_ERROR;
    }
    part_ = firstPart;

    pProgress progress = Progress_new();

#ifndef CREATE_MESHSIM_SOLID_FROM_FILE

    fprintf(stdout,"note: creating model from SV part.\n");
    fflush(stdout);
    
    pParasolidNativeModel paramodel_ = NULL;
    paramodel_ = ParasolidNM_createFromPart(firstPart);
    if (paramodel_ == NULL) {
      fprintf(stderr,"ERROR: Problem from ParasolidNM_createFromPart.\n");
      fflush(stderr);
      return CV_ERROR;
    } 
    if (!isAssembly) {

      model = GM_createFromNativeModel(paramodel_,progress);

    } else {

      pGAModel pGAM = NULL;
      pGAM = GAM_createFromNativeModel(paramodel_,progress);
      if (pGAM == NULL) {
        fprintf(stderr,"ERROR: Problem from GM_createFromNativeModel.\n");
        fflush(stderr);
        return CV_ERROR;
      }
      fprintf(stdout,"GAM_numAssemblies(%i)\n",GAM_numAssemblies(pGAM));
      fprintf(stdout,"GAM_numParts(%i)\n",GAM_numParts(pGAM));
      fflush(stdout);
      // what is a connector?
      pMConnector connector = MC_new();
      model = GM_createFromAssemblyModel (pGAM, connector, progress);

    }

    if (model == NULL) {
      fprintf(stderr,"ERROR: Problem from GM_createFromAssemblyModel.\n");
      fflush(stderr);
      return CV_ERROR;
    } 
    // Should we release the model here or not?????
    //NM_release(paramodel_);

#else

   fprintf(stdout,"note: loading assembly model from file.\n");
   fflush(stdout);
   char fname[1024];
    fname[0] = '\0';
    sprintf(fname, "%s", filename);
    
    // must remove file extension for frustrum
    if (strlen(filename) > 8) {
      int loc = strlen(filename)-8;
      fprintf(stdout,"extension: %s\n",&filename[loc]);
      if (!strncmp(&filename[loc],".xmt_txt",8)) {
          fname[loc] = '\0';
      }
    }

    char fnamesmd[1024];
    fnamesmd[0] = '\0';
    sprintf(fnamesmd,"%s%s",fname,".smd");
    fprintf(stdout,"looking for gmodel (%s).\n",fnamesmd);
    pNativeModel simmetrix_native_model = NULL;
    GM_load(fnamesmd,simmetrix_native_model,progress);
    if (simmetrix_native_model == NULL) {
      fprintf(stderr,"ERROR: Problem from GM_load.\n");
      return CV_ERROR;
    }
    model = NULL;
    model = GM_createFromNativeModel(simmetrix_native_model,progress);
    if (model == NULL) {
      fprintf(stderr,"ERROR: Problem from GM_createFromNativeModel.\n");
      return CV_ERROR;
    }

    /*
    int fileFormat = 0;
    pParasolidNativeModel paramodel_ = ParasolidNM_createFromFile(fname,fileFormat);
    pGAModel pGAM = GAM_createFromNativeModel(paramodel_,progress);
   // what is a connector?  pass in NULL for now..
    model = GM_createFromAssemblyModel (pGAM, NULL, progress); 
    NM_release(paramodel_);
    */

#endif

    Progress_delete(progress);
    fprintf(stdout,"attach case.\n");
    fflush(stdout);
    case_ = NULL;
    case_ = AMAN_newCase(manager_,"case name", "info type", model);
    if (case_ == NULL) {
      fprintf(stderr,"ERROR: Problem from AMAN_newCase.\n");
      fflush(stderr);
      return CV_ERROR;
    }
    solidFileName_[0] = '\0';
    sprintf(solidFileName_,"%s",filename);
    return CV_OK;
#else
    return CV_ERROR;
#endif

  } else if (solidmodeling_kernel_ == SM_KT_DISCRETE) {

#ifdef USE_DISCRETE_MODEL
    discreteModel_ = new cvMeshSimDiscreteSolidModel();

    if (discreteModel_->ReadNative(filename) != CV_OK) {
      return CV_ERROR;
    }

    model = discreteModel_->geom_;

    case_ = AMAN_newCase(manager_,"case name", "info type", model);

    solidFileName_[0] = '\0';
    sprintf(solidFileName_,"%s",filename);
#else
    return CV_ERROR;
#endif

  }

  return CV_OK;

}


int cvMeshSimMeshObject::LoadMesh(char *filename,char *surfilename) {

  if (filename == NULL) {
    return CV_ERROR;
  }

  // must load model before mesh!
  if (model == NULL) {
    return CV_ERROR;
  }

  // create an empty mesh object if it doesn't already exist
  if (mesh == NULL) {
    NewMesh();
  } 

  pProgress progress = Progress_new();

  try {
    mesh = M_load(filename, model,progress);
  } catch (pSimError err) {
    fprintf(stderr,"Simmetrix error caught:\n");
    fprintf(stderr,"  Error code: %d\n",SimError_code(err));
    fprintf(stderr,"  Error string: %s\n",SimError_toString(err));
    Progress_delete(progress);
    return CV_ERROR;
  } catch (...) {
    fprintf(stderr,"Unhandled exception caught\n");
    Progress_delete(progress);
    return CV_ERROR;
  }
  
  Progress_delete(progress);

  meshFileName_[0] = '\0';
  sprintf(meshFileName_,"%s",filename);

  InitTraversal();

  return CV_OK;

}

int cvMeshSimMeshObject::NewMesh() {

  // must load model before mesh!
  if (model == NULL) {
    return CV_ERROR;
  }
  // cant overwrite mesh
  if (mesh != NULL) {
    return CV_ERROR;
  }

  // by default create a full mesh
  int reptype = 0;

  mesh = M_new(reptype, model);
  case_ = AMAN_newCase(manager_,"case name", "info type", model);
  // need to prevent update method from trying to
  // read a mesh file if you are creating one!
  meshloaded_ = 1;
  loadedVolumeMesh_ = 1;
  return CV_OK;

}

int cvMeshSimMeshObject::MapIDtoPID(int id, pGEntity *pid) {

  (*pid) = NULL;
  if (solidmodeling_kernel_ == SM_KT_PARASOLID) {

#ifdef USE_PARASOLID
    PK_ENTITY_t entity = PK_ENTITY_null;
    if (PsdUtils_GetIdent(part_,id,&entity) != CV_OK) {
      return CV_ERROR;
    }
    
    (*pid) = GEN_getGEntityFromParasolidEntity(model,(PK_ENTITY_t)entity);
    // assume indentifier numbering corresponds to simmetrix numbering
#else
    return CV_ERROR;
#endif

  } else if (solidmodeling_kernel_ == SM_KT_DISCRETE) {
 
    // assume entity is face!
    (*pid) = GM_entityByTag(model,2,id);
    fprintf(stdout,"pid: %p  id: %i\n",pid,id);

  } else {
 
    return CV_ERROR;

  }

  return CV_OK;

}

// --------------------
//  SetMeshOptions
// --------------------
/** 
 * @brief Function to set the options for meshing. Store temporarily in 
 * meshoptions_ object until the mesh is run
 * @param *flag char containing the flag to set
 * @param value if the flag requires a value, this double contains that 
 * value to be set
 * @return *result: CV_ERROR if the mesh doesn't exist. New Mesh must be 
 * called before the options can be set
 */

int cvMeshSimMeshObject::SetMeshOptions(char *flags,int numValues, double *values) {
  // must have created mesh
  if (mesh == NULL) {
    return CV_ERROR;
  }

	fprintf(stderr,"Flag: %s\n  NumVals:  %d\n  Val:  %.2f\n",flags,numValues,values[0]);
  if (!strncmp(flags,"SurfaceMeshFlag",15)) {    //Surface flag, on or off
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.surface=(int)values[0];
  }
  else if (!strncmp(flags,"VolumeMeshFlag",14)) {    //Volume flag, on or off
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.volume=(int)values[0];
  }
   else if (!strncmp(flags,"GlobalEdgeSize",14)) {    //Global edge size, type, size
      if (numValues < 2)
      {
	fprintf(stderr,"Must give type (absolute,relative) and edge size\n");
	return CV_ERROR;
      }
      meshoptions_.gsize_type=values[0];
      meshoptions_.gsize=values[1];
      // old api 5.4: MS_setGlobalMeshSize(mesh,type,gsize);
      MS_setMeshSize(case_,GM_domain(model),values[0],values[1],NULL);
  }
  else if (!strncmp(flags,"LocalEdgeSize",13)) {    //Local edge size, surface id, type, size
      if (numValues < 3)
      {
	fprintf(stderr,"Must give face id, type (absolute,relative) and edge size\n");
	return CV_ERROR;
      }
      pGEntity pid = NULL;
      if (MapIDtoPID(values[0],&pid) == CV_ERROR) {
	return CV_ERROR;
      }
      MS_setMeshSize(case_,pid,(int)values[1],values[2],NULL);
  }
  else if (!strncmp(flags,"GlobalCurvature",15)) {  //Global Curv, type, gcurv value
      if (numValues < 2)
      {
	fprintf(stderr,"Must give type (absolute,relative) and curvature\n");
	return CV_ERROR;
      }
      meshoptions_.gcurv_type=values[0];
      meshoptions_.gcurv=values[1];
      MS_setMeshSize(case_,GM_domain(model),values[0],values[1],NULL);
  }
  else if(!strncmp(flags,"LocalCurvature",14)) {  //Local Curv, surface id, type, gcurv value
      if (numValues < 3)
      {
	fprintf(stderr,"Must give face id, type (absolute,relative) and curvature\n");
	return CV_ERROR;
      }
      pGEntity pid = NULL;
      if (MapIDtoPID(values[0],&pid) == CV_ERROR) {
	return CV_ERROR;
      }
      MS_setMeshCurv(case_,pid,(int)values[1],values[2]);
  }
  else if(!strncmp(flags,"GlobalCurvatureMin",18)) {  //Global Curv Min, type, gcurv min value
      if (numValues < 2)
      {
	fprintf(stderr,"Must give type (absolute,relative) and curvature min\n");
	return CV_ERROR;
      }
      meshoptions_.gmincurv_type=values[0];
      meshoptions_.gmincurv=values[1];
      MS_setMinCurvSize(case_,GM_domain(model),values[0],values[1]);
  }
  else if(!strncmp(flags,"LocalCurvatureMin",17)) {  //Local Curv Min, surface id, type, gcurv min value
      if (numValues < 3)
      {
	fprintf(stderr,"Must give face id, type (absolute,relative) and curvature min\n");
	return CV_ERROR;
      }
      pGEntity pid = NULL;
      if (MapIDtoPID(values[0],&pid) == CV_ERROR) {
	return CV_ERROR;
      }
      MS_setMinCurvSize(case_,pid,(int)values[1],values[2]);
  }
  else if(!strncmp(flags,"SurfaceOptimization",19)) {  //Set Surface Optimization
      if (numValues < 1)
      {
	fprintf(stderr,"Must give optimization level value\n");
	return CV_ERROR;
      }
      meshoptions_.surface_optimization = values[0];
  }
  else if(!strncmp(flags,"VolumeOptimization",18)) {  //Set Volume Optimization
      if (numValues < 1)
      {
	fprintf(stderr,"Must give optimization level value\n");
	return CV_ERROR;
      }
      meshoptions_.volume_optimization = values[0];
  }
  else if(!strncmp(flags,"SurfaceSmoothing",19)) {  //Set Surface Optimization
      if (numValues < 1)
      {
	fprintf(stderr,"Must give optimization level value\n");
	return CV_ERROR;
      }
      meshoptions_.surface_smoothing = values[0];
  }
  else if(!strncmp(flags,"VolumeSmoothing",18)) {  //Set Volume Optimization
      if (numValues < 1)
      {
	fprintf(stderr,"Must give optimization level value\n");
	return CV_ERROR;
      }
      meshoptions_.volume_smoothing = values[0];
  }
  else {
      fprintf(stderr,"%s: flag is not recognized\n",flags);
  }

  return CV_OK;
}

int cvMeshSimMeshObject::GenerateMesh() {
  // must have created mesh
  if (mesh == NULL) {
    return CV_ERROR;
  }

  try {

    // create the meshing processes and run

    pProgress progress = Progress_new();

    // create surface mesh
    pSurfaceMesher surfaceMesher = SurfaceMesher_new(case_,mesh);

    SurfaceMesher_setOptimization(surfaceMesher,meshoptions_.surface_optimization);
    SurfaceMesher_setSmoothing(surfaceMesher,meshoptions_.surface_smoothing);

    // create it
    SurfaceMesher_execute(surfaceMesher,progress);

    Progress_delete(progress);

    progress = Progress_new();

    pVolumeMesher volumeMesher = VolumeMesher_new(case_,mesh);

    // specify options
    VolumeMesher_setOptimization(volumeMesher,meshoptions_.volume_optimization);
    VolumeMesher_setSmoothing(volumeMesher,meshoptions_.volume_smoothing);

    // create it
    VolumeMesher_execute(volumeMesher,progress); 

    // clean up
    Progress_delete(progress);
    SurfaceMesher_delete(surfaceMesher);
    VolumeMesher_delete(volumeMesher);

    // should I do this here??
    // largest dihedral angle
    //int criteria = 1;
    //MS_optimizeMesh(mesh,options,1);

    // undocumented simmetrix call (ver 6.3)
    //fix4NodesOnSurface(mesh);

    progress = Progress_new();

    // takes case of bad brdy. elements (elements with no interior nodes)
    // is this the replacement for 7+?
    int dimfilter = 12;
    MS_ensureInteriorVertices(mesh,dimfilter,progress);

    Progress_delete(progress);

    InitTraversal();

  } catch (pSimError err) {
    fprintf(stderr,"Simmetrix error caught:\n");
    fprintf(stderr,"  Error code: %d\n",SimError_code(err));
    fprintf(stderr,"  Error string: %s\n",SimError_toString(err));
    return CV_ERROR;
  } catch (...) {
    fprintf(stderr,"Unhandled exception caught\n");
    return CV_ERROR;
  }

  return CV_OK;

}

int cvMeshSimMeshObject::WriteMesh(char *filename, int smsver) {
  // must have created mesh
  if (mesh == NULL) {
    return CV_ERROR;
  }

  pProgress progress = Progress_new();

  try {
    M_write(mesh,filename,smsver,progress);
  } catch (pSimError err) {
    fprintf(stderr,"Simmetrix error caught:\n");
    fprintf(stderr,"  Error code: %d\n",SimError_code(err));
    fprintf(stderr,"  Error string: %s\n",SimError_toString(err));
    Progress_delete(progress);
    return CV_ERROR;
  } catch (...) {
    fprintf(stderr,"Unhandled exception caught\n");
    Progress_delete(progress);
    return CV_ERROR;
  }
  
  Progress_delete(progress);

  meshFileName_[0] = '\0';
  sprintf(meshFileName_,"%s",filename);

  return CV_OK;
}

// scorec function from stats.cxx
int M_writeSTS(pMesh mesh, char *fname, char *mname);
int cvMeshSimMeshObject::WriteStats(char *filename) {
  // must have created mesh
  if (mesh == NULL) {
    return CV_ERROR;
  }
  if(M_writeSTS(mesh, filename, "dummy") == CV_ERROR) {
        fprintf(stderr,"ERROR:  could not write statistics file!\n");
        return CV_ERROR;
  }
  return CV_OK;
}

int cvMeshSimMeshObject::SetCylinderRefinement(double size, double radius, 
                            double length,double* center, double *normal) {
  // must have created mesh
  if (mesh == NULL) {
    return CV_ERROR;
  }
 
  MS_addCylinderRefinement(case_,size,radius,length, center,normal);  

  return CV_OK;
}

int cvMeshSimMeshObject::SetSphereRefinement(double size, double radius, 
                                           double* center) {
  // must have created mesh
  if (mesh == NULL) {
    return CV_ERROR;
  }
 
  MS_addSphereRefinement(case_,size,radius,center);  

  return CV_OK;
}

int cvMeshSimMeshObject::SetBoundaryLayer(int type, int id, int side, int nL, double* H) {

  pGEntity pid;

  if (MapIDtoPID(id,&pid) == CV_ERROR) {
    return CV_ERROR;
  }

  int mixed = 0;
  int blends = 0;
  int propagate = 0;

  MS_setBoundaryLayer(case_,pid,side,type,nL,H,mixed,blends,propagate);
  return CV_OK;
}

cvPolyData* cvMeshSimMeshObject::GetFacePolyData (int orgfaceid) {

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.
  int n;
  vtkPolyData *mypd = NULL;
  cvPolyData *result = NULL; 

  // get the identifier/tag corresponding to the given parasolid face entity
  int faceID = 0;
  if (getIdentForFaceId(orgfaceid,&faceID) == CV_ERROR) {
     fprintf(stderr,"ERROR:  could not find idenitifer for face (%i)\n",orgfaceid);
     fflush(stderr);
     return NULL;
  }

  // check to make sure the faceID exists for this model
  pGFace modelface;
  int nFace = GM_numFaces(model);
  int foundFace = 0;
  GFIter myGFiter;
  myGFiter = GM_faceIter(model);
  while (modelface = GFIter_next(myGFiter)) {
    if(GEN_tag((GEntity*)modelface) == faceID) {foundFace=1;break;}
  }
  if (foundFace == 0) {
    fprintf(stderr,"Error: face id %i not found in model.\n",faceID);
    return NULL;
  }
  GFIter_delete(myGFiter);

  // vtk requires single precision
  vtkFloatingPointType p[3];

  // create the vtk object
  vtkPoints* myPoints = vtkPoints::New();
  myPoints->Allocate(1000,1000);
  mypd = vtkPolyData::New();
  mypd->Allocate(1000,1000);
  mypd->SetPoints(myPoints);

  vtkIntArray* gid = vtkIntArray::New();
  gid->SetNumberOfComponents(1);
  gid->Allocate(1000,1000);
  gid->SetName("GlobalNodeID");

  vtkIntArray* eid = vtkIntArray::New();
  eid->SetNumberOfComponents(1);
  eid->Allocate(1000,1000);
  eid->SetName("GlobalElementID");

  vtkIntArray* eid2 = vtkIntArray::New();
  eid2->SetNumberOfComponents(1);
  eid2->Allocate(1000,1000);
  eid2->SetName("GlobalElementID2");

  vtkIdList* PointIds = vtkIdList::New();
  PointIds->Allocate(10,10);  

  // node numbering in polydata object
  int poly_nodes = 0;

  int num_faces = M_numFaces (mesh);

  FIter myFiter = M_faceIter(mesh);
  pFace myface = NULL;
  while (myface = FIter_next(myFiter)) {

        //if (!(loopfaces % 10000)) fprintf(stdout,"face %i\n",loopfaces);
  
        // Get the model tag for the face
        int facetag=0;
        pGEntity modelentity = F_whatIn(myface);
        
        if (modelentity != NULL) {
          // ignore unless face is classified on model face
	  if (F_whatInType(myface) != Gface) continue;
          facetag=GEN_tag(modelentity);
        }

        /*  only output element face if it is on the model face */
        if (facetag == faceID) {

          int nodes[20];
          int num_nodes = FindNodesOnElementFace(myface,nodes);

          for (n=0;n < num_nodes; n++) {
            if (GetNodeCoords(nodes[n]) != CV_OK) {
                  mypd->Delete();
                  return CV_ERROR;
            }
            // vtk requires single precision
            vtkFloatingPointType p[3];
            p[0]=nodeX_;p[1]=nodeY_;p[2]=nodeZ_;
            myPoints->InsertNextPoint(p);
            gid->InsertNextTuple1(nodeID_);
          }

          PointIds->Reset();
          for (n = 0; n < num_nodes; n++) {
            PointIds->InsertNextId(poly_nodes++);
          }
          mypd->InsertNextCell(VTK_POLYGON,PointIds);

          pPList fregions = F_regions(myface);
          if (PList_size(fregions) == 2) {
            //fprintf(stderr,"ERROR: number of face regions (%i)\n",PList_size(fregions));
            // need to add one here to get elem nu since regions start at 0
            eid->InsertNextTuple1(EN_id((pRegion)PList_item(fregions,0))+1);
            eid2->InsertNextTuple1(EN_id((pRegion)PList_item(fregions,1))+1);
	  } else {
            // need to add one here to get elem nu since regions start at 0
            eid->InsertNextTuple1(EN_id((pRegion)PList_item(fregions,0))+1);
            eid2->InsertNextTuple1(-1);
	  }
          PList_delete(fregions);
	}

  }
  FIter_delete(myFiter);
  PointIds->Delete();

  // attach the scalars to the polydata
  mypd->GetPointData()->SetScalars(gid);
  mypd->GetCellData()->SetScalars(eid);
  mypd->GetCellData()->AddArray(eid2);
  mypd->GetPointData()->SetActiveScalars("GlobalNodeID");
  mypd->GetCellData()->SetActiveScalars("GlobalElementID");

  vtkCleanPolyData* cleaner = vtkCleanPolyData::New();
  cleaner->PointMergingOn();
  cleaner->PieceInvariantOff();
  cleaner->SetInputDataObject(mypd);
  cleaner->Update();

  result = new cvPolyData( cleaner->GetOutput() );

  cleaner->Delete();
  mypd->Delete();
  myPoints->Delete();
  gid->Delete();
  eid->Delete();
  eid2->Delete();

  return result;

}


int cvMeshSimMeshObject::GetModelFaceInfo(char rtnstr[99999]) {

  char tmpstr[99999];

  rtnstr[0]='\0';


  pGFace modelface;
  int nFace = GM_numFaces(model);

  GFIter myGFiter;
  myGFiter = GM_faceIter(model);

  if (solidmodeling_kernel_ == SM_KT_PARASOLID) {

#ifdef USE_PARASOLID

    while (modelface = GFIter_next(myGFiter)) {
      tmpstr[0] = '\0';
      char *namestr;
      PsdUtils_GetFaceAttribute( "gdscName",GEN_getParasolidEntity(modelface), &namestr );
      sprintf(tmpstr,"%s {%i %i {%s}} ",rtnstr,GEN_getParasolidEntity(modelface),GEN_tag((GEntity*)modelface),namestr);
      rtnstr[0]='\0';
      sprintf(rtnstr,"%s",tmpstr);
    }

# else

    return CV_ERROR;

#endif

  } else if (solidmodeling_kernel_ == SM_KT_DISCRETE) {

#ifdef USE_DISCRETE_MODEL

    while (modelface = GFIter_next(myGFiter)) {
      tmpstr[0] = '\0';
      char *namestr;
      sprintf(tmpstr,"%s {%i %i {%s}} ",rtnstr,GEN_tag((GEntity*)modelface),GEN_tag((GEntity*)modelface),"");
      rtnstr[0]='\0';
      sprintf(rtnstr,"%s",tmpstr);
    }

#else

    return CV_ERROR;

#endif
  }

  GFIter_delete(myGFiter);

  return CV_OK;

}

int cvMeshSimMeshObject::FindFaceNumber (pRegion region, int pseudoface,int *facenum) {

  int i, k, loopfaces;

  pFace myface = R_face(region,pseudoface);

  // get nodes of element
  pPList vert_list = R_vertices (region,MY_MESHSIM_VERTEX_ORDERING);

  // Routine only good for linear tets
  if (PList_size (vert_list) != 4) {
    fprintf(stderr,"ERROR:  Must be linear tets to extract bc's.\n");
    return CV_ERROR;
  }

  // keep track of the node numbers so we can map the face nodes back
  // to the element definition so we know which face to apply bc on.
  int elemverts[4];
  for (k = 0; k < PList_size (vert_list); k++){
    elemverts[k] = P_id (V_point ((pVertex)PList_item (vert_list, k)));
  }
    
  PList_delete(vert_list); 
 
  // Note, for below the function F_region(.,n) does NOT seem to
  // return the faces in a specific order.  Thus, the code below
  // is needed to properly sort the bc's into separate files
  // (so you know the first number corresonds to a material region
  // and the second corresponds to its interface material).

  // Perform mapping between scorec face #'s and prophlex face #'s
  // ASSUME linear tet.
 
  // Get the vertexes for the face (counterclockwise)
  pPList myverts = F_vertices (myface,MY_MESHSIM_VERTEX_ORDERING);
 
  // Routine only good for linear tets
  if (PList_size (myverts) != 3) {
    fprintf(stderr,"ERROR:  Must be linear tets to extract bc's.\n");
    return CV_ERROR;
  }

  int keepme = 0;

  // Get the node numbers for the face
  // See below for the definition of "keepme"
  for (int loopcoords=0;loopcoords <  PList_size(myverts);loopcoords++) {
    int tmpnodenum= P_id(V_point((MVertex*)PList_item(myverts,loopcoords)));
    for (int whyme = 0;whyme < 4;whyme++) {
      if (tmpnodenum == elemverts[whyme]) {
        keepme = keepme + pow(2.0,whyme);
        break;
      }
    }
  }
  PList_delete(myverts);

  // NOTE: keepme is a special variable which indicates the face we need to
  // specify in Prophlex to apply a bc on an element.  The problem is that
  // the faces for the various regions (elements) in SCOREC are not consistently
  // numbered.  That is to say, unlike FE solvers where a face is implicitly defined
  // based on the order of the nodes defining the element, scorec appears random.
  // The variable keepme takes on a unique value depending on which nodes of the element
  // are used to define the face.  This unique number is then mapped to the proper proplex 
  // face. 
  // Keepme = 2^(local element_node_number #0) + 2^(local element_node_number #1) +
  //          2^(local element_node_number #2)
  //
  // Example: the nodes defining a given face correspond to nodes 0, 2, and 3 of the element
  // definition.  keepme = 2^(0) + 2^(2) +2^(3) = 13 which corresponds to face = 1 in prophlex.
  // This is a royal pain in the ass to calculate, probably horribly cpu intensive, but I
  // don't know a better way...
 
  int phlexface;
  if ( keepme == 14 ) {
    phlexface = 0;
  } else if ( keepme == 13 ) {
    phlexface = 1;
  } else if ( keepme == 11 ) {
    phlexface = 2;
  } else if ( keepme == 7 ) {
    phlexface = 3;
  } else {
    fprintf(stderr,"ERROR:  could not resolve phlex face for element.\n");
    fprintf(stderr,"  keepme: %i   element nodes: %i %i %i %i \n", keepme,
                     elemverts[0],elemverts[1],elemverts[2],elemverts[3]); 
    return CV_ERROR;
  }

  *facenum = phlexface;

  return CV_OK;
}

int cvMeshSimMeshObject::FindNodesOnElementFace (pFace face, int* nodes) {
 
  int i;
  int num_nodes = 0;

  pPList vert_list = F_vertices (face,MY_MESHSIM_VERTEX_ORDERING);
  int numElemFaceVerts = PList_size (vert_list);

  // write out "linear" nodes
  for (i = 0; i < numElemFaceVerts; i++) {
    pVertex vertex = (pVertex)PList_item (vert_list, i);
    pPoint point = V_point (vertex);
    nodes[i] = P_id(point);
  }

  num_nodes = numElemFaceVerts;

  // quad nodes
  if (quadElem_ == 1) {
    pPList edge_list = F_edges(face, 1, NULL);
    int numEdges = PList_size (edge_list);

    for (i = 0; i < numEdges; i++) {
      pEdge xedge = (pEdge)PList_item (edge_list, i);
      if (E_numPoints(xedge) != 1) {
        fprintf(stderr,"ERROR:  no interior point!\n");
        exit(-1);
      }       
      nodes[numElemFaceVerts+i] = P_id(E_point(xedge,0));     
    }

    num_nodes+=numEdges;
  }

  return num_nodes;

}

// --------------------
//  Adapt
// --------------------
/** 
 * @brief Function to Adapt Mesh based on input adaption features etc.
 * @return CV_OK if adaptions performs correctly
 */
int cvMeshSimMeshObject::Adapt()
{ 
#ifdef USE_MESHSIM_ADAPTOR
  pProgress progressAdapt = Progress_new();

  MSA_adapt(simAdapter, progressAdapt);

  Progress_delete(progressAdapt);

  pProgress progressFix = Progress_new();
  // 7.0+ version
  // takes case of bad brdy. elements (elements with no interior nodes)
  // is this the replacement for 7+?
  int dimfilter = 12;
  MS_ensureInteriorVertices(mesh,dimfilter,progressFix);
  Progress_delete(progressFix);

  printf("-- Adaptation Done...\n");
  printf(" Total # of elements: %d\n", M_numRegions(mesh));
  printf(" Total # of vertices: %d\n\n", M_numVertices(mesh));

  MSA_delete(simAdapter);

  return CV_OK;
#else
  fprintf(stderr,"Error: MeshSim Adaptor not available\n");
  return CV_ERROR;
#endif
}

int cvMeshSimMeshObject::GetAdaptedMesh(vtkUnstructuredGrid *ug, vtkPolyData *pd)
{
#ifdef USE_MESHSIM_ADAPTOR
  if (ug == NULL)
  {
    fprintf(stderr,"UGrid is NULL!\n");
    return CV_ERROR;
  }
  if (pd == NULL)
  {
    fprintf(stderr,"PolyData is NULL!\n");
    return CV_ERROR;
  }
  double xyz[3];
  vtkIdType pointId;
  vtkIdType closestPoint;

  vtkSmartPointer<vtkPoints> outPoints = 
    vtkSmartPointer<vtkPoints>::New();

  int count = 0;

  int nshg_adapted = M_numVertices(mesh);

  vtkIntArray* gid = vtkIntArray::New();
  gid->SetNumberOfComponents(1);
  gid->Allocate(nshg_adapted,1000);
  gid->SetNumberOfTuples(nshg_adapted);
  gid->SetName("GlobalNodeID");

  outPoints->SetNumberOfPoints(nshg_adapted);
  VIter myViter;
  myViter = M_vertexIter(mesh);
  for (int i = 0; i < nshg_adapted; i++) {
    pPoint point = V_point (VIter_next(myViter));
    //int nodenumber = P_id (point);
    int nodenumber = i + 1;
    P_setID(point,nodenumber);  
    double x = P_x (point);
    double y = P_y (point);
    double z = P_z (point);
    //fprintf(stdout,"%i %lf %lf %lf\n",nodenumber,x,y,z);  
    outPoints->SetPoint(nodenumber-1,x,y,z);
    gid->SetTuple1(nodenumber-1,nodenumber);
  } // i 
  VIter_delete(myViter);

  count = 0;

  int neltot = M_numRegions(mesh);

  //  vtkSmartPointer<vtkIdList> ptids = 
  //  vtkSmartPointer<vtkIdList>::New();
  vtkIdList* ptids = vtkIdList::New();
 
  vtkSmartPointer<vtkIntArray> eid = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> rid = 
    vtkSmartPointer<vtkIntArray>::New();

  ug->SetPoints(outPoints);
  ug->GetPointData()->AddArray(gid);

  ptids->Allocate(10,10);
  ptids->Initialize();
  ptids->SetNumberOfIds(4);

  eid->SetNumberOfComponents(1);
  eid->Allocate(neltot,1000);
  eid->Initialize();
  eid->SetName("GlobalElementID");

  rid->SetNumberOfComponents(1);
  rid->Allocate(neltot,1000);
  rid->Initialize();
  rid->SetName("ModelRegionID");

  // only for linear tets

  pRegion myelement = NULL;
  RIter myRIter = M_regionIter(mesh);

  // only allow one model region for now
  int curMdlRegID = 1;

  while ((myelement = RIter_next(myRIter)) != NULL) {
    // the elements are numbered from 1 to N.
    int curElemID = EN_id((pEntity)myelement)+1;
    pPList vert_list = R_vertices (myelement,MY_MESHSIM_VERTEX_ORDERING);
    int num_elem_verts = PList_size (vert_list);
    // must be linear
    if (num_elem_verts != 4) {
      exit(-1);
    }
    for (i = 0; i < num_elem_verts; i++) {
        pVertex vertex = (pVertex)PList_item (vert_list, i);
        // vtk nodes must start at zero
        ptids->SetId(i,P_id(V_point(vertex))-1);
    } // i
    PList_delete(vert_list);      
    ug->InsertNextCell(VTK_TETRA,ptids);
    eid->InsertNextTuple1(curElemID);
    rid->InsertNextTuple1(curMdlRegID);
  }

  ptids->Delete();
   
  ug->GetCellData()->AddArray(rid);
  ug->GetCellData()->SetScalars(eid);
  ug->GetCellData()->SetActiveScalars("GlobalElementID");
  ug->GetCellData()->CopyAllOn();

  //Now get PolyData surface from mesh
  vtkSmartPointer<vtkGeometryFilter> surfFilt = vtkSmartPointer<vtkGeometryFilter>::New();
  surfFilt->MergingOff();
  surfFilt->SetInputDataObject(ug);
  surfFilt->Update();
  vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->PointMergingOff();
  cleaner->PieceInvariantOff();
  cleaner->SetInputDataObject(surfFilt->GetOutput());
  cleaner->Update();

  pd->DeepCopy(cleaner->GetOutput());

  return CV_OK;
#else
  fprintf(stderr,"Error: MeshSim Adaptor not available\n");
  return CV_ERROR;
#endif
}

int cvMeshSimMeshObject::SetMetricOnMesh(double *error_indicator,int lstep,double factor, double hmax, double hmin,int strategy)
{
#ifdef USE_MESHSIM_ADAPTOR
  if (mesh == NULL)
  {
    fprintf(stderr,"Must load .sms mesh before setting metric on mesh\n");
    return CV_ERROR;
  }
  int nshg = M_numVertices(mesh);
  simAdapter = MSA_new(mesh,1);

  pVertex vertex;
  VIter vit=M_vertexIter(mesh);
  int i=0;
  while ( vertex=VIter_next(vit)) {
    if (EN_isBLEntity(vertex)) {
      continue;
    }
    if (strategy == 1) 
    {
      MSA_setVertexSize(simAdapter,vertex,error_indicator[i++]);
    }
    else if (strategy == 2)
    {
       double scaled_eigenvecs[3][3];
       //fprintf(stdout,"\nAfter hessian for node %d is:\n",i);
       for (int j=0;j<3;j++)
       {
	 for (int k=0;k<3;k++)
	 {
	   scaled_eigenvecs[j][k] = error_indicator[i++];
	   //fprintf(stdout,"%.4f ",scaled_eigenvecs[j][k]);
	 }
	 //fprintf(stdout,"\n");
       }
       //fprintf(stdout,"\n");
       MSA_setAnisoVertexSize(simAdapter, 
			    vertex,
			    scaled_eigenvecs);
    }
    else 
    {
      fprintf(stderr,"Strategy is not available\n");
      return CV_ERROR;
    }
  }
  VIter_delete(vit);

  return CV_OK;
#else
  fprintf(stderr,"Error: MeshSim Adaptor not available\n");
  return CV_ERROR;
#endif
}

