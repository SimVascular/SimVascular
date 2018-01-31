/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "SimVascular.h"
#include "cvMeshSimDiscreteSolidModel.h"
#include "cv_discrete_utils.h"
#include "cv_sys_geom.h"

// -----------------
// cvMeshSimDiscreteSolidModel
// -----------------

cvMeshSimDiscreteSolidModel::cvMeshSimDiscreteSolidModel()
  : cvSolidModel( SM_KT_DISCRETE )
{
  geom_ = NULL;
  quadElem_ = 0;
  progress_ = Progress_new();
  Progress_setDefaultCallback(progress_);
}


// ------------------
// ~cvMeshSimDiscreteSolidModel
// ------------------

cvMeshSimDiscreteSolidModel::~cvMeshSimDiscreteSolidModel()
{
  if ( geom_ != NULL ) {
      M_release(DM_getMesh(geom_));
      SimDiscrete_stop(0);
      SimDiscrete_start(0);
  }
  if (progress_ != NULL) Progress_delete(progress_);
}


// -----
// Clear
// -----

void cvMeshSimDiscreteSolidModel::Clear()
{
  if ( geom_ != NULL ) {
      M_release(DM_getMesh(geom_));
      SimDiscrete_stop(0);
      SimDiscrete_start(0);
  }
}


// -----
// Print
// -----

void cvMeshSimDiscreteSolidModel::Print() const
{
  if ( geom_ != NULL ) {
    int DMnumFaces = GM_numFaces((pGModel)geom_);
    int DMnumRegions = GM_numRegions((pGModel)geom_);
    int DMnumEdges = GM_numEdges((pGModel)geom_);
    cout<<"num faces: "<<DMnumFaces<<endl;
    cout<<"num regions: "<<DMnumRegions<<endl;
    cout<<"num edges: "<<DMnumEdges<<endl;
  }
}


// ----------
// GetFaceIds
// ----------

int cvMeshSimDiscreteSolidModel::GetFaceIds (int *numFaces, int **faceIds) {

  if ( geom_ == NULL) {
    return SV_ERROR;
  }


  this->Print();

  int num = 0;

  GFIter myFiter = GM_faceIter(geom_);
  pGFace myface;
  while (myface = GFIter_next(myFiter)) {
    num++;
  }
  GFIter_delete(myFiter);

  *numFaces = num;

  if (num == 0) return SV_ERROR;

  (*faceIds) = new int [num];

  myFiter = GM_faceIter(geom_);
  int j = 0;
  while (myface = GFIter_next(myFiter)) {
    (*faceIds)[j++] = GEN_tag(myface);
  }
  GFIter_delete(myFiter);

  return SV_OK;

}


int cvMeshSimDiscreteSolidModel::FindNodesOnElementFace (pFace face, int* nodes, double *xyz) const {

  int i;
  int num_nodes = 0;

  int pos = 0;

  pPList vert_list = F_vertices (face,MY_MESHSIM_VERTEX_ORDERING);
  int numElemFaceVerts = PList_size (vert_list);

  // write out "linear" nodes
  for (i = 0; i < numElemFaceVerts; i++) {
    pVertex vertex = (pVertex)PList_item (vert_list, i);
    pPoint point = V_point (vertex);
    xyz[pos++]=P_x(point);
    xyz[pos++]=P_y(point);
    xyz[pos++]=P_z(point);
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
      xyz[pos++]=P_x(E_point(xedge,0));
      xyz[pos++]=P_y(E_point(xedge,0));
      xyz[pos++]=P_z(E_point(xedge,0));
    }

    num_nodes+=numEdges;
  }

  return num_nodes;

}


cvPolyData* cvMeshSimDiscreteSolidModel::GetFacePolyData(int faceID, int useMaxDist, double max_dist) const {

  // useMaxDist and max_dist are ignored!

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.

  int n;
  vtkPolyData *mycvPolyData;
  cvPolyData *result;

  int foundIt = 0;

  GFIter myGFiter = GM_faceIter(geom_);
  pGFace mygface;
  while (mygface = GFIter_next(myGFiter)) {
    if (faceID == GEN_tag(mygface)) {
      foundIt++;
      break;
    }
  }
  GFIter_delete(myGFiter);

  if (!foundIt) {
    fprintf(stdout,"Error:  face id (%i) not found in model!\n",faceID);
    return NULL;
  }

  pMesh mesh = DM_getMesh(geom_);

  // vtk requires single precision
  vtkFloatingPointType p[3];

  // create the vtk object
  vtkPoints* myPoints = vtkPoints::New();
  myPoints->Allocate(1000,1000);
  mycvPolyData = vtkPolyData::New();
  mycvPolyData->Allocate(1000,1000);
  mycvPolyData->SetPoints(myPoints);

  vtkIdList* PointIds = vtkIdList::New();
  PointIds->Allocate(10,10);

  // track the actual mesh node numbers in a scalar array
  vtkFloatArray *myScalars = vtkFloatArray::New();
  myScalars->Allocate(1000,1000);

  // node numbering in polydata object
  int poly_nodes = 0;

  int num_faces = M_numFaces (mesh);

  FIter myFiter = M_faceIter(mesh);
  pFace myface = NULL;
  while (myface = FIter_next(myFiter)) {

        //if (!(loopfaces % 10000)) fprintf(stdout,"face %i\n",loopfaces);

        // Get the model tag for the face
        int facetag=0;
        pGEntity modelface = F_whatIn(myface);
        if (modelface != NULL) {
           facetag=GEN_tag(F_whatIn(myface));
        }

        /*  only output element face if it is on the model face */
        if (facetag == faceID) {
          int nodes[20];
          double xyz[60];
          int num_nodes = FindNodesOnElementFace(myface,nodes,xyz);

          int pos = 0;
          for (n=0;n < num_nodes; n++) {
            // vtk requires single precision
            vtkFloatingPointType p[3];
            p[0]=xyz[pos++];p[1]=xyz[pos++];p[2]=xyz[pos++];
            myPoints->InsertNextPoint(p);
            myScalars->InsertNextTuple1(nodes[n]);
          }

          PointIds->Reset();
          for (n = 0; n < num_nodes; n++) {
            PointIds->InsertNextId(poly_nodes++);
          }
          mycvPolyData->InsertNextCell(VTK_POLYGON,PointIds);
        }


  }
  FIter_delete(myFiter);

  // attach the scalars to the polydata
  mycvPolyData->GetPointData()->SetScalars(myScalars);

  PointIds->Delete();
  cvPolyData *tmpresult = new cvPolyData( mycvPolyData );
  mycvPolyData->Delete();  // cvPolyData uses ref-counting

  // delete points and scalars here?
  myPoints->Delete();
  myScalars->Delete();

  // delete duplicate points
  result = sys_geom_MergePts( tmpresult );

  delete tmpresult;

  return result;

}



// -----
// Check
// -----

void cvMeshSimDiscreteSolidModel::Check( int *nerr) const
{
  if ( geom_ != NULL ) {
  }
}


// ----------
// ReadNative
// ----------

int cvMeshSimDiscreteSolidModel::ReadNative( char *filename )
{

  if (geom_ != NULL) {
    return SV_ERROR;
  }

  // read discrete model
  geom_ = DM_load(filename, progress_);

  return SV_OK;
}


// -----------
// WriteNative
// -----------

int cvMeshSimDiscreteSolidModel::WriteNative( int file_version, char *filename ) const
{
  if ( geom_ == NULL ) return SV_ERROR;

  DM_write(geom_,filename,file_version,progress_);

  return SV_OK;

}


// -----------
// GetPolyData
// -----------

cvPolyData *cvMeshSimDiscreteSolidModel::GetPolyData(int useMaxDist, double max_dist) const
{

  // max_dist is equivalent to global max size

  vtkPolyData *bound;
  cvPolyData *result;

  if ( geom_ == NULL ) return NULL;
  if ( DiscreteUtils_GetVtkPolyData( geom_, useMaxDist, max_dist, &bound ) != SV_OK ) {
    return NULL;
  }

  result = new cvPolyData( bound );
  bound->Delete();  // cvPolyData uses ref-counting

  return result;
}


int cvMeshSimDiscreteSolidModel::MakePoly3dSolid( cvPolyData *pd , double angle )
{

  int code;

  if ( geom_ != NULL ) {
    return SV_ERROR;
  }

  code = DiscreteUtils_MakePoly3dSolidVtk( pd->GetVtkPolyData(), 0, angle, &geom_);
  if ( code != SV_OK ) {
    this->Clear();
    return SV_ERROR;
  }

  return SV_OK;
}




