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

#include "sv_TetGenMeshObject.h"
#include "sv_SolidModel.h"
#include "sv_misc_utils.h"
#include "sv_polydatasolid_utils.h"

#include "sv_tetgenmesh_utils.h"

#include "sv_sys_geom.h"
#ifdef SV_USE_PYTHON
#include "Python.h"
#endif
#include "vtkGeometryFilter.h"
#include "vtkCleanPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkPolyData.h"
#include "vtkDataArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkCellArray.h"
#include "vtkThreshold.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkAppendPolyData.h"
#include "vtkPolyDataConnectivityFilter.h"
#include "vtkCenterOfMass.h"

#ifdef SV_USE_VMTK
  #include "sv_vmtk_utils.h"
  #include "vtkvmtkPolyDataToUnstructuredGridFilter.h"
#endif

#ifdef SV_USE_MMG
  #include "sv_mmg_mesh_utils.h"
#endif

#include <array>
#include <set>
#include <math.h>

// -----------
// cvTetGenMeshObject
// -----------
/**
 * @brief Constructor for cvTetGenMeshObject(Should never be called directly)
 */
#ifdef SV_USE_TCL
cvTetGenMeshObject::cvTetGenMeshObject(Tcl_Interp *interp)
  : cvMeshObject()
{
  interp_ = interp;
  inmesh_ = NULL;
  outmesh_ = NULL;
  polydatasolid_ = NULL;
  inputug_ = NULL;
  meshloaded_ = 0;
  loadedVolumeMesh_ = 0;
  //nodemap_ = NULL;
  pts_ = NULL;

  originalpolydata_ = NULL;
  surfacemesh_ = NULL;
  volumemesh_ = NULL;
  boundarylayermesh_ = NULL;
  innerblmesh_ = NULL;
  holelist_ = NULL;
  regionlist_ = NULL;
  regionsizelist_ = NULL;

  meshFileName_[0] = '\0';
  solidFileName_[0] = '\0';

  solidmodeling_kernel_ = SM_KT_POLYDATA;
  numModelRegions_ = 0;
  numBoundaryRegions_ = 0;

  //All the different mesh options. Originally set to zero. Changed by calls to object from GUI
  //Specifically ->SetMeshOptions()
  meshoptions_.surfacemeshflag=0;
  meshoptions_.volumemeshflag=0;
  meshoptions_.nomerge=0;
  meshoptions_.quiet=0;
  meshoptions_.docheck=0;
  meshoptions_.verbose=0;
  meshoptions_.diagnose=0;
  meshoptions_.nobisect=0;
  meshoptions_.optlevel=0;
  meshoptions_.maxedgesize=0;
  meshoptions_.epsilon=0;
  meshoptions_.minratio=0;
  meshoptions_.mindihedral=0;
  meshoptions_.coarsenpercent=0;
  meshoptions_.boundarylayermeshflag=0;
  meshoptions_.numsublayers=0;
  meshoptions_.blthicknessfactor=0;
  meshoptions_.sublayerratio=0;
  meshoptions_.useconstantblthickness=0;
  meshoptions_.newregionboundarylayer=0;
  meshoptions_.boundarylayerdirection=1;
  meshoptions_.refinement=0;
  meshoptions_.refinedsize=0;
  meshoptions_.sphereradius=0;
  meshoptions_.cylinderradius=0;
  meshoptions_.cylinderlength=0;
  meshoptions_.functionbasedmeshing=0;
  meshoptions_.secondarrayfunction=0;
  meshoptions_.meshwallfirst=0;
  meshoptions_.startwithvolume=0;
  meshoptions_.refinecount=0;
  meshoptions_.numberofholes=0;
  meshoptions_.numberofregions=0;
  meshoptions_.allowMultipleRegions=false;

#ifdef SV_USE_MMG
  meshoptions_.usemmg=1;
#else
  meshoptions_.usemmg=0;
#endif
  meshoptions_.hausd=0;
  for (int i=0;i<3;i++)
  {
    meshoptions_.spherecenter[i] = 0;
    meshoptions_.cylindercenter[i] = 0;
    meshoptions_.cylindernormal[i] = 0;
  }
}
#endif
// -----------
// cvTetGenMeshObject for python
// -----------
/**
 * @brief Constructor for cvTetGenMeshObject(Should never be called directly)
 */
#ifdef SV_USE_PYTHON
cvTetGenMeshObject::cvTetGenMeshObject()
: cvMeshObject()
{
  inmesh_ = NULL;
  outmesh_ = NULL;
  polydatasolid_ = NULL;
  inputug_ = NULL;
  meshloaded_ = 0;
  loadedVolumeMesh_ = 0;
  //nodemap_ = NULL;
  pts_ = NULL;

  originalpolydata_ = NULL;
  surfacemesh_ = NULL;
  volumemesh_ = NULL;
  boundarylayermesh_ = NULL;
  innerblmesh_ = NULL;
  holelist_ = NULL;
  regionlist_ = NULL;
  regionsizelist_ = NULL;

  meshFileName_[0] = '\0';
  solidFileName_[0] = '\0';

  solidmodeling_kernel_ = SM_KT_POLYDATA;
  numModelRegions_ = 0;
  numBoundaryRegions_ = 0;

  //All the different mesh options. Originally set to zero. Changed by calls to object from GUI
  //Specifically ->SetMeshOptions()
  meshoptions_.surfacemeshflag=0;
  meshoptions_.volumemeshflag=0;
  meshoptions_.nomerge=0;
  meshoptions_.quiet=0;
  meshoptions_.docheck=0;
  meshoptions_.verbose=0;
  meshoptions_.diagnose=0;
  meshoptions_.nobisect=0;
  meshoptions_.optlevel=0;
  meshoptions_.maxedgesize=0;
  meshoptions_.epsilon=0;
  meshoptions_.minratio=0;
  meshoptions_.mindihedral=0;
  meshoptions_.coarsenpercent=0;
  meshoptions_.boundarylayermeshflag=0;
  meshoptions_.numsublayers=0;
  meshoptions_.blthicknessfactor=0;
  meshoptions_.sublayerratio=0;
  meshoptions_.useconstantblthickness=0;
  meshoptions_.newregionboundarylayer=0;
  meshoptions_.boundarylayerdirection=1;
  meshoptions_.refinement=0;
  meshoptions_.refinedsize=0;
  meshoptions_.sphereradius=0;
  meshoptions_.cylinderradius=0;
  meshoptions_.cylinderlength=0;
  meshoptions_.functionbasedmeshing=0;
  meshoptions_.secondarrayfunction=0;
  meshoptions_.meshwallfirst=0;
  meshoptions_.startwithvolume=0;
  meshoptions_.refinecount=0;
  meshoptions_.numberofholes=0;
  meshoptions_.numberofregions=0;
  meshoptions_.allowMultipleRegions=false;
#ifdef SV_USE_MMG
  meshoptions_.usemmg=1;
#else
  meshoptions_.usemmg=0;
#endif
  meshoptions_.hausd=0;
  for (int i=0;i<3;i++)
  {
    meshoptions_.spherecenter[i] = 0;
    meshoptions_.cylindercenter[i] = 0;
    meshoptions_.cylindernormal[i] = 0;
  }
}
#endif
// -----------
// cvTetGenMeshObject
// -----------
//
cvTetGenMeshObject::cvTetGenMeshObject( const cvTetGenMeshObject& sm )
  : cvMeshObject()
{

  // Copy( sm );   // relying on automatic upcast to cvMeshObject*

}

// ------------
// ~cvTetGenMeshObject
// ------------
/**
 * @brief Destructor for cvTetGenMeshObject
 */

cvTetGenMeshObject::~cvTetGenMeshObject()
{
  if (inmesh_ != NULL)
    delete inmesh_;

  if (outmesh_ != NULL)
    delete outmesh_;

  if (surfacemesh_ != NULL)
    surfacemesh_->Delete();

  if (volumemesh_ != NULL)
    volumemesh_->Delete();

  if (polydatasolid_ != NULL)
    polydatasolid_->Delete();

  if (inputug_ != NULL)
    inputug_->Delete();

  if (originalpolydata_ != NULL)
    originalpolydata_->Delete();

  if (boundarylayermesh_ != NULL)
    boundarylayermesh_->Delete();

  if (innerblmesh_ != NULL)
    innerblmesh_->Delete();

  if (holelist_ != NULL)
    holelist_->Delete();

  if (regionlist_ != NULL)
    regionlist_->Delete();

  if (regionsizelist_ != NULL)
    regionsizelist_->Delete();
}

//-----------------------------
// EnableSizeFunctionBasedMesh
//-----------------------------
// Enable meshing based on a size function.
//
// This is only called by the Python API.
//
void cvTetGenMeshObject::EnableSizeFunctionBasedMesh()
{
  meshoptions_.functionbasedmeshing = 1;
}

//------------------------------
// DisableSizeFunctionBasedMesh  
//------------------------------
// Disable meshing based on a size function.
//
// This is only called by the Python API.
//
void cvTetGenMeshObject::DisableSizeFunctionBasedMesh()
{
  meshoptions_.functionbasedmeshing = 0;
}

//--------------------------------
// SizeFunctionBasedMeshIsEnabled
//--------------------------------
//
bool cvTetGenMeshObject::SizeFunctionBasedMeshIsEnabled()
{
  return (meshoptions_.functionbasedmeshing == 1);
}

int cvTetGenMeshObject::SetMeshFileName( const char* meshFileName )
{
  if (meshFileName != NULL)
    sprintf(meshFileName_, "%s", meshFileName);
  else
    meshFileName_[0] = '\0';

  return SV_OK;
}

void cvTetGenMeshObject::SetAllowMultipleRegions(bool value) 
{
  meshoptions_.allowMultipleRegions = value;
}

int cvTetGenMeshObject::SetSolidFileName( const char* solidFileName )
{
  if (solidFileName != NULL)
    sprintf(solidFileName_, "%s", solidFileName);
  else
    solidFileName_[0] = '\0';

  return SV_OK;
}

// -----
// Print
// -----
/**
 * @brief Function to print out the mesh statistics
 * @return SV_OK if executed correctly
 */
#ifdef SV_USE_TCL
int cvTetGenMeshObject::Print()
{
  int num_nodes = 0;
  int nMeshFaces = 0;
  int num_elems = 0;
  int nMeshEdges = 0;
  int nRegion = 1;
  int nFace = 0;
  int nEdge = 0;
  int nVertex = 0;

  if (polydatasolid_ == NULL)
  {
    fprintf(stderr,"Solid has not been loaded\n");
    return SV_ERROR;
  }
  if(meshoptions_.surfacemeshflag && !meshoptions_.volumemeshflag)
  {
    num_nodes = polydatasolid_->GetNumberOfPoints();
    nMeshFaces = polydatasolid_->GetNumberOfCells();
  }
  else if(meshoptions_.boundarylayermeshflag)
  {
    if (volumemesh_ == NULL)
    {
      fprintf(stderr,"Mesh has not been created\n");
      return SV_ERROR;
    }
    num_nodes = volumemesh_->GetNumberOfPoints();
    num_elems = volumemesh_->GetNumberOfCells();
    nMeshFaces = 4.0*(volumemesh_->GetNumberOfCells());
    nMeshEdges = 3.0*(nMeshFaces);
  }
  else
  {
    if (outmesh_ == NULL)
    {
      fprintf(stderr,"Mesh has not been created\n");
      return SV_ERROR;
    }
    num_nodes = outmesh_->numberofpoints;
    num_elems = outmesh_->numberoftetrahedra;
    nMeshFaces = outmesh_->numberoftrifaces;
    nMeshEdges = outmesh_->numberofedges;
    nFace = outmesh_->numberoftrifaces;
    nEdge = outmesh_->numberofregions;
    nVertex = outmesh_->numberofpoints;
  }
   /* output the statistics */

  nFace = originalpolydata_->GetNumberOfPolys();
  nVertex = originalpolydata_->GetNumberOfPoints();
  nEdge = 3*(originalpolydata_->GetNumberOfPolys());

  fprintf(stdout,"\nMESH STATISTICS:\n");
  fprintf(stdout,"  elements         = %i\n",num_elems);
  fprintf(stdout,"  nodes            = %i\n",num_nodes);
  fprintf(stdout,"  mesh edges       = %i\n",nMeshEdges);
  fprintf(stdout,"  mesh faces       = %i\n",nMeshFaces);
  fprintf(stdout,"\nMODEL STATISTICS:\n");
  fprintf(stdout,"  material regions = %i\n",nRegion);
  fprintf(stdout,"  edges            = %i\n",nEdge);
  fprintf(stdout,"  vertices         = %i\n\n",nVertex);

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

  //Reset all the meshoptions to be ready for next mesh
  meshoptions_.volumemeshflag = 0;
  meshoptions_.surfacemeshflag = 0;
  meshoptions_.boundarylayermeshflag = 0;

  return SV_OK;
}
#endif
// -----
// pyPrint
// -----
/**
 * @brief Function to print out the mesh statistics
 * @return SV_OK if executed correctly
 */
#ifdef SV_USE_PYTHON
int cvTetGenMeshObject::pyPrint()
{
  int num_nodes = 0;
  int nMeshFaces = 0;
  int num_elems = 0;
  int nMeshEdges = 0;
  int nRegion = 1;
  int nFace = 0;
  int nEdge = 0;
  int nVertex = 0;

  if (polydatasolid_ == NULL)
  {
    fprintf(stderr,"Solid has not been loaded\n");
    return SV_ERROR;
  }
  if(meshoptions_.surfacemeshflag && !meshoptions_.volumemeshflag)
  {
    num_nodes = polydatasolid_->GetNumberOfPoints();
    nMeshFaces = polydatasolid_->GetNumberOfCells();
  }
  else if(meshoptions_.boundarylayermeshflag)
  {
    if (volumemesh_ == NULL)
    {
      fprintf(stderr,"Mesh has not been created\n");
      return SV_ERROR;
    }
    num_nodes = volumemesh_->GetNumberOfPoints();
    num_elems = volumemesh_->GetNumberOfCells();
    nMeshFaces = 4.0*(volumemesh_->GetNumberOfCells());
    nMeshEdges = 3.0*(nMeshFaces);
  }
  else
  {
    if (outmesh_ == NULL)
    {
      fprintf(stderr,"Mesh has not been created\n");
      return SV_ERROR;
    }
    num_nodes = outmesh_->numberofpoints;
    num_elems = outmesh_->numberoftetrahedra;
    nMeshFaces = outmesh_->numberoftrifaces;
    nMeshEdges = outmesh_->numberofedges;
    nFace = outmesh_->numberoftrifaces;
    nEdge = outmesh_->numberofregions;
    nVertex = outmesh_->numberofpoints;
  }
   /* output the statistics */

  nFace = originalpolydata_->GetNumberOfPolys();
  nVertex = originalpolydata_->GetNumberOfPoints();
  nEdge = 3*(originalpolydata_->GetNumberOfPolys());

  fprintf(stdout,"\nMESH STATISTICS:\n");
  fprintf(stdout,"  elements         = %i\n",num_elems);
  fprintf(stdout,"  nodes            = %i\n",num_nodes);
  fprintf(stdout,"  mesh edges       = %i\n",nMeshEdges);
  fprintf(stdout,"  mesh faces       = %i\n",nMeshFaces);
  fprintf(stdout,"\nMODEL STATISTICS:\n");
  fprintf(stdout,"  material regions = %i\n",nRegion);
  fprintf(stdout,"  edges            = %i\n",nEdge);
  fprintf(stdout,"  vertices         = %i\n\n",nVertex);

  char rtnstr[2048];
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_nodes %i\n",num_nodes);
  PySys_WriteStdout(rtnstr);
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_elements %i\n",num_elems);
  PySys_WriteStdout(rtnstr);
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_mesh_edges %i\n",nMeshEdges);
  PySys_WriteStdout(rtnstr);
  rtnstr[0]='\0';
  sprintf(rtnstr,"number_of_mesh_faces %i\n",nMeshFaces);
  PySys_WriteStdout(rtnstr);

  //Reset all the meshoptions to be ready for next mesh
  meshoptions_.volumemeshflag = 0;
  meshoptions_.surfacemeshflag = 0;
  meshoptions_.boundarylayermeshflag = 0;

  return SV_OK;
}
#endif
// ----
// Copy
// ----

cvMeshObject *cvTetGenMeshObject::Copy() const
{
  cvTetGenMeshObject *result = new cvTetGenMeshObject( *this );
  return result;
}


// -----------
// Update
// -----------
/**
 * @brief Function that updates in between routines to make sure solid
 * is loaded
 * @return SV_OK if executed correctly
 */

int cvTetGenMeshObject::Update() {

  //I'm not sure why this is used. Would really mesh with some of the
  //meshing functions to Update the mesh or solid periodically.
  //However, must return SV_OK, otherwise running of anything breaks.

  return SV_OK;

}

// -----------------------
// GetSolid
// -----------------------
//
/**
 * @brief Function that returns the polydatasolid member data loaded
 * @return SV_OK if executed correctly
 */
cvPolyData* cvTetGenMeshObject::GetSolid() {

  if (polydatasolid_ == NULL)
  {
    //Mesh must be created first
    return SV_ERROR;
  }

  cvPolyData* result =NULL;

  result = new cvPolyData(polydatasolid_);

  return result;

}

//-----------
// HasSolid
//-----------
// Check if the mesh has a solid model defined for it.
//
bool cvTetGenMeshObject::HasSolid() {

  if (polydatasolid_ == NULL) {
    return false;
  }

  return true;
}


// -----------------------
// GetPolyData
// -----------------------
//
/**
 * @brief Function that returns the polydatasolid member data loaded
 * @return SV_OK if executed correctly
 */
cvPolyData* cvTetGenMeshObject::GetPolyData() {

  if (surfacemesh_ == NULL)
  {
    //Mesh must be created first
    return SV_ERROR;
  }

  cvPolyData* result =NULL;

  result = new cvPolyData(surfacemesh_);

  return result;

}

//---------------
// HasSurfaceMesh
//---------------
// Check if a surface mesh has been generated.
//
bool cvTetGenMeshObject::HasSurfaceMesh()
{
  return (surfacemesh_ != NULL);
}

//---------------
// HasVolumeMesh
//---------------
// Check if a volume mesh has been generated.
//
bool cvTetGenMeshObject::HasVolumeMesh()
{
  return (volumemesh_ != NULL);
}

// --------------------
//  GetUnstructuredGrid
// --------------------
/**
 * @brief Function that returns the volume mesh if the mesh has been
 * calculated
 * @return the mesh if executed correctly
 */

cvUnstructuredGrid* cvTetGenMeshObject::GetUnstructuredGrid() {

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.
  if (volumemesh_ == NULL)
  {
    //Mesh must be created first
    return NULL;
  }

  cvUnstructuredGrid *result = NULL;

  result = new cvUnstructuredGrid(volumemesh_);

  return result;

}
/**
 * @brief Function that writes the adjacency between tetrahedral elements
 * @param *filename char holding the name of the file to be written to
 * @return SV_OK if executed correctly
 * @note This function is not necessarily needed anymore. The functionality
 * to extract the adjacency from the vtu is added in the presolver
 */

int cvTetGenMeshObject::WriteMetisAdjacency(char *filename) {

  //No longer need to write adjacency
  return SV_OK;

  if (filename == NULL) {
        return SV_ERROR;
  }

  if (meshoptions_.volumemeshflag)
  {
    // open the output file
    if (openOutputFile(filename) != SV_OK) return SV_ERROR;

    //Different way to write Adjacency, used for comparison
    int i;
    int numCells;
    int *xadj;
    int *adjacency;
    vtkIdType cellId;
    vtkIdType meshCellId;
    vtkIdType p1,p2,p3;
    vtkIdType npts = 0;
    vtkIdType *pts = 0;
    vtkSmartPointer<vtkCellArray> volCells = vtkSmartPointer<vtkCellArray>::New();
    vtkSmartPointer<vtkIntArray> globalIds = vtkSmartPointer<vtkIntArray>::New();
    vtkSmartPointer<vtkIdList> ptIds = vtkSmartPointer<vtkIdList>::New();
    vtkSmartPointer<vtkIdList> cellIds = vtkSmartPointer<vtkIdList>::New();
    volumemesh_->BuildLinks();

    if (VtkUtils_UGCheckArrayName(volumemesh_,1,"GlobalElementID") != SV_OK)
    {
      fprintf(stderr,"Array name 'GlobalElementID' does not exist in volume mesh. \
		      Something wrong with ids on mesh");
      return SV_ERROR;
    }
    globalIds = vtkIntArray::SafeDownCast(volumemesh_->GetCellData()->
      GetScalars("GlobalElementID"));
    numCells = volumemesh_->GetNumberOfCells();
    volCells = volumemesh_->GetCells();

    xadj = new int[numCells+1];
    adjacency = new int[4*numCells];
    int adj = 0;
    int xcheck = 0;
    xadj[xcheck] = 0;

    ptIds->SetNumberOfIds(3);
    for (cellId = 0;cellId<numCells;cellId++)
    {
      meshCellId = globalIds->LookupValue(cellId+1);
      volumemesh_->GetCellPoints(meshCellId,npts,pts);
      for (i=0;i < npts; i++)
      {
	p1 = pts[i];
	p2 = pts[(i+1)%(npts)];
	p3 = pts[(i+2)%(npts)];

	ptIds->InsertId(0,p1);
	ptIds->InsertId(1,p2);
	ptIds->InsertId(2,p3);

	volumemesh_->GetCellNeighbors(meshCellId,ptIds,cellIds);

	//If it is zero, it is a face on the exterior. Otherwise, it has
	//neighbors
	if (cellIds->GetNumberOfIds() != 0)
	{
	  adjacency[adj++] = (int) globalIds->GetValue(cellIds->GetId(0))-1;
	}

      }
      xadj[++xcheck] = adj;
    }

    gzprintf(fp_,"xadj: %i\n",numCells+1);
    gzprintf(fp_,"adjncy: %i\n",adj);

    for (i=0;i < numCells+1; i++)
    {
	gzprintf(fp_,"%i\n",xadj[i]);
    }
    for (i=0;i < adj; i++)
    {
	gzprintf(fp_,"%i\n",adjacency[i]);
    }

    delete [] xadj;
    delete [] adjacency;

    return closeOutputFile();
  }
  else
  {
  //  fprintf(stdout,"No volumemesh, not writing adjacency file\n");
    return SV_OK;
  }
}

int cvTetGenMeshObject::GetNodeCoords(int node)
{
  if (volumemesh_ == 0)
  {
    fprintf(stderr,"Mesh needs to be computed before node coords can be retrieved\n");
    return SV_ERROR;
  }
  nodeID_ = node;
  nodeX_ = 0; nodeY_ = 0; nodeZ_ = 0;
  double pts[3];

  volumemesh_->GetPoint(node,pts);
  nodeX_ = pts[0];
  nodeY_ = pts[1];
  nodeZ_ = pts[2];

  return SV_OK;
}

// --------------------
//  LoadModel
// --------------------
/**
 * @brief Function that loads a model from solid and store it in the
 * member data polydatasolid
 * @param *filename char holding the name of the file to read
 * @return SV_OK if executed correctly
 * @note uses the PolyDataUtils to read the solid: see cv_polydatasolid_utils
 */

int cvTetGenMeshObject::LoadModel(char *filename) {

  if (filename == NULL) {
    return SV_ERROR;
  }

  // must load model before mesh!
  if (inmesh_ != NULL) {
    return SV_ERROR;
  }

  //polydatasolid cannot already exist
  if (polydatasolid_ != NULL)
  {
    polydatasolid_->Delete();
  }
  if (originalpolydata_ != NULL) {
    originalpolydata_->Delete();
  }

  polydatasolid_ = vtkPolyData::New();
  originalpolydata_ = vtkPolyData::New();
  if (PlyDtaUtils_ReadNative(filename,polydatasolid_) != SV_OK) {
    return SV_ERROR;
  }

  originalpolydata_->DeepCopy(polydatasolid_);
  return SV_OK;

}

int cvTetGenMeshObject::LoadModel(vtkPolyData *pd) {

  if (pd == NULL) {
    return SV_ERROR;
  }

  // must load model before mesh!
  if (inmesh_ != NULL) {
    return SV_ERROR;
  }

  //polydatasolid cannot already exist
  if (polydatasolid_ != NULL)
  {
    polydatasolid_->Delete();
  }
  if (originalpolydata_ != NULL)
  {
    originalpolydata_->Delete();
  }

  originalpolydata_=vtkPolyData::New();
  originalpolydata_->DeepCopy(pd);

  polydatasolid_ = vtkPolyData::New();
  polydatasolid_->DeepCopy(pd);

  return SV_OK;

}

// --------------------
//  GetBoundaryFaces
// --------------------
/**
 * @brief Function to extract the boundaries for the member vtkPolyData
 * @param angle double that specifies the extraction angle. Any faces
 * with a difference between face normals larger than this angle will be
 * considered a separate face
 * @return *result: SV_ERROR is member data hasn't been loaded, or if the
 * GetBoundaryFaces function does not work properly. SV_OK is executed
 * properly
 */

int cvTetGenMeshObject::GetBoundaryFaces(double angle)
{
  if (polydatasolid_ == NULL) {
    return SV_ERROR;
  }

  if(PlyDtaUtils_GetBoundaryFaces(polydatasolid_,angle,
	&numBoundaryRegions_) != SV_OK) {
    return SV_ERROR;
  }

  return SV_OK;
}

// --------------------
//  LoadMesh
// --------------------
/**
 * @brief Function to load the vtkUnstructuredGrid of a Mesh
 * @return SV_OK if function executes properly.
 * @note If a current volume
 * mesh exists, the current volumemesh is deleted and the new one is loaded
 */
int cvTetGenMeshObject::LoadMesh(char *filename,char *surfilename) {

  if (filename == NULL) {
    return SV_ERROR;
  }
  if (volumemesh_ != NULL)
    volumemesh_->Delete();
  volumemesh_ = vtkUnstructuredGrid::New();
  if (TGenUtils_LoadMesh(filename,volumemesh_) != SV_OK)
    return SV_ERROR;
  if (surfilename != 0)
  {
    if (surfacemesh_ != NULL)
      surfacemesh_->Delete();

    surfacemesh_ = vtkPolyData::New();
    if (PlyDtaUtils_ReadNative(surfilename,surfacemesh_) != SV_OK)
      return SV_ERROR;
  }
  return SV_OK;

}

// --------------------
//  NewMesh
// --------------------
/**
 * @brief Function to prepare a mesh from the polydatasolid. Takes input
 * mesh and converts to tetgen structures in preparation.
 * @return *result: SV_ERROR if solid hasn't been loaded. If a current mesh
 * exists, it is deleted so that a new one can be made.
 */

int cvTetGenMeshObject::NewMesh() {

  // cant overwrite mesh
  if (inmesh_ != NULL) {
    delete inmesh_;
  }
  if (outmesh_ != NULL)
  {
    delete outmesh_;
  }

  // need solid to convert to new tetgen mesh
  if (polydatasolid_ == NULL) {
    return SV_ERROR;
  }

  //Create new tetgen mesh objects and set first number of output mesh to 0
  inmesh_ = new tetgenio;
  inmesh_->firstnumber = 0;
  outmesh_ = new tetgenio;
  outmesh_->firstnumber = 0;

  //MarkerListName
  std::string markerListName;
  int useSizingFunction;
  int useBoundary;

  if (meshoptions_.boundarylayermeshflag)
  {
    useSizingFunction = 1;
    useBoundary = 0;
    markerListName = "CellEntityIds";
  }
  else if (meshoptions_.functionbasedmeshing || meshoptions_.refinement)
  {
    fprintf(stdout,"Using size function\n");
    useSizingFunction = 1;
    useBoundary = 1;
    markerListName = "ModelFaceID";
  }
  else
  {
    useSizingFunction = 0;
    useBoundary = 1;
    markerListName = "ModelFaceID";
  }

  vtkSmartPointer<vtkCleanPolyData> cleaner =
   vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->SetInputData(polydatasolid_);
  cleaner->Update();
  polydatasolid_->DeepCopy(cleaner->GetOutput());
  fprintf(stderr,"Converting to TetGen...\n");
  //Convert the polydata to tetgen for meshing with given option
  if (TGenUtils_ConvertSurfaceToTetGen(inmesh_,polydatasolid_) != SV_OK)
  {
    fprintf(stderr,"Error converting surface to tetgen object\n");
    return SV_ERROR;
  }

  // Add mesh sizing function
  if (useSizingFunction)
  {
    if (TGenUtils_AddPointSizingFunction(inmesh_,polydatasolid_,
          "MeshSizingFunction", meshoptions_.maxedgesize) != SV_OK)
    {
      fprintf(stderr,"Could not add mesh sizing function to mesh\n");
      return SV_ERROR;
    }
  }

  // Add facet markers
  if (useBoundary)
  {
    if(TGenUtils_AddFacetMarkers(inmesh_,polydatasolid_,
      markerListName) != SV_OK)
    {
      fprintf(stderr,"Could not add facet markers to mesh\n");
      return SV_ERROR;
    }
  }

  // Add holes
  if (meshoptions_.numberofholes > 0)
  {
    if (TGenUtils_AddHoles(inmesh_, holelist_) != SV_OK)
    {
      fprintf(stderr,"Could not add hole to mesh\n");
      return SV_ERROR;
    }
  }

  if (meshoptions_.numberofregions > 0)
  {
    if (TGenUtils_AddRegions(inmesh_, regionlist_, regionsizelist_) != SV_OK)
    {
      fprintf(stderr,"Could not add region to mesh\n");
      return SV_ERROR;
    }
  }

  //The mesh is now loaded, and TetGen is ready to be called
  meshloaded_ = 1;

  return SV_OK;
}

//------------------------------
// GenerateLocalSizeSizingArray
//------------------------------
//
int
cvTetGenMeshObject::GenerateLocalSizeSizingArray(int faceID, double edgeSize)
{
  meshoptions_.functionbasedmeshing = 1;

  if (TGenUtils_SetLocalMeshSize(polydatasolid_, faceID, edgeSize) != SV_OK) {
      return SV_ERROR;
  }   
  meshoptions_.secondarrayfunction = 1;

  return SV_OK;
}

// --------------------
//  SetMeshOptions
// --------------------
/**
 * @brief Function to set the options for tetgen. Store temporarily in
 * meshoptions_ object until the mesh is run
 * @param *flag char containing the flag to set
 * @param value if the flag requires a value, this double contains that
 * value to be set
 * @return *result: SV_ERROR if the mesh doesn't exist. New Mesh must be
 * called before the options can be set
 */

int cvTetGenMeshObject::SetMeshOptions(char *flags,int numValues,double *values) 
{
  if(!strncmp(flags,"GlobalEdgeSize",14)) {            //Global edge size
     if (numValues < 1)
       return SV_ERROR;

    meshoptions_.maxedgesize=values[0];
  }
  else if(!strncmp(flags,"LocalEdgeSize",13)) {

    if (numValues < 2)
    {
      fprintf(stderr,"Must give face id and local edge size\n");
      return SV_ERROR;
    }
    meshoptions_.functionbasedmeshing = 1;
    //Create a new mesh sizing function and call TGenUtils to compute function.
    //Store in the member data vtkDouble Array meshsizingfunction
    if (TGenUtils_SetLocalMeshSize(polydatasolid_,values[0],values[1]) != SV_OK)
      return SV_ERROR;
    meshoptions_.secondarrayfunction = 1;
  }
  else if(!strncmp(flags,"SurfaceMeshFlag",15)) {
#ifdef SV_USE_VMTK
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.surfacemeshflag = values[0];
#else
      fprintf(stderr,"Plugin VMTK is not being used!\
	  In order to use surface meshing, plugin VMTK must be available!\n");
      return SV_ERROR;
#endif
  }
  else if(!strncmp(flags,"VolumeMeshFlag",14)) {
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.volumemeshflag = values[0];
  }
  else if(!strncmp(flags,"QualityRatio",12)) {//q
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.minratio=values[0];
  }
  else if(!strncmp(flags,"MinDihedral",11)) {//q
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.mindihedral=values[0];
  }
  else if(!strncmp(flags,"Optimization",12)) {//O
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.optlevel=(int)values[0];
  }
  else if(!strncmp(flags,"Epsilon",7)) {//T
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.epsilon=values[0];
  }
  else if(!strncmp(flags,"CoarsenPercent",14)) {//R
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.coarsenpercent=values[0]/100;
  }
  else if(!strncmp(flags,"AddHole",7)) {
    if (numValues < 3)
    {
      fprintf(stderr,"Must provide x,y,z coordinate of hole\n");
      return SV_ERROR;
    }
    meshoptions_.numberofholes++;
    if (holelist_ == NULL)
      holelist_ = vtkPoints::New();
    holelist_->InsertNextPoint(values[0], values[1], values[2]);
  }
  else if(!strncmp(flags,"AddSubDomain",12)) {
    if (numValues < 4)
    {
      fprintf(stderr,"Must provide x,y,z, size of region, and coordinate of region\n");
      return SV_ERROR;
    }
    meshoptions_.numberofregions++;
    if (regionsizelist_ == NULL)
      regionsizelist_ = vtkDoubleArray::New();
    regionsizelist_->InsertNextTuple1(values[0]);
    if (regionlist_ == NULL)
      regionlist_ = vtkPoints::New();
    regionlist_->InsertNextPoint(values[1], values[2], values[3]);

  }
  else if(!strncmp(flags,"Verbose",7)) {//V
    meshoptions_.verbose=1;
  }
  else if(!strncmp(flags,"NoMerge",7)) {//M
    meshoptions_.nomerge=1;
  }
  else if(!strncmp(flags,"Check",5)) {//C
    meshoptions_.docheck=1;
  }
  else if(!strncmp(flags,"NoBisect",8)) {//Y
    meshoptions_.nobisect=1;
  }
  else if(!strncmp(flags,"Quiet",5)) {//Q
    meshoptions_.quiet=1;
  }
  else if(!strncmp(flags,"Diagnose",8)) {//d
    meshoptions_.diagnose=1;
  }
  else if(!strncmp(flags,"MeshWallFirst",13)) {//k
    meshoptions_.meshwallfirst=1;
  }
  else if(!strncmp(flags,"StartWithVolume",15)) {//r
    meshoptions_.startwithvolume=1;
  }
  else if(!strncmp(flags,"Hausd",5)) {
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.hausd=values[0];
  }
  else if (!strncmp(flags,"UseMMG",6)){
      if (numValues < 1)
        return SV_ERROR;
      meshoptions_.usemmg=values[0];
  }
  else if (!strncmp(flags,"NewRegionBoundaryLayer",22)) {
    meshoptions_.newregionboundarylayer=1;
  }
  else if (!strncmp(flags,"BoundaryLayerDirection",22)) {
    if (numValues < 1)
      return SV_ERROR;
    meshoptions_.boundarylayerdirection=values[0];
  }
  else if (!strncmp(flags,"AllowMultipleRegions",20)) {
      meshoptions_.allowMultipleRegions = (int(values[0]) == 1);
  }
  else {
      fprintf(stderr,"%s: flag is not recognized\n",flags);
  }
  return SV_OK;
}

// --------------------
//  SetBoundaryLayer
// --------------------
/**
 * @brief Function to set the boundary layer of the mesh. In this case
 * @brief the only things set are the meshoptions_ parameters.
 * @note The polydatasolid_ object is prepared for meshing by separating the
 * @note surface that is to have the boundary layer away from the rest of
 * @note the mesh.
 * @note Must have VMTK to use Boundary Layer!
 * @param type UNUSED
 * @param id This is the value of the region on which we want the BL.
 * @param side UNUSED
 * @param nL This is the number of layers for the BL mesh.
 * @param H This is sublayer ratio or how much one layer decreases from last.
 * @return SV_OK if surface exists and is extracted with Threshold correctly
 */
int cvTetGenMeshObject::SetBoundaryLayer(int type, int id, int side,
    int nL, double* H)
{
#ifdef SV_USE_VMTK
  meshoptions_.boundarylayermeshflag = 1;
  meshoptions_.numsublayers = nL;
  meshoptions_.blthicknessfactor = *H;
  meshoptions_.sublayerratio = *(H+1);
  meshoptions_.useconstantblthickness = *(H+2);
  meshoptions_.meshwallfirst = 1;
#else
  fprintf(stderr,"Plugin VMTK is not being used! \
      In order to use boundary layer meshing, \
      plugin VMTK must be available!\n");
  return SV_ERROR;
#endif

  return SV_OK;
}

// --------------------
//  SetWalls
// --------------------
/**
 * @brief Function to set the walls of the mesh with an integer array
 * @param numWalls number of walls being set with value
 * @param walls, integer list for ModelFaceIds in the wall.
 * @return SV_OK if surface exists and array is set correctly
 */
int cvTetGenMeshObject::SetWalls(int numWalls, int *walls)
{
  int max=0;
  double range[2];
  vtkIntArray *wallArray = vtkIntArray::New();
  meshoptions_.meshwallfirst = 1;

  if (VtkUtils_PDCheckArrayName(polydatasolid_,1,"ModelFaceID") != SV_OK)
  {
    fprintf(stderr,"ModelFaceID array not on object, so cannot set walls\n");
    return SV_ERROR;
  }
  vtkIntArray *modelIds;
  modelIds = vtkIntArray::SafeDownCast( polydatasolid_->GetCellData()->GetArray("ModelFaceID"));
  modelIds->GetRange(range);
  max = range[1];

  int *isWall = new int[max];
  for (int i=0; i < max; i++)
    isWall[i] = 0;
  for (int i=0; i < numWalls; i++)
  {
    int wallid = *(walls+i);
    isWall[wallid-1] = 1;
    wallFaceIDs_.insert(wallid);
  }

  int numCells = polydatasolid_->GetNumberOfCells();
  for (vtkIdType cellId = 0; cellId < numCells ; cellId++)
  {
    int value = modelIds->GetValue(cellId);
    if (isWall[value - 1] == 1)
      wallArray->InsertValue(cellId,1);
    else
      wallArray->InsertValue(cellId,0);
  }

  wallArray->SetName("WallID");
  polydatasolid_->GetCellData()->AddArray(wallArray);
  wallArray->Delete();

#ifdef SV_USE_MMG
  if (meshoptions_.usemmg == 0)
  {
#endif
    auto thresholder = vtkSmartPointer<vtkThreshold>::New();
    thresholder->SetInputData(polydatasolid_);
     //Set Input Array to 0 port,0 connection,1 for Cell Data, and WallID is the type name
    thresholder->SetInputArrayToProcess(0,0,0,1,"WallID");
    thresholder->ThresholdBetween(1,1);
    thresholder->Update();

    auto surfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
    surfacer->SetInputData(thresholder->GetOutput());
    surfacer->Update();

    polydatasolid_->DeepCopy(surfacer->GetOutput());
#ifdef SV_USE_MMG
  }
#endif

  delete [] isWall;
  return SV_OK;
}

// --------------------
//  SetCylinderRefinement
// --------------------
/**
 * @brief Function to set the region to refine based on input cylinder
 * @param size This is the smaller refined of the edges within cylinder region.
 * @param radius This is the radius of the refinement cylinder.
 * @param center This is the center of the refinement cylinder.
 * Halfway along the length.
 * @param normal This is the normal direction from the center that the length
 * of the cylinder follows.
 * @return SV_OK if the mesh sizing function based on the circle is computed
 * correctly
 */
int cvTetGenMeshObject::SetCylinderRefinement(double size, double radius,
    double length, double* center, double *normal)
{
  //Set meshoptions_ parameters based on input.
  int i;
  meshoptions_.refinement = 1;
  meshoptions_.refinedsize = size;
  meshoptions_.cylinderradius = radius;
  meshoptions_.cylinderlength = length;
  for (i=0;i<3;i++)
  {
    meshoptions_.cylindercenter[i] = center[i];
    meshoptions_.cylindernormal[i] = normal[i];
  }

  //Create a new mesh sizing function and call TGenUtils to compute function.
  //Store in the member data vtkDouble Array meshsizingfunction
  if (TGenUtils_SetRefinementCylinder(polydatasolid_,"MeshSizingFunction",
	size,radius,center,length,normal,meshoptions_.secondarrayfunction,
	meshoptions_.maxedgesize,"RefineID",meshoptions_.refinecount) != SV_OK)
  {
    return SV_ERROR;
  }

  meshoptions_.secondarrayfunction = 1;
  meshoptions_.refinecount += 1;
  return SV_OK;
}


// --------------------
//  SetSphereRefinement
// --------------------
/**
 * @brief Function to set the region to refine based on input sphere
 * @param size This is the smaller refined of the edges within sphere region.
 * @param radius This is the radius of the refinement sphere.
 * @param center This is the center of the refinement sphere.
 * @return SV_OK if the mesh sizing function based on the circle is computed
 * correctly
 */
int cvTetGenMeshObject::SetSphereRefinement(double size, double radius,
    double* center)
{
  //Set meshoptions_ parameters based on input.
  int i;
  meshoptions_.refinement = 1;
  meshoptions_.refinedsize = size;
  meshoptions_.sphereradius = radius;
  for (i=0;i<3;i++)
  {
    meshoptions_.spherecenter[i] = center[i];
  }

  //Create a new mesh sizing function and call TGenUtils to compute function.
  //Store in the member data vtkDouble Array meshsizingfunction
  if (TGenUtils_SetRefinementSphere(polydatasolid_,"MeshSizingFunction",
	size,radius,center,meshoptions_.secondarrayfunction,
	meshoptions_.maxedgesize,"RefineID",meshoptions_.refinecount) != SV_OK)
  {
    return SV_ERROR;
  }

  meshoptions_.secondarrayfunction = 1;
  meshoptions_.refinecount += 1;
  return SV_OK;
}

// --------------------
//  SetSizeFunctionBasedMesh
// --------------------
/**
 * @brief Function to set up the mesh edge size for VMTK and TetGen
 * based on an input size array. For radius-based, this will take
 * the array, normalize it based on the minimum value and apply the
 * metric to the surface
 * @param size This is the size to be specified on selected surface if
 * there is no value specified in the array.
 * @param sizefunctionname This is the name of the function name
 * on the surface
 * @return SV_OK if the mesh sizing function is computed correctly
 */
int cvTetGenMeshObject::SetSizeFunctionBasedMesh(double size,char *sizefunctionname)
{

  fprintf(stderr,"Setting size based function...\n");
  //Set meshoptions_ parameters based on input.
  int i;
  meshoptions_.functionbasedmeshing = 1;

  //Create a new mesh sizing function and call TGenUtils to compute function.
  //Store in the member data vtkDouble Array meshsizingfunction
  if (TGenUtils_SetSizeFunctionArray(polydatasolid_,"MeshSizingFunction",
	size,sizefunctionname,meshoptions_.secondarrayfunction) != SV_OK)
  {
    return SV_ERROR;
  }

  //originalpolydata_->DeepCopy(polydatasolid_);
  meshoptions_.secondarrayfunction = 1;
  return SV_OK;
}

/**
 * @brief Function to generate a mesh
 * @return *result: SV_ERROR if the mesh doesn't exist. New Mesh must be
 * called before a mesh can be generated
 * @note Function checks to see if any of the mesh options have been set.
 * It they have, the corresponding tetgenbehavior object values are set.
 */

int cvTetGenMeshObject::GenerateMesh() {

  if (surfacemesh_ != NULL)
  {
    surfacemesh_->Delete();
    surfacemesh_ = nullptr;
  }

  if (volumemesh_ != NULL)
  {
    volumemesh_->Delete();
    volumemesh_ = nullptr;
  }

//All these complicated options exist if using VMTK. Should be stopped prior
//to this if trying to use VMTK options and don't have VMTK.
#ifdef SV_USE_VMTK
  //If doing surface remeshing!
  if (meshoptions_.surfacemeshflag)
  {
     if (GenerateSurfaceRemesh() != SV_OK)
       return SV_ERROR;

    //If we are doing a volumemesh based off the surface mesh!
    if (meshoptions_.volumemeshflag)
    {
      //If we are doing boundary layer mesh, it gets complicated!
      if (meshoptions_.boundarylayermeshflag)
      {
        if (GenerateBoundaryLayerMesh() != SV_OK)
          return SV_ERROR;

        if (GenerateAndMeshCaps() != SV_OK)
          return SV_ERROR;
      }

      if (meshoptions_.boundarylayermeshflag || meshoptions_.functionbasedmeshing
        || meshoptions_.refinement)
      {
        if (GenerateMeshSizingFunction() != SV_OK)
          return SV_ERROR;
      }
      NewMesh();
    }
    //In this case, we only are doing a surface remesh, and we are essentially
    //done
    else
    {
      surfacemesh_ = vtkPolyData::New();
      surfacemesh_->DeepCopy(polydatasolid_);
    }
  }
#endif
  //If we are doing sphere refinement and only a volumemesh, then we need
  //to re-set up the mesh based on sizing function for sphere refinement
 
  if (meshoptions_.volumemeshflag && !meshoptions_.surfacemeshflag)
  {
    int meshInfo[3];
    if (TGenUtils_CheckSurfaceMesh(polydatasolid_, meshInfo) != SV_OK)
    {
      fprintf(stderr,"Error checking surface\n");
      return SV_ERROR;
    }

    if (!(meshoptions_.numberofholes > 0 || meshoptions_.numberofregions > 0))
    {
      if (!meshoptions_.allowMultipleRegions && meshInfo[0] > 1)
      {
        fprintf(stderr,"There are too many regions here!\n");
        fprintf(stderr,"Terminating meshing!\n");
        return SV_ERROR;
      }
      if (meshInfo[1] > 0 && !meshoptions_.boundarylayermeshflag)
      {
        fprintf(stderr,"There are free edes on surface!\n");
        fprintf(stderr,"Terminating meshing!\n");
        return SV_ERROR;
      }
      if (meshInfo[2] > 0)
      {
        fprintf(stderr,"There are bad edes on surface!\n");
        fprintf(stderr,"Terminating meshing!\n");
        return SV_ERROR;
      }
    }
    NewMesh();
  }

  //Here we set all the mesh flags for TetGen!
  if (meshoptions_.volumemeshflag)
  {
    tetgenbehavior* tgb = new tetgenbehavior;
    //Default flags: plc and neihgborlist/adjtetlist output
    tgb->plc=1;
    tgb->neighout=2;

    //User defined options below
    if (meshoptions_.maxedgesize != 0)
    {
      double mES = meshoptions_.maxedgesize;
      //Volume of an element is approximately (a^3)/(6*sqrt(2))
      double maxvol = (mES*mES*mES)/(6*sqrt(2.));
      tgb->fixedvolume=1;
      tgb->maxvolume=maxvol;
    }
    if (meshoptions_.minratio != 0)
    {
      tgb->quality=1;
      tgb->minratio=meshoptions_.minratio;
    }
    if (meshoptions_.mindihedral != 0.0)
    {
      tgb->quality=1;
      tgb->mindihedral = meshoptions_.mindihedral;
    }

    if (meshoptions_.optlevel != 0)
    {
      tgb->optlevel=meshoptions_.optlevel;
    }
    if (meshoptions_.epsilon != 0)
    {
      tgb->epsilon=meshoptions_.epsilon;
    }
    if (meshoptions_.verbose)
    {
      tgb->verbose=1;
    }
    if (meshoptions_.docheck)
    {
      tgb->docheck=1;
    }
    if (meshoptions_.quiet)
    {
      tgb->quiet=1;
    }
    if (meshoptions_.nobisect)
    {
      tgb->nobisect=1;
    }
    if (meshoptions_.diagnose)
    {
      tgb->diagnose=1;
    }
    if (meshoptions_.boundarylayermeshflag)
    {
      tgb->quality = 3;
      tgb->metric = 1;
      tgb->mindihedral = 10.0;
      tgb->nobisect=1;
    }
    if (meshoptions_.refinement)
    {
      tgb->quality = 3;
      tgb->metric = 1;
      tgb->mindihedral = 10.0;
    }
    if (meshoptions_.functionbasedmeshing)
    {
      tgb->quality = 3;
      tgb->metric = 1;
      tgb->mindihedral = 10.0;
    }
    if (meshoptions_.numberofregions > 0)
    {
      tgb->fixedvolume = 0;
      tgb->varvolume = 1;
      tgb->regionattrib = 1;
    }
#if defined(TETGEN150) || defined(TETGEN151)
    if (meshoptions_.coarsenpercent != 0)
    {
      tgb->coarsen=1;
      tgb->coarsen_percent=meshoptions_.coarsenpercent;
    }
    if (meshoptions_.nomerge)
    {
      tgb->nomergefacet=1;
      tgb->nomergevertex=1;
    }
#endif
#ifdef TETGEN143
    if (meshoptions_.boundarylayermeshflag)
    {
      tgb->goodratio = 2.0;
    }
    else
    {
      tgb->goodratio = 4.0;
    }
    tgb->goodangle = 0.88;
    tgb->useshelles = 1;
#endif

    if (meshloaded_ != 1)
    {
      fprintf(stderr,"For some reason, mesh is not loaded! TetGen cannot\
	  be run.\n");
    }

    fprintf(stdout,"TetGen Meshing Started...\n");
    try
    {
//      std::freopen("mesh_stats.txt","w",stdout);
      tetrahedralize(tgb, inmesh_, outmesh_);
//      std::fclose(stdout);
    }
    catch (int r)
    {
      fprintf(stderr,"ERROR: TetGen quit and returned error code %d\n",r);
      return SV_ERROR;
    }
    fprintf(stdout,"TetGen Meshing Finished...\n");
  }

  else
  {
    fprintf(stdout,"Only surface\n");
  }

#ifdef SV_USE_VMTK
  // This is a post meshing step that needs to be done for boundary layer mesh.
  if (meshoptions_.boundarylayermeshflag)
  {
    AppendBoundaryLayerMesh();
  }
#endif

  // must have created mesh
  if (meshoptions_.volumemeshflag && !meshoptions_.boundarylayermeshflag)
  {
    if (outmesh_ == NULL) {
      return SV_ERROR;
    }
    if (surfacemesh_ != NULL)
    {
      surfacemesh_->Delete();
    }
    if (volumemesh_ != NULL)
    {
      volumemesh_->Delete();
    }

    surfacemesh_ = vtkPolyData::New();
    volumemesh_ = vtkUnstructuredGrid::New();

    if (TGenUtils_ConvertToVTK(outmesh_,volumemesh_,surfacemesh_,
	  &numBoundaryRegions_,1) != SV_OK)
      return SV_ERROR;
  }

  // Boundary layer meshing creates 'ModelFaceID' IDs for regions of local mesh
  // refinement (e.g. local sphere refinement). Set the 'ModelFaceID' IDs to
  // the IDs from the original input model. This is not done for boundary layer
  // meshing extruded outward.
  //
  if (meshoptions_.boundarylayermeshflag && (meshoptions_.boundarylayerdirection == 1)) { 
    if (TGenUtils_ResetOriginalRegions(surfacemesh_ ,originalpolydata_, "ModelFaceID") != SV_OK) {
      std::cout << "Failed to reset original face IDs for boundary layer mesh." << std::endl;
    }
  }

  return SV_OK;
}

//-----------
// WriteMesh
//-----------
// Write the volume mesh to a file.
//
// [TODO:DaveP] What is 'smsver' ?
//
int cvTetGenMeshObject::WriteMesh(char *filename, int smsver) 
{
  if (volumemesh_ == NULL) {
    return SV_ERROR;
  }

  TGenUtils_WriteVTU(filename, volumemesh_);

  return SV_OK;
}

int cvTetGenMeshObject::WriteStats(char *filename) {
  // must have created mesh
  if (inmesh_ == NULL) {
    return SV_ERROR;
  }
  return SV_OK;
}

/**
 * @brief Procedure gets one face based on faceid defined in PolyDataUtils
 * @param orgfaceid int which is the number of the face to extract
 * @return *result: cvPolyData containg the face vtkPolyData
 */
cvPolyData* cvTetGenMeshObject::GetFacePolyData (int orgfaceid) {

  // recall the node numbers start at 1 in the P_id,
  // but in vtkPolyData file they start at 0.
  vtkPolyData *face = vtkPolyData::New();
  cvPolyData *result = NULL;

  if (TGenUtils_GetFacePolyData(orgfaceid,surfacemesh_,face) != SV_OK)
  {
    return SV_ERROR;
  }

  result = new cvPolyData(face);

  face->Delete();

  return result;

}

//------------------
// GetModelFaceInfo
//------------------
// Get the face information used by the mesh.
//
// Returns:
//   desc: The description of the information returned.
//   faceInfo: A list of strings containing face IDs.
//
int cvTetGenMeshObject::GetModelFaceInfo(std::map<std::string,std::vector<std::string>>& faceInfo)
{
  faceInfo.clear();

  // [TODO:DaveP] What else can the kernel be?
  //
  if (solidmodeling_kernel_ == SM_KT_POLYDATA) {
      if (VtkUtils_PDCheckArrayName(originalpolydata_,1,"ModelFaceID") != SV_OK) {
          fprintf(stderr,"ModelFaceID does not exist\n");
          return SV_ERROR;
      }

      // Get face IDs.
      int *faces;
      int numFaces = 0;
      if (PlyDtaUtils_GetFaceIds(originalpolydata_, &numFaces, &faces) != SV_OK) {
          fprintf(stderr,"Could not get face ids\n");
          return SV_ERROR;
      }

      // Store face IDs into the returned string.
      for (int i = 0; i < numFaces; i++) {
          faceInfo[ModelFaceInfo::ID].push_back(std::to_string(faces[i]));
      }

      delete [] faces;
  }

  return SV_OK;
}

int cvTetGenMeshObject::GetModelFaceIDs(std::vector<int>& faceIDs)
{
  faceIDs.clear();

  // [TODO:DaveP] What else can the kernel be?
  //
  if (solidmodeling_kernel_ == SM_KT_POLYDATA) {
      if (VtkUtils_PDCheckArrayName(originalpolydata_,1,"ModelFaceID") != SV_OK) {
          fprintf(stderr,"ModelFaceID does not exist\n");
          return SV_ERROR;
      }

      // Get face IDs.
      int *faces;
      int numFaces = 0;
      if (PlyDtaUtils_GetFaceIds(originalpolydata_, &numFaces, &faces) != SV_OK) {
          fprintf(stderr,"Could not get face ids\n");
          return SV_ERROR;
      }

      // Store face IDs into the returned string.
      for (int i = 0; i < numFaces; i++) {
          faceIDs.push_back(faces[i]);
      }

      delete [] faces;
  }

  return SV_OK;
}


/**
 * @brief Function to set the PolyData member object
 * @param *newPolyData Pointer to vtkPolyData object that you want to be
 * set as the class member data
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::SetVtkPolyDataObject(vtkPolyData *newPolyData)
{
  if (polydatasolid_ != NULL)
  {
    polydatasolid_->Delete();
  }
  polydatasolid_ = vtkPolyData::New();
  polydatasolid_->DeepCopy(newPolyData);

  return SV_OK;
}

/**
 * @brief Function to set an input unstructured grid
 * @param *newPolyData Pointer to vtkPolyData object that you want to be
 * set as the class member data
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::SetInputUnstructuredGrid(vtkUnstructuredGrid *ug)
{
  if (inputug_ != NULL)
  {
    inputug_->Delete();
  }
  inputug_ = vtkUnstructuredGrid::New();
  inputug_->DeepCopy(ug);

  return SV_OK;
}

/**
 * @brief Helper function to generate surface mesh
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for generating surface meshes
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateSurfaceRemesh()
{
#ifdef SV_USE_VMTK
  int meshcapsonly = 0;
  int preserveedges;
  int trianglesplitfactor;
  int useSizingFunction = 0;
  double collapseanglethreshold;
  std::string markerListName;
  vtkSmartPointer<vtkDoubleArray> meshsizingfunction =
    vtkSmartPointer<vtkDoubleArray>::New();
  //If we are doing a boundary layer mesh, we do not want to retain edges
  //for our surface remeshing
  //Else, we would like to preserve the edges of the boundary layer mesh
  if (meshoptions_.meshwallfirst)
  {
    preserveedges = 0;
    trianglesplitfactor = 5.0;
    collapseanglethreshold = 0.2;
    markerListName = "CellEntityIds";
  }
  else
  {
    preserveedges = 1;
    trianglesplitfactor = NULL;
    collapseanglethreshold = NULL;
    markerListName = "ModelFaceID";
  }
  //If doing sphere refinement, we need to base surface mesh on mesh
  //sizing function
  if (meshoptions_.refinement || meshoptions_.functionbasedmeshing)
  {
    useSizingFunction = 1;
    if (VtkUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") != SV_OK)
    {
      fprintf(stderr,"Array name 'MeshSizingFunction' does not exist. \
	              Something may have gone wrong when setting up BL");
      return SV_ERROR;
    }
    fprintf(stdout,"Getting sizing function Surface\n");
    meshsizingfunction = vtkDoubleArray::SafeDownCast(polydatasolid_->\
	  GetPointData()->GetScalars("MeshSizingFunction"));

  }
  //If not doing sphere refinement or function based meshing,
  //we do not base surface mesh on sizing function
  else
  {
    useSizingFunction = 0;
    meshsizingfunction = NULL;
  }

#ifdef SV_USE_MMG
  if (meshoptions_.usemmg)
  {
    double meshFactor = 0.8;
    double meshsize = meshFactor*meshoptions_.maxedgesize;
    double mmg_maxsize = 1.5*meshsize;
    double mmg_minsize = 0.5*meshsize;
    if (meshoptions_.hausd == 0)
      meshoptions_.hausd = 10.0*meshsize;
    double hausd = meshoptions_.hausd;
    double dumAng = 45.0;
    double hgrad = 1.01;

    //Generate Surface Remeshing
    if(MMGUtils_SurfaceRemeshing(polydatasolid_, mmg_minsize,
	  mmg_maxsize, hausd, dumAng, hgrad,
	  useSizingFunction, meshsizingfunction, meshoptions_.refinecount) != SV_OK)
    {
      fprintf(stderr,"Problem with surface meshing\n");
      return SV_ERROR;
    }
  }
  else
  {
#endif
    //Generate Surface Remeshing
    if(VMTKUtils_SurfaceRemeshing(polydatasolid_,meshoptions_.maxedgesize,
          meshcapsonly,preserveedges,trianglesplitfactor,
          collapseanglethreshold,NULL,markerListName,
          useSizingFunction,meshsizingfunction) != SV_OK)
    {
      fprintf(stderr,"Problem with surface meshing\n");
      return SV_ERROR;
    }
    ResetOriginalRegions("ModelFaceID");
#ifdef SV_USE_MMG
  }
#endif

  int meshInfo[3];
  if (TGenUtils_CheckSurfaceMesh(polydatasolid_, meshInfo) != SV_OK)
  {
    fprintf(stderr,"Mesh surface is bad\n");
    return SV_ERROR;
  }

  if (!(meshoptions_.numberofholes > 0 ||
        meshoptions_.numberofregions > 0))
  {
    if (!meshoptions_.allowMultipleRegions && meshInfo[0] > 1)
    {
      fprintf(stderr,"There are too many regions here!\n");
      fprintf(stderr,"Terminating meshing!\n");
      return SV_ERROR;
    }
    if (meshInfo[1] > 0 && !meshoptions_.meshwallfirst)
    {
      fprintf(stderr,"There are free edes on surface!\n");
      fprintf(stderr,"Terminating meshing!\n");
      return SV_ERROR;
    }
    if (meshInfo[2] > 0)
    {
      fprintf(stderr,"There are bad edes on surface!\n");
      fprintf(stderr,"Terminating meshing!\n");
      return SV_ERROR;
    }
  }

  if (meshoptions_.meshwallfirst && !meshoptions_.boundarylayermeshflag)
  {
    if (GenerateAndMeshCaps() != SV_OK)
      return SV_ERROR;
    ResetOriginalRegions("ModelFaceID");
  }

#else
  fprintf(stderr,"Cannot do a surface remesh without using VMTK\n");
  return SV_ERROR;
#endif

  return SV_OK;
}

//-----------------------
// SetCapBoundaryNormals 
//-----------------------
// Set the normals for caps boundary points.
//
// The point normals for the mesh at the cap ends typically do not lie in the
// cap plane due to the averaging of polygon face normals. This causes the
// boundary layer mesh at the cap not to be flat.
//
// Cap boundary points normals are set for 'surface' which is used as the surface 
// for computing the boundary layer mesh. The normals are computed as the vector 
// from a boundary point to the cap boundary center.
//
// Data modified:
//   surface - 'Normals' data array. 
//
void cvTetGenMeshObject::SetCapBoundaryNormals(vtkPolyData* surface) 
{
  // Extract surface cap boundaries. 
  //
  auto feature_edges = vtkFeatureEdges::New();
  feature_edges->SetInputData(surface);
  feature_edges->BoundaryEdgesOn();
  feature_edges->ManifoldEdgesOff();
  feature_edges->NonManifoldEdgesOff();
  feature_edges->FeatureEdgesOff();
  feature_edges->Update();

  auto boundary_edges = feature_edges->GetOutput();
  auto clean_filter = vtkCleanPolyData::New();
  clean_filter->SetInputData(boundary_edges);
  clean_filter->Update();
  auto cleaned_edges = clean_filter->GetOutput();

  auto conn_filter = vtkPolyDataConnectivityFilter::New();
  conn_filter->SetInputData(cleaned_edges);
  conn_filter->SetExtractionModeToSpecifiedRegions();
  std::vector<vtkPolyData*> cap_boundaries;
  int edge_id = 0;

  while (true) {
    conn_filter->AddSpecifiedRegion(edge_id);
    conn_filter->Update();
    auto component = vtkPolyData::New();
    component->DeepCopy(conn_filter->GetOutput());
    if (component->GetNumberOfCells() <= 0) {
      break;
    }
    auto clean_filter = vtkCleanPolyData::New();
    clean_filter->SetInputData(component);
    clean_filter->Update();
    auto cleaned_edges = clean_filter->GetOutput();
    cap_boundaries.push_back(cleaned_edges);
    conn_filter->DeleteSpecifiedRegion(edge_id);
    edge_id += 1;
  }

  // Set cap boundary normals.
  //
  // For each point on cap boundaries find the corresponding
  // point on the surface, compute the normal there, and set it
  // for the surface.
  //
  auto surface_points = surface->GetPoints();
  auto surface_normals = surface->GetPointData()->GetArray("Normals");

  auto pointLocator = vtkSmartPointer<vtkPointLocator>::New();
  pointLocator->SetDataSet(surface);
  pointLocator->BuildLocator();

  for (auto& cap_boundary : cap_boundaries) { 
    auto com_filter = vtkSmartPointer<vtkCenterOfMass>::New();
    com_filter->SetInputData(cap_boundary);
    com_filter->Update();
    auto cap_center = com_filter->GetCenter();
    auto cap_points = cap_boundary->GetPoints();

    for (int i = 0; i < cap_boundary->GetNumberOfPoints(); i++) {
      auto cap_point = cap_boundary->GetPoint(i);
      // Find the cap point on the surface.
      int point_id = pointLocator->FindClosestPoint(cap_point);
      auto surface_point = surface_points->GetPoint(point_id);

      // Compute the normal.
      double normal[3];
      double mag = 0.0;
      for (int j = 0; j < 3; j++) {
        normal[j] = surface_point[j] - cap_center[j];
        mag += normal[j]*normal[j];
      }
      mag = sqrt(mag);
      for (int j = 0; j < 3; j++) {
        normal[j] /= mag; 
      }

      // Set the surface normal. 
      surface_normals->SetComponent(point_id, 0, normal[0]);
      surface_normals->SetComponent(point_id, 1, normal[1]);
      surface_normals->SetComponent(point_id, 2, normal[2]);
    }
  }
}

/**
 * @brief Helper function to generate boundary layer mesh
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for generating a boundary layer mesh
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateBoundaryLayerMesh()
{
  std::cout << "[GenerateBoundaryLayerMesh] " << std::endl;
  std::cout << "[GenerateBoundaryLayerMesh] ========== cvTetGenMeshObject::GenerateBoundaryLayerMesh ==========" << std::endl;

#ifdef SV_USE_VMTK
  if (boundarylayermesh_ != NULL)
  {
    boundarylayermesh_->Delete();
  }

  if (innerblmesh_ != NULL)
  {
    innerblmesh_->Delete();
  }

  std::string markerListName;

  if (meshoptions_.boundarylayermeshflag)
  {
    markerListName = "CellEntityIds";
  }
  else
  {
    markerListName = "ModelFaceID";
  }
  std::cout << "[GenerateBoundaryLayerMesh] markerListName: " << markerListName << std::endl;

  // Clean and convert the polydata to a vtu.
  //
  auto normaler = vtkSmartPointer<vtkPolyDataNormals>::New();
  normaler->SetInputData(polydatasolid_);
  normaler->SetConsistency(1);
  normaler->SetAutoOrientNormals(1);
  normaler->SetFlipNormals(0);
  normaler->SetComputeCellNormals(0);
  normaler->SplittingOff();
  normaler->Update();

  auto cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->SetInputData(normaler->GetOutput());
  cleaner->Update();

  auto originalsurfpd = vtkSmartPointer<vtkPolyData>::New();
  originalsurfpd->DeepCopy(cleaner->GetOutput());

  // Create a sizing function vtk data array named 'MeshSizingFunction' for the 
  // current mesh used to define the size of the mesh at each node. 
  //
  // This modifies 'originalsurfpd'.
  //
  if (VMTKUtils_ComputeSizingFunction(originalsurfpd, NULL, "MeshSizingFunction") != SV_OK) {
    fprintf(stderr,"Problem when computing sizing function");
    return SV_ERROR;
  }

  // Set cap boundary normals.
  SetCapBoundaryNormals(originalsurfpd);

  // Convert the surface to an vtkUnstructuredGrid.
  auto converter = vtkSmartPointer<vtkvmtkPolyDataToUnstructuredGridFilter>::New();
  converter->SetInputData(originalsurfpd);
  converter->Update();

  // Copy the vtkUnstructuredGrid.
  innerblmesh_ = vtkUnstructuredGrid::New();
  innerblmesh_->DeepCopy(converter->GetOutput());
  boundarylayermesh_ = vtkUnstructuredGrid::New();
  boundarylayermesh_->DeepCopy(converter->GetOutput());


  // Compute the boundary layer mesh using vmtk. 
  //
  // Modifies: 'boundarylayermesh_' and 'innerSurface'.
  //
  int negateWarpVectors = meshoptions_.boundarylayerdirection;
  int innerSurfaceCellId = 1;
  int sidewallCellEntityId = 9999;
  int useConstantThickness = meshoptions_.useconstantblthickness;
  std::string layerThicknessArrayName = "MeshSizingFunction";
  auto innerSurface = vtkSmartPointer<vtkUnstructuredGrid>::New();
  std::cout << "[GenerateBoundaryLayerMesh] negateWarpVectors: " << negateWarpVectors << std::endl;
  std::cout << "[GenerateBoundaryLayerMesh] useConstantThickness: " << useConstantThickness << std::endl;

  if (!boundarylayermesh_->GetPointData()->GetArray("Normals")) {
    std::cout << "[GenerateBoundaryLayerMesh] **** boundarylayermesh_ does not have point normals." << std::endl;
  } else {
    std::cout << "[GenerateBoundaryLayerMesh] boundarylayermesh_ has point normals." << std::endl;
  }

  TGenUtils_WriteVTU("boundarylayermesh_normals.vtu", boundarylayermesh_);

  if (VMTKUtils_BoundaryLayerMesh(boundarylayermesh_, innerSurface, meshoptions_.maxedgesize, meshoptions_.blthicknessfactor,
	meshoptions_.numsublayers, meshoptions_.sublayerratio, sidewallCellEntityId, innerSurfaceCellId, negateWarpVectors,
	markerListName, useConstantThickness, layerThicknessArrayName) != SV_OK)
  {
    fprintf(stderr,"Problem with boundary layer meshing\n");
    return SV_ERROR;
  }

  TGenUtils_WriteVTU("boundarylayermesh.vtu", boundarylayermesh_);

  TGenUtils_WriteVTU("innerSurface.vtu", innerSurface);

  // We take the inside surface of the boundary layer mesh and set the
  // member vtkPolyData polydatasolid_ to be equal to this. We will
  // use this to create the volume mesh with TetGen.
  //
  auto surfacer = vtkSmartPointer<vtkGeometryFilter>::New();
  surfacer->SetInputData(innerSurface);
  surfacer->Update();

  if (meshoptions_.boundarylayerdirection)
  {
    polydatasolid_->DeepCopy(surfacer->GetOutput());
  }
  else
  {
    // The remeshing excludes cell entity ids with value of 1. Need to
    // set all to one so that only caps are remeshed.
    //
    polydatasolid_->DeepCopy(originalsurfpd);
    polydatasolid_->GetCellData()->RemoveArray("CellEntityIds");
    auto newEntityIds = vtkSmartPointer<vtkIntArray>::New();
    newEntityIds->SetNumberOfTuples(polydatasolid_->GetNumberOfCells());
    newEntityIds->FillComponent(0, 1);
    newEntityIds->SetName("CellEntityIds");
    polydatasolid_->GetCellData()->AddArray(newEntityIds);
  }

#else
  fprintf(stderr,"Cannot generate a boundary layer mesh without VMTK\n");
  return SV_ERROR;
#endif

  return SV_OK;
}

/**
 * @brief Helper function to cap the surface and remesh them
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for generating caps and remeshing them
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateAndMeshCaps()
{
#ifdef SV_USE_VMTK
  vtkSmartPointer<vtkIdList> excluded =
    vtkSmartPointer<vtkIdList>::New();
  int marker;
  int meshcapsonly = 1;
  int preserveedges = 0;
  int captype = 0;
  int trioutput = 1;
  int cellOffset = 1;
  int useSizeFunction = 0;
  int trianglesplitfactor;
  double collapseanglethreshold;
  vtkSmartPointer<vtkDoubleArray> meshsizingfunction =
    vtkSmartPointer<vtkDoubleArray>::New();
  std::string markerListName = "ModelFaceID";
  if (meshoptions_.meshwallfirst)
  {
    collapseanglethreshold = 0.2;
    trianglesplitfactor = 5.0;
  }
  else
  {
    collapseanglethreshold = NULL;
    trianglesplitfactor = NULL;
  }
  if (meshoptions_.functionbasedmeshing || meshoptions_.refinement)
  {
    useSizeFunction = 1;
    if (VtkUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") != SV_OK)
    {
      fprintf(stderr,"Array name 'MeshSizingFunctionID' does not exist. \
	              Something may have gone wrong when setting up BL");
      return SV_ERROR;
    }
    fprintf(stderr,"Getting sizing function caps\n");
    meshsizingfunction = vtkDoubleArray::SafeDownCast(polydatasolid_->\
	  GetPointData()->GetScalars("MeshSizingFunction"));
    fprintf(stderr,"Got function caps\n");
  }

  vtkDataArray *currentMarkers = polydatasolid_->GetCellData()->GetArray(markerListName.c_str());
  if (currentMarkers == NULL)
  {
    excluded->SetNumberOfIds(1);
    excluded->InsertId(0,1);
    cellOffset = 1;
  }
  else
  {
    cellOffset = -1;
    for (int i=0; i<polydatasolid_->GetNumberOfCells(); i++)
    {
      marker = currentMarkers->GetTuple1(i);
      excluded->InsertUniqueId(marker);
      if (marker > cellOffset)
      {
        cellOffset = marker;
      }
    }
  }

  //We cap the inner surface of the boundary layer mesh
  if (VMTKUtils_Capper(polydatasolid_,captype,trioutput,cellOffset,
	markerListName) != SV_OK)
  {
    fprintf(stderr,"Problem with capping\n");
    return SV_ERROR;
  }

  //Set caps equal to caps on original
  if (TGenUtils_ResetOriginalRegions(polydatasolid_, originalpolydata_,
        markerListName, excluded) != SV_OK)
  {
    fprintf(stderr,"Problem with name resetting on cap remeshing\n");
    return SV_ERROR;
  }

  //We use VMTK surface remeshing to remesh just the caps to be
  //the same mesh size as the inner surface of the BL mesh
  if (VMTKUtils_SurfaceRemeshing(polydatasolid_,
	meshoptions_.maxedgesize,meshcapsonly,preserveedges,
	trianglesplitfactor,collapseanglethreshold,excluded,
	markerListName,useSizeFunction,meshsizingfunction) != SV_OK)
  {
    fprintf(stderr,"Problem with cap remeshing\n");
    return SV_ERROR;
  }
#else
  fprintf(stderr,"Cannot generate and mesh caps without VMTK\n");
  return SV_ERROR;
#endif

  // Add wall id back on to the surface
  vtkSmartPointer<vtkIntArray> wallIds = vtkSmartPointer<vtkIntArray>::New();
  wallIds->SetNumberOfTuples(polydatasolid_->GetNumberOfCells());
  wallIds->SetName("WallID");
  wallIds->FillComponent(0, 0);

  currentMarkers = polydatasolid_->GetCellData()->GetArray(markerListName.c_str());
  for (int i=0; i<polydatasolid_->GetNumberOfCells(); i++)
  {
    marker = currentMarkers->GetTuple1(i);
    if (excluded->IsId(marker) != -1)
    {
      wallIds->SetTuple1(i, 1);
    }
  }
  polydatasolid_->GetCellData()->AddArray(wallIds);

  return SV_OK;
}

/**
 * @brief Helper function to generate a mesh sizing function
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils to generate a mesh sizing function
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateMeshSizingFunction()
{
#ifdef SV_USE_VMTK
  vtkSmartPointer<vtkCleanPolyData> cleaner =
    vtkSmartPointer<vtkCleanPolyData>::New();
  //Compute a mesh sizing function to send to TetGen and create a
  //volume mesh based on. Must do this otherwise when appending
  //volume mesh and BL mesh, they won't match up!
  if (VtkUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") == 1)
  {
    fprintf(stderr,"MeshSizingFunction Name exists. Delete!\n");
    polydatasolid_->GetPointData()->RemoveArray("MeshSizingFunction");
  }
  if (VMTKUtils_ComputeSizingFunction(polydatasolid_,NULL,
	"MeshSizingFunction") != SV_OK)
  {
    fprintf(stderr,"Problem when computing sizing function");
    return SV_ERROR;
  }

  //Clean the output and make it a vtkUnstructuredGrid
  cleaner->SetInputData(polydatasolid_);
  cleaner->Update();

  polydatasolid_->DeepCopy(cleaner->GetOutput());
#else
  fprintf(stderr,"Cannot apply mesh sizing function without using VMTK\n");
  return SV_ERROR;
#endif

  return SV_OK;
}

/**
 * @brief Helper function to append meshes together for a full bl mesh
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for appending meshes
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::AppendBoundaryLayerMesh()
{
#ifdef SV_USE_VMTK
  if (surfacemesh_ != NULL)
  {
    surfacemesh_->Delete();
  }
  if (volumemesh_ != NULL)
  {
    volumemesh_->Delete();
  }
  if (boundarylayermesh_ == NULL)
  {
    fprintf(stderr,"Cannot append mesh without a Boundary Layer Mesh\n");
    return SV_ERROR;
  }
  if (innerblmesh_ == NULL)
  {
    fprintf(stderr,"Cannot append mesh without inner surface from boundary layer\n");
    return SV_ERROR;
  }
  surfacemesh_ = vtkPolyData::New();
  volumemesh_ = vtkUnstructuredGrid::New();

  auto surfacetomesh = vtkSmartPointer<vtkvmtkPolyDataToUnstructuredGridFilter>::New();
  auto newnormaler = vtkSmartPointer<vtkPolyDataNormals>::New();

  if (TGenUtils_ConvertToVTK(outmesh_,volumemesh_,surfacemesh_,
    &numBoundaryRegions_,0) != SV_OK)
  {
  return SV_ERROR;
  }

  surfacetomesh->SetInputData(polydatasolid_);
  surfacetomesh->Update();

  // We append the volume mesh from tetgen, the inner surface, and the
  // boundary layer mesh all together.
  //
  auto newVolumeMesh = vtkSmartPointer<vtkUnstructuredGrid>::New();
  auto newSurfaceMesh = vtkSmartPointer<vtkPolyData>::New();
  int giveblnewregion = meshoptions_.newregionboundarylayer;
  fprintf(stdout,"Appending Boundary Layer and Volume Mesh\n");
  if (VMTKUtils_AppendData(volumemesh_,boundarylayermesh_,
    surfacetomesh->GetOutput(), newVolumeMesh, newSurfaceMesh, giveblnewregion) != SV_OK)
  {
    return SV_ERROR;
  }
  volumemesh_->DeepCopy(newVolumeMesh);

  //We generate normals for region detection and save in surfacemesh_
  newnormaler->SetInputData(newSurfaceMesh);
  newnormaler->SplittingOff();
  newnormaler->ComputeCellNormalsOn();
  newnormaler->ComputePointNormalsOff();
  newnormaler->AutoOrientNormalsOn();
  newnormaler->Update();

  surfacemesh_->DeepCopy(newnormaler->GetOutput());
  polydatasolid_->DeepCopy(newnormaler->GetOutput());
#else
  fprintf(stderr,"Cannot append the BL mesh without using VMTK\n");
  return SV_ERROR;
#endif

  return SV_OK;
}

/**
 * @brief Helper function to reset the original region ids
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils in order to set the regions back
 * to the regions originally identified on the first surface
 * @return SV_OK if executed correctly
 */
int cvTetGenMeshObject::ResetOriginalRegions(std::string regionName)
{
  if (polydatasolid_ == NULL || originalpolydata_ == NULL)
  {
    fprintf(stderr,"Cannot reset original regions without orignal surface \
		    or without new surface\n");
    return SV_ERROR;
  }

  if (TGenUtils_ResetOriginalRegions(polydatasolid_,originalpolydata_, regionName)
      != SV_OK)
  {
    fprintf(stderr,"Error while resetting the original region values\n");
    return SV_ERROR;
  }


 return SV_OK;
}

// --------------------
//  Adapt
// --------------------
/**
 * @brief Function to Adapt Mesh based on input adaption features etc.
 * @return SV_OK if adaptions performs correctly
 */
int cvTetGenMeshObject::Adapt()
{
  cout<<"Starting Adaptive Mesh..."<<endl;
  tetgenbehavior* newtgb = new tetgenbehavior;

  newtgb->refine=1;
  newtgb->metric=1;
  newtgb->quality = 3;
  newtgb->mindihedral = 10.0;
  newtgb->minratio = 1.2;
  newtgb->neighout=2;
  newtgb->verbose=1;
  //newtgb->coarsen=1;
  //newtgb->coarsen_param=8;
  //newtgb->coarsenpercent=1;
#if USE_TETGEN143
  newtgb->goodratio = 4.0;
  newtgb->goodangle = 0.88;
  newtgb->useshelles = 1;
#endif

  try
  {
    tetrahedralize(newtgb, inmesh_, outmesh_);
  }
  catch (int r)
  {
    fprintf(stderr,"ERROR: TetGen quit and returned error code %d\n",r);
    return SV_ERROR;
  }

  cout<<"Done with Adaptive Mesh..."<<endl;

  if (outmesh_ == NULL) {
    return SV_ERROR;
  }
  if (surfacemesh_ != NULL)
    surfacemesh_->Delete();

  if (volumemesh_ != NULL)
    volumemesh_->Delete();

  surfacemesh_ = vtkPolyData::New();
  volumemesh_ = vtkUnstructuredGrid::New();
  if (TGenUtils_ConvertToVTK(outmesh_,volumemesh_,surfacemesh_,
	&numBoundaryRegions_,1) != SV_OK)
    return SV_ERROR;

  if (TGenUtils_ResetOriginalRegions(surfacemesh_,originalpolydata_,
	"ModelFaceID")
      != SV_OK)
  {
    fprintf(stderr,"Error while resetting the original region values\n");
    return SV_ERROR;
  }
  return SV_OK;
}

// --------------------
//  SetMetricOnMesh
// --------------------
int cvTetGenMeshObject::SetMetricOnMesh(double *error_indicator,int lstep,double factor, double hmax, double hmin,int strategy)
{
  // cant overwrite mesh
  if (inmesh_ != NULL) {
    delete inmesh_;
  }
  if (outmesh_ != NULL)
  {
    delete outmesh_;
  }

  if (inputug_ == NULL)
    return SV_ERROR;

  if (polydatasolid_ == NULL)
    return SV_ERROR;

  //Create new tetgen mesh objects and set first number of output mesh to 0
  inmesh_ = new tetgenio;
  inmesh_->firstnumber = 0;
  outmesh_ = new tetgenio;
  outmesh_->firstnumber = 0;

  if (TGenUtils_ConvertVolumeToTetGen(inputug_,polydatasolid_,inmesh_) != SV_OK)
  {
    fprintf(stderr,"Conversion from volume to TetGen failed\n");
    return SV_ERROR;
  }

  return SV_OK;
}

// --------------------
//  GetAdaptedMesh
// --------------------
int cvTetGenMeshObject::GetAdaptedMesh(vtkUnstructuredGrid *ug, vtkPolyData *pd)
{
  if (outmesh_ == NULL) {
    return SV_ERROR;
  }
  if (volumemesh_ == NULL) {
    return SV_ERROR;
  }
  if (surfacemesh_ == NULL) {
    return SV_ERROR;
  }

  ug->DeepCopy(volumemesh_);
  pd->DeepCopy(surfacemesh_);

  return SV_OK;
}
