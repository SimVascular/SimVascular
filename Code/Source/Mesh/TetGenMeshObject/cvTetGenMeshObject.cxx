/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

/** @file cvTetGenMeshObject.cxx
 *  @brief The implementations of functions in cvTetGenMeshObject
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_tetgenmesh_utils.
 */

#include "SimVascular.h" 

#include "cvTetGenMeshObject.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"
#include "cv_polydatasolid_utils.h"

#include "cv_tetgenmesh_utils.h"

#include "cv_sys_geom.h"

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

#ifdef USE_VMTK
	#include "cv_VMTK_utils.h"
	#include "vtkvmtkPolyDataToUnstructuredGridFilter.h"
	#include "vtkvmtkUnstructuredGridTetraFilter.h"
#endif


// -----------
// cvTetGenMeshObject
// -----------
/** 
 * @brief Constructor for cvTetGenMeshObject(Should never be called directly) 
 */

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
  meshoptions_.coarsen_percent=0;
  meshoptions_.boundarylayermeshflag=0;
  meshoptions_.numsublayers=0;
  meshoptions_.blthicknessfactor=0;
  meshoptions_.sublayerratio=0;
  meshoptions_.refinement=0;
  meshoptions_.refinedsize=0;
  meshoptions_.sphereradius=0;
  meshoptions_.cylinderradius=0;
  meshoptions_.cylinderlength=0;
  meshoptions_.functionbasedmeshing=0;
  meshoptions_.secondarrayfunction=0;
  meshoptions_.meshwallfirst=0;
  meshoptions_.startwithvolume=0;
  for (int i=0;i<3;i++)
  {
    meshoptions_.spherecenter[i] = 0;
    meshoptions_.cylindercenter[i] = 0;
    meshoptions_.cylindernormal[i] = 0;
  }
}

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
  
}

int cvTetGenMeshObject::SetMeshFileName( const char* meshFileName )
{
  if (meshFileName != NULL)
    sprintf(meshFileName_, "%s", meshFileName);
  else
    meshFileName_[0] = '\0';

  return CV_OK;
}

int cvTetGenMeshObject::SetSolidFileName( const char* solidFileName )
{
  if (solidFileName != NULL)
    sprintf(solidFileName_, "%s", solidFileName);
  else
    solidFileName_[0] = '\0';

  return CV_OK;
}

// -----
// Print
// -----
/** 
 * @brief Function to print out the mesh statistics 
 * @return CV_OK if executed correctly
 */

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
    return CV_ERROR;
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
      return CV_ERROR;
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
      return CV_ERROR;
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

  return CV_OK;
}


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
 * @return CV_OK if executed correctly
 */

int cvTetGenMeshObject::Update() {

  //I'm not sure why this is used. Would really mesh with some of the 
  //meshing functions to Update the mesh or solid periodically.
  //However, must return CV_OK, otherwise running of anything breaks.

  return CV_OK;
  
}

// -----------------------
// GetSolid
// -----------------------
//
/** 
 * @brief Function that returns the polydatasolid member data loaded  
 * @return CV_OK if executed correctly
 */
cvPolyData* cvTetGenMeshObject::GetSolid() {

  if (polydatasolid_ == NULL)
  {
    //Mesh must be created first
    return CV_ERROR;
  }

  cvPolyData* result =NULL;

  result = new cvPolyData(polydatasolid_);

  return result;

}

// -----------------------
// GetPolyData
// -----------------------
//
/** 
 * @brief Function that returns the polydatasolid member data loaded  
 * @return CV_OK if executed correctly
 */
cvPolyData* cvTetGenMeshObject::GetPolyData() {

  if (surfacemesh_ == NULL)
  {
    //Mesh must be created first
    return CV_ERROR;
  }

  cvPolyData* result =NULL;

  result = new cvPolyData(surfacemesh_);

  return result;

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
    return CV_ERROR;
  }

  cvUnstructuredGrid *result = NULL; 

  result = new cvUnstructuredGrid(volumemesh_);

  return result;

}
/** 
 * @brief Function that writes the adjacency between tetrahedral elements 
 * @param *filename char holding the name of the file to be written to 
 * @return CV_OK if executed correctly
 * @note This function is not necessarily needed anymore. The functionality
 * to extract the adjacency from the vtu is added in the presolver
 */

int cvTetGenMeshObject::WriteMetisAdjacency(char *filename) {

  //No longer need to write adjacency
  return CV_OK;

  if (filename == NULL) {
        return CV_ERROR;
  }

  if (meshoptions_.volumemeshflag)
  {
    // open the output file
    if (openOutputFile(filename) != CV_OK) return CV_ERROR;

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

    if (PlyDtaUtils_UGCheckArrayName(volumemesh_,1,"GlobalElementID") != CV_OK)
    {
      fprintf(stderr,"Array name 'GlobalElementID' does not exist in volume mesh. \
		      Something wrong with ids on mesh");
      return CV_ERROR;
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
    return CV_OK;
  }
}

int cvTetGenMeshObject::GetNodeCoords(int node)
{
  if (volumemesh_ == 0)
  {
    fprintf(stderr,"Mesh needs to be computed before node coords can be retrieved\n");
    return CV_ERROR;
  }
  nodeID_ = node;
  nodeX_ = 0; nodeY_ = 0; nodeZ_ = 0;
  double pts[3];

  volumemesh_->GetPoint(node,pts);
  nodeX_ = pts[0];
  nodeY_ = pts[1];
  nodeZ_ = pts[2];

  return CV_OK;
}

// --------------------
//  LoadModel
// --------------------
/** 
 * @brief Function that loads a model from solid and store it in the 
 * member data polydatasolid
 * @param *filename char holding the name of the file to read 
 * @return CV_OK if executed correctly
 * @note uses the PolyDataUtils to read the solid: see cv_polydatasolid_utils
 */

int cvTetGenMeshObject::LoadModel(char *filename) {

	fprintf(stderr,"Loading Model\n");
  if (filename == NULL) {
    return CV_ERROR;
  }

  // must load model before mesh!
  if (inmesh_ != NULL) {
    return CV_ERROR;
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
  if (PlyDtaUtils_ReadNative(filename,polydatasolid_) != CV_OK) {
    return CV_ERROR;
  }

  originalpolydata_->DeepCopy(polydatasolid_);
  return CV_OK;

}

// --------------------
//  GetBoundaryFaces
// --------------------
/** 
 * @brief Function to extract the boundaries for the member vtkPolyData
 * @param angle double that specifies the extraction angle. Any faces
 * with a difference between face normals larger than this angle will be 
 * considered a separate face
 * @return *result: CV_ERROR is member data hasn't been loaded, or if the 
 * GetBoundaryFaces function does not work properly. CV_OK is executed 
 * properly
 */

int cvTetGenMeshObject::GetBoundaryFaces(double angle)
{
  if (polydatasolid_ == NULL) {
    return CV_ERROR;
  }

  if(PlyDtaUtils_GetBoundaryFaces(polydatasolid_,angle,
	&numBoundaryRegions_) != CV_OK) {
    return CV_ERROR;
  }

  if (meshoptions_.boundarylayermeshflag)
  {
    if (surfacemesh_ == NULL)
    {
      fprintf(stderr,"Surface mesh should not be null\
	  to get boundary layer regions\n");
      return CV_ERROR;
    }

    ResetOriginalRegions("ModelFaceID","ModelFaceID");
    surfacemesh_->DeepCopy(polydatasolid_);
  }

  return CV_OK;
}

// --------------------
//  LoadMesh
// --------------------  
/**
 * @brief Function to load the vtkUnstructuredGrid of a Mesh
 * @return CV_OK if function executes properly. 
 * @note If a current volume
 * mesh exists, the current volumemesh is deleted and the new one is loaded
 */
int cvTetGenMeshObject::LoadMesh(char *filename,char *surfilename) {

  if (filename == NULL) {
    return CV_ERROR;
  }

  if (volumemesh_ != NULL)
    volumemesh_->Delete();

  volumemesh_ = vtkUnstructuredGrid::New();
  if (TGenUtils_LoadMesh(filename,volumemesh_) != CV_OK)
    return CV_ERROR;

  if (surfilename != 0)
  {
    if (surfacemesh_ != NULL)
      surfacemesh_->Delete();

    surfacemesh_ = vtkPolyData::New();
    if (PlyDtaUtils_ReadNative(surfilename,surfacemesh_) != CV_OK)
      return CV_ERROR;
  }

  return CV_OK;

}

// --------------------
//  NewMesh
// --------------------
/** 
 * @brief Function to prepare a mesh from the polydatasolid. Takes input
 * mesh and converts to tetgen structures in preparation.
 * @return *result: CV_ERROR if solid hasn't been loaded. If a current mesh
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
    return CV_ERROR;
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
  vtkSmartPointer<vtkDoubleArray> meshsizingfunction = 
    vtkSmartPointer<vtkDoubleArray>::New();

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

  //If using boundary layer mesh, must apply mesh sizing function that is 
  //attached to the polydatasolid from VMTKUtils_ComputeSizingFunction
  if (meshoptions_.boundarylayermeshflag || meshoptions_.functionbasedmeshing ||
      meshoptions_.refinement)
  {
    if (PlyDtaUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") != CV_OK)
    {
      fprintf(stderr,"Array name 'MeshSizingFunctionID' does not exist. \
		      Something may have gone wrong when setting up BL");
      return CV_ERROR;
    }
    meshsizingfunction = vtkDoubleArray::SafeDownCast(polydatasolid_->\
	  GetPointData()->GetScalars("MeshSizingFunction"));
  }
  else
  {
    meshsizingfunction = NULL;
  }

  vtkSmartPointer<vtkCleanPolyData> cleaner =
   vtkSmartPointer<vtkCleanPolyData>::New(); 
  cleaner->SetInputData(polydatasolid_);
  cleaner->Update();
  polydatasolid_->DeepCopy(cleaner->GetOutput());
  fprintf(stderr,"Converting to TetGen...\n");
  //Convert the polydata to tetgen for meshing with given option
  if (TGenUtils_ConvertSurfaceToTetGen(inmesh_,polydatasolid_,useSizingFunction,
	  meshsizingfunction,useBoundary,markerListName,
	  meshoptions_.maxedgesize) != CV_OK)
  {
      return CV_ERROR;
  }
  //The mesh is now loaded, and TetGen is ready to be called
  meshloaded_ = 1;

  return CV_OK;
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
 * @return *result: CV_ERROR if the mesh doesn't exist. New Mesh must be 
 * called before the options can be set
 */

int cvTetGenMeshObject::SetMeshOptions(char *flags,int numValues,double *values) {
  // must have created mesh
//  if (inmesh_ == NULL) {
//    return CV_ERROR;
//  }
  
  if(!strncmp(flags,"GlobalEdgeSize",14)) {            //Global edge size
       if (numValues < 1)
	 return CV_ERROR;
      meshoptions_.maxedgesize=values[0];
  }
  else if(!strncmp(flags,"LocalEdgeSize",13)) {
      if (numValues < 2)
      {
	fprintf(stderr,"Must give face id and local edge size\n");
	return CV_ERROR;
      }
      meshoptions_.functionbasedmeshing = 1;
      //Create a new mesh sizing function and call TGenUtils to compute function.
      //Store in the member data vtkDouble Array meshsizingfunction
      if (TGenUtils_SetLocalMeshSize(polydatasolid_,values[0],values[1]) != CV_OK)
        return CV_ERROR;
      meshoptions_.secondarrayfunction = 1;
  }
  else if(!strncmp(flags,"SurfaceMeshFlag",15)) {
#ifdef USE_VMTK
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.surfacemeshflag = values[0];
#else
      fprintf(stderr,"Plugin VMTK is not being used!\
	  In order to use surface meshing, plugin VMTK must be available!\n");
      return CV_ERROR;
#endif
  }
  else if(!strncmp(flags,"VolumeMeshFlag",14)) {
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.volumemeshflag = values[0];
  }
  else if(!strncmp(flags,"QualityRatio",12)) {//q
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.minratio=values[0];
  }
  else if(!strncmp(flags,"Optimization",12)) {//O
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.optlevel=(int)values[0];
  }
  else if(!strncmp(flags,"Epsilon",7)) {//T
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.epsilon=values[0];
  }
  else if(!strncmp(flags,"CoarsenPercent",14)) {//R
      if (numValues < 1)
	return CV_ERROR;
      meshoptions_.coarsen_percent=values[0]/100;
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
  else {
      fprintf(stderr,"%s: flag is not recognized\n",flags);
  }

  return CV_OK;
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
 * @return CV_OK if surface exists and is extracted with Threshold correctly 
 */
int cvTetGenMeshObject::SetBoundaryLayer(int type, int id, int side, 
    int nL, double* H)
{
#ifdef USE_VMTK
  meshoptions_.boundarylayermeshflag = 1;
  meshoptions_.numsublayers = nL;
  meshoptions_.blthicknessfactor = *H;
  meshoptions_.sublayerratio = *(H+1);
#else
  fprintf(stderr,"Plugin VMTK is not being used! \ 
      In order to use boundary layer meshing, \
      plugin VMTK must be available!\n");
  return CV_ERROR;
#endif

  return CV_OK;
}

// --------------------
//  SetWalls
// --------------------
/** 
 * @brief Function to set the walls of the mesh with an integer array
 * @param numWalls number of walls being set with value
 * @param walls, integer list for ModelFaceIds in the wall.
 * @return CV_OK if surface exists and array is set correctly 
 */
int cvTetGenMeshObject::SetWalls(int numWalls, int *walls) 
{
  int max=0;
  double range[2];
  vtkIntArray *wallArray = vtkIntArray::New();
  meshoptions_.meshwallfirst = 1;

  if (PlyDtaUtils_PDCheckArrayName(polydatasolid_,1,"ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"ModelFaceID array not on object, so cannot set walls\n");
    return CV_ERROR;
  }
  vtkIntArray *modelIds;
  modelIds = vtkIntArray::SafeDownCast(
      polydatasolid_->GetCellData()->GetArray("ModelFaceID"));
  modelIds->GetRange(range);
  max = range[1];

  int *isWall = new int[max];
  for (int i=0; i < max; i++)
    isWall[i] = 0;
  for (int i=0; i < numWalls; i++)
  {
    int wallid = *(walls+i);
    isWall[wallid-1] = 1;
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

  vtkSmartPointer<vtkThreshold> thresholder = 
    vtkSmartPointer<vtkThreshold>::New();
  thresholder->SetInputData(polydatasolid_);
   //Set Input Array to 0 port,0 connection,1 for Cell Data, and WallID is the type name
  thresholder->SetInputArrayToProcess(0,0,0,1,"WallID");
  thresholder->ThresholdBetween(1,1); 
  thresholder->Update();

  vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer = 
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  surfacer->SetInputData(thresholder->GetOutput());
  surfacer->Update();

  polydatasolid_->DeepCopy(surfacer->GetOutput());

  delete [] isWall;
  return CV_OK;
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
 * @return CV_OK if the mesh sizing function based on the circle is computed
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
	meshoptions_.maxedgesize) != CV_OK)
  {
    return CV_ERROR;
  }

  meshoptions_.secondarrayfunction = 1;
  return CV_OK;
}


// --------------------
//  SetSphereRefinement
// --------------------
/** 
 * @brief Function to set the region to refine based on input sphere
 * @param size This is the smaller refined of the edges within sphere region.
 * @param radius This is the radius of the refinement sphere.
 * @param center This is the center of the refinement sphere.
 * @return CV_OK if the mesh sizing function based on the circle is computed
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
	meshoptions_.maxedgesize) != CV_OK)
  {
    return CV_ERROR;
  }

  meshoptions_.secondarrayfunction = 1;
  return CV_OK;
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
 * @return CV_OK if the mesh sizing function is computed correctly 
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
	size,sizefunctionname,meshoptions_.secondarrayfunction) != CV_OK)
  {
    return CV_ERROR;
  }

  //originalpolydata_->DeepCopy(polydatasolid_);
  meshoptions_.secondarrayfunction = 1;
  return CV_OK;
}

/** 
 * @brief Function to generate a mesh
 * @return *result: CV_ERROR if the mesh doesn't exist. New Mesh must be 
 * called before a mesh can be generated
 * @note Function checks to see if any of the mesh options have been set. 
 * It they have, the corresponding tetgenbehavior object values are set.
 */

int cvTetGenMeshObject::GenerateMesh() {
  // must have created mesh
//  if (inmesh_ == NULL) {
//    return CV_ERROR;
//  } 

  if (surfacemesh_ != NULL)
  {
    surfacemesh_->Delete();
  }
  if (volumemesh_ != NULL)
  {
    volumemesh_->Delete();
  }

//All these complicated options exist if using VMTK. Should be stopped prior
//to this if trying to use VMTK options and don't have VMTK.
#ifdef USE_VMTK
  //If doing surface remeshing!
  if (meshoptions_.surfacemeshflag)
  {
     if (GenerateSurfaceRemesh() != CV_OK)
       return CV_ERROR;

    //If we are doing a volumemesh based off the surface mesh!
    if (meshoptions_.volumemeshflag)
    {
      //If we are doing boundary layer mesh, it gets complicated!
      if (meshoptions_.boundarylayermeshflag)
      {
	if (GenerateBoundaryLayerMesh() != CV_OK)
	  return CV_ERROR;

	if (GenerateAndMeshCaps() != CV_OK)
	  return CV_ERROR;
      }

      if (meshoptions_.boundarylayermeshflag || meshoptions_.functionbasedmeshing 
        || meshoptions_.refinement)
      {
	if (GenerateMeshSizingFunction() != CV_OK)
	  return CV_ERROR;
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
    if (TGenUtils_CheckSurfaceMesh(polydatasolid_,
	  meshoptions_.boundarylayermeshflag) != CV_OK)
    {
      fprintf(stderr,"Mesh surface is bad\n");
      return CV_ERROR;
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
#if defined(TETGEN150) || defined(TETGEN151)
    if (meshoptions_.coarsen_percent != 0)
    {
      tgb->coarsen=1;
      tgb->coarsen_percent=meshoptions_.coarsen_percent;
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
      return CV_ERROR;
    }
    fprintf(stdout,"TetGen Meshing Finished...\n");
  }

  else 
  {
    fprintf(stdout,"Only surface\n");
  }

#ifdef USE_VMTK
  //This is a post meshing step that needs to be done for boundary layer
  //mesh
  if (meshoptions_.boundarylayermeshflag)
  {
    AppendBoundaryLayerMesh();
  }
#endif

  return CV_OK;
}

/** 
 * @brief Function to convert mesh back into vtkPolyData
 * @return *result: CV_ERROR if the output mesh doesn't exist. CV_OK if 
 * function returns properly.
 * @note This function actually doesn't write a mesh! it only converts back
 * to vtkPolyData from TetGen structures. If you want to have a volume or
 * surface mesh, or manipulate the mesh, this function must be called 
 */

int cvTetGenMeshObject::WriteMesh(char *filename, int smsver) {
  // must have created mesh
  if (meshoptions_.volumemeshflag && !meshoptions_.boundarylayermeshflag)
  {

    if (outmesh_ == NULL) {
      return CV_ERROR;
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
	  &numBoundaryRegions_,1) != CV_OK)
      return CV_ERROR;
  }

  return CV_OK;
}

int cvTetGenMeshObject::WriteStats(char *filename) {
  // must have created mesh
  if (inmesh_ == NULL) {
    return CV_ERROR;
  }
  return CV_OK;
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

  if (TGenUtils_GetFacePolyData(orgfaceid,surfacemesh_,face) != CV_OK)
  {
    return CV_ERROR;
  }

  result = new cvPolyData(face);

  face->Delete();

  return result;

}

/** 
 * @brief Function to return the list of face ids used by the mesh
 * @param rtnstr char string containg the list of ids
 * @return CV_OK if the function executes properly
 */

int cvTetGenMeshObject::GetModelFaceInfo(char rtnstr[99999]) {

  int i=0;
  int max = 0;
  char tmpstr[99999];
  rtnstr[0]='\0';

  if (solidmodeling_kernel_ == SM_KT_POLYDATA) {
    if (PlyDtaUtils_PDCheckArrayName(originalpolydata_,1,"ModelFaceID") != CV_OK)
    {
      fprintf(stderr,"ModelFaceID does not exist\n");
      return CV_ERROR;
    }

    int *faces;
    int numFaces = 0;
    if (PlyDtaUtils_GetFaceIds(originalpolydata_,&numFaces,&faces) != CV_OK)
    {
      fprintf(stderr,"Could not get face ids\n");
      return CV_ERROR;
    }

    for(i=0;i<numFaces;i++)
    { 
      tmpstr[0] = '\0';
      char *namestr;
      sprintf(tmpstr,"%s {%i %i {%s}} ",rtnstr,faces[i],faces[i],"");
      rtnstr[0]='\0';
      sprintf(rtnstr,"%s",tmpstr);
    }
    delete [] faces;

  }

  return CV_OK;

}

/** 
 * @brief Function to set the PolyData member object
 * @param *newPolyData Pointer to vtkPolyData object that you want to be 
 * set as the class member data
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::SetVtkPolyDataObject(vtkPolyData *newPolyData)
{
  if (polydatasolid_ != NULL)
  {
    polydatasolid_->Delete();
  }
  polydatasolid_ = vtkPolyData::New();
  polydatasolid_->DeepCopy(newPolyData);

  return CV_OK;
}

/** 
 * @brief Function to set an input unstructured grid
 * @param *newPolyData Pointer to vtkPolyData object that you want to be 
 * set as the class member data
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::SetInputUnstructuredGrid(vtkUnstructuredGrid *ug)
{
  if (inputug_ != NULL)
  {
    inputug_->Delete();
  }
  inputug_ = vtkUnstructuredGrid::New();
  inputug_->DeepCopy(ug);

  return CV_OK;
}

/** 
 * @brief Helper function to generate surface mesh
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for generating surface meshes
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateSurfaceRemesh()
{
#ifdef USE_VMTK
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
    if (PlyDtaUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") != CV_OK)
    {
      fprintf(stderr,"Array name 'MeshSizingFunction' does not exist. \
	              Something may have gone wrong when setting up BL");
      return CV_ERROR;
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

  //Generate Surface Remeshing
  if(VMTKUtils_SurfaceRemeshing(polydatasolid_,meshoptions_.maxedgesize,
	meshcapsonly,preserveedges,trianglesplitfactor,
	collapseanglethreshold,NULL,markerListName,
	useSizingFunction,meshsizingfunction) != CV_OK)
  {
    fprintf(stderr,"Problem with surface meshing\n");
    return CV_ERROR;
  }

  if (TGenUtils_CheckSurfaceMesh(polydatasolid_,
	  meshoptions_.meshwallfirst) != CV_OK)
  {
    fprintf(stderr,"Mesh surface is bad\n");
    return CV_ERROR;
  }

  if (meshoptions_.meshwallfirst && !meshoptions_.boundarylayermeshflag)
  {
    GenerateAndMeshCaps();
    ResetOriginalRegions("ModelFaceID","ModelFaceID");
  }

#else
  fprintf(stderr,"Cannot do a surface remesh without using VMTK\n");
  return CV_ERROR;
#endif

  return CV_OK;
}

/** 
 * @brief Helper function to generate boundary layer mesh
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for generating a boundary layer mesh
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateBoundaryLayerMesh()
{
#ifdef USE_VMTK
  if (boundarylayermesh_ != NULL)
  {
    boundarylayermesh_->Delete();
  }

  if (innerblmesh_ != NULL)
  {
    innerblmesh_->Delete();
  }
  innerblmesh_ = vtkUnstructuredGrid::New();
  boundarylayermesh_ = vtkUnstructuredGrid::New();

  vtkSmartPointer<vtkGeometryFilter> surfacer 
    = vtkSmartPointer<vtkGeometryFilter>::New();
  vtkSmartPointer<vtkUnstructuredGrid> innerSurface = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  vtkSmartPointer<vtkvmtkPolyDataToUnstructuredGridFilter> converter
    = vtkSmartPointer<vtkvmtkPolyDataToUnstructuredGridFilter>::New();
  vtkSmartPointer<vtkCleanPolyData> cleaner = 
    vtkSmartPointer<vtkCleanPolyData>::New();
  std::string markerListName;
  if (meshoptions_.boundarylayermeshflag)
  {
    markerListName = "CellEntityIds";
  }
  else
  {
    markerListName = "ModelFaceID";
  }

  //Clean and convert the polydata to a vtu
  //Then we call VMTK to generate a boundary layer mesh, and we set 
  //the boundary layer mesh as the member vtkUnstructuredGrid 
  //called boundarylayermesh_
  cleaner->SetInputData(polydatasolid_);
  cleaner->Update();

  converter->SetInputData(cleaner->GetOutput());
  converter->Update();

  innerblmesh_->DeepCopy(converter->GetOutput());

  boundarylayermesh_->DeepCopy(converter->GetOutput());
  int negateWarpVectors = 1;
  int innerSurfaceCellId = 1;
  int sidewallCellEntityId = 9999;
  if (VMTKUtils_BoundaryLayerMesh(boundarylayermesh_,innerSurface,
	meshoptions_.maxedgesize,meshoptions_.blthicknessfactor,
	meshoptions_.numsublayers,meshoptions_.sublayerratio,
	sidewallCellEntityId,innerSurfaceCellId,negateWarpVectors,
	markerListName) != CV_OK)
  {
    fprintf(stderr,"Problem with boundary layer meshing\n");
    return CV_ERROR;
  } 

  //We take the inside surface of the boundary layer mesh and set the 
  //member vtkPolyData polydatasolid_ to be equal to this. We will 
  //use this to create the volume mesh with TetGen.
  surfacer->SetInputData(innerSurface);
  surfacer->Update();

  polydatasolid_->DeepCopy(surfacer->GetOutput());
#else
  fprintf(stderr,"Cannot generate a boundary layer mesh without VMTK\n");
  return CV_ERROR;
#endif
  
  return CV_OK;
}

/** 
 * @brief Helper function to cap the surface and remesh them
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for generating caps and remeshing them
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateAndMeshCaps()
{
#ifdef USE_VMTK
  vtkSmartPointer<vtkIdList> excluded = 
    vtkSmartPointer<vtkIdList>::New();
  int meshcapsonly = 1;
  int preserveedges = 0;
  int captype = 0;                      
  int trioutput = 1;
  int cellOffset = 1;
  int useSizeFunction = 0;
  int trianglesplitfactor;
  double collapseanglethreshold;
  std::string markerListName;
  vtkSmartPointer<vtkDoubleArray> meshsizingfunction = 
    vtkSmartPointer<vtkDoubleArray>::New();
  if (meshoptions_.meshwallfirst)
  {
    collapseanglethreshold = 0.2;
    trianglesplitfactor = 5.0;
    markerListName = "CellEntityIds";
  }
  else
  {
    collapseanglethreshold = NULL;
    trianglesplitfactor = NULL;
    markerListName = "ModelFaceID";
  }
  if (meshoptions_.functionbasedmeshing || meshoptions_.refinement)
  {
    useSizeFunction = 1;
    if (PlyDtaUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") != CV_OK)
    {
      fprintf(stderr,"Array name 'MeshSizingFunctionID' does not exist. \
	              Something may have gone wrong when setting up BL");
      return CV_ERROR;
    }
    fprintf(stderr,"Getting sizing function caps\n");
    meshsizingfunction = vtkDoubleArray::SafeDownCast(polydatasolid_->\
	  GetPointData()->GetScalars("MeshSizingFunction"));
    fprintf(stderr,"Got function caps\n");
  }

  //We cap the inner surface of the boundary layer mesh
  if (VMTKUtils_Capper(polydatasolid_,captype,trioutput,cellOffset,
	markerListName) != CV_OK)
  {
    fprintf(stderr,"Problem with capping\n");
    return CV_ERROR;
  }

  //We use VMTK surface remeshing to remesh just the caps to be 
  //the same mesh size as the inner surface of the BL mesh
  excluded->SetNumberOfIds(1);
  excluded->InsertId(0,1);
  if (VMTKUtils_SurfaceRemeshing(polydatasolid_,
	meshoptions_.maxedgesize,meshcapsonly,preserveedges,
	trianglesplitfactor,collapseanglethreshold,excluded,
	markerListName,useSizeFunction,meshsizingfunction) != CV_OK)
  {
    fprintf(stderr,"Problem with cap remeshing\n");
    return CV_ERROR;
  }
  return CV_OK;
#else
  fprintf(stderr,"Cannot generate and mesh caps without VMTK\n");
  return CV_ERROR;
#endif

  return CV_OK;	
}

/** 
 * @brief Helper function to generate a mesh sizing function
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils to generate a mesh sizing function
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::GenerateMeshSizingFunction()
{
#ifdef USE_VMTK
  vtkSmartPointer<vtkCleanPolyData> cleaner = 
    vtkSmartPointer<vtkCleanPolyData>::New();
  //Compute a mesh sizing function to send to TetGen and create a 
  //volume mesh based on. Must do this otherwise when appending 
  //volume mesh and BL mesh, they won't match up!
  if (PlyDtaUtils_PDCheckArrayName(polydatasolid_,0,"MeshSizingFunction") == 1)
  {
    fprintf(stderr,"MeshSizingFunction Name exists. Delete!\n");
    polydatasolid_->GetPointData()->RemoveArray("MeshSizingFunction");
  }
  if (VMTKUtils_ComputeSizingFunction(polydatasolid_,NULL,
	"MeshSizingFunction") != CV_OK)
  {
    fprintf(stderr,"Problem when computing sizing function");
    return CV_ERROR;
  }

  //Clean the output and make it a vtkUnstructuredGrid
  cleaner->SetInputData(polydatasolid_);
  cleaner->Update();

  polydatasolid_->DeepCopy(cleaner->GetOutput());
#else
  fprintf(stderr,"Cannot apply mesh sizing function without using VMTK\n");
  return CV_ERROR;
#endif

  return CV_OK;
}

/** 
 * @brief Helper function to append meshes together for a full bl mesh
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils for appending meshes
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::AppendBoundaryLayerMesh()
{
#ifdef USE_VMTK
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
    return CV_ERROR;
  }
  if (innerblmesh_ == NULL)
  {
    fprintf(stderr,"Cannot append mesh without inner surface from boundary layer\n");
    return CV_ERROR;
  }
  surfacemesh_ = vtkPolyData::New();
  volumemesh_ = vtkUnstructuredGrid::New();

  vtkSmartPointer<vtkvmtkPolyDataToUnstructuredGridFilter> surfacetomesh= 
  vtkSmartPointer<vtkvmtkPolyDataToUnstructuredGridFilter>::New();
  vtkSmartPointer<vtkPolyDataNormals> newnormaler =
  vtkSmartPointer<vtkPolyDataNormals>::New();
  std::string markerListName;
  if (meshoptions_.boundarylayermeshflag)
  {
    markerListName = "CellEntityIds";
  }
  else
  {
    markerListName = "ModelFaceID";
  }

  if (TGenUtils_ConvertToVTK(outmesh_,volumemesh_,surfacemesh_,
    &numBoundaryRegions_,0) != CV_OK)
  {
  return CV_ERROR;
  }

  surfacetomesh->SetInputData(polydatasolid_);
  surfacetomesh->Update();

  //We append the volume mesh from tetgen, the inner surface, and the 
  //boundary layer mesh all together. 
  fprintf(stdout,"Appending Boundary Layer and Volume Mesh\n");
  if (VMTKUtils_AppendMesh(volumemesh_,innerblmesh_,boundarylayermesh_,
    surfacetomesh->GetOutput(),markerListName) != CV_OK)
  {
  return CV_ERROR;
  }

  vtkSmartPointer<vtkvmtkUnstructuredGridTetraFilter> tetrahedralizer = 
      vtkSmartPointer<vtkvmtkUnstructuredGridTetraFilter>::New();

  //Tetrahedralize this mesh
  tetrahedralizer->SetInputData(volumemesh_);
  tetrahedralizer->Update();

  //Because no surface ids are obviously retained through this complex
  //boundary layer mesh, we must regenerate GlobalElementIds,
  //GlobalNodeIds.
  volumemesh_->DeepCopy(tetrahedralizer->GetOutput());
  if (VMTKUtils_InsertIds(volumemesh_,surfacemesh_) != CV_OK)
  {
    fprintf(stdout,"Error adding cell and point ids\n");
    return CV_ERROR;
  }

  //We generate normals for region detection and save in surfacemesh_
  newnormaler->SetInputData(surfacemesh_);
  newnormaler->SplittingOff();
  newnormaler->AutoOrientNormalsOn();
  newnormaler->Update();

  surfacemesh_->DeepCopy(newnormaler->GetOutput());
  polydatasolid_->DeepCopy(newnormaler->GetOutput());
#else
  fprintf(stderr,"Cannot append the BL mesh without using VMTK\n");
  return CV_ERROR;
#endif
  
  return CV_OK;
}

/** 
 * @brief Helper function to reset the original region ids
 * @note This is a helper function. It is called from GenerateMesh
 * and it calls the VMTK utils in order to set the regions back
 * to the regions originally identified on the first surface
 * @return CV_OK if executed correctly
 */
int cvTetGenMeshObject::ResetOriginalRegions(std::string newName,std::string originalName)
{
  if (polydatasolid_ == NULL || originalpolydata_ == NULL)
  {
    fprintf(stderr,"Cannot reset original regions without orignal surface \
		    or without new surface\n");
    return CV_ERROR;
  }

  if (TGenUtils_ResetOriginalRegions(polydatasolid_,originalpolydata_,
	newName,originalName) 
      != CV_OK)
  {
    fprintf(stderr,"Error while resetting the original region values\n");
    return CV_ERROR;
  }


 return CV_OK;   
}

// --------------------
//  Adapt
// --------------------
/** 
 * @brief Function to Adapt Mesh based on input adaption features etc.
 * @return CV_OK if adaptions performs correctly
 */
int cvTetGenMeshObject::Adapt()
{ 
  cout<<"Starting Adaptive Mesh..."<<endl;
  tetgenbehavior* newtgb = new tetgenbehavior;

  newtgb->refine=1;
  newtgb->metric=1;
  newtgb->quality=1;
  newtgb->neighout=2;
  newtgb->verbose=1;
  //newtgb->coarsen=1;
  //newtgb->coarsen_param=8;
  //newtgb->coarsen_percent=1;
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
    return CV_ERROR;
  }

  cout<<"Done with Adaptive Mesh..."<<endl;

  if (outmesh_ == NULL) {
    return CV_ERROR;
  }
  if (surfacemesh_ != NULL)
    surfacemesh_->Delete();

  if (volumemesh_ != NULL)
    volumemesh_->Delete();

  surfacemesh_ = vtkPolyData::New();
  volumemesh_ = vtkUnstructuredGrid::New();
  if (TGenUtils_ConvertToVTK(outmesh_,volumemesh_,surfacemesh_,
	&numBoundaryRegions_,1) != CV_OK)
    return CV_ERROR;

  if (TGenUtils_ResetOriginalRegions(surfacemesh_,originalpolydata_,
	"ModelFaceID","ModelFaceID") 
      != CV_OK)
  {
    fprintf(stderr,"Error while resetting the original region values\n");
    return CV_ERROR;
  }
  return CV_OK;
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
    return CV_ERROR;

  if (polydatasolid_ == NULL)
    return CV_ERROR;

  //Create new tetgen mesh objects and set first number of output mesh to 0
  inmesh_ = new tetgenio;
  inmesh_->firstnumber = 0;
  outmesh_ = new tetgenio;
  outmesh_->firstnumber = 0;

  if (TGenUtils_ConvertVolumeToTetGen(inputug_,polydatasolid_,inmesh_) != CV_OK)
  {
    fprintf(stderr,"Conversion from volume to TetGen failed\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// --------------------
//  GetAdaptedMesh
// --------------------
int cvTetGenMeshObject::GetAdaptedMesh(vtkUnstructuredGrid *ug, vtkPolyData *pd)
{
  if (outmesh_ == NULL) {
    return CV_ERROR;
  }
  if (volumemesh_ == NULL) {
    return CV_ERROR;
  }
  if (surfacemesh_ == NULL) {
    return CV_ERROR;
  }

  ug->DeepCopy(volumemesh_);
  pd->DeepCopy(surfacemesh_);

  return CV_OK;
}
