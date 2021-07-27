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

/** @file sv_PolyDataSolid.cxx
 *  @brief The implementations of functions in cvPolyDataSolid
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#include "SimVascular.h"

#include "sv_PolyDataSolid.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkSVLoopBooleanPolyDataFilter.h"
#include "vtkCubeSource.h"
#include "sv_polydatasolid_utils.h"
#include "sv_misc_utils.h"
#include "sv_sys_geom.h"
#include <string.h>
#include <assert.h>
#include "vtkMath.h"
#include "vtkCubeSource.h"
#include "vtkCylinderSource.h"
#include "vtkSphereSource.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "sv_vtk_utils.h"

#ifdef SV_USE_VMTK
  #include "sv_vmtk_utils.h"
#endif

// ----------
// cvPolyDataSolid
// ----------
/**
 * @brief Constructor for cvPolyDataSolid (Should never be called directly)
 */

cvPolyDataSolid::cvPolyDataSolid()
  : cvSolidModel( SM_KT_POLYDATA)
{
/**
 * @brief Data Member is a vtkPolyData. It is initiated as NULL. When a
 * solid is loaded, a new PolyData is created
 */
  geom_ = NULL;
  numBoundaryRegions = 0;
}

// -----------
// ~cvPolyDataSolid
// -----------
/**
 * @brief Destructor for cvPolyDataSolid
 */

cvPolyDataSolid::~cvPolyDataSolid()
{
  if (geom_ != NULL)
  {
    geom_->Delete();
  }
}

// -----------
// ~Copy( const cvPolyDataSolid& sm)
// -----------
/**
 * @brief Copy Constructor for cvPolyDataSolid
 */
cvPolyDataSolid::cvPolyDataSolid( const cvPolyDataSolid& sm)
	: cvSolidModel( SM_KT_POLYDATA)
{
  geom_ = NULL;
  numBoundaryRegions=0;
  Copy( sm );
}

// -----------
// ~Copy( const cvSolidModel& src )
// -----------
/**
 * @brief Copy for cvSolidModel
 */

int cvPolyDataSolid::Copy(const cvSolidModel& src )
{
  cvPolyDataSolid *solidPtr;

  if (geom_ != NULL) {
    return SV_ERROR;
  }
  if (src.GetKernelT() != SM_KT_POLYDATA) {
    return SV_ERROR;
  }

  solidPtr = (cvPolyDataSolid *)( &src );
  if ( solidPtr->geom_ == NULL ) {
    return SV_OK;
  }

  geom_ = vtkPolyData::New();
  geom_->DeepCopy(solidPtr->geom_);
  numBoundaryRegions = solidPtr->numBoundaryRegions;

  return SV_OK;
}

// -----------
// SetVtkPolyDataObject
// -----------
/**
 * @brief Function to set the PolyData member object
 * @param *newPolyData Pointer to vtkPolyData object that you want to be
 * set as the class member data
 * @return SV_OK if executed correctly
 */

int cvPolyDataSolid::SetVtkPolyDataObject(vtkPolyData *newPolyData)
{
  if (geom_ != NULL) {
    geom_->Delete();
  }

  auto cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->SetInputData(newPolyData);
  cleaner->Update();

  geom_ = vtkPolyData::New();
  geom_->DeepCopy(cleaner->GetOutput());
  geom_->BuildLinks();

  if (VtkUtils_PDCheckArrayName(geom_,1,"ModelFaceID") == SV_OK)
  {
    int *faceIds;
    int result = PlyDtaUtils_GetFaceIds( geom_, &numBoundaryRegions, &faceIds);
    delete [] faceIds;
  }

  return SV_OK;
}

// ----
// Copy
// ----

cvSolidModel *cvPolyDataSolid::Copy() const
{
  cvPolyDataSolid *result = new cvPolyDataSolid(*this);
  return result;
}

// ----------
// ReadNative
// ----------
/**
 * @brief Function to load in a solid file
 * @param *filename Pointer to a char filename of the file to read in
 * @return SV_OK if executed correctly, SV_ERROR if the geometry is not NULL
 * or the read function does not return properly.
 * @note Current accepted filetypes include:
 * @note STL
 * @note VTP
 * @note VTK
 * @note PLY
 */

int cvPolyDataSolid::ReadNative( char *filename )
{
  if ( geom_ != NULL ) {
    return SV_ERROR;
  }
  geom_ = vtkPolyData::New();

  if ( PlyDtaUtils_ReadNative( filename, geom_) != SV_OK) {
    return SV_ERROR;
  }

  vtkSmartPointer<vtkCleanPolyData> cleaner =
    vtkSmartPointer<vtkCleanPolyData>::New();

  cleaner->SetInputData(geom_);
  cleaner->Update();

  geom_->DeepCopy(cleaner->GetOutput());
  geom_->BuildLinks();

  return SV_OK;
}


// -----------
// WriteNative
// -----------
/**
 * @brief Function to write the polydata
 * @param file_version int for filetype (UNUSED CURRENTLY)
 * @param *filename Pointer to a char filename of the file to write
 * @return SV_OK if executed correctly, SV_ERROR if the geometry is NULL
 * or the write function does not return properly.
 */

int cvPolyDataSolid::WriteNative( int file_version, char *filename ) const
{
  //Procdure calls PolyData Utils to write the file using vtkWriters
  if ( geom_ == NULL ) {
    return SV_ERROR;
  }

  if (PlyDtaUtils_WriteNative(geom_, file_version, filename ) != SV_OK) {
    return SV_ERROR;
  }

  return SV_OK;
}


// -----------
// GetPolyData
// -----------
/**
 * @brief Function to get the member vtkPolyData
 * @param useMaxDist UNUSED CURRENTLY
 * @param max_dist UNUSED CURRENTLY
 * @return *result: cvPolyData containing the member vtkPolyData
 */

cvPolyData *cvPolyDataSolid::GetPolyData(int useMaxDist, double max_dist) const
{
  ///Procedure returns the current vtkPolyData member as a cvPolyData
  cvPolyData *result;

  result = new cvPolyData(geom_);
  return result;
}


// ---------------
// GetFacePolyData
// ---------------
/**
 * @brief Procedure gets one face based on faceid defined in PolyDataUtils
 * @param face_id int which is the number of the face to extract
 * @param useMaxDist UNUSED CURRENTLY
 * @param max_dist UNUSED CURRENTLY
 * @return *result: cvPolyData containg the face vtkPolyData
 */

cvPolyData *cvPolyDataSolid::GetFacePolyData(int faceid, int useMaxDist, double max_dist) const
{
  vtkPolyData *facepd = vtkPolyData::New();
  cvPolyData *result;

  if (geom_ == NULL ) {
    return NULL;
  }

  if (PlyDtaUtils_GetFacePolyData(geom_, &faceid, facepd) != SV_OK)
  {
   fprintf(stderr,"ERROR: Failed to get Face of PolyData");
    return SV_ERROR;
  }

  //fprintf(stderr,"Check num Points: %d\n",facepd->GetNumberOfPoints());
  //fprintf(stderr,"Check Polys: %d\n",facepd->GetNumberOfPolys());

  cvPolyData *tmpresult = new cvPolyData(facepd);
  facepd->Delete();

  result = sys_geom_MergePts(tmpresult);

  delete tmpresult;

  return result;
}

// ----------------
// GetBoundaryFaces
// ----------------
/**
 * @brief Function to extract the boundaries for the member vtkPolyData
 * @param angle double that specifies the extraction angle. Any faces
 * with a difference between face normals larger than this angle will be
 * considered a separate face
 * @return *result: SV_ERROR is member data hasn't been loaded, or if the
 * GetBoundaryFaces function does not work properly. SV_OK is executed
 * properly
 */

int cvPolyDataSolid::GetBoundaryFaces(double angle)
{
  if (geom_ == NULL ) {
    return SV_ERROR;
  }

  if (PlyDtaUtils_GetBoundaryFaces(geom_,angle,&numBoundaryRegions) != SV_OK)
    return SV_ERROR;

  return SV_OK;
}

// ----------
// GetFaceIds
// ----------
/**
 * @brief Function to get the Ids associated with the face of the PolyData
 * @param *numFaces Pointer to the number of faces the poly should have
 * @param **faceIds Pointer to a pointer of vector containing the numerical
 * values corresponding to each face region
 * @return *result: SV_ERROR is member data hasn't been loaded, if the number
 * of faces is zero or faceIds don't exist
 */

int cvPolyDataSolid::GetFaceIds(int *numFaces,int **faceIds)
{
  //Procedure calls PolyDataUtils to get the different faceId numbers extracted from GetBoundaryFaces
  if ( geom_ == NULL ) {
      *numFaces = 0;
      faceIds = NULL;
      return SV_OK;
  }

  int result = PlyDtaUtils_GetFaceIds( geom_, &numBoundaryRegions, faceIds);
  *numFaces = numBoundaryRegions;

  return result;
}

// ----------------
// GetFaceAttribute
// ----------------

int cvPolyDataSolid::GetFaceAttribute(char *attr,int faceid, char **value)
{
  //Not sure if this procedure is needed currently
  if ( geom_ == NULL ) {
      *value = NULL;
      return SV_ERROR;
  }

  *value = NULL;
  return SV_ERROR;
}


// ----------------
// SetFaceAttribute
// ----------------

int cvPolyDataSolid::SetFaceAttribute(char *attr,int faceid, char *value)
{
  //Not sure if this procedure is needed currently

  if ( geom_ == NULL ) {
      *value = NULL;
      return SV_ERROR;
  }

  *value = NULL;
  return SV_ERROR;
}

// ----------------
// DeleteRegion
// ----------------
/**
 * @brief Function to delete a region in the polydata
 * @param regionid this is the region id to delete all of the cells in
 * @return SV_OK if executed correctly, SV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */

int cvPolyDataSolid::DeleteRegion(int regionid)
{

  if ( geom_ == NULL ) {
      fprintf(stderr,"Solid must exist in order to delete regions\n");
      return SV_ERROR;
  }
  if (numBoundaryRegions == 0) {
      fprintf(stderr,"Must have extracted boundaries in order to delete region\n");
      return SV_ERROR;
  }

  if (PlyDtaUtils_DeleteRegion(geom_,&regionid) != SV_OK)
  {
    fprintf(stderr,"Error: Faces were not deleted correctly\n");
    return SV_ERROR;
  }

  //Must update the number of regions
  numBoundaryRegions = numBoundaryRegions - 1;


  return SV_OK;
}

// ----------------
// Intersect
// ----------------
/**
 * @brief Function to intersect two PolyData using the vtk Booleans filter
 * @param *a Pointer to the first object to be intersected
 * @param *b Pointer to the second object to be intersected
 * @return SV_ERROR if the objects don't exist of the filter doesn't work
 * correctly
 */

int cvPolyDataSolid::Intersect( cvSolidModel *a, cvSolidModel *b,
       		 SolidModel_SimplifyT st )
{
  //Geometry should be empty prior to boolean
  if (geom_ != NULL)
  {
    return SV_ERROR;
  }

  //Need both objects to create an intersection
  if (a == NULL)
    return SV_ERROR;
  if (a->GetKernelT() != SM_KT_POLYDATA ) {
    fprintf(stderr,"Model not of type POLYDATA\n");
    return SV_ERROR;
  }

  if (b == NULL)
    return SV_ERROR;
  if (b->GetKernelT() != SM_KT_POLYDATA ) {
    fprintf(stderr,"Model not of type POLYDATA\n");
    return SV_ERROR;
  }
  vtkSVLoopBooleanPolyDataFilter *intersectPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  intersectPolyData = vtkSVLoopBooleanPolyDataFilter::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();;

  intersectPolyData->SetOperationToIntersection();

  intersectPolyData->SetInputData(0,pd1);
  intersectPolyData->SetInputData(1,pd2);
  intersectPolyData->Update();

  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();
  normaler->SetInputData(intersectPolyData->GetOutput());
  normaler->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(normaler->GetOutput());

  intersectPolyData->Delete();

  return SV_OK;


}
// ----------------
// Union
// ----------------
/**
 * @brief Function to union two PolyData using the vtk Booleans filter
 * @param *a Pointer to the first object to be unioned
 * @param *b Pointer to the second object to be unioned
 * @return SV_ERROR if the objects don't exist of the filter doesn't work
 * correctly
 */

int cvPolyDataSolid::Union( cvSolidModel *a, cvSolidModel *b,
       		 SolidModel_SimplifyT st )
{
  //Geometry should be empty prior to boolean
  if (geom_ != NULL)
  {
    return SV_ERROR;
  }

  //Need both objects to create a union
  if (a == NULL)
    return SV_ERROR;
  if (a->GetKernelT() != SM_KT_POLYDATA ) {
    fprintf(stderr,"Model not of type POLYDATA\n");
    return SV_ERROR;
  }
  if (b == NULL)
    return SV_ERROR;
  if (b->GetKernelT() != SM_KT_POLYDATA ) {
    fprintf(stderr,"Model not of type POLYDATA\n");
    return SV_ERROR;
  }

  vtkSVLoopBooleanPolyDataFilter *unionPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  unionPolyData = vtkSVLoopBooleanPolyDataFilter::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();

  unionPolyData->SetOperationToUnion();

  unionPolyData->SetInputData(0,pd1);
  unionPolyData->SetInputData(1,pd2);
  unionPolyData->Update();

  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();
  normaler->SetInputData(unionPolyData->GetOutput());
  normaler->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(normaler->GetOutput());

  unionPolyData->Delete();

  return SV_OK;
}
// ----------------
// Subtract
// ----------------
/**
 * @brief Function to subtract one PolyData from another using the
 * vtk Booleans filter
 * @param *a Pointer to the first object to be subtracted
 * @param *b Pointer to the second object to be subtracted
 * @return SV_ERROR if the objects don't exist of the filter doesn't work
 * correctly
 */

int cvPolyDataSolid::Subtract( cvSolidModel *a, cvSolidModel *b,
       		SolidModel_SimplifyT st )
{
  //Geometry should be empty prior to boolean
  if (geom_ != NULL)
  {
    return SV_ERROR;
  }

  //Need both objects to create a subtraction
  if (a == NULL)
    return SV_ERROR;
  if (a->GetKernelT() != SM_KT_POLYDATA ) {
    fprintf(stderr,"Model not of type POLYDATA\n");
    return SV_ERROR;
  }

  if (b == NULL)
    return SV_ERROR;
  if (b->GetKernelT() != SM_KT_POLYDATA ) {
    fprintf(stderr,"Model not of type POLYDATA\n");
    return SV_ERROR;
  }
  vtkSVLoopBooleanPolyDataFilter *subtractPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  subtractPolyData = vtkSVLoopBooleanPolyDataFilter::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();;

  subtractPolyData->SetOperationToDifference();

  subtractPolyData->SetInputData(0,pd1);
  subtractPolyData->SetInputData(1,pd2);
  subtractPolyData->Update();

  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();
  normaler->SetInputData(subtractPolyData->GetOutput());
  normaler->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(normaler->GetOutput());

  subtractPolyData->Delete();

  return SV_OK;
}

// -------------------
// DeleteFaces
// -------------------
/**
 * @brief Function to delete the cells in the polydata
 * @param numfaces this is the number of cells to delete from the polydata
 * @param faces this is an array containing the ids of the cells to delete
 * @return SV_OK if executed correctly, SV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
int cvPolyDataSolid::DeleteFaces( int numfaces, int *faces)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need PolyData to perform operation\n");
    return SV_ERROR;
  }

  if (PlyDtaUtils_DeleteCells(geom_,&numfaces,faces) != SV_OK)
  {
    fprintf(stderr,"Error: Faces were not deleted correctly\n");
    return SV_ERROR;
  }



  return SV_OK;
}

// -------------------
// CombineFaces
// -------------------
/**
 * @brief Function to combine the ids of two faces in the polydata
 * @param targetface id of the face to set the two faces to have new id of
 * @param loseface id of the second face, id will be lost
 * @return SV_OK if executed correctly, SV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
int cvPolyDataSolid::CombineFaces( int targetface, int loseface)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need PolyData to perform operation\n");
    return SV_ERROR;
  }

  if (PlyDtaUtils_CombineFaces(geom_,&targetface,&loseface) != SV_OK)
  {
    fprintf(stderr,"Error: Faces were not combined correctly\n");
    return SV_ERROR;
  }

  //Must update the number of regions
  numBoundaryRegions = numBoundaryRegions - 1;

  return SV_OK;
}

int cvPolyDataSolid::RemeshFace(int numfaces,int *excludedFaces,double size)
{
#ifdef SV_USE_VMTK
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need PolyData to perform operation\n");
    return SV_ERROR;
  }

  int i;
  vtkSmartPointer<vtkIdList> excluded =
    vtkSmartPointer<vtkIdList>::New();

  for (i = 0; i< numfaces; i++)
  {
    excluded->InsertNextId(excludedFaces[i]);
  }

  int meshcaps = 1;
  int preserveedges = 0;
  int triangleoutput = 1;
  int collapseanglethreshold = NULL;
  int trianglesplitfactor = NULL;
  int useSizeFunction = 0;
  std::string markerListName = "ModelFaceID";

  if (VMTKUtils_SurfaceRemeshing(geom_,size,meshcaps,preserveedges,
	trianglesplitfactor,collapseanglethreshold,excluded,
	markerListName,useSizeFunction,NULL) != SV_OK)
  {
    fprintf(stderr,"Issue while remeshing surface\n");
    return SV_ERROR;
  }


  return SV_OK;
#else
  fprintf(stderr,"Must have VMTK to be able to remesh caps\n");
  return SV_ERROR;
#endif
}

// ----------------
// MakeBox3d
// ----------------
/**
 * Creates a 3D Box
 * @param dims
 * @param ctr
 * @return *result: a box is created
 */

int cvPolyDataSolid::MakeBox3d(double dims[], double ctr[])
{
  if ( geom_ != NULL ) {
    return SV_ERROR;
  }
  geom_ = vtkPolyData::New();

  vtkSmartPointer<vtkCubeSource> cube = vtkSmartPointer<vtkCubeSource>::New();
  cube->SetCenter(ctr[0], ctr[1], ctr[2]);
  cube->SetXLength(dims[0]);
  cube->SetYLength(dims[1]);
  cube->SetZLength(dims[2]);
  cube->Update();

  vtkSmartPointer<vtkTriangleFilter> triangulator =
    vtkSmartPointer<vtkTriangleFilter>::New();
  triangulator->SetInputData(cube->GetOutput());
  triangulator->Update();

  geom_->DeepCopy(triangulator->GetOutput());

  return SV_OK;
}

// ----------------
// MakeSphere
// ----------------
/**
 * Creates a Sphere
 * @param r
 * @param ctr
 * @return *result a sphere
 */

int cvPolyDataSolid::MakeSphere(double r, double ctr[])
{
  if ( geom_ != NULL ) {
    return SV_ERROR;
  }
  geom_ = vtkPolyData::New();

  vtkSmartPointer<vtkSphereSource> sphere =
    vtkSmartPointer<vtkSphereSource>::New();
  sphere->SetCenter(ctr[0], ctr[1], ctr[2]);
  sphere->SetRadius(r);
  sphere->SetThetaResolution(50);
  sphere->SetPhiResolution(50);
  sphere->Update();

  vtkSmartPointer<vtkTriangleFilter> triangulator =
    vtkSmartPointer<vtkTriangleFilter>::New();
  triangulator->SetInputData(sphere->GetOutput());
  triangulator->Update();

  geom_->DeepCopy(triangulator->GetOutput());

  return SV_OK;
}

//--------------
// MakeCylinder
//--------------
//
// Create a cylinder aligned with an axis.
//
// A VTK cylinder is by default oriented along the vector [0, 1, 0].
// The cylinder is rotated by the angle between [0, 1, 0] and axis about 
// the vector [0, 1, 0] x axis.  
//
// Arguments:
//   r: Cylinder radius.
//   length: Cylinder length. 
//   center: Cylinder center. 
//   axis: Cylinder axis. 
//
int cvPolyDataSolid::MakeCylinder(double r, double length, double center[3], double axis[3] )
{
  if ( geom_ != NULL ) {
    return SV_ERROR;
  }
  geom_ = vtkPolyData::New();

  // Create a cylinder oriented along [0.0, 1.0, 0.0].
  vtkSmartPointer<vtkCylinderSource> cylinder = vtkSmartPointer<vtkCylinderSource>::New();
  cylinder->SetCenter(0.0,0.0,0.0);
  cylinder->SetHeight(length);
  cylinder->SetRadius(r);
  cylinder->SetResolution(50);
  cylinder->Update();

  // Define the transformation rotating the default cylinder 
  // to the given axis.
  double vec[3] = {0.0, 1.0, 0.0}; 
  double rotateAxis[3], tmpCross[3];
  vtkMath::Cross(vec, axis, rotateAxis);
  vtkMath::Cross(vec, axis, tmpCross);
  auto radangle = acos(vtkMath::Dot(axis,vec));
  auto degangle = vtkMath::DegreesFromRadians(radangle);
  vtkSmartPointer<vtkTransform> transformer = vtkSmartPointer<vtkTransform>::New();
  transformer->Translate(center[0],center[1],center[2]);
  transformer->RotateWXYZ(degangle, rotateAxis);

  // Transform the cylinder geometry.
  vtkSmartPointer<vtkTransformPolyDataFilter> polyDataTransformer = vtkSmartPointer<vtkTransformPolyDataFilter>::New();
  polyDataTransformer->SetInputData(cylinder->GetOutput());
  polyDataTransformer->SetTransform(transformer);
  polyDataTransformer->Update();

  // Get the transformed cylinder geometry.
  vtkSmartPointer<vtkTriangleFilter> triangulator = vtkSmartPointer<vtkTriangleFilter>::New();
  triangulator->SetInputData(polyDataTransformer->GetOutput());
  triangulator->Update();

  geom_->DeepCopy(triangulator->GetOutput());

  return SV_OK;
}

