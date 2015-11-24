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

/** @file cvPolyDataSolid.cxx
 *  @brief The implementations of functions in cvPolyDataSolid
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#include "SimVascular.h" 

#include "cvPolyDataSolid.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkBooleanOperationPolyDataFilter2.h"
#include "vtkCubeSource.h"
#include "cv_get_tcl_interp_init.h"
#include "cv_polydatasolid_utils.h"
#include "cv_misc_utils.h"
#include "cv_sys_geom.h"
#include <string.h>
#include <assert.h>
#include "vtkCubeSource.h"

#ifdef USE_GTS
  #include "vtkSurfaceBooleanOperations.h"
#endif

#ifdef USE_VMTK
  #include "cv_VMTK_utils.h"
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
    return CV_ERROR;
  }
  if (src.GetKernelT() != SM_KT_POLYDATA) {
    return CV_ERROR;
  }

  solidPtr = (cvPolyDataSolid *)( &src );
  if ( solidPtr->geom_ == NULL ) {
    return CV_OK;
  }

  geom_ = vtkPolyData::New();
  geom_->DeepCopy(solidPtr->geom_);

  return CV_OK;
}

// -----------
// SetVtkPolyDataObject
// -----------
/** 
 * @brief Function to set the PolyData member object
 * @param *newPolyData Pointer to vtkPolyData object that you want to be 
 * set as the class member data
 * @return CV_OK if executed correctly
 */

int cvPolyDataSolid::SetVtkPolyDataObject(vtkPolyData *newPolyData)
{
  if (geom_ != NULL)
  {
    geom_->Delete();
  }

  geom_ = vtkPolyData::New();
  geom_->DeepCopy(newPolyData);

  return CV_OK;
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
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is not NULL
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
    return CV_ERROR;
  }
  geom_ = vtkPolyData::New();

  if ( PlyDtaUtils_ReadNative( filename, geom_) != CV_OK) {
    return CV_ERROR;
  }

  vtkSmartPointer<vtkCleanPolyData> cleaner = 
    vtkSmartPointer<vtkCleanPolyData>::New();

  cleaner->SetInputData(geom_);
  cleaner->Update();
  
  geom_->DeepCopy(cleaner->GetOutput());
  geom_->BuildLinks();

  return CV_OK;
}


// -----------
// WriteNative
// -----------
/** 
 * @brief Function to write the polydata
 * @param file_version int for filetype (UNUSED CURRENTLY)
 * @param *filename Pointer to a char filename of the file to write
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the write function does not return properly.
 */

int cvPolyDataSolid::WriteNative( int file_version, char *filename ) const
{
  //Procdure calls PolyData Utils to write the file using vtkWriters
  if ( geom_ == NULL ) {
    return CV_ERROR;
  }

  if (PlyDtaUtils_WriteNative(geom_, file_version, filename ) != CV_OK) {
    return CV_ERROR;
  }

  return CV_OK;
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

  if (PlyDtaUtils_GetFacePolyData(geom_, &faceid, facepd) != CV_OK)
  {
   fprintf(stderr,"ERROR: Failed to get Face of PolyData"); 
    return CV_ERROR;
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
 * @return *result: CV_ERROR is member data hasn't been loaded, or if the 
 * GetBoundaryFaces function does not work properly. CV_OK is executed 
 * properly
 */

int cvPolyDataSolid::GetBoundaryFaces(double angle)
{
  if (geom_ == NULL ) {
    return CV_ERROR;
  }

  if (PlyDtaUtils_GetBoundaryFaces(geom_,angle,&numBoundaryRegions) != CV_OK)
    return CV_ERROR;

  return CV_OK;
}

// ----------
// GetFaceIds
// ----------
/** 
 * @brief Function to get the Ids associated with the face of the PolyData
 * @param *numFaces Pointer to the number of faces the poly should have
 * @param **faceIds Pointer to a pointer of vector containing the numerical
 * values corresponding to each face region
 * @return *result: CV_ERROR is member data hasn't been loaded, if the number
 * of faces is zero or faceIds don't exist 
 */

int cvPolyDataSolid::GetFaceIds(int *numFaces,int **faceIds)
{
  //Procedure calls PolyDataUtils to get the different faceId numbers extracted from GetBoundaryFaces
  if ( geom_ == NULL ) {
      *numFaces = 0;
      faceIds = NULL;
      return CV_OK;
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
      return CV_ERROR;
  }

  *value = NULL;
  return CV_ERROR;
}


// ----------------
// SetFaceAttribute
// ----------------

int cvPolyDataSolid::SetFaceAttribute(char *attr,int faceid, char *value)
{
  //Not sure if this procedure is needed currently

  if ( geom_ == NULL ) {
      *value = NULL;
      return CV_ERROR;
  }

  *value = NULL;
  return CV_ERROR;
}

// ----------------
// DeleteRegion
// ----------------
/** 
 * @brief Function to delete a region in the polydata
 * @param regionid this is the region id to delete all of the cells in
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */

int cvPolyDataSolid::DeleteRegion(int regionid)
{

  if ( geom_ == NULL ) {
      fprintf(stderr,"Solid must exist in order to delete regions\n");
      return CV_ERROR;
  }
  if (numBoundaryRegions == 0) {
      fprintf(stderr,"Must have extracted boundaries in order to delete region\n");
      return CV_ERROR;
  }

  if (PlyDtaUtils_DeleteRegion(geom_,&regionid) != CV_OK)
  {
    fprintf(stderr,"Error: Faces were not deleted correctly\n");
    return CV_ERROR;
  }

  //Must update the number of regions
  numBoundaryRegions = numBoundaryRegions - 1;

  
  return CV_OK;
}

// ----------------
// Intersect
// ----------------
/** 
 * @brief Function to intersect two PolyData using the vtk Booleans filter
 * @param *a Pointer to the first object to be intersected
 * @param *b Pointer to the second object to be intersected
 * @return CV_ERROR if the objects don't exist of the filter doesn't work
 * correctly 
 */

int cvPolyDataSolid::Intersect( cvSolidModel *a, cvSolidModel *b,
       		 SolidModel_SimplifyT st ) 
{
  //Geometry should be empty prior to boolean
  if (geom_ != NULL)
  {
    return CV_ERROR;
  }

  //Need both objects to create an intersection
  if (a == NULL)
  {
    return CV_ERROR;
  }
  if (b == NULL)
  {
    return CV_ERROR;
  }
#ifdef USE_GTS
  vtkSurfaceBooleanOperations *intersectPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  intersectPolyData = vtkSurfaceBooleanOperations::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();

  intersectPolyData->AddInputData(pd1);
  intersectPolyData->AddInputData(pd2);
  intersectPolyData->SetModeToIntersection();
  intersectPolyData->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(intersectPolyData->GetOutput());

  intersectPolyData->Delete();
#else
  vtkBooleanOperationPolyDataFilter2 *intersectPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  intersectPolyData = vtkBooleanOperationPolyDataFilter2::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();;

  intersectPolyData->SetOperationToIntersection();

  intersectPolyData->SetInputData(0,pd1);
  intersectPolyData->SetInputData(1,pd2);
  intersectPolyData->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(intersectPolyData->GetOutput());

  intersectPolyData->Delete();
#endif

  return CV_OK;


}
// ----------------
// Union
// ----------------
/** 
 * @brief Function to union two PolyData using the vtk Booleans filter
 * @param *a Pointer to the first object to be unioned
 * @param *b Pointer to the second object to be unioned
 * @return CV_ERROR if the objects don't exist of the filter doesn't work
 * correctly 
 */

int cvPolyDataSolid::Union( cvSolidModel *a, cvSolidModel *b,
       		 SolidModel_SimplifyT st )
{
  //Geometry should be empty prior to boolean
  if (geom_ != NULL)
  {
    return CV_ERROR;
  }

  //Need both objects to create a union
  if (a == NULL)
  {
    return CV_ERROR;
  }
  if (b == NULL)
  {
    return CV_ERROR;
  }
#ifdef USE_GTS
  vtkSurfaceBooleanOperations *unionPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  unionPolyData = vtkSurfaceBooleanOperations::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();

  unionPolyData->AddInputData(pd1);
  unionPolyData->AddInputData(pd2);
  unionPolyData->SetModeToUnion();
  unionPolyData->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(unionPolyData->GetOutput());

  unionPolyData->Delete();
#else
  vtkBooleanOperationPolyDataFilter2 *unionPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  unionPolyData = vtkBooleanOperationPolyDataFilter2::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();

  unionPolyData->SetOperationToUnion();

  unionPolyData->SetInputData(0,pd1);
  unionPolyData->SetInputData(1,pd2);
  unionPolyData->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(unionPolyData->GetOutput());

  unionPolyData->Delete();
#endif

  return CV_OK;
}
// ----------------
// Subtract
// ----------------
/** 
 * @brief Function to subtract one PolyData from another using the 
 * vtk Booleans filter
 * @param *a Pointer to the first object to be subtracted
 * @param *b Pointer to the second object to be subtracted
 * @return CV_ERROR if the objects don't exist of the filter doesn't work
 * correctly 
 */

int cvPolyDataSolid::Subtract( cvSolidModel *a, cvSolidModel *b,
       		SolidModel_SimplifyT st )
{
  //Geometry should be empty prior to boolean
  if (geom_ != NULL)
  {
    return CV_ERROR;
  }

  //Need both objects to create a subtraction
  if (a == NULL)
  {
    return CV_ERROR;
  }
  if (b == NULL)
  {
    return CV_ERROR;
  }
#ifdef USE_GTS
  vtkSurfaceBooleanOperations *subtractPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  subtractPolyData = vtkSurfaceBooleanOperations::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();

  subtractPolyData->AddInputData(pd1);
  subtractPolyData->AddInputData(pd2);
  subtractPolyData->SetModeToDifference();
  subtractPolyData->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(subtractPolyData->GetOutput());

  subtractPolyData->Delete();
#else
  vtkBooleanOperationPolyDataFilter2 *subtractPolyData;
  vtkPolyData *pd1;
  vtkPolyData *pd2;

  subtractPolyData = vtkBooleanOperationPolyDataFilter2::New();
  pd1 = (a->GetPolyData(0,0.))->GetVtkPolyData();
  pd2 = (b->GetPolyData(0,0.))->GetVtkPolyData();;

  subtractPolyData->SetOperationToDifference();

  subtractPolyData->SetInputData(0,pd1);
  subtractPolyData->SetInputData(1,pd2);
  subtractPolyData->Update();

  //set output vtp to output from filter
  geom_ = vtkPolyData::New();
  geom_->DeepCopy(subtractPolyData->GetOutput());

  subtractPolyData->Delete();
#endif

  return CV_OK;
}

// -------------------
// DeleteFaces
// -------------------
/** 
 * @brief Function to delete the cells in the polydata
 * @param numfaces this is the number of cells to delete from the polydata
 * @param faces this is an array containing the ids of the cells to delete
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
int cvPolyDataSolid::DeleteFaces( int numfaces, int *faces)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need PolyData to perform operation\n");
    return CV_ERROR;
  }

  if (PlyDtaUtils_DeleteCells(geom_,&numfaces,faces) != CV_OK)
  {
    fprintf(stderr,"Error: Faces were not deleted correctly\n");
    return CV_ERROR;
  }



  return CV_OK;
}

// -------------------
// CombineFaces
// -------------------
/** 
 * @brief Function to combine the ids of two faces in the polydata
 * @param targetface id of the face to set the two faces to have new id of
 * @param loseface id of the second face, id will be lost
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
int cvPolyDataSolid::CombineFaces( int targetface, int loseface)
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need PolyData to perform operation\n");
    return CV_ERROR;
  }

  if (PlyDtaUtils_CombineFaces(geom_,&targetface,&loseface) != CV_OK)
  {
    fprintf(stderr,"Error: Faces were not combined correctly\n");
    return CV_ERROR;
  }
  
  //Must update the number of regions
  numBoundaryRegions = numBoundaryRegions - 1;

  return CV_OK;
}

int cvPolyDataSolid::RemeshFace(int numfaces,int *excludedFaces,double size)
{
#ifdef USE_VMTK
  if (geom_ == NULL)
  {
    fprintf(stderr,"Need PolyData to perform operation\n");
    return CV_ERROR;
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
	markerListName,useSizeFunction,NULL) != CV_OK)
  {
    fprintf(stderr,"Issue while remeshing surface\n");
    return CV_ERROR;
  }


  return CV_OK;
#else
  fprintf(stderr,"Must have VMTK to be able to remesh caps\n");
  return CV_ERROR;
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
    return CV_ERROR;
  }
  geom_ = vtkPolyData::New();

vtkSmartPointer<vtkCubeSource> cube = vtkSmartPointer<vtkCubeSource>::New();
cube->SetCenter(ctr[0], ctr[1], ctr[2]);
cube->SetXLength(dims[0]);
cube->SetYLength(dims[1]);
cube->SetZLength(dims[2]);
cube->Update();

geom_->DeepCopy(cube->GetOutput());

  return CV_OK;
}

