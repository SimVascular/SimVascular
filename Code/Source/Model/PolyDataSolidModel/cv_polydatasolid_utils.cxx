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

/** @file cv_polydatasolid_utils.cxx
 *  @brief The implementations of functions in cv_polydatasolid_utils
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "SimVascular.h" 

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include "cv_polydatasolid_utils.h"
#include "cv_misc_utils.h"
#include "cv_sys_geom.h"

#include "cv_globals.h"
#include "vtkSmartPointer.h"
#include "vtkSTLReader.h"
#include "vtkSTLWriter.h"
#include "vtkGenericDataObjectReader.h"
#include "vtkGenericDataObjectWriter.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkPLYReader.h"
#include "vtkPLYWriter.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkGetBoundaryFaces.h"
#include "vtkUnstructuredGrid.h"
#include "vtkGeometryFilter.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkAppendFilter.h"

// -------------
// PlyDtaUtils_Init
// -------------
/** 
 * @brief Initialization function for cv_polydata_utils (not necessary) 
 */

int PlyDtaUtils_Init()
{
  return CV_OK;
}


// ---------------------
// PlyDtaUtils_GetFaceIds
// ---------------------
/** 
 * @brief Procedure to get face numbers that correspond to the scalars 
 * assigned to the geometry in vtkGetBoundaryFaces.cxx 
 * @param *geom input vtkPolyData on which to get the face ids
 * @param *v_num_faces int that contains the number of total face regions
 * @param **v_faces vector containing the array of numerical values 
 * corresponding to each face region
 * @return CV_OK if function completes properly
 */

int PlyDtaUtils_GetFaceIds( vtkPolyData *geom, int *v_num_faces, int **v_faces)
{
  //Initiate variables used by function
  vtkSmartPointer<vtkIntArray> boundaryScalars;
  vtkIdType faceId;
  vtkIdType value;
  vtkIdType max = 0;
  int i;
  int *faceNums;
  bool *checkNums;
  int check = 0;
  int faceid = 0;
  double range[2];
  *v_faces = NULL;

  boundaryScalars = vtkSmartPointer<vtkIntArray>::New();
  if (PlyDtaUtils_PDCheckArrayName(geom,1,"ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified");
    fprintf(stderr," and named 'ModelFaceID' prior to this function call\n");
    *v_num_faces = 0;
    return CV_OK;
  }
  boundaryScalars = vtkIntArray::SafeDownCast(geom->GetCellData()->GetArray("ModelFaceID"));
//  boundaryScalars = static_cast<vtkIntArray*>(geom->GetCellData()->GetArray("ModelFaceID"));
  
  boundaryScalars->GetRange(range,0);

  max = range[1];
  
  checkNums = new bool[max];
  for (i=0;i<max;i++)
  {
    checkNums[i] = false;
  }

  for (faceId=0;faceId<geom->GetNumberOfPolys();faceId++)
  {
    value = boundaryScalars->GetValue(faceId);
    if (checkNums[value-1] == false)
    {
      check++;
      checkNums[value-1] = true;
    }
  }
  *v_num_faces = check;

  faceNums = new int[check];
  for (i=0;i<max;i++)
  {
    if (checkNums[i] == true)
    { 
      faceNums[faceid++] = i+1;
    }
  }
  *v_faces = faceNums;

  delete [] checkNums;

  return CV_OK;
  //}
}


// -------------------
// PlyDtaUtils_GetBoundaryFaces
// -------------------
// 
/** 
 * @brief Custom filter to extract the boundaries from surface  
 * @param *geom input vtkPolyData on which to get the surfaces
 * @param angle double that specifies the extraction angle. Any faces
 * with a difference between face normals larger than this angle will be 
 * considered a separate face
 * @return CV_OK if function completes properly
 */

int PlyDtaUtils_GetBoundaryFaces( vtkPolyData *geom,double angle,int *numRegions)
{
  //Create BoundarySurface Filter to get the boundaries
  vtkSmartPointer<vtkGetBoundaryFaces> boundFacs; 

  //Custom Filter vtkGetBoundaryFaces located in Core/TetMesh folder. Uses
  //vtkFeatureEdges as a base class
  boundFacs= vtkSmartPointer<vtkGetBoundaryFaces>::New();
  boundFacs->SetInputData(geom);
  boundFacs->SetFeatureAngle(angle);
  boundFacs->Update();

  //Transfer information from filter to class member
  geom->SetPoints(boundFacs->GetOutput()->GetPoints());
  geom->SetPolys(boundFacs->GetOutput()->GetPolys());
  geom->SetLines(boundFacs->GetOutput()->GetLines());
  geom->GetPointData()->PassData(boundFacs->GetOutput()->GetPointData());
  geom->GetCellData()->PassData(boundFacs->GetOutput()->GetCellData());
  geom->BuildLinks();

  *numRegions = boundFacs->GetNumberOfRegions();

  return CV_OK;

}

// -------------------
// PlyDtaUtils_GetFacePolyData
// -------------------
/** 
 * @brief Based on Scalars Defined by the GetBoundaryFaces filter, 
 * separate into face VTKs   
 * @param *geom input vtkPolyData on which to get the face PolyData
 * @param angle double that specifies the extraction angle. Any faces
 * with a difference between face normals larger than this angle will be 
 * considered a separate face
 * @return CV_OK if function completes properly
 * @note There is another method to do this that does not retain id 
 * information. It may be faster, but doesn't reatain info
 */
// 

int PlyDtaUtils_GetFacePolyData(vtkPolyData *geom, int *faceid, vtkPolyData *facepd)
{

  vtkSmartPointer<vtkThreshold> idThreshold = vtkSmartPointer<vtkThreshold>::New();
  vtkSmartPointer<vtkUnstructuredGrid> tempGrid = vtkSmartPointer<vtkUnstructuredGrid>::New();
  vtkSmartPointer<vtkDataSetSurfaceFilter> getPoly = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();

  double facenum;
  
  facenum = (double) *faceid;
  idThreshold->SetInputData(geom);
  //Set Input Array to 0 port,0 connection,1 for Cell Data, and Regions is the type name
  idThreshold->SetInputArrayToProcess(0,0,0,1,"ModelFaceID");
  idThreshold->ThresholdBetween(facenum,facenum);
  idThreshold->Update();

  tempGrid = idThreshold->GetOutput();

  getPoly->SetInputData(tempGrid);
  getPoly->Update();
  
  facepd->DeepCopy(getPoly->GetOutput());

  if (facepd->GetNumberOfPoints() != tempGrid->GetNumberOfPoints())
  {
    fprintf(stderr,"Transfer to Face PolyData was ineffective");
    return CV_ERROR;
  }
  return CV_OK;

}

// -------------------
// PlyDtaUtils_ReadNative
// -------------------
/** 
 * @brief Function to load in a solid file
 * @param *filename Pointer to a char filename of the file to read in
 * @param *result vtkPolyData that is to store input file info
 * @return CV_OK if executed correctly
 * @note Current accepted filetypes include:
 * @note STL
 * @note VTP
 * @note VTK
 * @note PLY
 */

int PlyDtaUtils_ReadNative( char *filename, vtkPolyData *result)
{
  const char *extension = strrchr(filename,'.');
  extension = extension +1;

  //Stereolithography Input
  if (!strncmp(extension,"stl",3)) {
    vtkSmartPointer<vtkSTLReader> reader = vtkSmartPointer<vtkSTLReader>::New();
    reader->SetFileName(filename);
    reader->Update();

    result->DeepCopy(reader->GetOutput());
    result->BuildLinks();
  }
  //VTK PolyData Input
  else if (!strncmp(extension,"vtp",3)) {
    vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
    reader->SetFileName(filename);
    reader->Update();

    result->DeepCopy(reader->GetOutput());
    result->BuildLinks();
  }

  //Legacy VTK
  else if (!strncmp(extension,"vtk",3)) {
    vtkSmartPointer<vtkGenericDataObjectReader> reader = vtkSmartPointer<vtkGenericDataObjectReader>::New();
    reader->SetFileName(filename);
    reader->Update();

    result->DeepCopy(reader->GetOutput());
    result->BuildLinks();
  }

  //Polygon File Format PLY
  else if (!strncmp(extension,"ply",3)) {
    vtkSmartPointer<vtkPLYReader> reader = vtkSmartPointer<vtkPLYReader>::New();
    reader->SetFileName(filename);
    reader->Update();

    result->DeepCopy(reader->GetOutput());
    result->BuildLinks();
  }

  else {
    fprintf(stderr,"Filetype is not supported");
    return CV_ERROR;
  }
  return CV_OK;
}
  
// -------------------
// PlyDtaUtils_WriteNative
// -------------------
/** 
 * @brief Function to write the polydata
 * @param *filename Pointer to a char filename of the file to write
 * @param file_version int for filetype
 * @param *geom vtkPolyData that contains the info to be written to file
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the write function does not return properly.
 */
    
int PlyDtaUtils_WriteNative( vtkPolyData *geom, int file_version, char *filename )
{
  const char *extension = strrchr(filename,'.');
  extension = extension +1;

  if (!strncmp(extension,"vtk",3))
  {
    //Writing a legacy vtk file
    vtkSmartPointer<vtkGenericDataObjectWriter> writer 
      = vtkSmartPointer<vtkGenericDataObjectWriter>::New();

    writer->SetInputData(geom);
    writer->SetFileName(filename);
    writer->Update();

    writer->Write();
  }
  else if (!strncmp(extension,"vtp",3))
  {
    //Writing a vtp file
    vtkSmartPointer<vtkXMLPolyDataWriter> writer 
      = vtkSmartPointer<vtkXMLPolyDataWriter>::New();

    writer->SetInputData(geom);
    writer->SetFileName(filename);
    writer->Update();

    writer->Write();
  } 
  else if (!strncmp(extension,"vtu",3))
  {
    //Writing a vtu file
    vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer 
      = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    vtkSmartPointer<vtkAppendFilter> converter 
      = vtkSmartPointer<vtkAppendFilter>::New();

    converter->AddInputData(geom);
    converter->Update();

    writer->SetInputData(converter->GetOutput());
    writer->SetFileName(filename);
    writer->Update();

    writer->Write();
  }
  else if (!strncmp(extension,"stl",3))
  {
    //Writing an stl file
    vtkSmartPointer<vtkSTLWriter> writer 
      = vtkSmartPointer<vtkSTLWriter>::New();

    writer->SetInputData(geom);
    writer->SetFileName(filename);
    writer->Update();

    writer->Write();
  }
  else if (!strncmp(extension,"ply",3))
  {
    //Writing an stl file
    vtkSmartPointer<vtkPLYWriter> writer 
      = vtkSmartPointer<vtkPLYWriter>::New();

    writer->SetInputData(geom);
    writer->SetFileName(filename);
    writer->Update();

    writer->Write();
  }
  else
  {
    fprintf(stderr,"File version is not accepted\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -------------------
// PlyDtaUtils_CombineFaces
// -------------------
/** 
 * @brief Function to combine the ids of two faces in the polydata
 * @param targetface id of the face to set the two faces to have new id of
 * @param loseface id of the second face, id will be lost
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
    
int PlyDtaUtils_CombineFaces(vtkPolyData *geom,int *targetface,int *loseface )
{
  int id1;
  int id2;
  vtkIdType cellId;
  vtkSmartPointer<vtkIntArray> boundaryRegions = 
    vtkSmartPointer<vtkIntArray>::New();

  id1 = *targetface;
  id2 = *loseface;

  if (PlyDtaUtils_PDCheckArrayName(geom,1,"ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified \
		    and named 'ModelFaceID' prior to this function call\n");
    return CV_ERROR;
  }
  boundaryRegions = vtkIntArray::SafeDownCast(geom->GetCellData()->
		    GetScalars("ModelFaceID"));
  
  for (cellId = 0;cellId<geom->GetNumberOfCells();cellId++)
  {
    if (boundaryRegions->GetValue(cellId) == id2)
    {
      boundaryRegions->SetValue(cellId,id1);
    }
  }
  geom->GetCellData()->RemoveArray("ModelFaceID");
  boundaryRegions->SetName("ModelFaceID");
  geom->GetCellData()->AddArray(boundaryRegions);
  geom->GetCellData()->SetActiveScalars("ModelFaceID");

  return CV_OK;
}

// -------------------
// PlyDtaUtils_DeleteCells
// -------------------
/** 
 * @brief Function to delete the cells in the polydata
 * @param numfaces this is the number of cells to delete from the polydata
 * @param faces this is an array containing the ids of the cells to delete
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
    
int PlyDtaUtils_DeleteCells(vtkPolyData *geom,int *numcells,int *cells )
{
  int i;
  int numCells = *numcells;
  vtkIdType cellId;

  geom->BuildLinks();
  for (int i=0; i< numCells; i++)
  {
    geom->DeleteCell(cells[i]);
  }

  geom->RemoveDeletedCells();

  return CV_OK;
}

// -------------------
// PlyDtaUtils_DeleteRegion
// -------------------
/** 
 * @brief Function to delete a region in the polydata
 * @param regionid this is the region id to delete all of the cells in
 * @return CV_OK if executed correctly, CV_ERROR if the geometry is NULL
 * or the function does not return properly.
 */
    
int PlyDtaUtils_DeleteRegion(vtkPolyData *geom,int *regionid)
{                          
  int id = *regionid;
  vtkIdType cellId;
  vtkSmartPointer<vtkIntArray> boundaryRegions =
    vtkSmartPointer<vtkIntArray>::New();

  if (PlyDtaUtils_PDCheckArrayName(geom,1,"ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified \
		    and named 'ModelFaceID' prior to this function call\n");
    return CV_ERROR;
  }
  boundaryRegions = vtkIntArray::SafeDownCast(geom->GetCellData()->
		    GetScalars("ModelFaceID"));

  geom->BuildLinks();
  for (int cellId=0; cellId< geom->GetNumberOfCells(); cellId++)
  {
    if (boundaryRegions->GetValue(cellId) == id)
    {
      geom->DeleteCell(cellId);
    }
  }

  geom->RemoveDeletedCells();

  return CV_OK;
}


// -------------------
// PlyDtaUtils_CheckArrayName
// -------------------
/** 
 * @brief Function to check is array with name exists in cell or point data
 * @param object this is the object to check if the array exists
 * @param datatype this is point or cell. point =0,cell=1
 * @param arrayname this is the name of the array to check
 * @reutrn this returns 1 if the array exists and zero if it doesn't
 * or the function does not return properly.
 */

int PlyDtaUtils_PDCheckArrayName(vtkPolyData *object,int datatype,std::string arrayname)
{
  vtkIdType i;
  int numArrays;
  int exists =0;

  if (datatype == 0)
  {
    numArrays = object->GetPointData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetPointData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }
  else 
  {
    numArrays = object->GetCellData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetCellData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }

  if (exists == 1)
  {
    return CV_OK;
  }
  else
  {
    return CV_ERROR;
  }
}

// -------------------
// PlyDtaUtils_UGCheckArrayName
// -------------------
/** 
 * @brief Function to check is array with name exists in cell or point data
 * @param object this is the object to check if the array exists
 * @param datatype this is point or cell. point =0,cell=1
 * @param arrayname this is the name of the array to check
 * @reutrn this returns 1 if the array exists and zero if it doesn't
 * or the function does not return properly.
 */

int PlyDtaUtils_UGCheckArrayName(vtkUnstructuredGrid *object,int datatype,std::string arrayname)
{
  vtkIdType i;
  int numArrays;
  int exists =0;

  if (datatype == 0)
  {
    numArrays = object->GetPointData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetPointData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }
  else 
  {
    numArrays = object->GetCellData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetCellData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }

  if (exists == 1)
  {
    return CV_OK;
  }
  else
  {
    return CV_ERROR;
  }
}
