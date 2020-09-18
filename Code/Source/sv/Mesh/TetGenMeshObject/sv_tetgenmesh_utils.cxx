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

/** @file sv_tetgenmesh_utils.cxx
 *  @brief The implementations of functions in cv_tetgenmesh_utils
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"

#include "vtkPolyData.h"
#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"
#include "vtkSmartPointer.h"
#include "vtkDataArray.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkIdList.h"
#include "vtkCellArray.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkXMLUnstructuredGridReader.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkCellLocator.h"
#include "vtkGenericCell.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"

#include "simvascular_tetgen.h"

#include "sv_polydatasolid_utils.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"

#define MAXPATHLEN 1024

#ifdef SV_USE_ZLIB
  #ifdef SV_USE_SYSTEM_ZLIB
    #include <zlib.h>
  #else
    #include "simvascular_zlib.h"
  #endif
#else
  #include <stdlib.h>
  #define gzopen fopen
  #define gzprintf fprintf
  #define gzFile FILE*
  #define gzclose fclose
#endif

#include "sv_tetgenmesh_utils.h"

// -----------------------------
// cvTetGenMeshObjectUtils_Init()
// -----------------------------
int TGenUtils_Init()
{
  return SV_OK;
}

// -----------------------------
// cvTGenUtils_ConvertToTetGen()
// -----------------------------
/**
 * @brief Takes vtkPolyData and turns it into TetGen data structures
 * @param *inmesh tetgenio structure in which to hold the input surface
 * @param *polydatasolid the solid in which to take the discrete points
 * from in order to form the mesh to put into TetGen
 * @return SV_OK if function completes properly
 */

int TGenUtils_ConvertSurfaceToTetGen(tetgenio *inmesh,vtkPolyData *polydatasolid)
{
  tetgenio::facet *f;
  tetgenio::polygon *p;

  //All input numbers start from zero, all outmesh_put number start from zero
  inmesh->firstnumber = 0;
  inmesh->numberofpoints = polydatasolid->GetNumberOfPoints();
  inmesh->pointlist = new REAL[inmesh->numberofpoints*3];

  //Do Point transition from polydatasolid into pointlist
  //fprintf(stderr,"Converting Points...\n");
  vtkSmartPointer<vtkPoints> inPts = vtkSmartPointer<vtkPoints>::New();
  inPts = polydatasolid->GetPoints();
  for (int i=0; i<inmesh->numberofpoints;i++)
  {
    double polyPt[3];
    inPts->GetPoint(i,polyPt);
    inmesh->pointlist[i*3] = polyPt[0];
    inmesh->pointlist[i*3+1] = polyPt[1];
    inmesh->pointlist[i*3+2] = polyPt[2];
  }

  // Convert faces
  inmesh->numberoffacets = (int) polydatasolid->GetNumberOfPolys();
  inmesh->facetlist = new tetgenio::facet[inmesh->numberoffacets];
  inmesh->facetmarkerlist = new int[inmesh->numberoffacets];

  //fprintf(stderr,"Converting Faces...\n");
  vtkSmartPointer<vtkIdList> ptIds = vtkSmartPointer<vtkIdList>::New();
  ptIds->SetNumberOfIds(3);
  for (int i=0;i<inmesh->numberoffacets;i++)
  {
    polydatasolid->GetCellPoints(i,ptIds);

    f = &inmesh->facetlist[i];
    f->numberofpolygons=1;

    f->polygonlist = new tetgenio::polygon[f->numberofpolygons];

    f->numberofholes = 0;
    f->holelist = NULL;

    p = &f->polygonlist[0];
    p->numberofvertices=3;
    p->vertexlist = new int[p->numberofvertices];
    p->vertexlist[0] =  (int) ptIds->GetId(0);
    p->vertexlist[1] =  (int) ptIds->GetId(1);
    p->vertexlist[2] =  (int) ptIds->GetId(2);

  }

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_AddPointSizingFunction
// -----------------------------
/**
 */
int TGenUtils_AddPointSizingFunction(tetgenio *inmesh,vtkPolyData *polydatasolid,
    std::string meshSizingFunctionName, double maxEdgeSize)
{
  // check number of points
  if (polydatasolid->GetNumberOfPoints() != inmesh->numberofpoints)
  {
    fprintf(stderr,"surface and tetgen object must match and must have already been converted to add sizing function\n");
    return SV_ERROR;
  }

  //Check if applying a mesh sizing function and initiate point metrics list
  //if ok
  if (VtkUtils_PDCheckArrayName(polydatasolid, 0, meshSizingFunctionName) != SV_OK)
  {
    fprintf(stderr,"Array name 'MeshSizingFunction' does not exist. \
        Something may have gone wrong when setting up BL\n");
    return SV_ERROR;
  }
  vtkDoubleArray *meshSizingFunction = vtkDoubleArray::SafeDownCast(
    polydatasolid->GetPointData()->GetArray(meshSizingFunctionName.c_str()));

  inmesh->numberofpointmtrs = 1;
  inmesh->pointmtrlist = new REAL[inmesh->numberofpoints];

  //Do Point transition from polydatasolid into pointlist
  fprintf(stderr,"Adding mesh sizing metric...\n");
  for (int i=0; i<inmesh->numberofpoints;i++)
  {
    inmesh->pointmtrlist[i] = meshSizingFunction->GetComponent(i,0);
    if (inmesh->pointmtrlist[i] == 0.0)
      inmesh->pointmtrlist[i] = maxEdgeSize;
  }

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_AddFacetMarkers
// -----------------------------
/**
 */
int TGenUtils_AddFacetMarkers(tetgenio *inmesh,vtkPolyData *polydatasolid,
    std::string markerListArrayName)
{
  // Check to make sure number of facets matches
  if (polydatasolid->GetNumberOfPolys() != inmesh->numberoffacets)
  {
    fprintf(stderr,"surface and tetgen object must match and must have already been converted to add facet markers\n");
    return SV_ERROR;
  }

  //Do Poly transition from polydatasolid into facetlist
  if (VtkUtils_PDCheckArrayName(polydatasolid,1,markerListArrayName) != SV_OK)
  {
    fprintf(stderr,"Array name does not exist in polydata. Regions must be identified \
      and named prior to this function call\n");
    return SV_ERROR;
  }
  vtkIntArray *boundaryScalars = static_cast<vtkIntArray*>(polydatasolid->GetCellData()->GetScalars(markerListArrayName.c_str()));

  fprintf(stderr,"Adding Facet Markers...\n");
  for (int i=0;i<inmesh->numberoffacets;i++)
  {
    double boundarymarker = (int) boundaryScalars->GetValue(i);
    inmesh->facetmarkerlist[i]=boundarymarker;
  }

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_AddHoles
// -----------------------------
/**
 */
int TGenUtils_AddHoles(tetgenio *inmesh, vtkPoints *holeList)
{
  inmesh->numberofholes = holeList->GetNumberOfPoints();
  inmesh->holelist = new REAL[inmesh->numberofholes * 3];

  for (int i=0; i<inmesh->numberofholes; i++)
  {
    double pt[3];
    holeList->GetPoint(i, pt);
    inmesh->holelist[3*i] =   pt[0];
    inmesh->holelist[3*i+1] = pt[1];
    inmesh->holelist[3*i+2] = pt[2];
  }

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_AddRegions
// -----------------------------
/**
 */
int TGenUtils_AddRegions(tetgenio *inmesh, vtkPoints *regionList, vtkDoubleArray *regionSizeList)
{
  inmesh->numberofregions = regionList->GetNumberOfPoints();
  inmesh->regionlist = new REAL[inmesh->numberofregions * 5];

  for (int i=0; i<inmesh->numberofregions; i++)
  {
    double pt[3];
    regionList->GetPoint(i, pt);
    inmesh->regionlist[5*i] =   pt[0];
    inmesh->regionlist[5*i+1] = pt[1];
    inmesh->regionlist[5*i+2] = pt[2];
    inmesh->regionlist[5*i+3] = i;

    double mES = regionSizeList->GetTuple1(i);
    double maxvol = (mES*mES*mES)/(6*sqrt(2.));
    inmesh->regionlist[5*i+4] = maxvol;
  }

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_ConvertVolumeToTetGen()
// -----------------------------
/**
 * @brief Function to convert the current mesh to a tetgen mesh object to be
 * able to remesh
 * @param mesh This is the full mesh to be remeshed
 * @param surfaceMesh This is the intial mesh; If we don't need the final
 * mesh regions, then we don't have to actually use this
 * @param inmesh This is the tegen mesh object to be transferred to
 */

int TGenUtils_ConvertVolumeToTetGen(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,
    tetgenio *inmesh)
{
  int numTets,numPolys;
  int numPoints,numSurfacePoints;
  double tetPts[3];
  tetgenio::facet *f;
  tetgenio::polygon *p;
  vtkIdType i,j;
  vtkIdType npts = 0;
  vtkIdType *pts = 0;
  vtkIdType cellId;
  vtkSmartPointer<vtkPoints> uPoints = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> pPolys = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkCellArray> uTets = vtkSmartPointer<vtkCellArray>::New();
  vtkIntArray *boundaryScalars;
  vtkDoubleArray *errorMetricArray;

  mesh->BuildLinks();
  numTets = mesh->GetNumberOfCells();
  numPoints = mesh->GetNumberOfPoints();
  uPoints = mesh->GetPoints();
  uTets = mesh->GetCells();

  numSurfacePoints = surfaceMesh->GetNumberOfPoints();
  numPolys = surfaceMesh->GetNumberOfPolys();
  pPolys = surfaceMesh->GetPolys();
  boundaryScalars = vtkIntArray::SafeDownCast(surfaceMesh->GetCellData()->GetArray("ModelFaceID"));
  errorMetricArray = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray("errormetric"));

  cout<<"Num Cells "<<numTets<<endl;
  cout<<"Num Points "<<numPoints<<endl;
  inmesh->firstnumber = 0;
  inmesh->numberofcorners = 4;
  inmesh->numberoftetrahedra = numTets;
  inmesh->numberofpoints = numPoints;
  inmesh->pointlist = new double[numPoints*3];
  inmesh->tetrahedronlist = new int[numTets*4];
  inmesh->numberofpointmtrs = 1;
  inmesh->pointmtrlist = new REAL[numPoints*inmesh->numberofpointmtrs];

  cout<<"Converting to Adapt Points..."<<endl;
  for (i = 0; i < numPoints; i++)
  {
    uPoints->GetPoint(i,tetPts);
    inmesh->pointlist[i*3] = tetPts[0];
    inmesh->pointlist[i*3+1] = tetPts[1];
    inmesh->pointlist[i*3+2] = tetPts[2];
    inmesh->pointmtrlist[i] = errorMetricArray->GetValue(i);
  }

  cout<<"Converting to Adapt Tets..."<<endl;
  for (i=0,uTets->InitTraversal();uTets->GetNextCell(npts,pts);i++)
  {
    for (j = 0;j < npts;j++)
    {
      inmesh->tetrahedronlist[i*npts+j] = pts[j];
    }
  }

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_ConvertToVTK()
// -----------------------------
//
/**
 * @brief Takes tetgenio and turns it into an output vtkPolyData and
 * vtkUnstructuredGrid
 * @param *outmesh tetgen structure for which the mesh is output
 * @param *volumemesh vtkPolyData on which to save the surface mesh
 * @param *surfacemesh vtkUnstructuredGrid on which to save the volume mesh
 * @return SV_OK if function completes properly
 */

int TGenUtils_ConvertToVTK(tetgenio *outmesh,vtkUnstructuredGrid *volumemesh,vtkPolyData *surfacemesh,int *modelRegions,int getBoundary)
{
  int modelId = 1;
  int globalId = 1;
  int count=0;
  int totRegions=0;
  double tmp;
  vtkIdType i, j;
  vtkIdType vtkId;
  vtkIdType npts = 0;
  vtkIdType *pts = 0;

  vtkIdType numPts,numPolys,numFaces;

  vtkSmartPointer<vtkUnstructuredGrid> fullUGrid = vtkSmartPointer<vtkUnstructuredGrid>::New();
  vtkSmartPointer<vtkPolyData> fullPolyData = vtkSmartPointer<vtkPolyData>::New();

  //Create pointers to vtk scalar lists, point lists, and element lists
  vtkSmartPointer<vtkIdList> polyPointIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> facePointIds = vtkSmartPointer<vtkIdList>::New();

  vtkSmartPointer<vtkCellArray> polys = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> faces = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkPoints> vtpPoints = vtkSmartPointer<vtkPoints>::New();

  vtkSmartPointer<vtkIntArray> modelRegionIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> globalNodeIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> globalElementIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> vtpNodeIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> vtpFaceIds = vtkSmartPointer<vtkIntArray>::New();

  vtkSmartPointer<vtkIntArray> boundaryScalars = vtkSmartPointer<vtkIntArray>::New();

  //Get number of points, polys, and faces
  numPts = outmesh->numberofpoints;
  numPolys = outmesh->numberoftetrahedra;
  numFaces = outmesh->numberoftrifaces;

  bool *pointOnSurface = new bool[numPts];
  int *pointMapping = new int[numPts];

  //Save all point information in a vtkPoints list
  //fprintf(stderr,"Converting Points to VTK Structures...\n");
  points->SetNumberOfPoints(numPts);
  for (i=0;i< outmesh->numberofpoints; i++)
  {
    points->SetPoint(i,outmesh->pointlist[i*3],outmesh->pointlist[i*3+1],outmesh->pointlist[i*3+2]);
    globalNodeIds->InsertValue(i,globalId);
    pointOnSurface[i] = false;
    globalId++;
  }

  //Save all point information in a vtkPoints list
  for (i=0;i<numFaces;i++)
  {
    for (j=0;j<3;j++)
    {
      if (pointOnSurface[outmesh->trifacelist[3*i+j]] == false)
      {
        pointOnSurface[outmesh->trifacelist[3*i+j]] = true;
        pointMapping[outmesh->trifacelist[3*i+j]] = count++;
      }
    }
  }

  vtpPoints->SetNumberOfPoints(count);
  //Create face point list
  for (i=0;i<numPts;i++)
  {
    if (pointOnSurface[i] == true)
    {
      vtpPoints->SetPoint(pointMapping[i],outmesh->pointlist[i*3],outmesh->pointlist[i*3+1],outmesh->pointlist[i*3+2]);
      vtpNodeIds->InsertValue(pointMapping[i],i+1);
    }
  }

  //Save all element information in a vtkCellArray list
  //fprintf(stderr,"Converting Elements to VTK Structures...\n");
  polyPointIds->SetNumberOfIds(4);
  globalId=1;
  for (i=0;i< numPolys;i++)
  {
    for (j=0; j< outmesh->numberofcorners;j++)
    {
      vtkId = outmesh->tetrahedronlist[i*outmesh->numberofcorners+j];
      polyPointIds->SetId(j,vtkId);
    }

    if (outmesh->numberoftetrahedronattributes > 0)
      modelRegionIds->InsertValue(i, outmesh->tetrahedronattributelist[i] + 1);
    else
      modelRegionIds->InsertValue(i,modelId);

    globalElementIds->InsertValue(i,globalId);
    globalId++;
    polys->InsertNextCell(polyPointIds);
  }

  //Create an unstructured grid and link scalar information to nodes and
  //elements
  fullUGrid->SetPoints(points);
  fullUGrid->SetCells(VTK_TETRA, polys);

  modelRegionIds->SetName("ModelRegionID");
  fullUGrid->GetCellData()->AddArray(modelRegionIds);
  fullUGrid->GetCellData()->SetActiveScalars("ModelRegionID");

  globalNodeIds->SetName("GlobalNodeID");
  fullUGrid->GetPointData()->AddArray(globalNodeIds);

  globalElementIds->SetName("GlobalElementID");
  fullUGrid->GetCellData()->AddArray(globalElementIds);

  //Save all external faces to a vtkCellArray list
  //fprintf(stdout,"Converting Faces to VTK Structures...\n");
  facePointIds->SetNumberOfIds(3);

  for (i=0;i< numFaces;i++)
  {
    for (j=0; j<3;j++)
    {
      facePointIds->SetId(j,pointMapping[outmesh->trifacelist[i*3+j]]);
    }

    faces->InsertNextCell(facePointIds);

    if (!(outmesh->adjtetlist[2*i] >= numPolys || outmesh->adjtetlist[2*i] < 0))
    {
      vtpFaceIds->InsertValue(i,globalElementIds->GetValue(outmesh->adjtetlist[2*i]));
    }
    else if (!(outmesh->adjtetlist[2*i+1] >= numPolys || outmesh->adjtetlist[2*i+1] < 0))
    {
      vtpFaceIds->InsertValue(i,globalElementIds->GetValue(outmesh->adjtetlist[2*i+1]));
    }
    else
    {
      fprintf(stderr,"WARNING: TetGen says face has no adjacent tetrahedron\n");
      vtpFaceIds->InsertValue(i,globalElementIds->GetValue(outmesh->adjtetlist[2*i+1]));
    }

    if (getBoundary)
    {
      if (outmesh->trifacemarkerlist != NULL)
      {
        boundaryScalars->InsertValue(i,outmesh->trifacemarkerlist[i]);
      }
      if (boundaryScalars->GetValue(i)>totRegions)
      {
        totRegions = outmesh->trifacemarkerlist[i];
      }
    }
  }

  //Create a polydata grid and link scalar information to nodes and elements
  fullPolyData->SetPoints(vtpPoints);
  fullPolyData->SetPolys(faces);
  fullPolyData->BuildCells();

  vtpNodeIds->SetName("GlobalNodeID");
  fullPolyData->GetPointData()->AddArray(vtpNodeIds);
  fullPolyData->GetPointData()->SetActiveScalars("GlobalNodeID");

  vtpFaceIds->SetName("GlobalElementID");
  fullPolyData->GetCellData()->AddArray(vtpFaceIds);
  fullPolyData->GetCellData()->SetActiveScalars("GlobalElementID");

  if (getBoundary)
  {
    boundaryScalars->SetName("ModelFaceID");
    fullPolyData->GetCellData()->AddArray(boundaryScalars);
    fullPolyData->GetCellData()->SetActiveScalars("ModelFaceID");

    *modelRegions = totRegions;
  }

//  //Flip the cells on the polydata for presolver
  for (i=0;i<fullPolyData->GetNumberOfCells();i++)
  {
    fullPolyData->GetCellPoints(i,npts,pts);

    tmp = pts[0];
    pts[0] = pts[1];
    pts[1] = tmp;
    fullPolyData->ReplaceCell(i,npts,pts);
  }

  delete [] pointMapping;
  delete [] pointOnSurface;

  surfacemesh->DeepCopy(fullPolyData);
  volumemesh->DeepCopy(fullUGrid);

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_WriteVTU()
// -----------------------------
/**
 * @brief Writes a vtu file file
 * @param *filename Name of desired file location
 * @param *UGrid vtkUnstructuredGrid to be written
 * @return SV_OK if function completes properly
 */

int TGenUtils_WriteVTU(char *filename,vtkUnstructuredGrid *UGrid)
{
  auto writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
  writer->SetFileName(filename);

#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(UGrid);
#else
  writer->SetInputData(UGrid);
#endif

  writer->Write();
  return SV_OK;
}

// -----------------------------
// cvTGenUtils_WriteVTP()
// -----------------------------
/**
 * @brief Writes a vtp file file
 * @param *filename Name of desired file location
 * @param *UGrid vtkPolyData to be written
 * @return SV_OK if function completes properly
 */

int TGenUtils_WriteVTP(char *filename,vtkPolyData *PData)
{
  vtkSmartPointer<vtkXMLPolyDataWriter> writer  = vtkSmartPointer<vtkXMLPolyDataWriter>::New();

  std::string fn = "out.vtp";
  writer->SetFileName(fn.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(PData);
#else
  writer->SetInputData(PData);
#endif
  //writer->SetDataModeToAscii();

  writer->Write();

  return SV_OK;
}


// -----------------------------
// cvTGenUtils_GetFacePolyData()
// -----------------------------
/**
 * @brief Based on Scalars Defined by the GetBoundaryFaces filter,
 * separate into face VTKs
 * @param *mesh vtkPolyData on which to extract the face from
 * @param *face vtkPolyData on which to set the face PolyData
 * @param angle double that specifies the extraction angle. Any faces
 * @param id int that specifies the face id to extract
 * @return SV_OK if function completes properly
 * @note There is another method to do this that does not retain id
 * information. It may be faster, but doesn't reatain info
 */
//

int TGenUtils_GetFacePolyData(int id,vtkPolyData *mesh, vtkPolyData *face)
{
  //Initiate variable used by function
  int i,j;
  int count=0;
  vtkIdType cellId;
  vtkIdType npts = 0;
  vtkIdType *pts = 0;
  vtkIdType globalElement2=-1;
  double ptCmps[3];

  vtkSmartPointer<vtkPolyData> tempFace = vtkSmartPointer<vtkPolyData>::New();

  vtkSmartPointer<vtkIdList> facePointIds = vtkSmartPointer<vtkIdList>::New();

  vtkSmartPointer<vtkCellArray> meshFaces = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkCellArray> selectFaces = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkPoints> meshPoints = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkPoints> selectPoints = vtkSmartPointer<vtkPoints>::New();

  vtkSmartPointer<vtkIntArray> globalNodeIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> globalElementIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> boundaryScalars = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> lessNodeIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> lessElementIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> globalElement2Ids = vtkSmartPointer<vtkIntArray>::New();

  if (VtkUtils_PDCheckArrayName(mesh,0,"GlobalNodeID") != SV_OK)
  {
    fprintf(stderr,"Array name 'GlobalNodeID' does not exist.");
    fprintf(stderr," IDs on mesh may not have been assigned properly\n");
    return SV_ERROR;
  }
  if (VtkUtils_PDCheckArrayName(mesh,1,"GlobalElementID") != SV_OK)
  {
    fprintf(stderr,"Array name 'GlobalElementID' does not exist.");
    fprintf(stderr," IDs on mesh may not have been assigned properly\n");
    return SV_ERROR;
  }
  if (VtkUtils_PDCheckArrayName(mesh,1,"ModelFaceID") != SV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified");
		fprintf(stderr," and named 'ModelFaceID' prior to this function call\n");
    return SV_ERROR;
  }

  globalNodeIds = vtkIntArray::SafeDownCast(mesh->GetPointData()->GetScalars("GlobalNodeID"));
  globalElementIds = vtkIntArray::SafeDownCast(mesh->GetCellData()->GetScalars("GlobalElementID"));
  boundaryScalars = vtkIntArray::SafeDownCast(mesh->GetCellData()->GetScalars("ModelFaceID"));

  meshFaces = mesh->GetPolys();
  meshPoints = mesh->GetPoints();
  int numPts = mesh->GetNumberOfPoints();
  int numFaces = mesh->GetNumberOfPolys();

  bool *cellOnFace = new bool[numFaces];
  bool *pointOnFace = new bool[numPts];
  int *pointMapping = new int[numPts];

  for (i = 0;i<numPts;i++)
  {
    pointOnFace[i] = false;
    pointMapping[i] = -1;
  }

  //Set up point mapping and boolean whether point is on face
  for (cellId = 0,meshFaces->InitTraversal();meshFaces->GetNextCell(npts,pts);cellId++)
  {
    if (boundaryScalars->GetValue(cellId) == id)
    {
      cellOnFace[cellId] = true;
      for(j=0;j<npts;j++)
      {
	if (pointOnFace[pts[j]] == false)
	{
	  pointOnFace[pts[j]] = true;
	  pointMapping[pts[j]] = count++;
	}
      }
    }
    else
    {
      cellOnFace[cellId] = false;
    }
  }

  selectPoints->SetNumberOfPoints(count);

  for (i=0;i<numPts;i++)
  {
    if (pointOnFace[i] == true)
    {
      meshPoints->GetPoint(i,ptCmps);
      selectPoints->SetPoint(pointMapping[i],ptCmps[0],ptCmps[1],ptCmps[2]);
      lessNodeIds->InsertValue(pointMapping[i],globalNodeIds->GetValue(i));
    }
  }

  facePointIds->SetNumberOfIds(3);
  //Get node and element information for the current boundary on the full
  //polydata and save to a smaller polydata
  for(cellId = 0,meshFaces->InitTraversal();meshFaces->GetNextCell(npts,pts); cellId++)
  {
    if (cellOnFace[cellId] == true)
    {
      for (j=0; j<npts; j++)
      {
        facePointIds->SetId(j,pointMapping[pts[j]]);
      }
      selectFaces->InsertNextCell(facePointIds);
      globalElement2Ids->InsertNextValue(globalElement2);
      lessElementIds->InsertNextValue(globalElementIds->GetValue(cellId));
    }
  }

  //Create links between points and faces and respective global node and
  //element information
  tempFace->SetPoints(selectPoints);
  tempFace->SetPolys(selectFaces);

  lessNodeIds->SetName("GlobalNodeID");
  tempFace->GetPointData()->AddArray(lessNodeIds);
  tempFace->GetPointData()->SetActiveScalars("GlobalNodeID");

  globalElement2Ids->SetName("GlobalElementID2");
  tempFace->GetCellData()->AddArray(globalElement2Ids);
  tempFace->GetCellData()->SetActiveScalars("GlobalElementID2");

  lessElementIds->SetName("GlobalElementID");
  tempFace->GetCellData()->AddArray(lessElementIds);
  tempFace->GetCellData()->SetActiveScalars("GlobalElementID");

  delete [] pointOnFace;
  delete [] pointMapping;
  delete [] cellOnFace;

  face->DeepCopy(tempFace);

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_writeDiffAdj()
// -----------------------------
/**
 * @brief This is the new way to write an adjacency file based on the mesh
 * @note now implemented in the presolver as new command
 */

int TGenUtils_writeDiffAdj(vtkUnstructuredGrid *volumemesh)
{
  gzFile myfile = NULL;

  std::string filename("compareAdjacency.xadj");

  #ifdef SV_USE_ZLIB
  char filenamegz[MAXPATHLEN];
  filenamegz[0]='\0';
  sprintf (filenamegz, "%s.gz", filename.c_str());
  myfile = gzopen (filenamegz, "wb");
  if (myfile == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filenamegz);
      return SV_ERROR;
  }
  #else
  myfile = gzopen (filename.c_str(), "wb");
  if (myfile == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filename.c_str());
      return SV_ERROR;
  }
  #endif

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
  volumemesh->BuildLinks();

  if (VtkUtils_UGCheckArrayName(volumemesh,1,"GlobalElementID") != SV_OK)
  {
    fprintf(stderr,"Array name 'GlobalElementID' does not exist. IDs on mesh may not have been assigned properly\n");
    return SV_ERROR;
  }
  globalIds = vtkIntArray::SafeDownCast(volumemesh->GetCellData()->GetScalars("GlobalElementID"));
  numCells = volumemesh->GetNumberOfCells();
  volCells = volumemesh->GetCells();

  xadj = new int[numCells];
  adjacency = new int[4*numCells];
  int adj = 0;
  int xcheck = 0;
  xadj[xcheck] = 0;

  ptIds->SetNumberOfIds(3);
  for (cellId = 0;cellId<numCells;cellId++)
  {
    meshCellId = globalIds->LookupValue(cellId+1);
    volumemesh->GetCellPoints(meshCellId,npts,pts);
    for (i=0;i < npts; i++)
    {
      p1 = pts[i];
      p2 = pts[(i+1)%(npts)];
      p3 = pts[(i+2)%(npts)];

      ptIds->InsertId(0,p1);
      ptIds->InsertId(1,p2);
      ptIds->InsertId(2,p3);

      volumemesh->GetCellNeighbors(meshCellId,ptIds,cellIds);

      //If it is zero, it is a face on the exterior. Otherwise, it has
      //neighbors
      if (cellIds->GetNumberOfIds() != 0)
      {
	adjacency[adj++] = (int) globalIds->GetValue(cellIds->GetId(0)-1);
      }

    }
    xadj[++xcheck] = adj;
  }

  gzprintf(myfile,"xadj: %i\n",numCells+1);
  gzprintf(myfile,"adjncy: %i\n",adj);

  for (i=0;i < numCells+1; i++)
  {
      gzprintf(myfile,"%i\n",xadj[i]);
  }
  for (i=0;i < adj; i++)
  {
      gzprintf(myfile,"%i\n",adjacency[i]);
  }

  delete xadj;
  delete adjacency;

  gzclose(myfile);
  return SV_OK;
}

// -----------------------------
// cvTGenUtils_SetRefinementCylinder()
// -----------------------------
/**
 * @brief computes the distance between each point on surface and center
 * @brief of cylinder. Then, if inside radius, the meshsizing function at the
 * @brief is set to the reduced size,
 * @param size This is the smaller refined of the edges within cylinder region.
 * @param radius This is the radius of the refinement cylinder.
 * @param center This is the center of the refinement cylinder.
 * @param length This is the length of the cylinder. Center is half the length.
 * @param normal This is the normal direction of the length of the cylinder.
 * It is normalized before being used for compuation.
 * @return SV_OK if function completes properly
 */

int TGenUtils_SetRefinementCylinder(vtkPolyData *polydatasolid,
    std::string sizingFunctionArrayName,double size,double radius, double *center,
    double length, double *normal, int secondarray,double maxedgesize,
    std::string refineIDArrayName, int refinecount)
{
  int numPts;
  double disttopoint;
  double distalonglength;
  double pts[3];
  double norm[3];
  for (int i=0;i < 3;i++)
    norm[i] = normal[i];
  vtkIdType pointId;
  vtkSmartPointer<vtkDoubleArray> meshSizeArray = vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkIntArray> refineIDArray = vtkSmartPointer<vtkIntArray>::New();

  //Set sizing function params
  numPts = polydatasolid->GetNumberOfPoints();
  if (secondarray)
  {
    if (VtkUtils_PDCheckArrayName(polydatasolid,0,sizingFunctionArrayName) != SV_OK)
    {
      fprintf(stderr,"Solid does not contain a double array of name %s. Regions must be identified \
		      Reset or remake the array and try again\n",sizingFunctionArrayName.c_str());
      return SV_ERROR;
    }
    meshSizeArray = vtkDoubleArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(sizingFunctionArrayName.c_str()));
    if (VtkUtils_PDCheckArrayName(polydatasolid,0,refineIDArrayName) != SV_OK)
    {
      fprintf(stderr,"Solid does not contain an int array of name %s. Regions must be identified \
		      Reset or remake the array and try again\n",refineIDArrayName.c_str());
      return SV_ERROR;
    }
    refineIDArray = vtkIntArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(refineIDArrayName.c_str()));
  }
  else
  {
    meshSizeArray->SetNumberOfComponents(1);
    meshSizeArray->Allocate(numPts,1000);
    meshSizeArray->SetNumberOfTuples(numPts);
    meshSizeArray->SetName(sizingFunctionArrayName.c_str());
    refineIDArray->SetNumberOfComponents(1);
    refineIDArray->Allocate(numPts,1000);
    refineIDArray->SetNumberOfTuples(numPts);
    refineIDArray->SetName(refineIDArrayName.c_str());
    for (pointId = 0;pointId<numPts;pointId++)
    {
      meshSizeArray->SetValue(pointId,0.0);
      refineIDArray->SetValue(pointId,0);
    }
  }

  for (pointId = 0;pointId<numPts;pointId++)
  {
    polydatasolid->GetPoint(pointId,pts);
    //compute distance
    double pvec[3];
    double scale;
    vtkMath::Norm(norm);
    vtkMath::Subtract(pts,center,pvec);
    scale = vtkMath::Dot(pvec,norm);
    vtkMath::MultiplyScalar(norm,scale);
    disttopoint = sqrt(pow(pts[0]-norm[0],2)+
	pow(pts[1]-norm[1],2)+
	pow(pts[2]-norm[2],2));

    distalonglength = sqrt(pow(norm[0]-center[0],2)+
	pow(norm[1]-center[1],2)+
	pow(norm[2]-center[2],2));

    //set value to new size
    if (disttopoint <= radius && distalonglength <= length/2)
    {
      meshSizeArray->SetValue(pointId,size);
      refineIDArray->SetValue(pointId,refinecount+1);
    }
    else
    {
      if (meshSizeArray->GetValue(pointId) == 0)
        meshSizeArray->SetValue(pointId,maxedgesize);
    }
  }

  if (secondarray)
  {
    polydatasolid->GetPointData()->RemoveArray(sizingFunctionArrayName.c_str());
  }
  polydatasolid->GetPointData()->AddArray(meshSizeArray);
  polydatasolid->GetPointData()->SetActiveScalars(sizingFunctionArrayName.c_str());
  polydatasolid->GetPointData()->AddArray(refineIDArray);

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_SetRefinementSphere()
// -----------------------------
/**
 * @brief computes the distance between each point on surface and center
 * @brief of sphere. Then, if inside radius, the meshsizing function at the
 * @brief is set to the reduced size,
 * @param size This is the smaller refined of the edges within sphere region.
 * @param radius This is the radius of the refinement sphere.
 * @param center This is the center of the refinement sphere.
 * @return SV_OK if function completes properly
 */

int TGenUtils_SetRefinementSphere(vtkPolyData *polydatasolid,
    std::string sizingFunctionArrayName,double size,double radius, double *center,
    int secondarray,double maxedgesize, std::string refineIDArrayName, int refinecount)
{
  int numPts;
  double dist;
  double pts[3];
  vtkIdType pointId, cellId;
  vtkSmartPointer<vtkDoubleArray> meshSizeArray = vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkIntArray> refineIDArray = vtkSmartPointer<vtkIntArray>::New();

  //Set sizing function params
  numPts = polydatasolid->GetNumberOfPoints();
  if (secondarray)
  {
    if (VtkUtils_PDCheckArrayName(polydatasolid,0,sizingFunctionArrayName) != SV_OK)
    {
      fprintf(stderr,"Solid does not contain a double array of name %s. Regions must be identified \
		      Reset or remake the array and try again\n",sizingFunctionArrayName.c_str());
      return SV_ERROR;
    }
    meshSizeArray = vtkDoubleArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(sizingFunctionArrayName.c_str()));
  }
  else
  {
    meshSizeArray->SetNumberOfComponents(1);
    meshSizeArray->Allocate(numPts,1000);
    meshSizeArray->SetNumberOfTuples(numPts);
    meshSizeArray->SetName(sizingFunctionArrayName.c_str());
    for (pointId = 0;pointId<numPts;pointId++)
    {
      meshSizeArray->SetValue(pointId,0.0);
    }
  }
  if (refinecount != 0)
  {
    if (VtkUtils_PDCheckArrayName(polydatasolid,0,refineIDArrayName) != SV_OK)
    {
      fprintf(stderr,"Solid does not contain an int array of name %s. Regions must be identified \
		      Reset or remake the array and try again\n",refineIDArrayName.c_str());
      return SV_ERROR;
    }
    refineIDArray = vtkIntArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(refineIDArrayName.c_str()));
  }
  else
  {
    refineIDArray->SetNumberOfComponents(1);
    refineIDArray->Allocate(numPts,1000);
    refineIDArray->SetNumberOfTuples(numPts);
    refineIDArray->SetName(refineIDArrayName.c_str());
    for (pointId = 0;pointId<numPts;pointId++)
    {
      refineIDArray->SetValue(pointId,0);
    }
  }

  for (pointId = 0;pointId<numPts;pointId++)
  {
    polydatasolid->GetPoint(pointId,pts);
    //compute distance
    dist = sqrt(pow(pts[0]-center[0],2)+
	pow(pts[1]-center[1],2)+
	pow(pts[2]-center[2],2));

    //set value to new size
    if (dist <= radius)
    {
      meshSizeArray->SetValue(pointId,size);
      refineIDArray->SetValue(pointId,refinecount+1);
    }
    else
    {
      if (meshSizeArray->GetValue(pointId) == 0)
        meshSizeArray->SetValue(pointId,maxedgesize);
    }
  }

  if (secondarray)
  {
    polydatasolid->GetPointData()->RemoveArray(sizingFunctionArrayName.c_str());
    polydatasolid->GetPointData()->RemoveArray(refineIDArrayName.c_str());
  }
  polydatasolid->GetPointData()->AddArray(meshSizeArray);
  polydatasolid->GetPointData()->SetActiveScalars(sizingFunctionArrayName.c_str());
  polydatasolid->GetPointData()->AddArray(refineIDArray);

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_SetSizeFunctionArray()
// -----------------------------
/**
 * @brief set a mesh size function based on given array.
 * @brief Values of given array are normalized based on minimum value. Then
 * @brief normalized values are multiplied by size in order to give the mesh
 * @brief size function for the mesher
 * @param size This is the smaller refined of the edges within sphere region.
 * @param sizingFunctionArrayName Name for which to pull values from
 * @param functionname This is the desired function name to be sent to the
 * mesher
 * @param secondarray This designates whether a previous function is already
 * applied.
 * @return SV_OK if function completes properly
 */

int TGenUtils_SetSizeFunctionArray(vtkPolyData *polydatasolid,
    std::string sizingFunctionArrayName,double size,char *functionname,
    int secondarray)
{
  int numPts,numCells;
  double dist;
  double value;
  double factor;
  vtkIdType npts,*pts;
  vtkIdType pointId,cellId;
  double min = 0;
  double range[2];
  vtkSmartPointer<vtkDoubleArray> arrayonmesh = vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkIntArray> regionarray = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkDoubleArray> meshSizeArray = vtkSmartPointer<vtkDoubleArray>::New();

  //Set sizing function params
  numPts = polydatasolid->GetNumberOfPoints();
  numCells = polydatasolid->GetNumberOfCells();
  if (secondarray)
  {
    if (VtkUtils_PDCheckArrayName(polydatasolid,0,sizingFunctionArrayName) != SV_OK)
    {
      fprintf(stderr,"Solid does not contain a double array of name %s. Regions must be identified \
		      Reset or remake the array and try again\n",sizingFunctionArrayName.c_str());
      return SV_ERROR;
    }
    meshSizeArray = vtkDoubleArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(sizingFunctionArrayName.c_str()));
  }
  else
  {
    meshSizeArray->SetNumberOfComponents(1);
    meshSizeArray->Allocate(numPts,1000);
    meshSizeArray->SetNumberOfTuples(numPts);
    meshSizeArray->SetName(sizingFunctionArrayName.c_str());
    for (pointId = 0;pointId<numPts;pointId++)
    {
      meshSizeArray->SetValue(pointId,0.0);
    }
  }

  if (VtkUtils_PDCheckArrayName(polydatasolid,0,functionname) != SV_OK)
  {
    fprintf(stderr,"Solid does not contain a double array of name %s.",
		    functionname);
    return SV_ERROR;
  }

  arrayonmesh = vtkDoubleArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(functionname));

  if (!strncmp(functionname,"DistanceToCenterlines",21))
  {
    arrayonmesh->GetRange(range,0);
    min = range[0];
    fprintf(stderr,"Size Function minimum is: %.4f\n",min);
    fprintf(stderr,"Size Function maximum is: %.4f\n",range[1]);
    if (min <= 0)
    {
      fprintf(stderr,"Min is Zero or negative. This will not work!!!\n",min);
      return SV_ERROR;
    }
    if (min < size)
    {
      std::cout<<"Given mesh size is smaller than minimum radius!!"<<endl;
      std::cout<<"Setting new mesh size to minimum radius :)"<<endl;
      size = min;
    }

    for (pointId = 0;pointId<numPts;pointId++)
    {
      value = arrayonmesh->GetValue(pointId);
      factor = value/min;
  //    fprintf(stderr,"Value is : %.4f\n",factor);
      //compute distance
      //set value to reduced size
      meshSizeArray->SetValue(pointId,factor*size);
    }
    polydatasolid->GetPointData()->RemoveArray(functionname);
  }
  else
  {
  }

  polydatasolid->GetPointData()->AddArray(meshSizeArray);
  polydatasolid->GetPointData()->SetActiveScalars(sizingFunctionArrayName.c_str());


  fprintf(stderr,"Sizing function set\n");
  return SV_OK;
}

// -----------------------------
// cvTGenUtils_LoadMesh()
// -----------------------------
/**
 * @brief Function to load in a vtkUnstructuredGrid
 * @note This is only used by LoadMesh in vtkTetGenMeshObject
 */
//

int TGenUtils_LoadMesh(char *filename,vtkUnstructuredGrid *result)
{
  const char *extension = strrchr(filename,'.');
  extension = extension +1;

  if (!strncmp(extension,"vtu",3)) {
    vtkSmartPointer<vtkXMLUnstructuredGridReader> ugreader =
      vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();
    ugreader->SetFileName(filename);
    ugreader->Update();

    result->DeepCopy(ugreader->GetOutput());
    result->BuildLinks();
  }
  else {
    fprintf(stderr,"Cannot load the mesh. \
	It must be of type vtkUnstructuredGrid\n");
    return SV_ERROR;
  }

  return SV_OK;
}

int TGenUtils_ResetOriginalRegions(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,
    std::string regionName)
{
  int i,j,k;
  int subId;
  int maxIndex;
  int temp;
  int flag = 1;
  int count;
  int bigcount;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double closestPt[3];
  double tolerance = 1.0;
  double centroid[3];
  int range;
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkIdType currentValue;
  vtkIdType realValue;
  vtkSmartPointer<vtkCellLocator> locator =
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkIntArray> currentRegions =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> realRegions =
    vtkSmartPointer<vtkIntArray>::New();

  newgeom->BuildLinks();
  originalgeom->BuildLinks();
  locator->SetDataSet(originalgeom);
  locator->BuildLocator();

  if (VtkUtils_PDCheckArrayName(originalgeom,1,regionName) != SV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified \
		    and named 'ModelFaceID' prior to this function call\n");
    return SV_ERROR;
  }

  realRegions = static_cast<vtkIntArray*>(originalgeom->GetCellData()->GetScalars(regionName.c_str()));


  for (cellId=0;cellId<newgeom->GetNumberOfCells();cellId++)
  {
      newgeom->GetCellPoints(cellId,npts,pts);
      //int eachValue[npts];
      vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
      vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
      for (i=0;i<npts;i++)
      {
	polyPtIds->InsertValue(i,i);
	polyPts->InsertNextPoint(newgeom->GetPoint(pts[i]));
      }
      vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

      locator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	  subId,distance);
      currentRegions->InsertValue(cellId,realRegions->GetValue(closestCell));
  }

  newgeom->GetCellData()->RemoveArray(regionName.c_str());
  currentRegions->SetName(regionName.c_str());
  newgeom->GetCellData()->AddArray(currentRegions);

  newgeom->GetCellData()->SetActiveScalars(regionName.c_str());

  return SV_OK;
}

int TGenUtils_ResetOriginalRegions(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,
    std::string regionName,
    vtkIdList *excludeList)
{
  int i,j,k;
  int subId;
  int region;
  int temp;
  int flag = 1;
  int count;
  int bigcount;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double closestPt[3];
  double tolerance = 1.0;
  double centroid[3];
  int range;
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkIdType currentValue;
  vtkIdType realValue;
  vtkSmartPointer<vtkCellLocator> locator =
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkPolyData> originalCopy =
    vtkSmartPointer<vtkPolyData>::New();

  if (excludeList == NULL)
  {
    fprintf(stderr,"Cannot give NULL excludeList. Use other reset function without exclude list\n");
    return SV_ERROR;
  }

  newgeom->BuildLinks();
  originalgeom->BuildLinks();
  originalCopy->DeepCopy(originalgeom);

  if (VtkUtils_PDCheckArrayName(originalCopy,1, regionName) != SV_OK)
  {
    fprintf(stderr,"Array name %s does not exist. Regions must be identified \
		    and named 'ModelFaceID' prior to this function call\n",  regionName.c_str());
    return SV_ERROR;
  }

  vtkDataArray *testRegions = originalCopy->GetCellData()->GetScalars( regionName.c_str());

    if (VtkUtils_PDCheckArrayName(newgeom,1, regionName.c_str()) != SV_OK)
    {
      fprintf(stderr,"Array name %s does not exist. Regions must be identified \
          and named 'ModelFaceID' prior to this function call\n", regionName.c_str());
      return SV_ERROR;
    }

    vtkDataArray *currentRegions = newgeom->GetCellData()->GetArray(regionName.c_str());

    for (int i=0; i<originalCopy->GetNumberOfCells(); i++)
    {
      region = testRegions->GetTuple1(i);
      if (excludeList->IsId(region) != -1)
      {
        originalCopy->DeleteCell(i);
      }
    }

    originalCopy->RemoveDeletedCells();

    vtkSmartPointer<vtkCleanPolyData> cleaner =
      vtkSmartPointer<vtkCleanPolyData>::New();
    cleaner->SetInputData(originalCopy);
    cleaner->Update();

    originalCopy->DeepCopy(cleaner->GetOutput());
    originalCopy->BuildLinks();

  locator->SetDataSet(originalCopy);
  locator->BuildLocator();
  vtkDataArray *realRegions = originalCopy->GetCellData()->GetScalars( regionName.c_str());

  for (cellId=0;cellId<newgeom->GetNumberOfCells();cellId++)
  {
    currentValue = currentRegions->GetTuple1(cellId);
    if (excludeList->IsId(currentValue) != -1)
    {
      continue;
    }

    newgeom->GetCellPoints(cellId,npts,pts);
    vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
    for (i=0;i<npts;i++)
    {
      polyPtIds->InsertValue(i,i);
      polyPts->InsertNextPoint(newgeom->GetPoint(pts[i]));
    }
    vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

    locator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	subId,distance);
    currentRegions->SetTuple1(cellId,realRegions->GetTuple1(closestCell));
  }

  newgeom->GetCellData()->SetActiveScalars(regionName.c_str());

  return SV_OK;
}

int TGenUtils_ResetOriginalRegions(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,
    std::string regionName,
    vtkIdList *onlyList,
    int dummy)
{
  int i,j,k;
  int subId;
  int region;
  int temp;
  int flag = 1;
  int count;
  int bigcount;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double closestPt[3];
  double tolerance = 1.0;
  double centroid[3];
  int range;
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkIdType currentValue;
  vtkIdType realValue;
  vtkSmartPointer<vtkCellLocator> locator =
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkPolyData> originalCopy =
    vtkSmartPointer<vtkPolyData>::New();

  if (onlyList == NULL)
  {
    fprintf(stderr,"Cannot give NULL onlyList. Use other reset function without only list\n");
    return SV_ERROR;
  }

  newgeom->BuildLinks();
  originalgeom->BuildLinks();
  originalCopy->DeepCopy(originalgeom);

  if (VtkUtils_PDCheckArrayName(originalCopy,1, regionName) != SV_OK)
  {
    fprintf(stderr,"Array name %s does not exist. Regions must be identified \
		    and named 'ModelFaceID' prior to this function call\n",  regionName.c_str());
    return SV_ERROR;
  }

  vtkDataArray *testRegions = originalCopy->GetCellData()->GetScalars( regionName.c_str());

  if (VtkUtils_PDCheckArrayName(newgeom,1, regionName.c_str()) != SV_OK)
  {
    fprintf(stderr,"Array name %s does not exist. Regions must be identified \
        and named 'ModelFaceID' prior to this function call\n", regionName.c_str());
    return SV_ERROR;
  }

  vtkDataArray *currentRegions = newgeom->GetCellData()->GetArray(regionName.c_str());

  locator->SetDataSet(originalCopy);
  locator->BuildLocator();
  vtkDataArray *realRegions = originalCopy->GetCellData()->GetScalars( regionName.c_str());

  for (cellId=0;cellId<newgeom->GetNumberOfCells();cellId++)
  {
    currentValue = currentRegions->GetTuple1(cellId);
    if (onlyList->IsId(currentValue) == -1)
    {
      continue;
    }

    newgeom->GetCellPoints(cellId,npts,pts);
    vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
    for (i=0;i<npts;i++)
    {
      polyPtIds->InsertValue(i,i);
      polyPts->InsertNextPoint(newgeom->GetPoint(pts[i]));
    }
    vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

    locator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	subId,distance);
    currentRegions->SetTuple1(cellId,realRegions->GetTuple1(closestCell));
  }

  newgeom->GetCellData()->SetActiveScalars(regionName.c_str());

  return SV_OK;
}

// -----------------------------
// cvTGenUtils_CheckSurfaceMesh()
// -----------------------------
/**
 * @brief Function to load in a vtkUnstructuredGrid
 * @note This is only used by LoadMesh in vtkTetGenMeshObject
 */
//

int TGenUtils_CheckSurfaceMesh(vtkPolyData *pd, int meshInfo[3])
{
  fprintf(stdout,"Checking surface mesh\n");
  vtkIdType npts,p0,p1;
  vtkIdType *pts;
  int NonManifoldEdges = 0,FreeEdges = 0;
  int Regions=0;
  vtkSmartPointer<vtkCleanPolyData> cleaner =
    vtkSmartPointer<vtkCleanPolyData>::New();
  vtkSmartPointer<vtkIdList> edgeneigh =
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkConnectivityFilter> connector =
    vtkSmartPointer<vtkConnectivityFilter>::New();
  vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer =
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();

  //Clean the input surface
  cleaner->SetInputData(pd);
  cleaner->Update();
  pd->DeepCopy(cleaner->GetOutput());
  pd->BuildLinks();

  surfacer->SetInputData(cleaner->GetOutput());
  surfacer->Update();

  connector->SetInputData(surfacer->GetOutput());
  connector->ColorRegionsOn();
  connector->Update();

  vtkDataArray *regionarray = connector->GetOutput()->
      GetCellData()->GetScalars("RegionId");

  //Loop through the surface and find edges with cells that have either more
  //than one neighbor or no neighbors. No neighbors can be okay,as this can
  //indicate a free edge. Multiple neighbors indicates a
  //non-manifold edge. This can cause issues as well in certain cases.
  for (int i = 0;i<pd->GetNumberOfCells();i++)
  {
    pd->GetCellPoints(i,npts,pts);
    for (int j=0;j<npts;j++)
    {
      p0 = pts[j];
      p1 = pts[(j+1)%npts];

      pd->GetCellEdgeNeighbors(i,p0,p1,edgeneigh);
      if (edgeneigh->GetNumberOfIds() > 1)
        NonManifoldEdges++;
      else if (edgeneigh->GetNumberOfIds() < 1)
        FreeEdges++;
    }
    int val = regionarray->GetTuple1(i);
    if (val > Regions)
      Regions = val;
  }

  fprintf(stdout,"Regions: %d\n", Regions + 1);
  fprintf(stdout,"Number of Free Edges on Surface: %d\n", FreeEdges);
  fprintf(stdout,"Number of Non-Manifold Edges on Surface: %d\n", NonManifoldEdges);

  meshInfo[0] = Regions+1;
  meshInfo[1] = FreeEdges;
  meshInfo[2] = NonManifoldEdges;

  return SV_OK;
}

int TGenUtils_SetLocalMeshSize(vtkPolyData *pd,int regionId,double size)
{
  vtkIdType pointId, cellId;
  vtkIdType npts, *pts;
  vtkSmartPointer<vtkIntArray> regionarray =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkDoubleArray> meshSizeArray =
    vtkSmartPointer<vtkDoubleArray>::New();

  int numPts = pd->GetNumberOfPoints();
  int numCells = pd->GetNumberOfCells();
  regionarray = vtkIntArray::SafeDownCast(pd->GetCellData()->GetArray("ModelFaceID"));
  if (VtkUtils_PDCheckArrayName(pd,0,"MeshSizingFunction") != SV_OK)
  {
    meshSizeArray->SetNumberOfComponents(1);
    meshSizeArray->Allocate(numPts,1000);
    meshSizeArray->SetNumberOfTuples(numPts);
    meshSizeArray->SetName("MeshSizingFunction");
    for (pointId = 0;pointId<numPts;pointId++)
    {
      meshSizeArray->SetValue(pointId,0.0);
    }
  }
  else
  {
    meshSizeArray = vtkDoubleArray::SafeDownCast(pd->GetPointData()->GetArray("MeshSizingFunction"));
  }
  pd->BuildLinks();
  for (cellId = 0;cellId<numCells;cellId++)
  {
    if (regionarray->GetValue(cellId) == regionId)
    {
      pd->GetCellPoints(cellId,npts,pts);
      for (int j=0;j<npts;j++)
      {
	meshSizeArray->SetValue(pts[j],size);
      }
    }
  }

  pd->GetPointData()->RemoveArray("MeshSizingFunction");
  meshSizeArray->SetName("MeshSizingFunction");
  pd->GetPointData()->AddArray(meshSizeArray);

  return SV_OK;
}
