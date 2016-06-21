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

#include "SimVascular.h"

#include "vtkCellArray.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkEdgeTable.h"
#include "vtkFindSeparateRegions.h"
#include "vtkIdList.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtkThreshold.h"

#include "cv_mmg_mesh_utils.h"
#include "cv_polydatasolid_utils.h"
#include "cv_vtk_utils.h"

#include "mmg/mmgs/libmmgs.h"

int MMGUtils_ConvertToMMG(MMG5_pMesh mesh, MMG5_pSol sol, vtkPolyData *polydatasolid,
    double hmin, double hmax, double hausd, double angle, double hgrad,
    int useSizingFunction, vtkDoubleArray *meshSizingFunction, int numAddedRefines)
{
  vtkSmartPointer<vtkIntArray> boundaryScalars =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> refineIDs =
    vtkSmartPointer<vtkIntArray>::New();
  if (VtkUtils_PDCheckArrayName(polydatasolid,1,"ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified");
    fprintf(stderr," and named 'ModelFaceID' prior to this function call\n");
    return CV_OK;
  }
  boundaryScalars = vtkIntArray::SafeDownCast(polydatasolid->GetCellData()->GetArray("ModelFaceID"));
  double minmax[2];
  boundaryScalars->GetRange(minmax, 0);
  int range = minmax[1] - minmax[0];

  if (useSizingFunction && meshSizingFunction == NULL)
  {
    fprintf(stderr,"Cannot use sizing function without a function!");
    return CV_ERROR;
  }
  vtkSmartPointer<vtkFindSeparateRegions> separator =
    vtkSmartPointer<vtkFindSeparateRegions>::New();
  separator->SetInputData(polydatasolid);
  separator->SetArrayName("ModelFaceID");
  separator->SetOutPointArrayName("ModelFaceBoundaryPts");
  separator->Update();
  polydatasolid->DeepCopy(separator->GetOutput());

  vtkSmartPointer<vtkEdgeTable> ridges = vtkSmartPointer<vtkEdgeTable>::New();
  if (MMGUtils_BuildRidgeTable(polydatasolid, ridges, "ModelFaceBoundaryPts") != CV_OK)
  {
    fprintf(stderr,"Problem creating ridge table from ModelFaceID boundaries");
    return CV_ERROR;
  }

  int numPts   = polydatasolid->GetNumberOfPoints();
  int numTris  = polydatasolid->GetNumberOfCells();
  int numEdges = ridges->GetNumberOfEdges();
  int *faces;
  int numFaces = 0;
  if (PlyDtaUtils_GetFaceIds(polydatasolid,&numFaces,&faces) != CV_OK)
  {
    fprintf(stderr,"Could not get face ids\n");
    return CV_ERROR;
  }
  if (numAddedRefines != 0)
  {
    if (VtkUtils_PDCheckArrayName(polydatasolid,0,"RefineID") != CV_OK)
    {
      fprintf(stderr,"Array %s does not exist on mesh\n","RefineID");
      return CV_ERROR;
    }
    refineIDs = vtkIntArray::SafeDownCast(polydatasolid->GetPointData()->GetArray("RefineID"));
  }
  int numSizes = numFaces + numAddedRefines;
  delete [] faces;
  if (!MMGS_Set_iparameter(mesh, sol, MMGS_IPARAM_numberOfLocalParam, numSizes))
  {
    fprintf(stderr,"Error in mmgs\n");
    return CV_ERROR;
  }
  //if (!MMGS_Set_dparameter(mesh, sol, MMGS_DPARAM_hmax, hmax))
  //{
  //  fprintf(stderr,"Error in mmgs\n");
  //  return CV_ERROR;
  //}
  //if (!MMGS_Set_dparameter(mesh, sol, MMGS_DPARAM_hmin, hmin))
  //{
  //  fprintf(stderr,"Error in mmgs\n");
  //  return CV_ERROR;
  //}
  //if (!MMGS_Set_dparameter(mesh, sol, MMGS_DPARAM_hgrad, hgrad))
  //{
  //  fprintf(stderr,"Error in mmgs\n");
  //  return CV_ERROR;
  //}
  //if ( !MMGS_Set_dparameter(mesh, sol, MMGS_DPARAM_hausd, hausd))
  //{
  //  fprintf(stderr,"Error in mmgs\n");
  //  return CV_ERROR;
  //}
  if (!MMGS_Set_meshSize(mesh, numPts, numTris, numEdges))
  {
    fprintf(stderr,"Error in mmgs\n");
    return CV_ERROR;
  }
  if (!MMGS_Set_solSize(mesh, sol, MMG5_Vertex, numPts, MMG5_Scalar))
  {
    fprintf(stderr,"Error in mmgs\n");
    return CV_ERROR;
  }
  if (!MMGS_Set_iparameter(mesh, sol, MMGS_IPARAM_angle,0))
  {
    fprintf(stderr,"Error in mmgs\n");
    return CV_ERROR;
  }

  MMG5_pPoint ppt;
  for (int i=0;i<numPts;i++)
  {
    ppt = &mesh->point[i+1];
    double pt[3];
    polydatasolid->GetPoint(i,pt);
    for (int j=0;j<3;j++)
    {
      ppt->c[j] = pt[j];
    }
    ppt->ref = 1;
    double ptsize = 0.0;
    if (useSizingFunction)
    {
      double meshFactor = 0.8;
      ptsize = meshFactor*meshSizingFunction->GetValue(i);
      if (ptsize == 0)
      {
	ptsize = (hmax+hmin)/2;
      }
    }
    else
    {
      ptsize = (hmax+hmin)/2;
    }
    if (!MMGS_Set_scalarSol(sol, ptsize, i+1))
    {
      fprintf(stderr,"Error in mmgs\n");
      return CV_ERROR;
    }
  }

  int refineCt=0;
  MMG5_pTria  tria;
  vtkIdType npts,*pts;
  for (int i=0;i<numTris;i++)
  {
    refineCt=0;
    tria = &mesh->tria[i+1];
    polydatasolid->GetCellPoints(i,npts,pts);
    for (int j=0;j<npts;j++)
    {
      tria->v[j] = pts[j] + 1;
      if (numAddedRefines != 0)
	refineCt += refineIDs->GetValue(pts[j]);;
    }
    tria->ref = boundaryScalars->GetValue(i);
    double newmax = 0.0;
    double newmin = 0.0;
    if (useSizingFunction)
    {
      if (numAddedRefines != 0 && refineCt%3 == 0 && refineCt != 0)
        tria->ref = minmax[1] + refineIDs->GetValue(pts[2]);
      double meshFactor = 0.8;
      double meshsize = meshFactor*meshSizingFunction->GetValue(pts[2]);
      newmax = 1.5*meshsize;
      newmin = 0.5*meshsize;
      if (meshsize == 0)
      {
        newmax = hmax;
        newmin = hmin;
      }
    }
    else
    {
      newmax = hmax;
      newmin = hmin;
    }
    if (!MMGS_Set_localParameter(mesh, sol, MMG5_Triangle, tria->ref, newmin, newmax, hausd))
    {
      fprintf(stderr,"Error in mmgs\n");
      return CV_ERROR;
    }
  }
  MMG5_pEdge edge;
  ridges->InitTraversal();
  for (int i=0;i<numEdges;i++)
  {
    vtkIdType p1,p2;
    ridges->GetNextEdge(p1,p2);

    edge = &mesh->edge[i+1];
    edge->a = p1+1;
    edge->b = p2+1;
    edge->ref = i+1;
    if (!MMGS_Set_ridge(mesh, i+1))
    {
      fprintf(stderr,"Error in mmgs\n");
      return CV_ERROR;
    }
  }

  return CV_OK;
}

int MMGUtils_ConvertToVTK(MMG5_pMesh mesh, MMG5_pSol sol, vtkPolyData *polydatasolid)
{
  MMG5_pPoint ppt;
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  points->SetNumberOfPoints(mesh->np);
  for (int i=0;i<mesh->np;i++)
  {
    ppt = &mesh->point[i+1];
    double pt[3];
    for (int j=0;j<3;j++)
    {
      pt[j] = ppt->c[j];
      points->SetPoint(i,pt[0], pt[1], pt[2]);
    }
  }

  vtkSmartPointer<vtkIdList> facePtIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkCellArray> faces = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkIntArray> outScalars =
    vtkSmartPointer<vtkIntArray>::New();
  outScalars->SetNumberOfComponents(1);
  outScalars->Allocate(mesh->nt,1000);
  outScalars->SetNumberOfTuples(mesh->nt);
  outScalars->SetName("ModelFaceID");
  MMG5_pTria  tria;
  for (int i=0;i<mesh->nt;i++)
  {
    facePtIds->Reset();
    facePtIds->SetNumberOfIds(3);
    tria = &mesh->tria[i+1];
    for (int j=0;j<3;j++)
    {
      facePtIds->SetId(j,tria->v[j] - 1);
    }
    faces->InsertNextCell(facePtIds);
    outScalars->SetValue(i,tria->ref);
  }
  polydatasolid->SetPoints(points);
  polydatasolid->SetPolys(faces);
  polydatasolid->GetCellData()->AddArray(outScalars);

  vtkSmartPointer<vtkCleanPolyData> cleaner =
    vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->SetInputData(polydatasolid);
  cleaner->Update();

  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();
  normaler->SetInputData(cleaner->GetOutput());
  normaler->ConsistencyOn();
  normaler->AutoOrientNormalsOn();
  normaler->FlipNormalsOff();
  normaler->ComputePointNormalsOn();
  normaler->ComputeCellNormalsOff();
  normaler->SplittingOn();
  normaler->Update();

  polydatasolid->DeepCopy(normaler->GetOutput());
  polydatasolid->GetPointData()->GetNormals()->SetName("Normals");
  polydatasolid->BuildCells();
  polydatasolid->BuildLinks();

  return CV_OK;
}

int MMGUtils_SurfaceRemeshing(vtkPolyData *surface, double hmin, double hmax, double hausd, double angle, double hgrad, int useSizingFunction, vtkDoubleArray *meshSizingFunction, int numAddedRefines)
{
  if (hmax < hmin)
  {
    fprintf(stderr,"Max edge size is smaller than min edge size!\n");
    return CV_ERROR;
  }
  MMG5_pMesh mesh;
  MMG5_pSol  sol;
  mesh = NULL;
  sol = NULL;

  MMGS_Init_mesh(MMG5_ARG_start,
        	 MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
        	 MMG5_ARG_end);
  mesh->ver = 2;
  mesh->dim = 3;

  vtkDoubleArray *dummyArray = NULL;
  surface->BuildCells();
  surface->BuildLinks();
  if (MMGUtils_ConvertToMMG(mesh, sol, surface, hmin, hmax,
	hausd, angle, hgrad, useSizingFunction, meshSizingFunction, numAddedRefines) != CV_OK)
  {
    fprintf(stderr,"Error converting to MMG\n");
    MMGS_Free_all(MMG5_ARG_start,
		  MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
		  MMG5_ARG_end);
    return CV_ERROR;
  }

  if (MMGS_Chk_meshData(mesh,sol) != 1)
  {
    fprintf(stderr,"Mesh and sol do not match\n");
    MMGS_Free_all(MMG5_ARG_start,
		  MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
		  MMG5_ARG_end);
    return CV_ERROR;
  }

  fprintf(stderr,"Remeshing surface with MMG...\n");
  try
  {
    MMGS_mmgslib(mesh, sol);
  }
  catch (int ier)
  {
    fprintf(stderr,"Remeshing exited with status ier %d...\n",ier);
    return CV_ERROR;
  }

  vtkPolyData *pd = vtkPolyData::New();

  if (MMGUtils_ConvertToVTK(mesh, sol, pd) != CV_OK)
  {
    fprintf(stderr,"Error converting to VTK\n");
    MMGS_Free_all(MMG5_ARG_start,
		  MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
		  MMG5_ARG_end);
    pd->Delete();
    return CV_ERROR;
  }
  if (MMGUtils_PassCellArray(pd, surface, "ModelFaceID", "ModelFaceID") != CV_OK)
  {
    fprintf(stderr,"Error resetting regions\n");
    MMGS_Free_all(MMG5_ARG_start,
		  MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
		  MMG5_ARG_end);
    pd->Delete();
    return CV_ERROR;
  }
  if (useSizingFunction)
  {
    if (MMGUtils_PassPointArray(pd, surface, "MeshSizingFunction", "MeshSizingFunction") != CV_OK)
    {
      fprintf(stderr,"Error resetting regions\n");
      MMGS_Free_all(MMG5_ARG_start,
		    MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
		    MMG5_ARG_end);
      pd->Delete();
      return CV_ERROR;
    }
  }
  if (VtkUtils_PDCheckArrayName(surface,1,"WallID"))
  {
    if (MMGUtils_PassCellArray(pd,surface,"WallID","WallID") != CV_OK)
    {
      fprintf(stderr,"Error passing walls\n");
      MMGS_Free_all(MMG5_ARG_start,
		    MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
		    MMG5_ARG_end);
      pd->Delete();
      return CV_ERROR;
    }
    vtkSmartPointer<vtkThreshold> thresholder =
      vtkSmartPointer<vtkThreshold>::New();
    thresholder->SetInputData(pd);
     //Set Input Array to 0 port,0 connection,1 for Cell Data, and WallID is the type name
    thresholder->SetInputArrayToProcess(0,0,0,1,"WallID");
    thresholder->ThresholdBetween(1,1);
    thresholder->Update();

    vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer =
      vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
    surfacer->SetInputData(thresholder->GetOutput());
    surfacer->Update();

    pd->DeepCopy(surfacer->GetOutput());
  }
  vtkSmartPointer<vtkCleanPolyData> cleaner =
    vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->SetInputData(pd);
  cleaner->Update();

  surface->DeepCopy(cleaner->GetOutput());
  pd->Delete();

  MMGS_Free_all(MMG5_ARG_start,
        	MMG5_ARG_ppMesh,&mesh,MMG5_ARG_ppMet,&sol,
        	MMG5_ARG_end);

  return CV_OK;
}

int MMGUtils_PassCellArray(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,std::string newName,
    std::string originalName)
{
  int i,j,k;
  int subId;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double closestPt[3];
  double minmax[2];
  double centroid[3];
  int range;
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkSmartPointer<vtkCellLocator> locator =
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkIntArray> currentRegionsInt =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> realRegions =
    vtkSmartPointer<vtkIntArray>::New();

  newgeom->BuildLinks();
  originalgeom->BuildLinks();
  locator->SetDataSet(originalgeom);
  locator->BuildLocator();

  if (VtkUtils_PDCheckArrayName(originalgeom,1,originalName) != CV_OK)
  {
    fprintf(stderr,"Array name 'ModelFaceID' does not exist. Regions must be identified \
		    and named 'ModelFaceID' prior to this function call\n");
    return CV_ERROR;
  }

  realRegions = static_cast<vtkIntArray*>(originalgeom->GetCellData()->GetScalars(originalName.c_str()));

  range = minmax[1]-minmax[0];
  int *mapper;
  mapper = new int[1+range];

  for (i=0;i<range+1;i++)
  {
    mapper[i] = -1;
  }

  for (cellId=0;cellId<newgeom->GetNumberOfCells();cellId++)
  {
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
    currentRegionsInt->InsertValue(cellId,realRegions->GetValue(closestCell));
  }

  newgeom->GetCellData()->RemoveArray(newName.c_str());
  currentRegionsInt->SetName(originalName.c_str());
  newgeom->GetCellData()->AddArray(currentRegionsInt);

  newgeom->GetCellData()->SetActiveScalars(originalName.c_str());

  delete [] mapper;
  return CV_OK;
}

int MMGUtils_PassPointArray(vtkPolyData *newgeom,
    vtkPolyData *originalgeom,std::string newName,
    std::string originalName)
{
  int i;
  int numVerts;
  double xyz[3];
//  double solution[nVar];
  vtkIdType pointId;
  vtkIdType closestPoint;
  vtkDoubleArray *inArray;
  vtkSmartPointer<vtkDoubleArray> outArray =
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkPointLocator> locator =
    vtkSmartPointer<vtkPointLocator>::New();

  numVerts = newgeom->GetNumberOfPoints();

  if (VtkUtils_PDCheckArrayName(originalgeom,0,originalName) != CV_OK)
  {
    fprintf(stderr,"Array %s does not exist on mesh\n",originalName.c_str());
    return CV_ERROR;
  }

  inArray = vtkDoubleArray::SafeDownCast(originalgeom->GetPointData()->GetArray(originalName.c_str()));
  outArray->SetNumberOfComponents(1);
  outArray->Allocate(numVerts,10000);
  outArray->SetNumberOfTuples(numVerts);
  outArray->SetName(newName.c_str());

  locator->SetDataSet(originalgeom);
  locator->BuildLocator();

  for (pointId=0;pointId<numVerts;pointId++)
  {
    newgeom->GetPoint(pointId,xyz);
    closestPoint = locator->FindClosestPoint(xyz);

    double val = 0.0;
    if (closestPoint >= 0)
    {
      val = inArray->GetValue(closestPoint);
    }
    outArray->SetValue(pointId,val);
  }

  newgeom->GetPointData()->AddArray(outArray);

  return CV_OK;
}

int MMGUtils_BuildRidgeTable(vtkPolyData *polydatasolid, vtkEdgeTable *ridges, std::string ridgePtArrayName)
{
  vtkIntArray *ridgePtArray;
  if (VtkUtils_PDCheckArrayName(polydatasolid,0,ridgePtArrayName) != CV_OK)
  {
    fprintf(stderr,"Array %s does not exist on mesh\n",ridgePtArrayName.c_str());
    return CV_ERROR;
  }
  ridgePtArray = vtkIntArray::SafeDownCast(polydatasolid->GetPointData()->GetArray(ridgePtArrayName.c_str()));

  vtkIdType npts, *pts;
  int numPts = polydatasolid->GetNumberOfPoints();
  int numTris = polydatasolid->GetNumberOfCells();
  ridges->InitEdgeInsertion(numPts, 1);

  for (int i=0;i<numTris;i++)
  {
    polydatasolid->GetCellPoints(i, npts, pts);
    for (int j=0;j<npts;j++)
    {
      vtkIdType p1 = pts[j];
      vtkIdType p2 = pts[(j+1)%3];
      if (ridgePtArray->GetValue(p1) && ridgePtArray->GetValue(p2))
      {
	if (ridges->IsEdge(p1, p2) == -1)
	{
	  vtkIdType edgeId = ridges->GetNumberOfEdges();
	  ridges->InsertEdge(p1, p2, edgeId);
	}
      }
    }
  }

  return CV_OK;
}
