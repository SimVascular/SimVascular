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

/** @file sv_vmtk_utils.cxx
 *  @brief These functions are utilities that implement vtkvmtk classes
 *  @details Provides boundary layer and surface remeshing.
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "sv_vmtk_utils.h"

#include "SimVascular.h"
#include "sv_tetgenmesh_utils.h"

#include "vtkMath.h"
#include "vtkAppendPolyData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkTriangleFilter.h"
#include "vtkDecimatePro.h"
#include "vtkCleanPolyData.h"
#include "vtkCellData.h"
#include "vtkPolyDataNormals.h"
#include "vtkPointData.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkGeometryFilter.h"
#include "vtkCellArray.h"
#include "vtkSVFillHolesFilterWithIds.h"

#include "vtkvmtkPolyDataSurfaceRemeshing.h"
#include "vtkvmtkPolyDataSizingFunction.h"
#include "vtkvmtkSurfaceProjection.h"
#include "vtkvmtkCapPolyData.h"
#include "vtkvmtkSimpleCapPolyData.h"
#include "vtkvmtkAppendFilter.h"
#include "vtkvmtkBoundaryLayerGenerator.h"
#include "sv_polydatasolid_utils.h"
#include "vtkvmtkPolyDataDistanceToCenterlines.h"
#include "vtkvmtkPolyDataCenterlines.h"
#include "vtkvmtkCenterlineBranchExtractor.h"
#include "vtkvmtkCapPolyData.h"
#include "vtkvmtkSimpleCapPolyData.h"
#include "vtkvmtkPolyDataCenterlineGroupsClipper.h"
#include "vtkvmtkMergeCenterlines.h"
#include "vtkvmtkUnstructuredGridTetraFilter.h"

#define vtkNew(type,name) \
  vtkSmartPointer<type> name = vtkSmartPointer<type>::New()

/* -------------- */
/* sys_geom_centerlines */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function to extract centerlines from a vtkPolyData surface
 *  @brief VTMK is called to do this. Each cap on PolyData has an
 *  @brief id and then each id is set as either inlet or outlet.
 *  @param *sources list of source cap ids
 *  @param nsources number of source cap ids
 *  @param *targets list of target cap ids
 *  @param ntargets number of target cap ids
 *  @param **lines returned center lines as vtkPolyData
 *  @param ** voronoi returned voronoi diagram as vtkPolyData
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_centerlines( cvPolyData *polydata,int *sources,int nsources,
		int *targets,int ntargets,
		cvPolyData **lines, cvPolyData **voronoi)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  cvPolyData *result1 = NULL;
  cvPolyData *result2 = NULL;
  *lines = NULL;
  *voronoi = NULL;

//  vtkSmartPointer<vtkvmtkPolyDataCenterlines> centerLiner =
//    vtkSmartPointer<vtkvmtkPolyDataCenterlines>::New();
  vtkSmartPointer<vtkIdList> capInletIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> capOutletIds = vtkSmartPointer<vtkIdList>::New();
  int pointId;

  fprintf(stderr,"NumSources %d\n",nsources);
  fprintf(stderr,"NumTargets %d\n",ntargets);
  for (pointId = 0;pointId<nsources;pointId++)
  {
    capInletIds->InsertNextId(*sources+pointId);
  }
  for (pointId = 0;pointId<ntargets;pointId++)
  {
    capOutletIds->InsertNextId(*targets+pointId);
  }

  vtkNew(vtkvmtkPolyDataCenterlines,centerLiner);
  try {
    std::cout<<"Getting Center Lines..."<<endl;
    centerLiner->SetInputData(geom);
    centerLiner->SetSourceSeedIds(capInletIds);
    centerLiner->SetTargetSeedIds(capOutletIds);
    centerLiner->SetRadiusArrayName("MaximumInscribedSphereRadius");
    centerLiner->SetCostFunction("1/R");
    centerLiner->SetFlipNormals(0);
    centerLiner->SetAppendEndPointsToCenterlines(1);
    centerLiner->SetSimplifyVoronoi(0);
    centerLiner->SetCenterlineResampling(0);
    centerLiner->SetResamplingStepLength(1);
    centerLiner->Update();

    result1 = new cvPolyData( centerLiner->GetOutput() );
    *lines = result1;
    result2 = new cvPolyData( centerLiner->GetVoronoiDiagram() );
    *voronoi = result2;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline operation.\n");
    fflush(stderr);
    return SV_ERROR;
  }

  return SV_OK;
}

/* -------------- */
/* sys_geom_mergecenterlines */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function to merge centerlines from a vtkPolyData surface
 *  @brief Must be called after separation of centerlines into groups
 *  @brief VTMK is called to do this.
 *  @param *sources list of source cap ids
 *  @param nsources number of source cap ids
 *  @param *targets list of target cap ids
 *  @param ntargets number of target cap ids
 *  @param **lines returned center lines as vtkPolyData
 *  @param ** voronoi returned voronoi diagram as vtkPolyData
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_mergecenterlines( cvPolyData *lines, int mergeblanked,
		cvPolyData **merged)
{
  vtkPolyData *geom = lines->GetVtkPolyData();
  cvPolyData *result1 = NULL;
  *merged = NULL;

  vtkNew(vtkvmtkMergeCenterlines, merger);
  try {
    std::cout<<"Merging Sections..."<<endl;
    merger->SetInputData(geom);
    merger->SetBlankingArrayName("Blanking");
    merger->SetRadiusArrayName("MaximumInscribedSphereRadius");
    merger->SetGroupIdsArrayName("GroupIds");
    merger->SetCenterlineIdsArrayName("CenterlineIds");
    merger->SetTractIdsArrayName("TractIds");
    merger->SetMergeBlanked(mergeblanked);
    merger->Update();

    result1 = new cvPolyData( merger->GetOutput() );
    *merged = result1;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline merging.\n");
    fflush(stderr);
    return SV_ERROR;
  }

  return SV_OK;

  return SV_OK;
}

/* -------------- */
/* sys_geom_separatecenterlines */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function to separate centerlines based on bifurcations of the geometry
 *  @param *lines from centerline extraction, the centerlines to be used
 *  @note  The output lines are not physically different than the input. The
 *  @note  simply applies cell data to the lines that describe the bifurcation regions.
 *  @note  The cell array names are "Blanking","GroupIds","CenterlineIds",and "TractIds"
 *  @param **separate vtkPolyData with cell arrays discribing branchin attached
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_separatecenterlines( cvPolyData *lines,
		cvPolyData **separate)
{
  vtkPolyData *geom = lines->GetVtkPolyData();
  cvPolyData *result1 = NULL;
  *separate = NULL;

  vtkNew(vtkvmtkCenterlineBranchExtractor,brancher);
  try {
    std::cout<<"Grouping Sections..."<<endl;
    brancher->SetInputData(geom);
    brancher->SetBlankingArrayName("Blanking");
    brancher->SetRadiusArrayName("MaximumInscribedSphereRadius");
    brancher->SetGroupIdsArrayName("GroupIds");
    brancher->SetCenterlineIdsArrayName("CenterlineIds");
    brancher->SetTractIdsArrayName("TractIds");
    brancher->Update();

    result1 = new cvPolyData( brancher->GetOutput() );
    *separate = result1;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline separation.\n");
    fflush(stderr);
    return SV_ERROR;
  }

  return SV_OK;
}

/* -------------- */
/* sys_geom_grouppolydata */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function to calculate distance to surface from points on line
 *  @param *polydata The polydata to find distance form lines on
 *  @param *lines from centerline extraction, the centerlines to be used
 *  @param **distance vtkPolyData with distance functiona attached returned
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_grouppolydata( cvPolyData *polydata,cvPolyData *lines,
		cvPolyData **grouped)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  vtkPolyData *centerlines = lines->GetVtkPolyData();
  cvPolyData *result = NULL;
  *grouped = NULL;

  vtkNew(vtkvmtkPolyDataCenterlineGroupsClipper,grouper);
  try {
    std::cout<<"Branching PolyData..."<<endl;
    grouper->SetInputData(geom);
    grouper->SetCenterlines(centerlines);
    grouper->SetCenterlineGroupIdsArrayName("GroupIds");
    grouper->SetGroupIdsArrayName("GroupIds");
    grouper->SetCenterlineRadiusArrayName("MaximumInscribedSphereRadius");
    grouper->SetBlankingArrayName("Blanking");
    grouper->SetCutoffRadiusFactor(1e16);
    grouper->SetClipValue(0.0);
    grouper->SetUseRadiusInformation(1);
    grouper->ClipAllCenterlineGroupIdsOn();
    grouper->Update();

    result = new cvPolyData( grouper->GetOutput());
    *grouped = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline operation.\n");
    fflush(stderr);
    return SV_ERROR;
  }
  return SV_OK;
}

/* -------------- */
/* sys_geom_distancetocenterlines */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function to calculate distance to surface from points on line
 *  @param *polydata The polydata to find distance form lines on
 *  @param *lines from centerline extraction, the centerlines to be used
 *  @param **distance vtkPolyData with distance functiona attached returned
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_distancetocenterlines( cvPolyData *polydata,cvPolyData *lines,
		cvPolyData **distance)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  vtkPolyData *centerlines = lines->GetVtkPolyData();
  cvPolyData *result = NULL;
  *distance = NULL;

  vtkNew(vtkvmtkPolyDataDistanceToCenterlines,distancer);
  try {
    std::cout<<"Getting Distance to Center Lines..."<<endl;
    distancer->SetInputData(geom);
    distancer->SetCenterlines(centerlines);
    distancer->SetUseRadiusInformation(1);
    distancer->SetEvaluateTubeFunction(0);
    distancer->SetEvaluateCenterlineRadius(0);
    distancer->SetProjectPointArrays(0);
    distancer->SetDistanceToCenterlinesArrayName("DistanceToCenterlines");
    distancer->SetCenterlineRadiusArrayName("MaximumInscribedSphereRadius");
    distancer->Update();

    result = new cvPolyData( distancer->GetOutput());
    *distance = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in centerline operation.\n");
    fflush(stderr);
    return SV_ERROR;
  }
  return SV_OK;
}

/* -------------- */
/* sys_geom_cap */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function that calls VMTK method to cap which assigns ids t caps
 *  @param *polydata The polydata to cap
 *  @param *cappedpolydata the capped polydata is returned in this
 *  @param *numcenterids Number of caps or ids for the center of caps
 *  @param **centerids If type is 1, a cap with a centerpoint is added.
 *  Then this centerpoint id is return in this object
 *  @param type This determines whether to cap regularly or cap with a point
 *  in the center. 0 - regular, 1 - point in center
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_cap( cvPolyData *polydata,cvPolyData **cappedpolydata,int *numcenterids,int **centerids,int type)
{
  vtkPolyData *geom = polydata->GetVtkPolyData();
  cvPolyData *result = NULL;
  *cappedpolydata = NULL;
  vtkSmartPointer<vtkIdList> capCenterIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkTriangleFilter> triangulate =
	  vtkSmartPointer<vtkTriangleFilter>::New();
  int numids;
  int *allids;
  int i;

  try {
    if (type ==0)
    {
      vtkSmartPointer<vtkvmtkSimpleCapPolyData> capper =
	      vtkSmartPointer<vtkvmtkSimpleCapPolyData>::New();
      capper->SetInputData(geom);
      capper->SetCellEntityIdsArrayName("CenterlineCapID");
      capper->SetCellEntityIdOffset(1);
      capper->Update();
      triangulate->SetInputData(capper->GetOutput());
      triangulate->Update();

      result = new cvPolyData( triangulate->GetOutput() );
      *cappedpolydata = result;
      capCenterIds->InsertNextId(0);
    }
    else if (type == 1)
    {
      vtkSmartPointer<vtkvmtkCapPolyData> capper =
	      vtkSmartPointer<vtkvmtkCapPolyData>::New();
      capper->SetInputData(geom);
      capper->SetDisplacement(0);
      capper->SetInPlaneDisplacement(0);
      capper->Update();
      triangulate->SetInputData(capper->GetOutput());
      triangulate->Update();

      result = new cvPolyData( triangulate->GetOutput() );
      *cappedpolydata = result;
      capCenterIds->DeepCopy(capper->GetCapCenterIds());

    }

  }
  catch (...) {
    fprintf(stderr,"ERROR in capping operation.\n");
    fflush(stderr);
    return SV_ERROR;
  }

  numids = capCenterIds->GetNumberOfIds();
  allids = new int[numids];

  for (i=0;i<numids;i++)
  {
    allids[i] = capCenterIds->GetId(i);
  }
  *numcenterids = numids;
  *centerids = allids;

  return SV_OK;
}

/* -------------- */
/* sys_geom_cap_with_ids */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function that calls customized vtk filter to cap with ids starting
 *  from the lowest number in the current "ModelFaceID" array
 *  @param *polydata The polydata to cap
 *  @param fillid the id number to use for capping. Depends on filltype
 *  @param filledholes the number of holes filled in process
 *  @param filltype How to assign ids to caps on polydata. 0 - start from 1
 *  and each cap gets a new id. 1 - give every cap the same id (fillid).
 *  2 - start from fillid and give each new cap a value increasing from this
 *  @param type This determines whether to cap regularly or cap with a point
 *  in the center. 0 - regular, 1 - point in center
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_cap_with_ids( cvPolyData *polydata,cvPolyData **cappedpolydata,
    int fillId,int filledholes,int filltype)
{

  vtkPolyData *geom = polydata->GetVtkPolyData();
  cvPolyData *result = NULL;
  *cappedpolydata = NULL;

  try {

    std::cout<<"Capping Surface..."<<endl;
    vtkSmartPointer<vtkSVFillHolesFilterWithIds> capper =
	    vtkSmartPointer<vtkSVFillHolesFilterWithIds>::New();
    capper->SetInputData(geom);
    capper->SetFillId(fillId);
    //Fill type, 0 for number of holes filled, 1 for a fillid, and 2 for
    //increasing number id starting at given fillid number+1
    capper->SetFillType(filltype);
    capper->SetHoleSize(capper->GetHoleSizeMaxValue());
    capper->Update();

    filledholes = capper->GetNumberOfHolesFilled();
    vtkNew(vtkPolyData,capout);
    capout->DeepCopy(capper->GetOutput());

    if (filltype ==2)
    {
      vtkNew(vtkIntArray, currentCapArray);
      vtkNew(vtkIntArray, currentFaceArray);
      vtkNew(vtkIntArray, newFaceArray);
      if (VtkUtils_PDCheckArrayName(capout,1,"CapID") != SV_OK)
      {
	fprintf(stderr,"CapID Array is not on the surface\n");
	return SV_ERROR;
      }
      if (VtkUtils_PDCheckArrayName(geom,1,"ModelFaceID") != SV_OK)
      {
	fprintf(stderr,"ModelFaceID Array is not on the surface\n");
	return SV_ERROR;
      }
      currentCapArray = vtkIntArray::SafeDownCast(capout->GetCellData()->
	  GetArray("CapID"));
      currentFaceArray = vtkIntArray::SafeDownCast(geom->GetCellData()->
	  GetArray("ModelFaceID"));
      for (int i=0;i<capout->GetNumberOfCells();i++)
      {
	if (currentCapArray->GetValue(i) == -1)
	  newFaceArray->InsertValue(i,currentFaceArray->GetValue(i));
	else
	  newFaceArray->InsertValue(i,currentCapArray->GetValue(i));
      }
      newFaceArray->SetName("ModelFaceID");
      capout->GetCellData()->AddArray(newFaceArray);
      capout->GetCellData()->SetActiveScalars("ModelFaceID");
    }
    result = new cvPolyData(capout);
    *cappedpolydata = result;
  }
  catch (...) {
    fprintf(stderr,"ERROR in capping operation.\n");
    fflush(stderr);
    return SV_ERROR;
  }

  return SV_OK;
}

/* -------------- */
/* sys_geom_mapandcorrectids */
/* -------------- */

/** @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *
 *  @brief Function to map and correct ids after a surface has been thresholded
 *  then capped.
 *  @param *polydata The polydata map and correct ids on
 *  @param *originalpd The original polydata with correct ids
 *  @param *newpd The new polydata to correct the ids on
 *  @param **polydata The returned polydata with everything all set
 *  @param *originalarray original id array name on the surface to correct
 *  @param *newarray new id array name to give to the corrected ids
 *  @return SV_OK if the VTMK function executes properly
 */

int sys_geom_mapandcorrectids( cvPolyData *originalpd, cvPolyData *newpd, cvPolyData **polydata,char *originalarray, char *newarray)
{
  vtkPolyData *originalgeom = originalpd->GetVtkPolyData();
  vtkPolyData *newgeom = newpd->GetVtkPolyData();
  cvPolyData *result = NULL;
  *polydata = NULL;

  int i,j,k;
  int subId;
  int count;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double closestPt[3];
  double minmax[2];
  double centroid[3];
  long range;
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkIdType currentValue;
  vtkPolyData *newcopy = vtkPolyData::New();
  vtkSmartPointer<vtkCellLocator> locator =
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkPointLocator> pointLocator =
    vtkSmartPointer<vtkPointLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkLongArray> currentRegionsLong =
    vtkSmartPointer<vtkLongArray>::New();
  vtkSmartPointer<vtkIntArray> currentRegionsInt =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> realRegions =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIdList> closestCells =
    vtkSmartPointer<vtkIdList>::New();

  newcopy->DeepCopy(newgeom);
  newcopy->BuildLinks();
  originalgeom->BuildLinks();
  locator->SetDataSet(originalgeom);
  locator->BuildLocator();
  pointLocator->SetDataSet(originalgeom);
  pointLocator->BuildLocator();

  currentRegionsLong = static_cast<vtkLongArray*>(newcopy->GetCellData()->GetScalars(newarray));
  currentRegionsLong->GetRange(minmax,0);

  realRegions = static_cast<vtkIntArray*>(originalgeom->GetCellData()->GetScalars(originalarray));
//  realRegions = vtkIntArray::SafeDownCast(originalgeom->GetCellData()->GetScalars(originalarray));

  range = minmax[1]-minmax[0];
  long *mapper;
  mapper = new long[1+range];

  for (i=0;i<range+1;i++)
  {
    mapper[i] = -1;
  }

  for (cellId=0;cellId<newcopy->GetNumberOfCells();cellId++)
  {
    currentValue = currentRegionsLong->GetValue(cellId);

    if (mapper[currentValue-1] == -1)
    {
      fprintf(stderr,"Getting Value: %d\n",currentValue-1);
      newcopy->GetCellPoints(cellId,npts,pts);

      vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
      vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
      for (i=0;i<npts;i++)
      {
	polyPtIds->InsertValue(i,i);
	polyPts->InsertNextPoint(newcopy->GetPoint(pts[i]));
      }

      vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

      locator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	  subId,distance);

      fprintf(stderr,"the value: %d\n",realRegions->GetValue(closestCell));
      mapper[currentValue-1] = realRegions->GetValue(closestCell);
    }
  }

  for (i=0;i<range+1;i++)
  {
    fprintf(stderr,"Want to see mapper vals: %d is %d\n",i,mapper[i]);
  }

  //Set original region values
  for (cellId=0;cellId<newcopy->GetNumberOfCells();cellId++)
  {
    currentValue = static_cast<int>(currentRegionsLong->GetValue(cellId));
    currentRegionsInt->InsertValue(cellId,mapper[currentValue-1]);
  }

  newcopy->GetCellData()->RemoveArray(newarray);

  currentRegionsInt->SetName(originalarray);
  newcopy->GetCellData()->AddArray(currentRegionsInt);
  newcopy->GetCellData()->SetActiveScalars(originalarray);

  result = new cvPolyData( newcopy );
  *polydata = result;

  delete [] mapper;
  return SV_OK;
}

// --------------------
//  VMTKUtils_SurfaceRemeshing
// --------------------
/**
 * @brief Function to remesh a surface
 * @param *surfaceMesh this is the current discrete polydata surface to be
 * remeshed. It is also the new returen surface remesh
 * @param maxEdgeSize This is the edge size that would like to be targeted
 * for the surface remesh
 * @param meshcapsonly This is boolean on whether the whole mesh should be
 * meshed or just caps. If meshcapsonly is 1, then you must provide a list
 * of excluded ids in the *excludedIds param.
 * @param preservedges This is a boolean on whether the feature edges should
 * be preserved or not. In most cases, this is 1.
 * @param trianglesplit factor. This is just the value for a split triangle
 * when it must be split. Default is 1.0.
 * @param collapseanglethreshold. This is the angle at which a triangle is
 * bad triangle and should be collapsed. Default is 0.5.
 * @param *excludedIds These are the excluded regions if meshcapsonly is
 * specified.
 * @param cellEntityIdsArrayName This is the array name assigned to the nodes
 * of the output surface remesh
 * @param useSizingFunction This is if we want to specify a distance at each
 * node for the mesh. Used for mesh refinement!
 * @param *meshSizingFunction This is the actual meshSizing Function. Must
 * exist if using a sizing function
 * @return SV_OK is executed correctly. If something goes wrong, SV_ERROR
 * is returned
 */
int VMTKUtils_SurfaceRemeshing(vtkPolyData *surfaceMesh, double maxEdgeSize,
    int meshcapsonly,int preserveedges,double trianglesplitfactor,
    double collapseanglethreshold, vtkIdList *excludedIds,
    std::string cellEntityIdsArrayName,int useSizingFunction,
    vtkDoubleArray *meshSizingFunction)
{
  double edgesize;
  vtkIdType i;
  vtkSmartPointer<vtkPolyData> surfacepd =
    vtkSmartPointer<vtkPolyData>::New();
  surfacepd->DeepCopy(surfaceMesh);

  //Triangle filters and decimator to produce mesh that is remeshable
  vtkSmartPointer<vtkTriangleFilter> tris1 =
    vtkSmartPointer<vtkTriangleFilter>::New();
  vtkSmartPointer<vtkCleanPolyData> cleaner =
    vtkSmartPointer<vtkCleanPolyData>::New();
  vtkSmartPointer<vtkCleanPolyData> cleaner2 =
    vtkSmartPointer<vtkCleanPolyData>::New();
  vtkSmartPointer<vtkvmtkSurfaceProjection> projector =
    vtkSmartPointer<vtkvmtkSurfaceProjection>::New();
  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();

  //Remeshing through vmtk
  vtkSmartPointer<vtkvmtkPolyDataSurfaceRemeshing> remesher =
    vtkSmartPointer<vtkvmtkPolyDataSurfaceRemeshing>::New();

  //Check the inputs to the function and apply default values if necessary
  if (trianglesplitfactor == NULL)
  {
    trianglesplitfactor = 1.0;
  }
  if (collapseanglethreshold == NULL)
  {
    collapseanglethreshold = 0.5;
  }
  if (meshcapsonly && excludedIds == NULL)
  {
    //Need list of ids to mesh only caps
    fprintf(stderr,"Cannot mesh only caps without Ids to exclude\n");
    return SV_ERROR;
  }

  if (useSizingFunction && meshSizingFunction == NULL)
  {
    fprintf(stderr,"Cannot use sizing function without a function!");
    return SV_ERROR;
  }

  //Apply the Entity Ids array name if not preserving edges and meshing all
  if (!preserveedges && !meshcapsonly)
  {
    vtkSmartPointer<vtkIntArray> cellIds = vtkSmartPointer<vtkIntArray>::New();
    cellIds->SetName(cellEntityIdsArrayName.c_str());
    cellIds->SetNumberOfTuples(surfacepd->GetNumberOfCells());
    cellIds->FillComponent(0,0.0);
    surfacepd->GetCellData()->AddArray(cellIds);
  }
  //If applying a meshsizing function, transfer the values to an array of
  //areas based on the edge sizes specified
  if (useSizingFunction)
  {
    meshSizingFunction->SetName("SizingFunction");
    for (i=0;i<meshSizingFunction->GetNumberOfTuples();i++)
    {
      edgesize = meshSizingFunction->GetValue(i);
      if (edgesize == 0)
      {
        meshSizingFunction->SetValue(i,(0.25)*pow(3.0,0.5)*pow(maxEdgeSize,2.0));
      }
      else
      {
        meshSizingFunction->SetValue(i,(0.25)*pow(3.0,0.5)*pow(edgesize,2.0));
      }
    }
    surfacepd->GetPointData()->AddArray(meshSizingFunction);
  }

  cleaner->SetInputData(surfacepd);
  cleaner->Update();

  tris1->SetInputData(cleaner->GetOutput());
  tris1->Update();

  //Set up input based on full or cap remesh
  if (meshcapsonly)
  {
    remesher->SetInputData(surfacepd);
  }
  else
  {
    remesher->SetInputData(tris1->GetOutput());
  }

  //Set the remesher options based on the input
  remesher->SetPreserveBoundaryEdges(preserveedges);
  remesher->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
  if (useSizingFunction)
  {
    remesher->SetElementSizeModeToTargetAreaArray();
    remesher->SetTargetAreaArrayName("SizingFunction");
  }
  else
  {
    remesher->SetElementSizeModeToTargetArea();
    remesher->SetTargetArea((0.25)*pow(3.0,0.5)*pow(maxEdgeSize,2.0));
  }
  remesher->SetMaxArea(1e16);
  remesher->SetMinArea(0.0);
  remesher->SetTriangleSplitFactor(trianglesplitfactor);
  remesher->SetCollapseAngleThreshold(collapseanglethreshold);
  if (meshcapsonly)
  {
    remesher->SetExcludedEntityIds(excludedIds);
  }
  //Remesh the surface
  remesher->Update();

  if (meshcapsonly)
  {
    surfaceMesh->DeepCopy(remesher->GetOutput());
  }
  //If meshing the whole thing, project it back to the original surface,
  //compute the normals, and clean the mesh
  else
  {
    projector->SetInputData(remesher->GetOutput());
    projector->SetReferenceSurface(surfacepd);
    projector->Update();

    normaler->SetInputData(projector->GetOutput());
    normaler->SetConsistency(1);
    normaler->SetAutoOrientNormals(1);
    normaler->SetFlipNormals(0);
    normaler->SetComputeCellNormals(0);
    normaler->SplittingOff();
    normaler->Update();

    cleaner2->SetInputData(normaler->GetOutput());
    cleaner2->Update();

    surfaceMesh->DeepCopy(cleaner2->GetOutput());
    surfaceMesh->GetPointData()->GetNormals()->SetName("Normals");
  }

  return SV_OK;
}

// --------------------
//  VMTKUtils_ComputeSizingFunction
// --------------------
/**
 * @brief Function to compute a sizing function of the current mesh. This
 * is a function describing the size of the mesh at each location
 * @param *inpd This is the input vtkPolyData to compute the funciton on.
 * @param scalefactor This is to scale the output by some factor if necessary
 * Default is 1.2.
 * @param sizingFunctionArrayName This is the array name given to the output
 * sizing function attached to the mesh.
 * @return SV_OK if the sizing function is computed correctly.
 */
int VMTKUtils_ComputeSizingFunction(vtkPolyData *inpd, double scalefactor,
    std::string sizingFunctionArrayName)
{
  vtkSmartPointer<vtkvmtkPolyDataSizingFunction> sizer =
    vtkSmartPointer<vtkvmtkPolyDataSizingFunction>::New();
  vtkSmartPointer<vtkPolyData> copypd =
    vtkSmartPointer<vtkPolyData>::New();
  copypd->DeepCopy(inpd);

  if (scalefactor == NULL)
  {
    scalefactor = 1.8;
  }

  sizer->SetInputData(copypd);
  //"VolumeSizingFunction"
  sizer->SetSizingFunctionArrayName(sizingFunctionArrayName.c_str());
  //sizer->SetScaleFactor(0.8);
  sizer->SetScaleFactor(scalefactor);
  sizer->Update();

  inpd->DeepCopy(sizer->GetOutput());

  fprintf(stderr,"Got Volume Mesh Func!\n");
  return SV_OK;
}

// --------------------
//  VMTKUtils_Capper
// --------------------
/**
 * @brief Function to cap or fill the holes of a vtkPolyData
 * @param *inpd This is the vtkPolyData to apply the filter
 * @param captype This is the type of cap desired. 0 is used for a basic or
 * simple cap. 1 is used if you would like a cap with a point in the center
 * of the holes.
 * @param trioutput This is boolean telling whether to make sure the output
 * caps have only triangles or not.
 * @param cellEntityIdOffset This is used if an offset from surface is desire
 * @param cellEntityIdsArrayName This is the name given if offset is desired
 * @return SV_OK if the mesh sizing function based on the circle is computed
 * correctly
 */
int VMTKUtils_Capper(vtkPolyData *inpd,int captype,int trioutput,
    int cellEntityIdOffset,std::string cellEntityIdsArrayName)
{
  vtkSmartPointer<vtkvmtkSimpleCapPolyData> simplecapper =
    vtkSmartPointer<vtkvmtkSimpleCapPolyData>::New();
  vtkSmartPointer<vtkvmtkCapPolyData> capper =
    vtkSmartPointer<vtkvmtkCapPolyData>::New();
  vtkSmartPointer<vtkPolyData> copypd =
    vtkSmartPointer<vtkPolyData>::New();
  vtkSmartPointer<vtkPolyDataNormals> normaler =
    vtkSmartPointer<vtkPolyDataNormals>::New();
  copypd->DeepCopy(inpd);
  vtkSmartPointer<vtkTriangleFilter> tris =
    vtkSmartPointer<vtkTriangleFilter>::New();

  if (cellEntityIdOffset == NULL)
  {
    cellEntityIdOffset = 1;
  }
  //0 is simple, 1 is center
  if (captype == 0)
  {
    simplecapper->SetInputData(copypd);
    simplecapper->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
    simplecapper->SetCellEntityIdOffset(cellEntityIdOffset);
    simplecapper->Update();
  }
  else if (captype == 1)
  {
    capper->SetInputData(copypd);
    capper->SetDisplacement(0.0);
    capper->SetInPlaneDisplacement(0.0);
    capper->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
    capper->SetCellEntityIdOffset(cellEntityIdOffset);
    capper->Update();
  }


  if (trioutput)
  {
    if (captype == 0)
    {
      tris->SetInputData(simplecapper->GetOutput());
    }
    else if (captype == 1)
    {
      tris->SetInputData(capper->GetOutput());
    }
    tris->PassLinesOff();
    tris->PassVertsOff();
    tris->Update();

    normaler->SetInputData(tris->GetOutput());
  }
  else
  {
    if (captype == 0)
    {
      normaler->SetInputData(simplecapper->GetOutput());
    }
    else if (captype == 1)
    {
      normaler->SetInputData(capper->GetOutput());
    }
  }

  normaler->AutoOrientNormalsOn();
  normaler->SplittingOff();
  normaler->ConsistencyOn();
  normaler->Update();

  inpd->DeepCopy(normaler->GetOutput());

  return SV_OK;
}

// --------------------
//  VMTKUtils_BoundaryLayerMesh
// --------------------
/**
 * @brief Function to compute the boundary layer on a mesh
 * @param blMesh This is the returned boundary layer mesh as a vtu
 * @param innerSurface This is the inner surface of the final bl mesh
 * @param edgeSize This is the desired edge size for the bl mesh
 * @param blThicknessFactor This is the portion of the edge size that you
 * would like to be used for the thickness of the boundary layer
 * @param numSublayers This is the number of layers in the bl mesh.
 * @param sublayerRation This is the ratio at which to decrease the bl mesh
 * by.
 * @param sidewallCellEntityId This is the value to assign to the tets that
 * align along the sidewall of the final bl mesh
 * @param innerSurfaceCellEntityId This is the value assigned to the inner
 * surface of the bl mesh
 * @param negateWarpVectors This is whether or not to use the current vector
 * direction or use the exact opposite. Typically this is 1 because you use
 * the normals as the direction and you would like the bl mesh to go inward
 * (normals are typically oriented outward).
 * @param cellEntityIdsArrayName This is the name of the array given to the
 * nodes of the output mesh.
 * @return SV_OK if the boundary layer mesh is created properly
 */
int VMTKUtils_BoundaryLayerMesh(vtkUnstructuredGrid *blMesh,
    vtkUnstructuredGrid *innerSurface,
    double edgeSize,double blThicknessFactor,int numSublayers,
    double sublayerRatio,int sidewallCellEntityId,
    int innerSurfaceCellEntityId,int negateWarpVectors,
    std::string cellEntityIdsArrayName,
    int useConstantThickness,
    std::string layerThicknessArrayName)
{
  vtkSmartPointer<vtkvmtkBoundaryLayerGenerator> layerer =
    vtkSmartPointer<vtkvmtkBoundaryLayerGenerator>::New();
// needs fixed!!! NMW 2014-08-04
#ifndef WIN32
  vtkSmartPointer<vtkDoubleArray> checkArray =
    vtkSmartPointer<vtkDoubleArray>::New();
#endif
  vtkSmartPointer<vtkUnstructuredGrid> copyug =
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  copyug->DeepCopy(blMesh);

  try
  {
// needs fixed!!!  NMW 2014-08-04, MSVC compiler doesn't like.
// should just check for array, not copy it anyway.
//
// neither does intel on linux!! 2014-08-14

//    checkArray = vtkDoubleArray::SafeDownCast(copyug->GetPointData()->GetArray("Normals"));
//
  }
  catch ( ... )
  {
    fprintf(stderr,"Normals vector doesn't exist in Unstructured Grid \
	and must exist for boundary layer formation\n");
    return SV_ERROR;
  }

  if (blThicknessFactor == NULL)
  {
    blThicknessFactor = 0.5;
  }
  if (numSublayers == NULL)
  {
    numSublayers = 2;
  }
  if (sublayerRatio == NULL)
  {
    sublayerRatio = 0.3;
  }
  if (!useConstantThickness)
  {
    if (VtkUtils_UGCheckArrayName(copyug,0,layerThicknessArrayName) != SV_OK)
    {
      fprintf(stderr,"%s Array is not on the surface\n", layerThicknessArrayName.c_str());
      return SV_ERROR;
    }
  }

  layerer->SetInputData(copyug);
  layerer->SetLayerThickness(edgeSize*blThicknessFactor);
  layerer->SetLayerThicknessRatio(blThicknessFactor);
  layerer->SetNumberOfSubLayers(numSublayers);
  layerer->SetSubLayerRatio(sublayerRatio);
  layerer->SetNegateWarpVectors(negateWarpVectors);
  layerer->SetWarpVectorsArrayName("Normals");
  layerer->SetCellEntityIdsArrayName(cellEntityIdsArrayName.c_str());
  layerer->SetUseWarpVectorMagnitudeAsThickness(0);
  layerer->SetConstantThickness(useConstantThickness);
  layerer->SetLayerThicknessArrayName(layerThicknessArrayName.c_str());
  //9999
  layerer->SetSidewallCellEntityId(sidewallCellEntityId);
  //1
  layerer->SetInnerSurfaceCellEntityId(innerSurfaceCellEntityId);
  layerer->SetSurfaceCellIdsArrayName("ModelFaceID");
  layerer->SetIncludeSurfaceCells(1);
  layerer->SetIncludeSidewallCells(1);
  layerer->Update();

  blMesh->DeepCopy(layerer->GetOutput());
  innerSurface->DeepCopy(layerer->GetInnerSurface());

  return SV_OK;
}
// --------------------
//  VMTKUtils_AppendData
// --------------------
/**
 * @brief Function append the final combined boundary layer mesh with the
 * volume mesh
 * @param meshFromTetGen This is the full vtu output from TetGen
 * interior of the boundary layer mesh
 * @param boundaryMesh This is the actual boundary layer mesh
 * @param surfaceWithSize This is the surface input to tetgen with the mesh
 * sizing function attached to it.
 * @param This is the name of the array within the surfaceWithSize mesh that
 * contains the values pertaining to whehter or not it is a capped surface or
 * not.
 * @return SV_OK if the meshes are appended into one final mesh correctly.
 * The full mesh is returned in the first argument
 */

int VMTKUtils_AppendData(vtkUnstructuredGrid *meshFromTetGen,
    vtkUnstructuredGrid *boundaryMesh,
    vtkUnstructuredGrid *surfaceWithSize,
    vtkUnstructuredGrid *newMeshVolume,
    vtkPolyData *newMeshSurface,
    int newRegionBoundaryLayer)
{
  // Tetrahedralize the boundaryMesh
  vtkSmartPointer<vtkvmtkUnstructuredGridTetraFilter> tetrahedralizer =
    vtkSmartPointer<vtkvmtkUnstructuredGridTetraFilter>::New();

  //Tetrahedralize this mesh
  tetrahedralizer->SetInputData(boundaryMesh);
  tetrahedralizer->Update();

  boundaryMesh->DeepCopy(tetrahedralizer->GetOutput());

  // Get model regions on tetgen mesh
  vtkDataArray *meshFromTetGenRegionIds = meshFromTetGen->GetCellData()->GetArray("ModelRegionID");
  if (meshFromTetGenRegionIds == NULL)
  {
    fprintf(stderr,"No model region id on tetgen mesh\n");
    return SV_ERROR;
  }

  // TODO: Will need to change if same region not on all exterior of mesh
  double minmax[2];
  meshFromTetGenRegionIds->GetRange(minmax);
  if (minmax[0] != minmax[1])
  {
    fprintf(stderr,"Cannot currently handle multi-domains on surface of tetgen mesh");
    return SV_ERROR;
  }
  int modelId = minmax[0];

  // Add model region id to surface
  vtkSmartPointer<vtkIntArray> surfaceRegionIds =
    vtkSmartPointer<vtkIntArray>::New();
  surfaceRegionIds->SetNumberOfComponents(1);
  surfaceRegionIds->SetNumberOfTuples(surfaceWithSize->GetNumberOfCells());
  surfaceRegionIds->FillComponent(0, modelId);
  surfaceRegionIds->SetName("ModelRegionID");
  surfaceWithSize->GetCellData()->AddArray(surfaceRegionIds);

  if (newRegionBoundaryLayer)
  {
    modelId++;
  }

  // Add model region id to boundary layer
  vtkSmartPointer<vtkIntArray> boundaryMeshRegionIds =
    vtkSmartPointer<vtkIntArray>::New();
  boundaryMeshRegionIds->SetNumberOfComponents(1);
  boundaryMeshRegionIds->SetNumberOfTuples(boundaryMesh->GetNumberOfCells());
  boundaryMeshRegionIds->FillComponent(0, modelId);
  boundaryMeshRegionIds->SetName("ModelRegionID");
  boundaryMesh->GetCellData()->AddArray(boundaryMeshRegionIds);

  // Separate the surface and volume cells from the boundary layer mesh
  vtkSmartPointer<vtkIntArray> isSurface =
    vtkSmartPointer<vtkIntArray>::New();
  isSurface->SetNumberOfComponents(1);
  isSurface->SetNumberOfTuples(boundaryMesh->GetNumberOfCells());
  isSurface->SetName("isSurface");

  for (int i=0;i<boundaryMesh->GetNumberOfCells();i++)
  {
    if (boundaryMesh->GetCellType(i) == VTK_TRIANGLE || boundaryMesh->GetCellType(i) == VTK_QUAD)
    {
      isSurface->SetTuple1(i,1);
    }
    else
    {
      isSurface->SetTuple1(i,0);
    }
  }
  boundaryMesh->GetCellData()->AddArray(isSurface);

  // Threshold out the surface
  vtkSmartPointer<vtkThreshold> thresholder =
    vtkSmartPointer<vtkThreshold>::New();
  thresholder->SetInputData(boundaryMesh);
  thresholder->SetInputArrayToProcess(0,0,0,1,"isSurface");
  thresholder->ThresholdBetween(1,1);
  thresholder->Update();

  vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer =
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  surfacer->SetInputData(thresholder->GetOutput());
  surfacer->Update();

  vtkSmartPointer<vtkPolyData> boundaryMeshSurface =
    vtkSmartPointer<vtkPolyData>::New();
  boundaryMeshSurface->DeepCopy(surfacer->GetOutput());

  // Threshold out the volume
  thresholder->SetInputData(boundaryMesh);
  thresholder->SetInputArrayToProcess(0,0,0,1,"isSurface");
  thresholder->ThresholdBetween(0,0);
  thresholder->Update();

  vtkSmartPointer<vtkUnstructuredGrid> boundaryMeshVolume =
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  boundaryMeshVolume->DeepCopy(thresholder->GetOutput());

  // Add the model face id back to the cap region that doesn't have it on surface bl
  thresholder->SetInputData(surfaceWithSize);
  thresholder->SetInputArrayToProcess(0,0,0,1,"WallID");
  thresholder->ThresholdBetween(0,0);
  thresholder->Update();

  surfacer->SetInputData(thresholder->GetOutput());
  surfacer->Update();

  vtkSmartPointer<vtkPolyData> surfaceMeshCaps =
    vtkSmartPointer<vtkPolyData>::New();
  surfaceMeshCaps->DeepCopy(surfacer->GetOutput());

  vtkDataArray *cellEntityIds = boundaryMeshSurface->GetCellData()->GetArray("CellEntityIds");
  int entityId;
  for (int i=0; i<boundaryMeshSurface->GetNumberOfCells(); i++)
  {
    entityId = cellEntityIds->GetTuple1(i);
    if (entityId == 9999)
    {
      boundaryMeshSurface->GetCellData()->GetArray("ModelFaceID")->SetTuple1(i, 9999);
    }
  }

  vtkSmartPointer<vtkIdList> onlyList =
    vtkSmartPointer<vtkIdList>::New();
  onlyList->InsertNextId(9999);
  if (VMTKUtils_ResetOriginalRegions(boundaryMeshSurface, surfaceMeshCaps, "ModelFaceID", onlyList, 1) != SV_OK)
  {
    fprintf(stderr,"Failure in resetting model face id on boundary layer caps\n");
    return SV_ERROR;
  }

  if (newRegionBoundaryLayer)
  {
    //Add global node Ids to tetgen volume
    vtkSmartPointer<vtkIntArray> globalNodeIds0 =
      vtkSmartPointer<vtkIntArray>::New();
    globalNodeIds0->SetNumberOfTuples(meshFromTetGen->GetNumberOfPoints());
    globalNodeIds0->SetName("GlobalNodeID");
    int globalId = 1;
    for (int i=0;i<meshFromTetGen->GetNumberOfPoints();i++)
    {
      globalNodeIds0->SetTuple1(i,globalId);
      globalId++;
    }
    meshFromTetGen->GetPointData()->AddArray(globalNodeIds0);

    //Add global node Ids to bl volume
    vtkSmartPointer<vtkIntArray> globalNodeIds1 =
      vtkSmartPointer<vtkIntArray>::New();
    globalNodeIds1->SetNumberOfTuples(boundaryMeshVolume->GetNumberOfPoints());
    globalNodeIds1->SetName("GlobalNodeID");
    for (int i=0;i<boundaryMeshVolume->GetNumberOfPoints();i++)
    {
      globalNodeIds1->SetTuple1(i,globalId);
      globalId++;
    }
    boundaryMeshVolume->GetPointData()->AddArray(globalNodeIds1);

    //Add global node Ids to tetgen volume
    vtkSmartPointer<vtkIntArray> globalElementIds0 =
      vtkSmartPointer<vtkIntArray>::New();
    globalElementIds0->SetNumberOfTuples(meshFromTetGen->GetNumberOfCells());
    globalElementIds0->SetName("GlobalElementID");
    globalId = 1;
    for (int i=0;i<meshFromTetGen->GetNumberOfCells();i++)
    {
      globalElementIds0->SetTuple1(i,globalId);
      globalId++;
    }
    meshFromTetGen->GetCellData()->AddArray(globalElementIds0);

    //Add global node Ids to bl volume
    vtkSmartPointer<vtkIntArray> globalElementIds1 =
      vtkSmartPointer<vtkIntArray>::New();
    globalElementIds1->SetNumberOfTuples(boundaryMeshVolume->GetNumberOfCells());
    globalElementIds1->SetName("GlobalElementID");
    for (int i=0;i<boundaryMeshVolume->GetNumberOfCells();i++)
    {
      globalElementIds1->SetTuple1(i,globalId);
      globalId++;
    }
    boundaryMeshVolume->GetCellData()->AddArray(globalElementIds1);

    // Extract surface of boundary layer mesh
    surfacer->SetInputData(boundaryMeshVolume);
    surfacer->Update();

    vtkSmartPointer<vtkPolyData> surface0 =
      vtkSmartPointer<vtkPolyData>::New();
    surface0->DeepCopy(surfacer->GetOutput());

    if (VMTKUtils_ResetOriginalRegions(surface0, boundaryMeshSurface, "ModelFaceID") != SV_OK)
    {
      fprintf(stderr,"Failure in resetting model face id on final mesh surface\n");
      return SV_ERROR;
    }

    // Extract surface of boundary layer mesh
    surfacer->SetInputData(meshFromTetGen);
    surfacer->Update();

    vtkSmartPointer<vtkPolyData> surface1 =
      vtkSmartPointer<vtkPolyData>::New();
    surface1->DeepCopy(surfacer->GetOutput());

    surfacer->SetInputData(surfaceWithSize);
    surfacer->Update();

    if (VMTKUtils_ResetOriginalRegions(surface1, surfacer->GetOutput(), "ModelFaceID") != SV_OK)
    {
      fprintf(stderr,"Failure in resetting model face id on final mesh surface\n");
      return SV_ERROR;
    }

    // Append surfaces together
    vtkSmartPointer<vtkAppendPolyData> boundaryAppender =
      vtkSmartPointer<vtkAppendPolyData>::New();
    boundaryAppender->AddInputData(surface0);
    boundaryAppender->AddInputData(surface1);
    boundaryAppender->Update();

    newMeshSurface->DeepCopy(boundaryAppender->GetOutput());

    // Append volumes together
    vtkSmartPointer<vtkAppendFilter> appender =
      vtkSmartPointer<vtkAppendFilter>::New();
    appender->AddInputData(boundaryMeshVolume);
    appender->AddInputData(meshFromTetGen);
    appender->Update();

    newMeshVolume->DeepCopy(appender->GetOutput());

  }
  else
  {
    vtkSmartPointer<vtkvmtkAppendFilter> appender =
      vtkSmartPointer<vtkvmtkAppendFilter>::New();

    ////Set the input data for the mesh appender and run
    appender->AddInputData(boundaryMeshVolume);
    appender->AddInputData(meshFromTetGen);
    appender->Update();

    newMeshVolume->DeepCopy(appender->GetOutput());

    //Add global node Ids to bl volume
    vtkSmartPointer<vtkIntArray> globalNodeIds0 =
      vtkSmartPointer<vtkIntArray>::New();
    globalNodeIds0->SetNumberOfTuples(newMeshVolume->GetNumberOfPoints());
    globalNodeIds0->SetName("GlobalNodeID");
    int globalId = 1;
    for (int i=0;i<newMeshVolume->GetNumberOfPoints();i++)
    {
      globalNodeIds0->SetTuple1(i,globalId);
      globalId++;
    }
    newMeshVolume->GetPointData()->AddArray(globalNodeIds0);

    //Add global node Ids to tetgen volume
    vtkSmartPointer<vtkIntArray> globalElementIds0 =
      vtkSmartPointer<vtkIntArray>::New();
    globalElementIds0->SetNumberOfTuples(newMeshVolume->GetNumberOfCells());
    globalElementIds0->SetName("GlobalElementID");
    globalId = 1;
    for (int i=0;i<newMeshVolume->GetNumberOfCells();i++)
    {
      globalElementIds0->SetTuple1(i,globalId);
      globalId++;
    }
    newMeshVolume->GetCellData()->AddArray(globalElementIds0);

    surfacer->SetInputData(newMeshVolume);
    surfacer->Update();
    newMeshSurface->DeepCopy(surfacer->GetOutput());

    vtkSmartPointer<vtkAppendFilter> boundaryAppender =
      vtkSmartPointer<vtkAppendFilter>::New();
    boundaryAppender->AddInputData(boundaryMeshSurface);
    boundaryAppender->AddInputData(surfaceMeshCaps);
    boundaryAppender->Update();

    surfacer->SetInputData(boundaryAppender->GetOutput());
    surfacer->Update();
    boundaryMeshSurface->DeepCopy(surfacer->GetOutput());

    if (VMTKUtils_ResetOriginalRegions(newMeshSurface, boundaryMeshSurface, "ModelFaceID") != SV_OK)
    {
      fprintf(stderr,"Failure in resetting model face id on final mesh surface\n");
      return SV_ERROR;
    }
  }
  fprintf(stdout,"Mesh Appended\n");

  return SV_OK;
}

int VMTKUtils_ResetOriginalRegions(vtkPolyData *newgeom,
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

int VMTKUtils_ResetOriginalRegions(vtkPolyData *newgeom,
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

int VMTKUtils_ResetOriginalRegions(vtkPolyData *newgeom,
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
