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

#include "vtkSVCenterlines.h"

#include "vtkArrayCalculator.h"
#include "vtkAppendPolyData.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDelaunay3D.h"
#include "vtkEdgeTable.h"
#include "vtkErrorCode.h"
#include "vtkIdList.h"
#include "vtkIdFilter.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPolyDataNormals.h"
#include "vtkPolyLine.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkTetra.h"
#include "vtkThreshold.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"

#include "vtkSVCellComplexThinner.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVPolyDataSurfaceInspector.h"
#include "vtkSVIOUtils.h"

#include "vtkvmtkInternalTetrahedraExtractor.h"
#include "vtkvmtkNonManifoldFastMarching.h"
#include "vtkvmtkSimplifyVoronoiDiagram.h"
#include "vtkvmtkSteepestDescentLineTracer.h"
#include "vtkvmtkVoronoiDiagram3D.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlines);

// ----------------------
// SetObjectMacros
// ----------------------
vtkCxxSetObjectMacro(vtkSVCenterlines,SourceSeedIds,vtkIdList);
vtkCxxSetObjectMacro(vtkSVCenterlines,TargetSeedIds,vtkIdList);
vtkCxxSetObjectMacro(vtkSVCenterlines,CapCenterIds,vtkIdList);

// ----------------------
// Constructors
// ----------------------
vtkSVCenterlines::vtkSVCenterlines()
{
  this->SourceSeedIds = NULL;
  this->TargetSeedIds = NULL;
  this->CapCenterIds = NULL;

  this->RadiusArrayName = NULL;
  this->CostFunction = new char[16];
  strcpy(this->CostFunction,"1/R");

  this->CostFunctionArrayName = new char[256];
  strcpy(this->CostFunctionArrayName,"CostFunctionArray");

  this->EikonalSolutionArrayName = new char[256];
  strcpy(this->EikonalSolutionArrayName,"EikonalSolutionArray");

  this->EdgeArrayName = new char[256];
  strcpy(this->EdgeArrayName,"EdgeArray");

  this->EdgePCoordArrayName = new char[256];
  strcpy(this->EdgePCoordArrayName,"EdgePCoordArray");

  this->FlipNormals = 0;
  this->SimplifyVoronoi = 0;
  this->CenterlineResampling = 0;
  this->AppendEndPointsToCenterlines = 0;
  this->ProcessCenterlinesIntoTree = 1;

  this->ResamplingStepLength = 1.0;

  this->GenerateDelaunayTessellation = 1;

  this->AbsoluteThreshold = 3;
  this->RelativeThreshold = 0.5;
  this->MedialEdgeThreshold = 5;

  this->DelaunayTessellation = NULL;
  this->DelaunayTolerance = 1E-3;

  this->VoronoiDiagram = vtkPolyData::New();
  this->RawCenterlines = vtkPolyData::New();
  this->PoleIds = vtkIdList::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlines::~vtkSVCenterlines()
{
  if (this->SourceSeedIds)
  {
    this->SourceSeedIds->Delete();
    this->SourceSeedIds = NULL;
  }

  if (this->TargetSeedIds)
  {
    this->TargetSeedIds->Delete();
    this->TargetSeedIds = NULL;
  }

  if (this->CapCenterIds)
  {
    this->CapCenterIds->Delete();
    this->CapCenterIds = NULL;
  }

  if (this->CostFunction)
  {
    delete[] this->CostFunction;
    this->CostFunction = NULL;
  }

  if (this->CostFunctionArrayName)
  {
    delete[] this->CostFunctionArrayName;
    this->CostFunctionArrayName = NULL;
  }

  if (this->EikonalSolutionArrayName)
  {
    delete[] this->EikonalSolutionArrayName;
    this->EikonalSolutionArrayName = NULL;
  }

  if (this->EdgeArrayName)
  {
    delete[] this->EdgeArrayName;
    this->EdgeArrayName = NULL;
  }

  if (this->EdgePCoordArrayName)
  {
    delete[] this->EdgePCoordArrayName;
    this->EdgePCoordArrayName = NULL;
  }

  if (this->RadiusArrayName)
  {
    delete[] this->RadiusArrayName;
    this->RadiusArrayName = NULL;
  }

  if (this->DelaunayTessellation)
  {
    this->DelaunayTessellation->Delete();
    this->DelaunayTessellation = NULL;
  }

  if (this->RawCenterlines != NULL)
  {
    this->RawCenterlines->Delete();
    this->RawCenterlines = NULL;
  }

  if (this->VoronoiDiagram != NULL)
  {
    this->VoronoiDiagram->Delete();
    this->VoronoiDiagram = NULL;
  }

  if (this->PoleIds != NULL)
  {
    this->PoleIds->Delete();
    this->PoleIds = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVCenterlines::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  vtkPolyData *input = vtkPolyData::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkPolyData *output = vtkPolyData::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  if (!this->SourceSeedIds)
  {
    vtkDebugMacro(<< "No SourceSeedIds set.");
  }
  if (this->SourceSeedIds)
  {
    if (this->SourceSeedIds->GetNumberOfIds() != 1)
    {
      vtkErrorMacro("Only one source seed can be provided with this method.");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }

  if (!this->TargetSeedIds)
  {
    vtkDebugMacro(<< "No TargetSeedIds set.");
  }

  if (!this->RadiusArrayName)
  {
    vtkErrorMacro(<< "No RadiusArrayName set.");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (!this->GenerateDelaunayTessellation && !this->DelaunayTessellation)
  {
    vtkErrorMacro(<< "GenerateDelaunayTessellation is off but a DelaunayTessellation has not been set.");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  // ------------------------------------------------------------------------
  // Check the input
  vtkDebugMacro("Checking Input...\n");
  input->BuildLinks();

  vtkNew(vtkSVPolyDataSurfaceInspector, surfaceInspector);
  surfaceInspector->SetInputData(input);
  surfaceInspector->Update();

  int numNonTriangleCells = surfaceInspector->GetNumberOfNonTriangularElements();
  int numNonManifoldEdges = surfaceInspector->GetNumberOfNonManifoldEdges();
  int numOpenEdges        = surfaceInspector->GetNumberOfOpenEdges();
  int surfaceGenus        = surfaceInspector->GetSurfaceGenus();

  vtkDebugMacro("Number of non triangle cells: " << numNonTriangleCells);
  vtkDebugMacro("Number of non manifold edges: " << numNonManifoldEdges);
  vtkDebugMacro("Number of open edges:         " << numOpenEdges);
  vtkDebugMacro("Genus of the surface:         " << surfaceGenus);

  // Make sure that the input fits the requirements of the filter
  if (numNonTriangleCells > 0)
  {
    vtkErrorMacro("Surface contains non-triangular cells. Number of non-triangular cells: " << numNonTriangleCells);
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  if (numNonManifoldEdges > 0)
  {
    vtkErrorMacro("Surface contains non-manifold edges. Number of non-manifold edges: " << numNonManifoldEdges);
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  if (numOpenEdges > 0)
  {
    vtkErrorMacro("Surface contains open edges. Number of open edges: " << numOpenEdges);
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  if (surfaceGenus > 0)
  {
    vtkErrorMacro("Surface genus is greater than 0. Surface genus is: " << surfaceGenus);
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Generate the normals
  vtkNew(vtkPolyDataNormals, surfaceNormals);
  surfaceNormals->SetInputData(input);
  surfaceNormals->SplittingOff();
  surfaceNormals->AutoOrientNormalsOn();
  surfaceNormals->SetFlipNormals(this->FlipNormals);
  surfaceNormals->ComputePointNormalsOn();
  surfaceNormals->ConsistencyOn();
  surfaceNormals->Update();
  // ------------------------------------------------------------------------
  if (this->SourceSeedIds != NULL)
  {
    vtkDebugMacro("SOURCE POINTS");
    for (int i=0; i<this->SourceSeedIds->GetNumberOfIds(); i++)
    {
      vtkDebugMacro(" " << this->SourceSeedIds->GetId(i));
    }
  }

  if (this->TargetSeedIds != NULL)
  {
    vtkDebugMacro("TARGET POINTS");
    for (int i=0; i<this->TargetSeedIds->GetNumberOfIds(); i++)
    {
      vtkDebugMacro(" " << this->TargetSeedIds->GetId(i));
    }
  }

  if (this->CapCenterIds != NULL)
  {
    vtkDebugMacro("CAP CENTER POINTS");
    for (int i=0; i<this->CapCenterIds->GetNumberOfIds(); i++)
    {
      vtkDebugMacro(" " << this->CapCenterIds->GetId(i));
    }
  }

  // ------------------------------------------------------------------------
  // Delaunay tesselation
  vtkDebugMacro("Generating Delaunay Tesselation...");
  if (this->GenerateDelaunayTessellation)
  {
    vtkNew(vtkDelaunay3D, delaunayTessellator);
    delaunayTessellator->CreateDefaultLocator();
    delaunayTessellator->SetInputConnection(surfaceNormals->GetOutputPort());
    delaunayTessellator->SetTolerance(this->DelaunayTolerance);
    delaunayTessellator->Update();

    vtkUnstructuredGrid* delaunay = delaunayTessellator->GetOutput();
    delaunay->GetPointData()->AddArray(surfaceNormals->GetOutput()->GetPointData()->GetNormals());

    vtkNew(vtkvmtkInternalTetrahedraExtractor, internalTetrahedraExtractor);
    internalTetrahedraExtractor->SetInputConnection(delaunayTessellator->GetOutputPort());
    internalTetrahedraExtractor->SetOutwardNormalsArrayName(surfaceNormals->GetOutput()->GetPointData()->GetNormals()->GetName());
    if (this->CapCenterIds)
    {
      internalTetrahedraExtractor->UseCapsOn();
      internalTetrahedraExtractor->SetCapCenterIds(this->CapCenterIds);
    }
    internalTetrahedraExtractor->Update();

    this->DelaunayTessellation = internalTetrahedraExtractor->GetOutput();
    this->DelaunayTessellation->Register(this);
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Voronoi
  vtkDebugMacro("Generating Voronoi Diagram...");
  vtkNew(vtkvmtkVoronoiDiagram3D, voronoiDiagramFilter);
  voronoiDiagramFilter->SetInputData(this->DelaunayTessellation);
  voronoiDiagramFilter->SetRadiusArrayName(this->RadiusArrayName);
  voronoiDiagramFilter->Update();

  this->PoleIds->DeepCopy(voronoiDiagramFilter->GetPoleIds());

  vtkPolyData* voronoiDiagram = voronoiDiagramFilter->GetOutput();
  if (this->SimplifyVoronoi)
  {
    vtkNew(vtkvmtkSimplifyVoronoiDiagram, voronoiDiagramSimplifier);
    voronoiDiagramSimplifier->SetInputConnection(voronoiDiagramFilter->GetOutputPort());
    voronoiDiagramSimplifier->SetUnremovablePointIds(voronoiDiagramFilter->GetPoleIds());
    voronoiDiagramSimplifier->Update();
    voronoiDiagram = voronoiDiagramSimplifier->GetOutput();
    voronoiDiagram->Register(this);
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Set up for pruning
  // triangulate
  vtkNew(vtkTriangleFilter, triangulator);
  triangulator->SetInputData(voronoiDiagram);
  triangulator->Update();

  // Copy triangulated pd
  vtkNew(vtkPolyData, triPd);
  triPd->DeepCopy(triangulator->GetOutput());
  triPd->BuildLinks();

  // Num cells and points
  int numCells = triPd->GetNumberOfCells();
  int numPts = triPd->GetNumberOfPoints();

  // Get polydata consisting of lines and points (edges of pd)
  vtkNew(vtkPolyData, edgePd);
  vtkSVGeneralUtils::GetEdgePolyData(triPd, edgePd);
  // ------------------------------------------------------------------------
  vtkDebugMacro("Number of Cells on input: " << input->GetNumberOfCells());
  vtkDebugMacro("Length of input: " << input->GetLength());
  vtkDebugMacro("Number of Cells on voronoi: " << triPd->GetNumberOfCells());
  vtkDebugMacro("Length of voronoi: " << triPd->GetLength());

  // ------------------------------------------------------------------------
  // Pruning voronoi diagram
  vtkDebugMacro("Pruning Voronoi Diagram...");
  vtkNew(vtkSVCellComplexThinner, voronoiThinner);
  voronoiThinner->SetInputData(triPd);
  voronoiThinner->SetInputEdgePd(edgePd);
  voronoiThinner->Update();

  vtkNew(vtkPolyData, newTriPd);
  newTriPd->DeepCopy(voronoiThinner->GetOutput());

  vtkNew(vtkPolyData, newEdgePd);
  newEdgePd->DeepCopy(voronoiThinner->GetOutputEdgePd());

  vtkDebugMacro("Done computing voronoi thinning iterations...");
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Give temporary ids for thresholding
  vtkNew(vtkIntArray, tmpEdgeArray);
  tmpEdgeArray->SetNumberOfTuples(newEdgePd->GetNumberOfCells());
  tmpEdgeArray->SetName("TmpInternalIds");
  for (int i=0; i<newEdgePd->GetNumberOfCells(); i++)
    tmpEdgeArray->SetTuple1(i, i);
  newEdgePd->GetCellData()->AddArray(tmpEdgeArray);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Threshold based on absolute retention
  int mAbsThr = this->AbsoluteThreshold; // TODO: FIGURE OUT GOOD VALUE FOR THIS!!!
  double mAbsRange[2];
  newEdgePd->GetCellData()->GetArray("MAbs")->GetRange(mAbsRange);
  vtkNew(vtkThreshold, mAbsThresholder);
  mAbsThresholder->SetInputData(newEdgePd);
  mAbsThresholder->SetInputArrayToProcess(0, 0, 0, 1, "MAbs");
  mAbsThresholder->ThresholdBetween(mAbsThr, mAbsRange[1]);
  mAbsThresholder->Update();
  vtkDebugMacro("Thresholded MAbs: " << mAbsThresholder->GetOutput()->GetNumberOfCells());
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Threshold based on relative retention
  double mRelThr = this->RelativeThreshold;
  double mRelRange[2];
  newEdgePd->GetCellData()->GetArray("MRel")->GetRange(mRelRange);
  vtkNew(vtkThreshold, mRelThresholder);
  mRelThresholder->SetInputData(mAbsThresholder->GetOutput());
  mRelThresholder->SetInputArrayToProcess(0, 0, 0, 1, "MRel");
  mRelThresholder->ThresholdBetween(mRelThr, mRelRange[1]);
  mRelThresholder->Update();
  vtkDebugMacro("Thresholded MRel: " << mRelThresholder->GetOutput()->GetNumberOfCells());
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Get the medial edges
  // Get all separated regions from the thresholded polydata
  vtkNew(vtkConnectivityFilter, connector);
  connector->SetInputData(mRelThresholder->GetOutput());
  connector->SetExtractionModeToAllRegions();
  connector->ColorRegionsOn();
  connector->Update();

  // Convert to surface
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(connector->GetOutput());
  surfacer->Update();

  // Now we have all thats left
  vtkNew(vtkPolyData, leftOver);
  leftOver->DeepCopy(surfacer->GetOutput());

  // Anything larger than out defined limit for connected components, is defined as medial edge
  vtkNew(vtkIntArray, leaveArray);
  leaveArray->SetNumberOfTuples(edgePd->GetNumberOfCells());
  leaveArray->SetName("MedialEdges");
  for (int i=0; i<edgePd->GetNumberOfCells(); i++)
    leaveArray->SetTuple1(i, 0);

  // Now loop through each connected region and mark the medial edges
  int connectThr = this->MedialEdgeThreshold;
  vtkNew(vtkThreshold, regionThresholder);
  regionThresholder->SetInputData(leftOver);
  regionThresholder->SetInputArrayToProcess(0, 0, 0, 1, "RegionId");

  // Loop through
  for (int i=0; i<connector->GetNumberOfExtractedRegions(); i++)
  {
    regionThresholder->ThresholdBetween(i, i);
    regionThresholder->Update();

    vtkDebugMacro("Thresholded region " << i << " " << regionThresholder->GetOutput()->GetNumberOfCells());
    if (regionThresholder->GetOutput()->GetNumberOfCells() > connectThr)
    {
      for (int j=0; j<regionThresholder->GetOutput()->GetNumberOfCells(); j++)
      {
        int origCellId = regionThresholder->GetOutput()->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);
        leaveArray->SetTuple1(origCellId, 1);
      }
    }
  }

  // Add new cell data to use as fixed edges
  edgePd->GetCellData()->AddArray(leaveArray);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Now prune again
  vtkDebugMacro("Thin voronoi diagram while maintaining the interior medialcells");
  vtkNew(vtkSVCellComplexThinner, medialAxisThinner);
  medialAxisThinner->SetInputData(triPd);
  medialAxisThinner->SetInputEdgePd(edgePd);
  medialAxisThinner->SetPreserveEdgeCellsArrayName("MedialEdges");
  medialAxisThinner->Update();

  vtkNew(vtkPolyData, nextTriPd);
  nextTriPd->DeepCopy(medialAxisThinner->GetOutput());
  vtkNew(vtkPolyData, nextEdgePd);
  nextEdgePd->DeepCopy(medialAxisThinner->GetOutputEdgePd());
  vtkDebugMacro("Done computing voronoi thinning iterations...");
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Threshold last removal iteration cells to get centerlines
  // Get last removal iteration
  double finalRange[2];
  nextEdgePd->GetCellData()->GetArray("RemovalIteration")->GetRange(finalRange);

  // Add radius array to edge pd
  nextEdgePd->GetPointData()->AddArray(triPd->GetPointData()->GetArray(this->RadiusArrayName));

  // Track ids of original for later
  vtkNew(vtkIntArray, tmpPtArray);
  tmpPtArray->Reset();
  tmpPtArray->SetNumberOfTuples(nextEdgePd->GetNumberOfPoints());
  tmpPtArray->SetName("TmpInternalIds");
  for (int i=0; i<nextEdgePd->GetNumberOfPoints(); i++)
    tmpPtArray->SetTuple1(i, i);

  nextEdgePd->GetPointData()->AddArray(tmpPtArray);

  // Threhsold to centerlines
  vtkNew(vtkThreshold, finalThreshold);
  finalThreshold->SetInputData(nextEdgePd);
  finalThreshold->SetInputArrayToProcess(0, 0, 0, 1, "RemovalIteration");
  finalThreshold->ThresholdBetween(finalRange[1], finalRange[1]);
  finalThreshold->Update();

  // Surface
  surfacer->SetInputData(finalThreshold->GetOutput());
  surfacer->Update();

  // Clean
  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(surfacer->GetOutput());
  cleaner->Update();

  // Copy to lines object
  vtkNew(vtkPolyData, linesPd);
  linesPd->DeepCopy(cleaner->GetOutput());
  linesPd->BuildLinks();

  // Remove cells that may have potentially been reduced to vertices
  for (int i=0; i<linesPd->GetNumberOfCells(); i++)
  {
    if (linesPd->GetCellType(i) != VTK_LINE)
    {
      linesPd->DeleteCell(i);
    }
  }
  linesPd->RemoveDeletedCells();
  linesPd->BuildLinks();

  // ------------------------------------------------------------------------
  // Check to see if duplicate lines, and remove if necessary
  vtkIdType npts, *pts;
  vtkNew(vtkIdList, cellEdgeNeighbors);
  std::vector<int> dupDeleted(linesPd->GetNumberOfCells(), 0);
  for (int i=0; i<linesPd->GetNumberOfCells(); i++)
  {
    if (dupDeleted[i])
      continue;

    linesPd->GetCellPoints(i, npts, pts);
    linesPd->GetCellEdgeNeighbors(i, pts[0], pts[1], cellEdgeNeighbors);

    if (cellEdgeNeighbors->GetNumberOfIds() > 0)
    {
      for (int j=0; j<cellEdgeNeighbors->GetNumberOfIds(); j++)
      {
        linesPd->DeleteCell(cellEdgeNeighbors->GetId(j));
        dupDeleted[cellEdgeNeighbors->GetId(j)] = 1;
        vtkWarningMacro("Duplicate cell in lines is being deleted");
      }
    }
  }
  linesPd->RemoveDeletedCells();
  linesPd->BuildLinks();
  // ------------------------------------------------------------------------

  // If we only raw centerlines, we dont need to process
  if (!this->ProcessCenterlinesIntoTree)
  {
    output->ShallowCopy(linesPd);
    this->RawCenterlines->ShallowCopy(linesPd);

    return SV_OK;
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Find the end points of the lines
	int firstVertex = -1;
  std::vector<std::vector<int> > connectedEdgePts(linesPd->GetNumberOfPoints());
  vtkNew(vtkIdList, linesEndPointIds);
  vtkNew(vtkPoints, linesEndPoints);

  this->GetLinesEndPoints(linesPd, linesEndPointIds, linesEndPoints, connectedEdgePts, firstVertex);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Check if an end point found
  if (firstVertex == -1)
  {
    vtkErrorMacro("No first vertex found, lines must form loop");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Set up to get edge data structures from leftover polydata
  std::vector<int> pointUsed(linesPd->GetNumberOfPoints(), 0);

  pointUsed[firstVertex] = 1;
  int startVertex = connectedEdgePts[firstVertex][0];

  std::vector<std::vector<int> > allEdges;
  std::vector<int> thisEdge;
  thisEdge.push_back(firstVertex);

  this->RecursiveGetPolylines(linesPd, connectedEdgePts, startVertex, pointUsed, allEdges, thisEdge);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Check the output lines and use graph cycles to remove duplicate lines
  vtkNew(vtkIdList, allEndIds);
  std::vector<int> nodeCount;
  std::vector<int> needToDelete(allEdges.size(), 0);
  for (int i=0; i<allEdges.size(); i++)
  {
    int edgeSize = allEdges[i].size();
    int edgeId0 = allEdges[i][0];
    int edgeIdN = allEdges[i][edgeSize-1];

    int edge0IsId = allEndIds->IsId(edgeId0);
    int edgeNIsId = allEndIds->IsId(edgeIdN);

    if (edge0IsId != -1 && edgeNIsId != -1)
    {
      // Both edges of this node already in list, delete it!
      needToDelete[i] = 1;
      nodeCount[edge0IsId]++;
      nodeCount[edgeNIsId]++;
    }
    else if (edgeId0 == edgeIdN)
    {
      // This is a loop with same start and end point, just remove, but
      // dont add to nodecount because it would mess the count up
      needToDelete[i] = 1;
    }
    else
    {
      if (edge0IsId == -1)
      {
        // This point not already in graph, add
        allEndIds->InsertNextId(edgeId0);
        nodeCount.push_back(1);
      }
      else
      {
        // This point already in graph, increase count
        nodeCount[edge0IsId]++;
      }
      if (edgeNIsId == -1)
      {
        // This point not already in graph, add
        allEndIds->InsertNextId(edgeIdN);
        nodeCount.push_back(1);
      }
      else
      {
        // This point already in graph, increase count
        nodeCount[edgeNIsId]++;
      }
    }
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Delete cells based on what still needs to be removed
  vtkDebugMacro("Deleting lines from one side of loops");
  std::vector<int> isDeleted(allEdges.size(), 0);

  this->LoopRemoveMarkedCells(linesPd, allEdges, needToDelete, isDeleted, allEndIds, nodeCount);
  std::fill(needToDelete.begin(), needToDelete.end(), 0);

  // ------------------------------------------------------------------------
  // Delete very small entrance lines
  for (int i=0; i<allEdges.size(); i++)
  {
    int edgeSize = allEdges[i].size();
    int nodeId0 = allEndIds->IsId(allEdges[i][0]);
    int nodeIdN = allEndIds->IsId(allEdges[i][edgeSize-1]);

    // if edge size is less that the connected cell threshold, delete
    if (edgeSize < connectThr)
    {
      if (nodeCount[nodeId0] <= 1 || nodeCount[nodeIdN] <= 1)
      {
        vtkDebugMacro("FOUND A SMALL LINE THAT NEEDS TO BE DELETED " << i );
        needToDelete[i] = 1;
      }
    }
  }

  vtkDebugMacro("Deleting lines that are from very small branching lines");
  vtkDebugMacro("LINES THAT ARE GONE");
  for (int i=0; i<isDeleted.size(); i++)
  {
    vtkDebugMacro("WHAT " << i << ": " << isDeleted[i]);
  }
  this->RemoveMarkedCells(linesPd, allEdges, needToDelete, isDeleted, allEndIds, nodeCount);
  std::fill(needToDelete.begin(), needToDelete.end(), 0);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Now re-calc the end points so that end points that were deleted previously
  // arent taken into consideration
  // Find the end points of the lines
  linesEndPointIds->Reset();
  linesEndPoints->Reset();

  // Remove the cells that have been deleted so that when we try to find the
  // close end points we dont find ones that have already been deleted
  linesPd->RemoveDeletedCells();
  linesPd->BuildLinks();

  this->GetLinesEndPoints(linesPd, linesEndPointIds, linesEndPoints, connectedEdgePts, firstVertex);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Set the end point locator
  vtkNew(vtkPointLocator, linesEndPointLocator);
  vtkNew(vtkPolyData, linesEndPointsPd);  linesEndPointsPd->SetPoints(linesEndPoints);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Delete cells based on given seeds
  if (this->SourceSeedIds)
  {
    if (this->SourceSeedIds->GetNumberOfIds() != 1)
    {
      vtkErrorMacro("Only one source seed can be provided with this method");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }

    // lines end point locator allows us to match up centerline ends with
    // given points or cap center points
    linesEndPointLocator->SetDataSet(linesEndPointsPd);
    linesEndPointLocator->BuildLocator();

    std::vector<int> endPointUsed(linesEndPointIds->GetNumberOfIds(), 0);
    std::vector<int> deleteSeeds;
    for (int j=0; j<this->SourceSeedIds->GetNumberOfIds(); j++)
    {
      double sourcePt[3];
      if (this->CapCenterIds)
        input->GetPoint(this->CapCenterIds->GetId(this->SourceSeedIds->GetId(j)), sourcePt);
      else
        input->GetPoint(this->SourceSeedIds->GetId(j), sourcePt);

      int endPointId = linesEndPointLocator->FindClosestPoint(sourcePt);
      int linesPtId = linesEndPointIds->GetId(endPointId);

      if (endPointUsed[endPointId] == 1)
      {
        vtkWarningMacro("Two end lines found for different target seeds, target seeds too close");
        //return SV_ERROR;
      }
      else
      {
        endPointUsed[endPointId] = 1;
      }
    }

    // If target seeds given, check those as well
    if (this->TargetSeedIds)
    {
      int numSeeds = this->SourceSeedIds->GetNumberOfIds() + this->TargetSeedIds->GetNumberOfIds();
      vtkDebugMacro("Number of seeds: " << numSeeds);
      vtkDebugMacro("Number of found line ends: " << linesEndPointIds->GetNumberOfIds());

      if (numSeeds > linesEndPointIds->GetNumberOfIds() && this->TargetSeedIds)
      {
        vtkDebugMacro("More seeds given than found ends");
        vtkNew(vtkPointLocator, seedPointLocator);
        vtkNew(vtkPoints, seedPoints);
        vtkNew(vtkPolyData, seedPointsPd);  seedPointsPd->SetPoints(seedPoints);
        vtkNew(vtkIdList, seedPointIds);
        for (int j=0; j<this->TargetSeedIds->GetNumberOfIds(); j++)
        {
          double targetPt[3];
          if (this->CapCenterIds)
            input->GetPoint(this->CapCenterIds->GetId(this->TargetSeedIds->GetId(j)), targetPt);
          else
            input->GetPoint(this->TargetSeedIds->GetId(j), targetPt);

          seedPointsPd->GetPoints()->InsertNextPoint(targetPt);
          seedPointIds->InsertNextId(j);
        }
        seedPointLocator->SetDataSet(seedPointsPd);
        seedPointLocator->BuildLocator();

        for (int j=0; j<linesEndPointIds->GetNumberOfIds(); j++)
        {
          if (endPointUsed[j] == 1)
          {
            continue;
          }
          endPointUsed[j] = 1;

          int linesPtId = linesEndPointIds->GetId(j);

          double endPt[3];
          linesPd->GetPoint(linesPtId, endPt);

          int closestSeed = seedPointLocator->FindClosestPoint(endPt);
          int targetSeedId = seedPointIds->GetId(closestSeed);
        }
      }
      else
      {
        if (numSeeds == linesEndPointIds->GetNumberOfIds() && this->TargetSeedIds)
        {
          vtkDebugMacro("Equal number of seeds and found ends");
        }
        else if (numSeeds < linesEndPointIds->GetNumberOfIds() && this->TargetSeedIds)
        {
          vtkDebugMacro("Less seeds given than found ends");
        }

        for (int j=0; j<this->TargetSeedIds->GetNumberOfIds(); j++)
        {
          double targetPt[3];
          if (this->CapCenterIds)
            input->GetPoint(this->CapCenterIds->GetId(this->TargetSeedIds->GetId(j)), targetPt);
          else
            input->GetPoint(this->TargetSeedIds->GetId(j), targetPt);

          int endPointId = linesEndPointLocator->FindClosestPoint(targetPt);
          int linesPtId = linesEndPointIds->GetId(endPointId);

          if (endPointUsed[endPointId] == 1)
          {
            vtkWarningMacro("Two end lines found for different target seeds, target seeds too close.");
            //return SV_ERROR;
          }
          else
          {
            endPointUsed[endPointId] = 1;
          }
        }
      }

      for (int i=0; i<endPointUsed.size(); i++)
      {
        if (endPointUsed[i] == 0)
        {
          int linesPtId = linesEndPointIds->GetId(i);
          deleteSeeds.push_back(linesPtId);
          vtkDebugMacro("End point " << linesPtId << " was not used, going to remove from search list");
        }
      }
    }

    // Get rid of the ones we just specified if there happen to be extra end line points
    for (int i=0; i<deleteSeeds.size(); i++)
    {
      vtkDebugMacro("Deleting cell with end id " << deleteSeeds[i]);
      for (int j=0; j<allEdges.size(); j++)
      {
        if (isDeleted[j])
        {
          continue;
        }

        int edgeSize = allEdges[j].size();
        int nodeId0 = allEdges[j][0];
        int nodeIdN = allEdges[j][edgeSize-1];

        if (isDeleted[j])
        {
          continue;
        }

        if (nodeId0 == deleteSeeds[i] || nodeIdN == deleteSeeds[i])
        {
          vtkDebugMacro("Marking to delete "<<  j);
          needToDelete[j] = 1;
        }
      }
    }

    // Again remove the marked cells
    vtkDebugMacro("Deleting lines that do not match to a given target or source point");
    this->LoopRemoveMarkedCells(linesPd, allEdges, needToDelete, isDeleted, allEndIds, nodeCount);
  }

  // ------------------------------------------------------------------------
  // Remove deleted cells
  linesPd->RemoveDeletedCells();
  cleaner->SetInputData(linesPd);
  cleaner->Update();

  linesPd->DeepCopy(cleaner->GetOutput());
  linesPd->BuildLinks();
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Remove cells that may have potentially been reduced to vertices
  for (int i=0; i<linesPd->GetNumberOfCells(); i++)
  {
    if (linesPd->GetCellType(i) != VTK_LINE)
    {
      linesPd->DeleteCell(i);
    }
  }
  linesPd->RemoveDeletedCells();
  linesPd->BuildLinks();
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Check to see if duplicate lines, and remove if necessary
  std::vector<int> checkDeleted(linesPd->GetNumberOfCells(), 0);
  for (int i=0; i<linesPd->GetNumberOfCells(); i++)
  {
    if (checkDeleted[i])
      continue;

    linesPd->GetCellPoints(i, npts, pts);
    linesPd->GetCellEdgeNeighbors(i, pts[0], pts[1], cellEdgeNeighbors);

    if (cellEdgeNeighbors->GetNumberOfIds() > 0)
    {
      for (int j=0; j<cellEdgeNeighbors->GetNumberOfIds(); j++)
      {
        linesPd->DeleteCell(cellEdgeNeighbors->GetId(j));
        checkDeleted[cellEdgeNeighbors->GetId(j)] = 1;
        vtkWarningMacro("Duplicate cell in lines is being deleted");
      }
    }
  }
  linesPd->RemoveDeletedCells();
  linesPd->BuildLinks();
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Check the genus of our final edges
  int nEdgeE = linesPd->GetNumberOfCells();
  int nEdgeV = linesPd->GetNumberOfPoints();
  int nFaces = nEdgeE - nEdgeV + 2;
  vtkDebugMacro("Number of line edges:    " << nEdgeE);
  vtkDebugMacro("Number of line vertices: " << nEdgeV);
  vtkDebugMacro("Number of line faces:    " << nFaces);
  if (nFaces != 1)
  {
    vtkErrorMacro("After processing, centerline contains faces, or contains loops or cycles");
    vtkNew(vtkIntArray, isSpecialPoint);
    isSpecialPoint->SetNumberOfTuples(nEdgeV);
    isSpecialPoint->FillComponent(0, 0);
    isSpecialPoint->SetName("SpecialPoint");

    vtkNew(vtkIdList, allPointCells);
    for (int i=0; i<nEdgeV; i++)
    {
      linesPd->GetPointCells(i, allPointCells);
      if (allPointCells->GetNumberOfIds() != 2)
        isSpecialPoint->SetTuple1(i, 1);
    }
    linesPd->GetPointData()->AddArray(isSpecialPoint);
    output->ShallowCopy(linesPd);

    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Get end points
  this->GetLinesEndPoints(linesPd, linesEndPointIds, linesEndPoints, connectedEdgePts, firstVertex);
  linesEndPointsPd->SetPoints(linesEndPoints);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Check if an end point found
  if (firstVertex == -1)
  {
    vtkErrorMacro("No first vertex found, lines must form loop");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Get starting seed point
  if (this->SourceSeedIds)
  {
    if (this->SourceSeedIds->GetNumberOfIds() != 1)
    {
      vtkErrorMacro("Only one source seed can be provided with this method");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }

    linesEndPointLocator->SetDataSet(linesEndPointsPd);
    linesEndPointLocator->BuildLocator();

    double firstPt[3];
    if (this->CapCenterIds)
      input->GetPoint(this->CapCenterIds->GetId(this->SourceSeedIds->GetId(0)), firstPt);
    else
      input->GetPoint(this->SourceSeedIds->GetId(0), firstPt);

    int endPointId = linesEndPointLocator->FindClosestPoint(firstPt);
    int linesPtId = linesEndPointIds->GetId(endPointId);

    // If source seed given, use this as starting seed!
    firstVertex = linesPtId;
  }

  if (firstVertex == -1)
  {
    vtkErrorMacro("No first vertex found, lines cannot form loop");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Re-get all the edges now that weve removed a bunch
  pointUsed.clear();
  pointUsed.resize(linesPd->GetNumberOfPoints(), 0);

  pointUsed[firstVertex] = 1;
  startVertex = connectedEdgePts[firstVertex][0];

  allEdges.clear();
  thisEdge.clear();
  thisEdge.push_back(firstVertex);

  this->RecursiveGetPolylines(linesPd, connectedEdgePts, startVertex, pointUsed, allEdges, thisEdge);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Turn these edges into constructed lines from source end point to all other
  // end points
  std::vector<std::vector<int> > fullCenterlineEdges;

  int startEdge = 0;
  int front = allEdges[startEdge][0];
  int back  = allEdges[startEdge][allEdges[0].size()-1];

  this->RecursiveGetFullCenterlines(allEdges, fullCenterlineEdges, startEdge, front, back);
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Get the cost function
  vtkNew(vtkArrayCalculator, voronoiCostFunctionCalculator);
#if (VTK_MAJOR_VERSION <= 5)
  voronoiCostFunctionCalculator->SetInput(voronoiDiagram);
#else
  voronoiCostFunctionCalculator->SetInputData(voronoiDiagram);
#endif
  voronoiCostFunctionCalculator->SetAttributeModeToUsePointData();
  voronoiCostFunctionCalculator->AddScalarVariable("R",this->RadiusArrayName,0);
  voronoiCostFunctionCalculator->SetFunction(this->CostFunction);
  voronoiCostFunctionCalculator->SetResultArrayName(this->CostFunctionArrayName);
  voronoiCostFunctionCalculator->Update();

  surfacer->SetInputData(voronoiCostFunctionCalculator->GetOutput());
  surfacer->Update();
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // Get the actual seed points on the voronoi diagram
  std::vector<std::vector<int> > voronoiSeeds(allEdges.size());
  for (int i=0; i<allEdges.size(); i++)
  {
    int edgeSize = allEdges[i].size();
    int voronoiId0 = linesPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(allEdges[i][0]);
    int voronoiId1 = linesPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(allEdges[i][edgeSize-1]);
    voronoiSeeds[i].push_back(voronoiId0);
    voronoiSeeds[i].push_back(voronoiId1);
  }
  // ------------------------------------------------------------------------

  // ------------------------------------------------------------------------
  // If CapCenterIds, SourceSeedIds, or TargetSeedIds are given, we need to
  // process to get actual start points
  vtkNew(vtkIdList, voronoiCapIds);
  if (this->CapCenterIds)
  {
    if (this->FindVoronoiSeeds(this->DelaunayTessellation,this->CapCenterIds,surfaceNormals->GetOutput()->GetPointData()->GetNormals(),voronoiCapIds) != SV_OK)
    {
      vtkErrorMacro("Error getting voronoi seeds from caps");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }

  if (this->SourceSeedIds)
  {
    if (this->SourceSeedIds->GetNumberOfIds() != 1)
    {
      vtkErrorMacro("Only one source seed can be provided with this method");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }

    std::vector<int> endPointUsed(linesEndPointIds->GetNumberOfIds(), 0);
    for (int j=0; j<this->SourceSeedIds->GetNumberOfIds(); j++)
    {
      double sourcePt[3];
      if (this->CapCenterIds)
        input->GetPoint(this->CapCenterIds->GetId(this->SourceSeedIds->GetId(j)), sourcePt);
      else
        input->GetPoint(this->SourceSeedIds->GetId(j), sourcePt);

      int endPointId = linesEndPointLocator->FindClosestPoint(sourcePt);
      int linesPtId = linesEndPointIds->GetId(endPointId);
      int voronoiId = linesPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(linesPtId);

      if (endPointUsed[endPointId] == 1)
      {
        vtkWarningMacro("Two end lines found for different target seeds, target seeds too close");
        //return SV_ERROR;
      }
      else
      {
        endPointUsed[endPointId] = 1;
      }

      for (int k=0; k<voronoiSeeds.size(); k++)
      {
        for (int l=0; l<voronoiSeeds[k].size(); l++)
        {
          if (voronoiSeeds[k][l] == voronoiId)
          {
            if (this->CapCenterIds)
            {
              voronoiSeeds[k][l] = voronoiCapIds->GetId(this->SourceSeedIds->GetId(j));
            }
            else
            {
              voronoiSeeds[k][l] = this->PoleIds->GetId(this->SourceSeedIds->GetId(j));
            }
          }
        }
      }
    }

    if (this->TargetSeedIds)
    {
      int numSeeds = this->SourceSeedIds->GetNumberOfIds() + this->TargetSeedIds->GetNumberOfIds();

      if (numSeeds > linesEndPointIds->GetNumberOfIds() && this->TargetSeedIds)
      {
        vtkErrorMacro("More seeds given than found ends");
        vtkNew(vtkPointLocator, seedPointLocator);
        vtkNew(vtkPoints, seedPoints);
        vtkNew(vtkPolyData, seedPointsPd);  seedPointsPd->SetPoints(seedPoints);
        vtkNew(vtkIdList, seedPointIds);
        for (int j=0; j<this->TargetSeedIds->GetNumberOfIds(); j++)
        {
          double targetPt[3];
          if (this->CapCenterIds)
          {
            input->GetPoint(this->CapCenterIds->GetId(this->TargetSeedIds->GetId(j)), targetPt);
          }
          else
          {
            input->GetPoint(this->TargetSeedIds->GetId(j), targetPt);
          }

          seedPointsPd->GetPoints()->InsertNextPoint(targetPt);
          seedPointIds->InsertNextId(j);
        }
        seedPointLocator->SetDataSet(seedPointsPd);
        seedPointLocator->BuildLocator();

        for (int j=0; j<linesEndPointIds->GetNumberOfIds(); j++)
        {
          if (endPointUsed[j] == 1)
          {
            continue;
          }
          endPointUsed[j] = 1;

          int linesPtId = linesEndPointIds->GetId(j);

          double endPt[3];
          linesPd->GetPoint(linesPtId, endPt);
          int voronoiId = linesPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(linesPtId);

          int closestSeed = seedPointLocator->FindClosestPoint(endPt);
          int targetSeedId = seedPointIds->GetId(closestSeed);

          for (int k=0; k<voronoiSeeds.size(); k++)
          {
            for (int l=0; l<voronoiSeeds[k].size(); l++)
            {
              if (voronoiSeeds[k][l] == voronoiId)
              {
                if (this->CapCenterIds)
                {
                  voronoiSeeds[k][l] = voronoiCapIds->GetId(this->TargetSeedIds->GetId(targetSeedId));
                }
                else
                {
                  voronoiSeeds[k][l] = this->PoleIds->GetId(this->TargetSeedIds->GetId(targetSeedId));
                }
              }
            }
          }
        }
      }
      else
      {
        if (numSeeds == linesEndPointIds->GetNumberOfIds() && this->TargetSeedIds)
        {
          vtkDebugMacro("Equal number of seeds and found ends");
        }
        else if (numSeeds < linesEndPointIds->GetNumberOfIds() && this->TargetSeedIds)
        {
          vtkDebugMacro("Less seeds given than found ends");
        }

        for (int j=0; j<this->TargetSeedIds->GetNumberOfIds(); j++)
        {
          double targetPt[3];
          if (this->CapCenterIds)
          {
            input->GetPoint(this->CapCenterIds->GetId(this->TargetSeedIds->GetId(j)), targetPt);
          }
          else
          {
            input->GetPoint(this->TargetSeedIds->GetId(j), targetPt);
          }

          int endPointId = linesEndPointLocator->FindClosestPoint(targetPt);
          int linesPtId = linesEndPointIds->GetId(endPointId);
          int voronoiId = linesPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(linesPtId);

          if (endPointUsed[endPointId] == 1)
          {
            vtkWarningMacro("Two end lines found for different target seeds, target seeds too close");
            //return SV_ERROR;
          }
          else
          {
            endPointUsed[endPointId] = 1;
          }

          for (int k=0; k<voronoiSeeds.size(); k++)
          {
            for (int l=0; l<voronoiSeeds[k].size(); l++)
            {
              if (voronoiSeeds[k][l] == voronoiId)
              {
                if (this->CapCenterIds)
                {
                  voronoiSeeds[k][l] = voronoiCapIds->GetId(this->TargetSeedIds->GetId(j));
                }
                else
                {
                  voronoiSeeds[k][l] = this->PoleIds->GetId(this->TargetSeedIds->GetId(j));
                }
              }
            }
          }
        }
      }
    }
  }
  // ------------------------------------------------------------------------

  vtkNew(vtkAppendPolyData, appender);

  vtkNew(vtkvmtkNonManifoldFastMarching, voronoiFastMarching);
  voronoiFastMarching->SetInputData(voronoiCostFunctionCalculator->GetOutput());
  voronoiFastMarching->SetCostFunctionArrayName(this->CostFunctionArrayName);
  voronoiFastMarching->SetSolutionArrayName(this->EikonalSolutionArrayName);
  voronoiFastMarching->SeedsBoundaryConditionsOn();
  for (int i=0; i<voronoiSeeds.size(); i++)
  {
    int voronoiId0 = voronoiSeeds[i][0];
    int voronoiIdN = voronoiSeeds[i][1];

    vtkNew(vtkIdList, voronoiSourceSeedIds);
    voronoiSourceSeedIds->SetNumberOfIds(1);
    voronoiSourceSeedIds->SetId(0, voronoiIdN);

    vtkNew(vtkIdList, voronoiTargetSeedIds);
    voronoiTargetSeedIds->SetNumberOfIds(1);
    voronoiTargetSeedIds->SetId(0, voronoiId0);

    vtkDebugMacro("Doing edge: " << voronoiId0 << " " << voronoiIdN);
    vtkDebugMacro("Marching...");
    voronoiFastMarching->SetSeeds(voronoiSourceSeedIds);
    voronoiFastMarching->Update();
    vtkDebugMacro("Done");

    //this->VoronoiDiagram->ShallowCopy(voronoiFastMarching->GetOutput());

    vtkNew(vtkvmtkSteepestDescentLineTracer, centerlineBacktracing);
    centerlineBacktracing->SetInputConnection(voronoiFastMarching->GetOutputPort());
    centerlineBacktracing->SetDataArrayName(this->RadiusArrayName);
    centerlineBacktracing->SetDescentArrayName(this->EikonalSolutionArrayName);
    centerlineBacktracing->SetEdgeArrayName(this->EdgeArrayName);
    centerlineBacktracing->SetEdgePCoordArrayName(this->EdgePCoordArrayName);
    centerlineBacktracing->SetSeeds(voronoiTargetSeedIds);
    centerlineBacktracing->MergePathsOff();
    centerlineBacktracing->StopOnTargetsOn();
    centerlineBacktracing->SetTargets(voronoiSourceSeedIds);
    vtkDebugMacro("Backtracing...");
    centerlineBacktracing->Update();
    vtkDebugMacro("Done");

    appender->AddInputData(centerlineBacktracing->GetOutput());
  }
  appender->Update();

  vtkNew(vtkPolyData, currentLine);
  vtkNew(vtkCellArray, newCells);
  vtkNew(vtkPoints, newPoints);
  vtkNew(vtkPointData, newPointData);
  newPointData->CopyAllocate(appender->GetOutput()->GetPointData(),
                             appender->GetOutput()->GetNumberOfPoints());
  for (int i=0; i<fullCenterlineEdges.size(); i++)
  {
    vtkNew(vtkPolyLine, newLine);

    // First point if wanted
    if (this->AppendEndPointsToCenterlines)
    {
      if (this->SourceSeedIds)
      {
        // Enter source seed here
        double startPt[3];
        if (this->CapCenterIds)
          input->GetPoint(this->CapCenterIds->GetId(this->SourceSeedIds->GetId(0)), startPt);
        else
          input->GetPoint(this->SourceSeedIds->GetId(0), startPt);

        int newPointId = newPoints->InsertNextPoint(startPt);
        newLine->GetPointIds()->InsertNextId(newPointId);
        newPointData->CopyData(appender->GetInput(fullCenterlineEdges[i][0])->GetPointData(), 0, newPointId);
      }
    }

    for (int j=0; j<fullCenterlineEdges[i].size(); j++)
    {
      currentLine = appender->GetInput(fullCenterlineEdges[i][j]);

      int kStart = 1;
      if (j == 0)
        kStart = 0;
      for (int k=kStart; k<currentLine->GetNumberOfPoints(); k++)
      {
        double pt[3];
        currentLine->GetPoint(k, pt);

        int newPointId = newPoints->InsertNextPoint(pt);
        newLine->GetPointIds()->InsertNextId(newPointId);
        newPointData->CopyData(currentLine->GetPointData(), k, newPointId);
      }
    }

    // End point if wanted
    if (this->AppendEndPointsToCenterlines)
    {
      if (this->TargetSeedIds)
      {
        // Enter target seed here
        int edgeSize = fullCenterlineEdges[i].size();
        int polePointId = voronoiSeeds[fullCenterlineEdges[i][edgeSize-1]][1];
        int targetPointId;
        if (this->CapCenterIds)
          targetPointId = voronoiCapIds->IsId(polePointId);
        else
          targetPointId = this->PoleIds->IsId(polePointId);
        if (targetPointId != -1)
        {
          double endPt[3];
          if (this->CapCenterIds)
            input->GetPoint(this->CapCenterIds->GetId(targetPointId), endPt);
          else
            input->GetPoint(targetPointId, endPt);

          int numPtsInLine = appender->GetInput(fullCenterlineEdges[i][edgeSize-1])->GetNumberOfPoints();

          int newPointId = newPoints->InsertNextPoint(endPt);
          newLine->GetPointIds()->InsertNextId(newPointId);
          newPointData->CopyData(appender->GetInput(fullCenterlineEdges[i][edgeSize-1])->GetPointData(), numPtsInLine-1, newPointId);
        }
      }
    }

    newCells->InsertNextCell(newLine);
  }
  newPointData->Squeeze();

  vtkNew(vtkPolyData, finalLinesPd);
  finalLinesPd->SetPoints(newPoints);
  finalLinesPd->SetLines(newCells);
  finalLinesPd->GetPointData()->PassData(newPointData);

  output->ShallowCopy(finalLinesPd);

  if (this->CenterlineResampling)
  {
    this->ResampleCenterlines();
  }
  //this->ReverseCenterlines();
//
  return SV_OK;
}

// ----------------------
// FindVoronoiSeeds
// ----------------------
int vtkSVCenterlines::FindVoronoiSeeds(vtkUnstructuredGrid *delaunay, vtkIdList *boundaryBaricenterIds, vtkDataArray *normals, vtkIdList *seedIds)
{
  vtkIdType i, j;
  vtkIdList *pointCells;
  vtkIdType baricenterId;
  double baricenter[3], normal[3];
  double maxRadius, secondMaxRadius;
  vtkTetra* tetra;
  double p0[3], p1[3], p2[3], p3[3];
  double circumcenter[3], circumradius, tetraRadius;
  double referenceVector[3];
  double pole[3], poleVector[3], secondPole[3], secondPoleVector[3];
  pole[0] = pole[1] = pole[2] = 0.0;
  vtkIdType maxRadiusCellId, secondMaxRadiusCellId;

  pointCells = vtkIdList::New();

  for (i=0; i<boundaryBaricenterIds->GetNumberOfIds(); i++)
    {
    baricenterId = boundaryBaricenterIds->GetId(i);
    delaunay->GetPoint(baricenterId,baricenter);
    normals->GetTuple(baricenterId,normal);
    pointCells->Initialize();
    delaunay->GetPointCells(baricenterId,pointCells);
    maxRadius = 0.0;
    maxRadiusCellId = -1;
    secondMaxRadiusCellId = -1;

    if (pointCells->GetNumberOfIds() == 0)
    {
      vtkErrorMacro("Cap point not attached to cell in delaunay");
      return SV_ERROR;
    }
    for (j=0; j<pointCells->GetNumberOfIds(); j++)
      {
      tetra = vtkTetra::SafeDownCast(delaunay->GetCell(pointCells->GetId(j)));
      tetra->GetPoints()->GetPoint(0,p0);
      tetra->GetPoints()->GetPoint(1,p1);
      tetra->GetPoints()->GetPoint(2,p2);
      tetra->GetPoints()->GetPoint(3,p3);

      circumradius = vtkTetra::Circumsphere(p0,p1,p2,p3,circumcenter);
      tetraRadius = sqrt(circumradius);

      if (tetraRadius - maxRadius > VTK_VMTK_DOUBLE_TOL)
        {
        maxRadius = tetraRadius;
        maxRadiusCellId = pointCells->GetId(j);
        pole[0] = circumcenter[0];
        pole[1] = circumcenter[1];
        pole[2] = circumcenter[2];
        }
      }

    poleVector[0] = pole[0] - baricenter[0];
    poleVector[1] = pole[1] - baricenter[1];
    poleVector[2] = pole[2] - baricenter[2];

    secondMaxRadius = 0.0;

    for (j=0; j<pointCells->GetNumberOfIds(); j++)
      {
      tetra = vtkTetra::SafeDownCast(delaunay->GetCell(pointCells->GetId(j)));
      tetra->GetPoints()->GetPoint(0,p0);
      tetra->GetPoints()->GetPoint(1,p1);
      tetra->GetPoints()->GetPoint(2,p2);
      tetra->GetPoints()->GetPoint(3,p3);

      circumradius = vtkTetra::Circumsphere(p0,p1,p2,p3,circumcenter);
      tetraRadius = sqrt(circumradius);

      referenceVector[0] = circumcenter[0] - baricenter[0];
      referenceVector[1] = circumcenter[1] - baricenter[1];
      referenceVector[2] = circumcenter[2] - baricenter[2];

      if ((tetraRadius - secondMaxRadius > VTK_VMTK_DOUBLE_TOL) && (vtkMath::Dot(poleVector,referenceVector) < VTK_VMTK_DOUBLE_TOL))
        {
        secondMaxRadius = tetraRadius;
        secondMaxRadiusCellId = pointCells->GetId(j);
        secondPole[0] = circumcenter[0];
        secondPole[1] = circumcenter[1];
        secondPole[2] = circumcenter[2];
        }
      }

    secondPoleVector[0] = secondPole[0] - baricenter[0];
    secondPoleVector[1] = secondPole[1] - baricenter[1];
    secondPoleVector[2] = secondPole[2] - baricenter[2];

    if (vtkMath::Dot(poleVector,normal) < VTK_VMTK_DOUBLE_TOL)
      {
      seedIds->InsertNextId(maxRadiusCellId);
      }
    else
      {
      seedIds->InsertNextId(secondMaxRadiusCellId);
      }
    }

  pointCells->Delete();

  return SV_OK;
}

// ----------------------
// AppendEndPoints
// ----------------------
void vtkSVCenterlines::AppendEndPoints(vtkPoints* endPointPairs)
{
  vtkIdType endPointId1, endPointId2;
  vtkPolyData* output = this->GetOutput();
  vtkPolyData* completeCenterlines = vtkPolyData::New();
  vtkPoints* completeCenterlinesPoints = vtkPoints::New();
  vtkCellArray* completeCenterlinesCellArray = vtkCellArray::New();
  vtkDoubleArray* completeCenterlinesRadiusArray = vtkDoubleArray::New();
  completeCenterlinesRadiusArray->SetName(this->RadiusArrayName);
  vtkIdList* completeCell = vtkIdList::New();

  vtkDoubleArray* centerlinesRadiusArray = vtkDoubleArray::SafeDownCast(output->GetPointData()->GetArray(this->RadiusArrayName));

  completeCenterlinesPoints->DeepCopy(output->GetPoints());
  completeCenterlinesRadiusArray->DeepCopy(centerlinesRadiusArray);

  for (int k=0; k<output->GetNumberOfCells(); k++)
    {
    vtkCell* cell = output->GetCell(k);

    endPointId1 = completeCenterlinesPoints->InsertNextPoint(endPointPairs->GetPoint(2*k));
    endPointId2 = completeCenterlinesPoints->InsertNextPoint(endPointPairs->GetPoint(2*k+1));

    completeCell->Initialize();
    completeCell->SetNumberOfIds(cell->GetNumberOfPoints()+2);

    completeCell->SetId(0,endPointId1);

    for (int i=0; i<cell->GetNumberOfPoints(); i++)
      {
      completeCell->SetId(i+1,cell->GetPointId(i));
      }
    completeCell->SetId(cell->GetNumberOfPoints()+1,endPointId2);

    completeCenterlinesCellArray->InsertNextCell(completeCell);

    completeCenterlinesRadiusArray->InsertNextValue(centerlinesRadiusArray->GetValue(cell->GetPointId(0)));
    completeCenterlinesRadiusArray->InsertNextValue(centerlinesRadiusArray->GetValue(cell->GetPointId(cell->GetNumberOfPoints()-1)));
    }

  completeCenterlines->SetPoints(completeCenterlinesPoints);
  completeCenterlines->SetLines(completeCenterlinesCellArray);
  completeCenterlines->GetPointData()->AddArray(completeCenterlinesRadiusArray);

  output->ShallowCopy(completeCenterlines);

  completeCell->Delete();
  completeCenterlines->Delete();
  completeCenterlinesPoints->Delete();
  completeCenterlinesCellArray->Delete();
  completeCenterlinesRadiusArray->Delete();
}

// ----------------------
// ResampleCenterlines
// ----------------------
void vtkSVCenterlines::ResampleCenterlines()
{
  vtkPolyData* output = this->GetOutput();
  vtkPolyData* resampledCenterlines = vtkPolyData::New();
  vtkPoints* resampledCenterlinesPoints = vtkPoints::New();
  vtkCellArray* resampledCenterlinesCellArray = vtkCellArray::New();
  vtkDoubleArray* resampledCenterlinesRadiusArray = vtkDoubleArray::New();
  resampledCenterlinesRadiusArray->SetName(this->RadiusArrayName);
  vtkIdList* resampledCell = vtkIdList::New();

  vtkDoubleArray* centerlinesRadiusArray = vtkDoubleArray::SafeDownCast(output->GetPointData()->GetArray(this->RadiusArrayName));

  for (int k=0; k<output->GetNumberOfCells(); k++)
    {
    vtkCell* cell = output->GetCell(k);

    resampledCell->Initialize();

    vtkIdType id = resampledCenterlinesPoints->InsertNextPoint(cell->GetPoints()->GetPoint(0));
    resampledCell->InsertNextId(id);
    resampledCenterlinesRadiusArray->InsertNextValue(centerlinesRadiusArray->GetValue(cell->GetPointId(0)));

    double point0[3], point1[3], point[3];
    double abscissa, lineAbscissa, lineLength, stepAbscissa;

    abscissa = 0.0;
    lineAbscissa = 0.0;
    lineLength = 0.0;
    stepAbscissa = 0.0;

    for (int i=0; i<cell->GetNumberOfPoints()-1; i++)
      {
      cell->GetPoints()->GetPoint(i,point0);
      cell->GetPoints()->GetPoint(i+1,point1);

      double scalar0 = centerlinesRadiusArray->GetValue(cell->GetPointId(i));
      double scalar1 = centerlinesRadiusArray->GetValue(cell->GetPointId(i+1));

      double length = sqrt(vtkMath::Distance2BetweenPoints(point0,point1));

      if (length < this->ResamplingStepLength - stepAbscissa)
        {
        stepAbscissa = stepAbscissa + length;
        continue;
        }

      double pcoord = 0.0;
      double pcoordStep = this->ResamplingStepLength / length;
      while (pcoord < 1.0)
        {
        point[0] = point0[0] + (point1[0] - point0[0]) * pcoord;
        point[1] = point0[1] + (point1[1] - point0[1]) * pcoord;
        point[2] = point0[2] + (point1[2] - point0[2]) * pcoord;

        double scalar = scalar0 + (scalar1 - scalar0) * pcoord;

        vtkIdType id = resampledCenterlinesPoints->InsertNextPoint(point);
        resampledCell->InsertNextId(id);
        resampledCenterlinesRadiusArray->InsertNextValue(scalar);

        if (pcoord + pcoordStep > 1.0)
          {
          break;
          }
        pcoord = pcoord + pcoordStep;
        }
      stepAbscissa = (1.0 - pcoord) * length;
      }

    id = resampledCenterlinesPoints->InsertNextPoint(cell->GetPoints()->GetPoint(cell->GetNumberOfPoints()-1));
    resampledCell->InsertNextId(id);
    resampledCenterlinesRadiusArray->InsertNextValue(centerlinesRadiusArray->GetValue(cell->GetPointId(cell->GetNumberOfPoints()-1)));

    resampledCenterlinesCellArray->InsertNextCell(resampledCell);
    }
  resampledCenterlines->SetPoints(resampledCenterlinesPoints);
  resampledCenterlines->SetLines(resampledCenterlinesCellArray);
  resampledCenterlines->GetPointData()->AddArray(resampledCenterlinesRadiusArray);

  output->ShallowCopy(resampledCenterlines);

  resampledCenterlines->Delete();
  resampledCenterlinesPoints->Delete();
  resampledCenterlinesCellArray->Delete();
  resampledCenterlinesRadiusArray->Delete();
  resampledCell->Delete();
}

// ----------------------
// ReserveCenterlines
// ----------------------
void vtkSVCenterlines::ReverseCenterlines()
{
  vtkPolyData* output = this->GetOutput();

  vtkCellArray* reversedCenterlinesCellArray = vtkCellArray::New();
  vtkIdList* reversedCell = vtkIdList::New();

  for (int k=0; k<output->GetNumberOfCells(); k++)
    {
    vtkCell* cell = output->GetCell(k);

    reversedCell->Initialize();

    vtkIdType numberOfCellPoints = cell->GetNumberOfPoints();

    for (int i=0; i<numberOfCellPoints; i++)
      {
      vtkIdType id = cell->GetPointId(numberOfCellPoints-1-i);
      reversedCell->InsertNextId(id);
      }
    reversedCenterlinesCellArray->InsertNextCell(reversedCell);
    }

  output->SetLines(reversedCenterlinesCellArray);

  reversedCell->Delete();
  reversedCenterlinesCellArray->Delete();
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVCenterlines::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}

// ----------------------
// RecursiveGetPolylines
// ----------------------
int vtkSVCenterlines::RecursiveGetPolylines(vtkPolyData *pd,
                                            std::vector<std::vector<int> > connectedEdgePts,
                                            int startVertex, std::vector<int> &pointUsed,
                                            std::vector<std::vector<int> > &allEdges,
                                            std::vector<int> &thisEdge)
{
	int i, j, index, testIndex0, testIndex1, firstVertex, prevVertex, secondVertex, countTotal;
	double tempDouble[3], tempX[3], tempY[3], tempZ[3], tempXPre[3];
  vtkNew(vtkIdList, pointCellIds);
  vtkNew(vtkIdList, edgePointIds);
	int stopCriteria;

  int numPts = pd->GetNumberOfPoints();

	stopCriteria = 0;
  firstVertex = startVertex;

	while (!stopCriteria)
	{
		prevVertex = -1;
		secondVertex = -1;

		if (connectedEdgePts[firstVertex].size() == 1)
		{
      // end point, only connected to one other point
			index = connectedEdgePts[firstVertex][0];
			if (pointUsed[index] == 0)
			{
				secondVertex = index;
			}
			else
			{
				prevVertex = index;
			}
		}
		else if (connectedEdgePts[firstVertex].size() == 2)
		{
      // continuing point, connected to two other points
      testIndex0 = connectedEdgePts[firstVertex][0];
      testIndex1 = connectedEdgePts[firstVertex][1];

      // make sure both points are not alreay used
      if (pointUsed[testIndex0]  && pointUsed[testIndex1])
      {
        pointUsed[firstVertex] = 1;
        thisEdge.push_back(firstVertex);

        // Have to add the last point
        int found0 = 0, found1 = 0;
        for (int i=0; i<thisEdge.size(); i++)
        {
          if (thisEdge[i] == testIndex0)
            found0 = 1;
          if (thisEdge[i] == testIndex1)
            found1 = 1;
        }
        if (!found0)
          thisEdge.push_back(testIndex0);
        else if(!found1)
          thisEdge.push_back(testIndex1);
        else if (found0 && testIndex0 == thisEdge[0])
          thisEdge.push_back(testIndex0);
        else if (found1 && testIndex1 == thisEdge[0])
          thisEdge.push_back(testIndex1);
        else
        {
          vtkWarningMacro("Both connected edge points already used");
          return SV_ERROR;
        }

        allEdges.push_back(thisEdge);
        return SV_OK;
      }

			for (i = 0; i < connectedEdgePts[firstVertex].size(); i++)
			{
				index = connectedEdgePts[firstVertex][i];
				if (pointUsed[index] == 0)
				{
					secondVertex = index;
				}
				else
				{
					prevVertex = index;
				}
			}
		}
    else if (connectedEdgePts[firstVertex].size() > 2)
    {
      // branching point, connected to more than two points
      pointUsed[firstVertex] = 1;
      thisEdge.push_back(firstVertex);
      allEdges.push_back(thisEdge);

      // Loop through each connected point, and call function recursively
			for (i = 0; i < connectedEdgePts[firstVertex].size(); i++)
			{
				index = connectedEdgePts[firstVertex][i];
        if (pointUsed[index] == 0)
        {
          std::vector<int> newEdge;
          newEdge.push_back(firstVertex);
          this->RecursiveGetPolylines(pd, connectedEdgePts, index, pointUsed, allEdges, newEdge);
        }
        else if (connectedEdgePts[index].size() > 2)
        {
          // We might have a very small loop here
          std::vector<int> potEdge;
          potEdge.push_back(firstVertex);
          potEdge.push_back(index);
          int alreadyEdge = 0;
          for (int j=0; j<allEdges.size(); j++)
          {
            // TODO: Figure out if actually right
            //if ((allEdges[j][0] == potEdge[0] && allEdges[j][allEdges[j].size()-1] == potEdge[1]) ||
            //    (allEdges[j][0] == potEdge[1] && allEdges[j][allEdges[j].size()-1] == potEdge[0]))
            if ((allEdges[j][0] == potEdge[0] && allEdges[j][1] == potEdge[1]) ||
                (allEdges[j][0] == potEdge[1] && allEdges[j][1] == potEdge[0]))
            {
              alreadyEdge = 1;
            }
          }
          if (!alreadyEdge)
          {
            // Not already part of edges, add
            allEdges.push_back(potEdge);
          }
        }
      }

      return SV_OK;
    }
    else
    {
      vtkErrorMacro("Somehow point is connected to nothing");
      return SV_ERROR;
    }

    // Mark point and move on
		pointUsed[firstVertex] = 1;
    thisEdge.push_back(firstVertex);

		if (connectedEdgePts[firstVertex].size() == 1)
		{

			index = connectedEdgePts[firstVertex][0];
			if (pointUsed[index] == 1)
			{
        // We are done with this connected edge
				stopCriteria = 1;
        allEdges.push_back(thisEdge);
        return SV_OK;
			}

		}

		firstVertex = secondVertex;
	}

  return SV_OK;
}

// ----------------------
// RecursiveGetFullCenterlines
// ----------------------
int vtkSVCenterlines::RecursiveGetFullCenterlines(std::vector<std::vector<int> > allEdges,
                                                  std::vector<std::vector<int> > &fullCenterlineEdges,
                                                  int thisEdge, int front, int back)
{
  // Set still recursing flag
  int stillRecursing = 0;
  for (int i=0; i<allEdges.size(); i++)
  {
    // Dont do the current edge
    if (i == thisEdge)
      continue;

    int edgeSize = allEdges[i].size();
    int edgeId0 = allEdges[i][0];
    int edgeIdN = allEdges[i][edgeSize-1];

    std::vector<std::vector<int> > newCenterlineEdges;
    // Get new edge from front
    if (edgeId0 == back)
    {
      int newFront = edgeId0;
      int newBack  = edgeIdN;

      this->RecursiveGetFullCenterlines(allEdges, newCenterlineEdges, i, newFront, newBack);

    }

    // Get new edge from back
    if (edgeIdN == back)
    {
      int newFront = edgeIdN;
      int newBack  = edgeId0;

      this->RecursiveGetFullCenterlines(allEdges, newCenterlineEdges, i, newFront, newBack);
    }

    // Copy data structure of all edge points to full centerline edges structure
    if (newCenterlineEdges.size() > 0)
    {
      for (int j=0; j<newCenterlineEdges.size(); j++)
      {
        std::vector<int> newEdge;
        newEdge.push_back(thisEdge);
        for (int k=0; k<newCenterlineEdges[j].size(); k++)
          newEdge.push_back(newCenterlineEdges[j][k]);

        fullCenterlineEdges.push_back(newEdge);
      }
      stillRecursing = 1;
    }
  }

  if (!stillRecursing)
  {
    std::vector<int> newEdge;
    newEdge.push_back(thisEdge);
    fullCenterlineEdges.push_back(newEdge);
  }

  return SV_OK;
}

// ----------------------
// LoopRemoveMarkedCells
// ----------------------
int vtkSVCenterlines::LoopRemoveMarkedCells(vtkPolyData *pd,
                                            std::vector<std::vector<int> > allEdges,
                                            std::vector<int> needToDelete,
                                            std::vector<int> &isDeleted,
                                            vtkIdList *allEndIds,
                                            std::vector<int> &nodeCount)
{

  // We loop as long as is necessary to remove duplicate graph edges
  int done = 0;
  while (!done)
  {
    // Remove cells that have been marked
    this->RemoveMarkedCells(pd, allEdges, needToDelete, isDeleted, allEndIds, nodeCount);

    // Get next iteration of cells to delete
    std::vector<int> newNeedToDelete(allEdges.size(), 0);
    for (int i=0; i<needToDelete.size(); i++)
    {
      // Look to see if points on deleted cells are in large graph or isolated and need to be deleted
      if (needToDelete[i] == 1)
      {
        int edgeSize = allEdges[i].size();
        vtkDebugMacro("EDGE WITH END POINTS " << allEdges[i][0] << " AND " << allEdges[i][edgeSize-1] << " NEEDS TO BE DELETED");

        int nodeId0 = allEndIds->IsId(allEdges[i][0]);
        int nodeIdN = allEndIds->IsId(allEdges[i][edgeSize-1]);

        // This cell is isolated, must delete
        if (nodeCount[nodeId0] == 1 || nodeCount[nodeIdN] == 1)
        {
          for (int j=0; j<allEdges.size(); j++)
          {
            if (isDeleted[j])
            {
              continue;
            }

            int delEdgeSize = allEdges[j].size();

            // Get edge that corresponds to where we are in iteration
            if (nodeCount[nodeId0] == 1)
            {
              if (allEdges[j][0] == allEdges[i][0] ||
                  allEdges[j][delEdgeSize-1] == allEdges[i][0])
              {
                vtkDebugMacro("THIS ONE NOW ONLY HAS ONE AND NEEDS TO BE MARKED " << allEdges[i][0]);
                newNeedToDelete[j] = 1;
              }
            }
            if (nodeCount[nodeIdN] == 1)
            {
              if (allEdges[j][0] == allEdges[i][edgeSize-1] ||
                  allEdges[j][delEdgeSize-1] == allEdges[i][edgeSize-1])
              {
                vtkDebugMacro("THIS ONE NOW ONLY HAS ONE AND NEEDS TO BE MARKED " << allEdges[i][edgeSize-1]);
                newNeedToDelete[j] = 1;
              }
            }
          }
        }
      }
    }

    // Check if done
    done = 1;
    for (int i=0; i<needToDelete.size(); i++)
    {
      if (newNeedToDelete[i])
        done = 0;
      needToDelete[i] = newNeedToDelete[i];
    }
  }

  // ------------------------------------------------------------------------
  return SV_OK;
}

// ----------------------
// RemoveMarkedCells
// ----------------------
int vtkSVCenterlines::RemoveMarkedCells(vtkPolyData *pd,
                                        std::vector<std::vector<int> > allEdges,
                                        std::vector<int> needToDelete,
                                        std::vector<int> &isDeleted,
                                        vtkIdList *allEndIds,
                                        std::vector<int> &nodeCount)
{
  int edgeSize, pointId0, pointId1, nodeId0, nodeIdN;
  vtkNew(vtkIdList, edgePointIds);
  vtkNew(vtkIdList, pointsEdgeId);
  for (int i=0; i<needToDelete.size(); i++)
  {
    // Dont need to look if already deleted
    if (isDeleted[i])
    {
      continue;
    }

    // Delete this one
    if (needToDelete[i] == 1)
    {
      // Loop through and delete each cell in connected edge
      edgeSize = allEdges[i].size();
      for (int j=0; j<edgeSize-1; j++)
      {
        pointId0 = allEdges[i][j];
        pointId1 = allEdges[i][j+1];
        edgePointIds->Reset();
        edgePointIds->InsertNextId(pointId0);
        edgePointIds->InsertNextId(pointId1);

        pd->GetCellNeighbors(-1, edgePointIds, pointsEdgeId);
        if (pointsEdgeId->GetNumberOfIds() == 1)
        {
          vtkDebugMacro("CELL " << pointsEdgeId->GetId(0) << " HAS BEEN MARKED IN PD TO BE DELETED");
          pd->DeleteCell(pointsEdgeId->GetId(0));
        }
      }

      // Loops arent added to node coutn, so dont remove here
      if (allEdges[i][0] != allEdges[i][edgeSize - 1])
      {
        nodeId0 = allEndIds->IsId(allEdges[i][0]);
        nodeIdN = allEndIds->IsId(allEdges[i][edgeSize-1]);
        nodeCount[nodeId0]--;
        nodeCount[nodeIdN]--;
        vtkDebugMacro("NODE COUNT OF " << allEdges[i][0] << " IS NOW " << nodeCount[nodeId0]);
        vtkDebugMacro("NODE COUNT OF " << allEdges[i][edgeSize-1] << " IS NOW " << nodeCount[nodeIdN]);
      }
      isDeleted[i] = 1;
    }
  }

  return SV_OK;
}

// ----------------------
// GetLinesEndPoints
// ----------------------
int vtkSVCenterlines::GetLinesEndPoints(vtkPolyData *pd,
                                        vtkIdList *endPointIds,
                                        vtkPoints *endPoints,
                                        std::vector<std::vector<int> > &connectedEdgePts,
                                        int &firstVertex)
{
	firstVertex = -1;
  connectedEdgePts.clear();
  connectedEdgePts.resize(pd->GetNumberOfPoints());

  endPointIds->Reset();
  endPoints->Reset();

  vtkIdType npts, *pts;
  int numEndPoints = 0;
  double maxRadiusValue = -1.0;
  vtkNew(vtkIdList, pointCellIds);

  for (int i=0; i<pd->GetNumberOfPoints(); i++)
  {
    pd->GetPointCells(i, pointCellIds);
    if (pointCellIds->GetNumberOfIds() == 1)
    {
      numEndPoints++;
      double radiusValue = pd->GetPointData()->
        GetArray(this->RadiusArrayName)->GetTuple1(i);

      if (radiusValue > maxRadiusValue)
      {
        maxRadiusValue = radiusValue;
        firstVertex = i;
      }

      endPointIds->InsertNextId(i);
      endPoints->InsertNextPoint(pd->GetPoint(i));
    }

    std::vector<int> connectedPts;
    for (int j=0; j<pointCellIds->GetNumberOfIds(); j++)
    {
      pd->GetCellPoints(pointCellIds->GetId(j), npts, pts);

      for (int k=0; k<npts; k++)
      {
        if (pts[k] != i)
          connectedPts.push_back(pts[k]);
      }
    }
    connectedEdgePts[i] = connectedPts;
  }

  return SV_OK;
}
