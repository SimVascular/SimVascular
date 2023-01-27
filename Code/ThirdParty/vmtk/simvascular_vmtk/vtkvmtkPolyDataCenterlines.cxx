/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkvmtkPolyDataCenterlines.cxx,v $
Language:  C++
Date:      $Date: 2006/04/06 16:46:43 $
Version:   $Revision: 1.6 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENCE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
//SimVascular Changes: Changes to update code to VTK pipeline 6.0
//Lines 197, 210, 219, 237, 249, 259, 310: SetInput -> SetInputData

#include "vtkvmtkPolyDataCenterlines.h"
#include "vtkvmtkConstants.h"
#include "vtkPolyDataNormals.h"
#include "vtkDelaunay3D.h"
#include "vtkvmtkInternalTetrahedraExtractor.h"
#include "vtkvmtkVoronoiDiagram3D.h"
#include "vtkvmtkSimplifyVoronoiDiagram.h"
#include "vtkArrayCalculator.h"
#include "vtkvmtkNonManifoldFastMarching.h"
#include "vtkvmtkSteepestDescentLineTracer.h"
#include "vtkMath.h"
#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"
#include "vtkTetra.h"
#include "vtkPointData.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"


vtkStandardNewMacro(vtkvmtkPolyDataCenterlines);

vtkCxxSetObjectMacro(vtkvmtkPolyDataCenterlines,SourceSeedIds,vtkIdList);
vtkCxxSetObjectMacro(vtkvmtkPolyDataCenterlines,TargetSeedIds,vtkIdList);
vtkCxxSetObjectMacro(vtkvmtkPolyDataCenterlines,CapCenterIds,vtkIdList);

vtkvmtkPolyDataCenterlines::vtkvmtkPolyDataCenterlines()
{
  this->SourceSeedIds = nullptr;
  this->TargetSeedIds = nullptr;
  this->CapCenterIds = nullptr;

  this->RadiusArrayName = nullptr;
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

  this->ResamplingStepLength = 1.0;

  this->GenerateDelaunayTessellation = 1;

  this->DelaunayTessellation = nullptr;
  this->DelaunayTolerance = 1E-3;

  this->VoronoiDiagram = vtkPolyData::New();
  this->PoleIds = vtkIdList::New();
}

vtkvmtkPolyDataCenterlines::~vtkvmtkPolyDataCenterlines()
{
  if (this->SourceSeedIds)
    {
    this->SourceSeedIds->Delete();
    this->SourceSeedIds = nullptr;
    }

  if (this->TargetSeedIds)
    {
    this->TargetSeedIds->Delete();
    this->TargetSeedIds = nullptr;
    }

  if (this->CapCenterIds)
    {
    this->CapCenterIds->Delete();
    this->CapCenterIds = nullptr;
    }

  if (this->CostFunction)
    {
    delete[] this->CostFunction;
    this->CostFunction = nullptr;
    }

  if (this->CostFunctionArrayName)
    {
    delete[] this->CostFunctionArrayName;
    this->CostFunctionArrayName = nullptr;
    }

  if (this->EikonalSolutionArrayName)
    {
    delete[] this->EikonalSolutionArrayName;
    this->EikonalSolutionArrayName = nullptr;
    }

  if (this->EdgeArrayName)
    {
    delete[] this->EdgeArrayName;
    this->EdgeArrayName = nullptr;
    }

  if (this->EdgePCoordArrayName)
    {
    delete[] this->EdgePCoordArrayName;
    this->EdgePCoordArrayName = nullptr;
    }

  if (this->RadiusArrayName)
    {
    delete[] this->RadiusArrayName;
    this->RadiusArrayName = nullptr;
    }

  if (this->DelaunayTessellation)
    {
    this->DelaunayTessellation->Delete();
    this->DelaunayTessellation = nullptr;
    }

  this->VoronoiDiagram->Delete();
  this->VoronoiDiagram = nullptr;

  this->PoleIds->Delete();
  this->PoleIds = nullptr;
}

int vtkvmtkPolyDataCenterlines::RequestData(
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
    vtkErrorMacro(<< "No SourceSeedIds set.");
    return 1;
    }

  if (!this->TargetSeedIds)
    {
    vtkErrorMacro(<< "No TargetSeedIds set.");
    return 1;
    }

  if (!this->RadiusArrayName)
    {
    vtkErrorMacro(<< "No RadiusArrayName set.");
    return 1;
    }

  if (!this->GenerateDelaunayTessellation && !this->DelaunayTessellation)
    {
    vtkErrorMacro(<< "GenerateDelaunayTessellation is off but a DelaunayTessellation has not been set.");
    return 1;
    }

  vtkPolyDataNormals* surfaceNormals = vtkPolyDataNormals::New();
//CHANGE: SetInput -> SetInputData
  surfaceNormals->SetInputData(input);
  surfaceNormals->SplittingOff();
  surfaceNormals->AutoOrientNormalsOn();
  surfaceNormals->SetFlipNormals(this->FlipNormals);
  surfaceNormals->ComputePointNormalsOn();
  surfaceNormals->ConsistencyOn();
  surfaceNormals->Update();

  if (this->GenerateDelaunayTessellation)
    {
    vtkDelaunay3D* delaunayTessellator = vtkDelaunay3D::New();
    delaunayTessellator->CreateDefaultLocator();
//CHANGE: SetInput -> SetInputData
    delaunayTessellator->SetInputData(surfaceNormals->GetOutput());
    delaunayTessellator->SetTolerance(this->DelaunayTolerance);
    delaunayTessellator->Update();

    vtkUnstructuredGrid* delaunay = delaunayTessellator->GetOutput();
    delaunay->GetPointData()->AddArray(surfaceNormals->GetOutput()->GetPointData()->GetNormals());

    vtkvmtkInternalTetrahedraExtractor* internalTetrahedraExtractor = vtkvmtkInternalTetrahedraExtractor::New();
//CHANGE: SetInput -> SetInputData
    internalTetrahedraExtractor->SetInputData(delaunayTessellator->GetOutput());
    internalTetrahedraExtractor->SetOutwardNormalsArrayName(surfaceNormals->GetOutput()->GetPointData()->GetNormals()->GetName());
    if (this->CapCenterIds)
      {
      internalTetrahedraExtractor->UseCapsOn();
      internalTetrahedraExtractor->SetCapCenterIds(this->CapCenterIds);
      }
    internalTetrahedraExtractor->Update();

    this->DelaunayTessellation = internalTetrahedraExtractor->GetOutput();
    this->DelaunayTessellation->Register(this);

    delaunayTessellator->Delete();
    internalTetrahedraExtractor->Delete();
    }

  vtkvmtkVoronoiDiagram3D* voronoiDiagramFilter = vtkvmtkVoronoiDiagram3D::New();
//CHANGE: SetInput -> SetInputData
  voronoiDiagramFilter->SetInputData(this->DelaunayTessellation);
  voronoiDiagramFilter->SetRadiusArrayName(this->RadiusArrayName);
  voronoiDiagramFilter->Update();

  this->PoleIds->DeepCopy(voronoiDiagramFilter->GetPoleIds());

  vtkPolyData* voronoiDiagram = voronoiDiagramFilter->GetOutput();

  if (this->SimplifyVoronoi)
    {
    vtkvmtkSimplifyVoronoiDiagram* voronoiDiagramSimplifier = vtkvmtkSimplifyVoronoiDiagram::New();
//CHANGE: SetInput -> SetInputData
    voronoiDiagramSimplifier->SetInputData(voronoiDiagramFilter->GetOutput());
    voronoiDiagramSimplifier->SetUnremovablePointIds(voronoiDiagramFilter->GetPoleIds());
    voronoiDiagramSimplifier->Update();
    voronoiDiagram = voronoiDiagramSimplifier->GetOutput();
    voronoiDiagram->Register(this);
    voronoiDiagramSimplifier->Delete();
    }

  vtkArrayCalculator* voronoiCostFunctionCalculator = vtkArrayCalculator::New();
//CHANGE: SetInput -> SetInputData
  voronoiCostFunctionCalculator->SetInputData(voronoiDiagram);
  voronoiCostFunctionCalculator->SetAttributeTypeToPointData();
  voronoiCostFunctionCalculator->AddScalarVariable("R",this->RadiusArrayName,0);
  voronoiCostFunctionCalculator->SetFunction(this->CostFunction);
  voronoiCostFunctionCalculator->SetResultArrayName(this->CostFunctionArrayName);
  voronoiCostFunctionCalculator->Update();

  vtkIdList* voronoiSourceSeedIds = vtkIdList::New();
  vtkIdList* voronoiTargetSeedIds = vtkIdList::New();

  vtkIdList* voronoiSeeds = vtkIdList::New();

  int i;
  if (this->CapCenterIds)
    {
    this->FindVoronoiSeeds(this->DelaunayTessellation,this->CapCenterIds,surfaceNormals->GetOutput()->GetPointData()->GetNormals(),voronoiSeeds);
    for (i=0; i<this->SourceSeedIds->GetNumberOfIds(); i++)
      {
      voronoiSourceSeedIds->InsertNextId(voronoiSeeds->GetId(this->SourceSeedIds->GetId(i)));
      }
    for (i=0; i<this->TargetSeedIds->GetNumberOfIds(); i++)
      {
      voronoiTargetSeedIds->InsertNextId(voronoiSeeds->GetId(this->TargetSeedIds->GetId(i)));
      }
    }
  else
    {
    for (i=0; i<this->SourceSeedIds->GetNumberOfIds(); i++)
      {
      voronoiSourceSeedIds->InsertNextId(this->PoleIds->GetId(this->SourceSeedIds->GetId(i)));
      }
    for (i=0; i<this->TargetSeedIds->GetNumberOfIds(); i++)
      {
      voronoiTargetSeedIds->InsertNextId(this->PoleIds->GetId(this->TargetSeedIds->GetId(i)));
      }
    }

  vtkvmtkNonManifoldFastMarching* voronoiFastMarching = vtkvmtkNonManifoldFastMarching::New();
//CHANGE: SetInput -> SetInputData
  voronoiFastMarching->SetInputData(vtkPolyData::SafeDownCast(voronoiCostFunctionCalculator->GetOutput()));
  voronoiFastMarching->SetCostFunctionArrayName(this->CostFunctionArrayName);
  voronoiFastMarching->SetSolutionArrayName(this->EikonalSolutionArrayName);
  voronoiFastMarching->SeedsBoundaryConditionsOn();
  voronoiFastMarching->SetSeeds(voronoiSourceSeedIds);
  voronoiFastMarching->Update();

  this->VoronoiDiagram->ShallowCopy(voronoiFastMarching->GetOutput());
  this->VoronoiDiagram->BuildLinks();

  vtkvmtkSteepestDescentLineTracer* centerlineBacktracing = vtkvmtkSteepestDescentLineTracer::New();
//CHANGE: SetInput -> SetInputData
  centerlineBacktracing->SetInputData(voronoiFastMarching->GetOutput());
  centerlineBacktracing->SetDataArrayName(this->RadiusArrayName);
  centerlineBacktracing->SetDescentArrayName(this->EikonalSolutionArrayName);
  centerlineBacktracing->SetEdgeArrayName(this->EdgeArrayName);
  centerlineBacktracing->SetEdgePCoordArrayName(this->EdgePCoordArrayName);
  centerlineBacktracing->SetSeeds(voronoiTargetSeedIds);
  centerlineBacktracing->MergePathsOff();
  centerlineBacktracing->StopOnTargetsOn();
  centerlineBacktracing->SetTargets(voronoiSourceSeedIds);
  centerlineBacktracing->Update();

  output->ShallowCopy(centerlineBacktracing->GetOutput());

  vtkIdList* hitTargets = centerlineBacktracing->GetHitTargets();

  vtkPoints* endPointPairs = vtkPoints::New();

  const vtkIdType numTargetSeedIds = this->TargetSeedIds->GetNumberOfIds();
  const vtkIdType numHitTargets = hitTargets->GetNumberOfIds();
  if(numHitTargets == numTargetSeedIds) {
  if (this->AppendEndPointsToCenterlines)
    {
    for (i=0; i<numTargetSeedIds; i++)
      {
      if (this->CapCenterIds)
        {
        vtkIdType endPointId1 = this->CapCenterIds->GetId(this->TargetSeedIds->GetId(i));
        vtkIdType hitTargetPointId = hitTargets->GetId(i);
        vtkIdType targetId = voronoiSourceSeedIds->IsId(hitTargetPointId);
        vtkIdType endPointId2 = this->CapCenterIds->GetId(this->SourceSeedIds->GetId(targetId));
        endPointPairs->InsertNextPoint(input->GetPoint(endPointId1));
        endPointPairs->InsertNextPoint(input->GetPoint(endPointId2));
        }
      else
        {
        vtkIdType endPointId1 = this->TargetSeedIds->GetId(i);
        vtkIdType hitTargetPointId = hitTargets->GetId(i);
        vtkIdType targetId = voronoiSourceSeedIds->IsId(hitTargetPointId);
        vtkIdType endPointId2 = this->SourceSeedIds->GetId(targetId);
        endPointPairs->InsertNextPoint(input->GetPoint(endPointId1));
        endPointPairs->InsertNextPoint(input->GetPoint(endPointId2));
        }
      }

    this->AppendEndPoints(endPointPairs);
    }
  }

  if (this->CenterlineResampling)
    {
    this->ResampleCenterlines();
    }
  this->ReverseCenterlines();

  surfaceNormals->Delete();
  voronoiDiagramFilter->Delete();
  voronoiCostFunctionCalculator->Delete();
  voronoiSeeds->Delete();
  voronoiSourceSeedIds->Delete();
  voronoiTargetSeedIds->Delete();
  voronoiFastMarching->Delete();
  centerlineBacktracing->Delete();
  endPointPairs->Delete();

  return 1;
}

void vtkvmtkPolyDataCenterlines::FindVoronoiSeeds(vtkUnstructuredGrid *delaunay, vtkIdList *boundaryBaricenterIds, vtkDataArray *normals, vtkIdList *seedIds)
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
}

void vtkvmtkPolyDataCenterlines::AppendEndPoints(vtkPoints* endPointPairs)
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
  completeCenterlines->BuildLinks();

  output->ShallowCopy(completeCenterlines);

  completeCell->Delete();
  completeCenterlines->Delete();
  completeCenterlinesPoints->Delete();
  completeCenterlinesCellArray->Delete();
  completeCenterlinesRadiusArray->Delete();
}

void vtkvmtkPolyDataCenterlines::ResampleCenterlines()
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
  resampledCenterlines->BuildLinks();

  output->ShallowCopy(resampledCenterlines);

  resampledCenterlines->Delete();
  resampledCenterlinesPoints->Delete();
  resampledCenterlinesCellArray->Delete();
  resampledCenterlinesRadiusArray->Delete();
  resampledCell->Delete();
}

void vtkvmtkPolyDataCenterlines::ReverseCenterlines()
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

void vtkvmtkPolyDataCenterlines::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
