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

#include "vtkSVSphericalMapper.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCenterOfMass.h"
#include "vtkCleanPolyData.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkFloatArray.h"
#include "vtkGradientFilter.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointDataToCellData.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyDataNormals.h"
#include "vtkSmartPointer.h"
#include "vtkTransform.h"
#include "vtkTriangle.h"
#include "vtkUnstructuredGrid.h"
#include "vtkXMLPolyDataWriter.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkSVPlacePointsOnS2.h"

#include <iostream>
#include <sstream>
#include <cmath>

//---------------------------------------------------------------------------
//vtkCxxRevisionMacro(vtkSVSphericalMapper, "$Revision: 0.0 $");
// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSphericalMapper);

// ----------------------
// Constructor
// ----------------------
vtkSVSphericalMapper::vtkSVSphericalMapper()
{
  this->Verbose                 = 1;
  this->InitialTimeStep         = 0.01;
  this->TimeStep                = 0.1;
  this->TutteEnergyCriterion    = 0.00001;
  this->HarmonicEnergyCriterion = 0.0000001;
  this->MaxNumIterations        = 1000;
  this->NumBoundaries           = 0;
  this->CGUpdateMethod          = CG_NONE;

  this->BoundaryType            = CLOSED;
  this->BoundaryConstraintType  = 0;
  for (int i=0; i<2; i++)
  {
    this->BoundaryStart[i] = -1;
  }
  this->FirstLoopPts      = NULL;
  this->SecondLoopPts     = NULL;
  this->FirstLoopHelper   = NULL;
  this->SecondLoopHelper  = NULL;
  for (int i=0; i<2; i++)
  {
    this->CubeStart[i] = -1;
  }

  this->InitialPd      = vtkPolyData::New();
  this->EdgeTable      = vtkEdgeTable::New();
  this->EdgeWeights    = vtkFloatArray::New();
  this->PrevDescent    = vtkFloatArray::New();
  this->CurrDescent    = vtkFloatArray::New();
  this->ConjugateDir   = vtkFloatArray::New();
  this->EdgeNeighbors  = vtkIntArray::New(); this->EdgeNeighbors->SetNumberOfComponents(2);
  this->IsBoundary     = vtkIntArray::New();
  this->HarmonicMap[0] = vtkPolyData::New();
  this->HarmonicMap[1] = vtkPolyData::New();
  this->Boundaries     = vtkPolyData::New();
  this->SetObjectXAxis(1.0, 0.0, 0.0);
  this->SetObjectZAxis(0.0, 0.0, 1.0);

  this->IterOutputFilename = NULL;
  this->NumSaveIterations  = 100;
  this->SaveIter           = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSphericalMapper::~vtkSVSphericalMapper()
{
  if (this->InitialPd != NULL)
  {
    InitialPd->Delete();
  }
  if (this->EdgeTable != NULL)
  {
    this->EdgeTable->Delete();
  }
  if (this->EdgeWeights != NULL)
  {
    this->EdgeWeights->Delete();
  }
  if (this->PrevDescent != NULL)
  {
    this->PrevDescent->Delete();
  }
  if (this->CurrDescent != NULL)
  {
    this->CurrDescent->Delete();
  }
  if (this->ConjugateDir != NULL)
  {
    this->ConjugateDir->Delete();
  }
  if (this->EdgeNeighbors != NULL)
  {
    this->EdgeNeighbors->Delete();
  }
  if (this->IsBoundary != NULL)
  {
    this->IsBoundary->Delete();
  }
  for (int i=0; i<2; i++)
  {
    if (this->HarmonicMap[i] != NULL)
    {
      this->HarmonicMap[i]->Delete();
    }
  }
  if (this->Boundaries != NULL)
  {
    this->Boundaries->Delete();
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSphericalMapper::PrintSelf(ostream& os, vtkIndent indent)
{
}

// ----------------------
// RequestData
// ----------------------
int vtkSVSphericalMapper::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  //Copy the input to operate on
  this->InitialPd->DeepCopy(input);

  vtkIdType numPolys = this->InitialPd->GetNumberOfPolys();
  //Check the input to make sure it is there
  if (numPolys < 1)
  {
    vtkDebugMacro("No input!");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  //Check the input to make sure it is manifold and a triangulated surface
  if (vtkSVGeneralUtils::CheckSurface(this->InitialPd) != SV_OK)
  {
    vtkErrorMacro("Error when checking input surface");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  //Get the number of Polys for scalar  allocation
  numPolys = this->InitialPd->GetNumberOfPolys();
  vtkIdType numPts = this->InitialPd->GetNumberOfPoints();

  //Create the edge table for the input surface
  vtkSVGeneralUtils::CreateEdgeTable(this->InitialPd, this->EdgeTable, this->EdgeWeights,
                                     this->EdgeNeighbors, this->IsBoundary);

  if (this->PerformMapping() != SV_OK)
  {
    vtkErrorMacro("Error while doing CG Solve");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  output->DeepCopy(this->HarmonicMap[HARMONIC]);
  output->GetPointData()->PassData(input->GetPointData());
  output->GetCellData()->PassData(input->GetCellData());
  if (vtkSVGeneralUtils::CheckArrayExists(output, 0 ,"Normals") == 1)
  {
    output->GetPointData()->RemoveArray("Normals");
  }
  if (vtkSVGeneralUtils::CheckArrayExists(output, 1,"cellNormals") == 1)
  {
    output->GetCellData()->RemoveArray("cellNormals");
  }
  return SV_OK;
}

// ----------------------
// PerformMapping
// ----------------------
int vtkSVSphericalMapper::PerformMapping()
{
  vtkDebugMacro("CG Update Method: " << this->CGUpdateMethod);

  //Compute Gauss Map (i.e. get normals off of mesh)
  //TODO Check normals exists!
  //this->ConvertFieldToPolyData(this->InitialPd, "Normals", this->HarmonicMap[0]);
  vtkNew(vtkSVPlacePointsOnS2, initialSpot);
  initialSpot->SetInputData(this->InitialPd);
  initialSpot->SetUseCustomAxisAlign(1);
  initialSpot->SetXAxis(this->ObjectXAxis);
  initialSpot->SetZAxis(this->ObjectZAxis);
  initialSpot->Update();
  this->HarmonicMap[0]->DeepCopy(initialSpot->GetOutput());

  if(this->SetBoundaries() != SV_OK)
  {
    vtkErrorMacro("Error when setting boundaries");
    return SV_ERROR;
  }

  if (this->InitiateCGArrays() != SV_OK)
  {
    vtkErrorMacro("Error when initiating arrays");
    return SV_ERROR;
  }

  //Run the Tutte Energy Step
  if (this->SphericalTutteMapping() != SV_OK)
  {
    vtkErrorMacro("Error when computing the tutte map");
    return SV_ERROR;
  }
  //Compute Initial Tutte energy
  this->HarmonicMap[1]->DeepCopy(this->HarmonicMap[0]);

  //Run the Spherical Conformal Mapping Step
  if (this->SphericalConformalMapper() != SV_OK)
  {
    vtkErrorMacro("Error when computing the conformal map");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// InitiateCGArrays
// ----------------------
int vtkSVSphericalMapper::InitiateCGArrays()
{
  int numPts = this->InitialPd->GetNumberOfPoints();

  this->PrevDescent->SetNumberOfComponents(3);
  this->PrevDescent->Allocate(numPts, 10000);
  this->PrevDescent->SetNumberOfTuples(numPts);

  this->CurrDescent->SetNumberOfComponents(3);
  this->CurrDescent->Allocate(numPts, 10000);
  this->CurrDescent->SetNumberOfTuples(numPts);

  this->ConjugateDir->SetNumberOfComponents(3);
  this->ConjugateDir->Allocate(numPts, 10000);
  this->ConjugateDir->SetNumberOfTuples(numPts);

  return SV_OK;
}

// ----------------------
// SetBoundaries
// ----------------------
int vtkSVSphericalMapper::SetBoundaries()
{
  if (this->FindBoundaries() != SV_OK)
  {
    vtkErrorMacro("Could not find boundaries");
    return SV_ERROR;
  }

  if (this->NumBoundaries != 0)
  {
    //TODO: Clean up correctly on errors!!!
    vtkPolyData **boundaryLoops = new vtkPolyData*[this->NumBoundaries];
    for (int i=0; i<this->NumBoundaries; i++)
    {
      boundaryLoops[i] = vtkPolyData::New();
      boundaryLoops[i]->SetPoints(this->Boundaries->GetPoints());
      boundaryLoops[i]->GetPointData()->PassData(this->Boundaries->GetPointData());
    }
    vtkIntArray *pointIds =
      vtkIntArray::SafeDownCast(this->Boundaries->GetPointData()->GetArray("PointIds"));

    int numBoundStarts = 0;
    int boundaryStart[2];
    for (int i=0; i<2; i++)
    {
      if (this->BoundaryStart[i] != -1)
      {
        numBoundStarts++;
        boundaryStart[i] = pointIds->LookupValue(this->BoundaryStart[i]);
      }
    }
    if (numBoundStarts != this->NumBoundaries)
    {
      vtkErrorMacro("The number of boundaries does not match with the information you have provided for the boundary");
      for (int j=0; j<this->NumBoundaries; j++)
      {
        boundaryLoops[j]->Delete();
      }
      delete [] boundaryLoops;
      return SV_ERROR;
    }
    if (vtkSVGeneralUtils::SeparateLoops(this->Boundaries, boundaryLoops, numBoundStarts,
                                         this->ObjectXAxis, this->ObjectZAxis, boundaryStart) != SV_OK)
    {
      vtkErrorMacro("No separate");
      for (int j=0; j<this->NumBoundaries; j++)
      {
        boundaryLoops[j]->Delete();
      }
      delete [] boundaryLoops;
      return SV_ERROR;
    }
    vtkIntArray *boundaryLoopPts[2];
    vtkIntArray *boundaryLoopHelper[2];
    boundaryLoopPts[0] = this->FirstLoopPts;
    boundaryLoopPts[1] = this->SecondLoopPts;
    boundaryLoopHelper[0] = this->FirstLoopHelper;
    boundaryLoopHelper[1] = this->SecondLoopHelper;
    //double rightval = 1 + sqrt(2.0)/2.0;
    double rightval = 1 + 0.58;
    double radius = sqrt(rightval/(2 - rightval));
    for (int i=0; i<this->NumBoundaries; i++)
    {
      int numLoopPts = boundaryLoopPts[i]->GetNumberOfTuples();
      double *lengths = new double[numLoopPts];
      if (this->CalculateSquareEdgeLengths(boundaryLoops[i], boundaryLoopPts[i], lengths) != SV_OK)
      {
        vtkErrorMacro("Didn't work");
        delete [] lengths;
        for (int j=0; j<this->NumBoundaries; j++)
        {
          boundaryLoops[j]->Delete();
        }
        delete [] boundaryLoops;
        return SV_ERROR;
      }
      double cubeStart[3];
      this->GetCubeStartPoint(this->CubeStart[i], cubeStart);
      int ok = 0;
      if (this->BoundaryConstraintType == 0)
      {
        ok = this->SetCircleBoundary(boundaryLoops[i], boundaryLoopPts[i], boundaryLoopHelper[i], cubeStart, lengths, radius);
      }
      else
      {
        ok = this->SetCubeBoundary(boundaryLoops[i], boundaryLoopPts[i], boundaryLoopHelper[i], cubeStart, lengths);
      }
      if (!ok)
      {
        delete [] lengths;
        for (int j=0; j<this->NumBoundaries; j++)
        {
          boundaryLoops[j]->Delete();
        }
        delete [] boundaryLoops;
        return SV_ERROR;
      }

      //double length = 0.0;
      //if (this->CalculateCircleLength(boundaryLoops[i], length) != SV_OK)
      //{
      //  vtkErrorMacro("Didn't work");
      //  for (int j=0; j<this->NumBoundaries; j++)
      //  {
      //    boundaryLoops[j]->Delete();
      //  }
      //  delete [] boundaryLoops;
      //  return SV_ERROR;
      //}
      //if (this->SetLoopOnUnitCircle(boundaryLoops[i], length, radius) != SV_OK)
      //{
      //  vtkErrorMacro("Didn't work");
      //  for (int j=0; j<this->NumBoundaries; j++)
      //  {
      //    boundaryLoops[j]->Delete();
      //  }
      //  delete [] boundaryLoops;
      //  return SV_ERROR;
      //}
      //delete [] lengths;

      radius = sqrt((2 - rightval)/rightval);
    }

    for (int i=0; i<this->NumBoundaries; i++)
    {
      boundaryLoops[i]->Delete();
    }
    delete [] boundaryLoops;
  }

  return SV_OK;
}

// ----------------------
// FindBoundaries
// ----------------------
int vtkSVSphericalMapper::FindBoundaries()
{
  int numPts = this->InitialPd->GetNumberOfPoints();
  vtkNew(vtkIntArray, pointIds);
  pointIds->SetNumberOfValues(numPts);
  pointIds->Allocate(numPts, 10000);
  pointIds->SetName("PointIds");
  for (int i=0; i<numPts; i++)
  {
    pointIds->SetValue(i, i);
  }
  this->InitialPd->GetPointData()->AddArray(pointIds);
  vtkNew(vtkFeatureEdges, finder);
  finder->SetInputData(this->InitialPd);
  finder->FeatureEdgesOff();
  finder->NonManifoldEdgesOff();
  finder->BoundaryEdgesOn();
  finder->Update();

  vtkNew(vtkConnectivityFilter, connector);
  connector->SetInputData(finder->GetOutput());
  connector->SetExtractionMode(VTK_EXTRACT_ALL_REGIONS);
  connector->ColorRegionsOn();
  connector->Update();

  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(connector->GetOutput());
  surfacer->Update();

  this->NumBoundaries = connector->GetNumberOfExtractedRegions();
  this->Boundaries->DeepCopy(surfacer->GetOutput());

  return SV_OK;
}

// ----------------------
// FirstStep
// ----------------------
int vtkSVSphericalMapper::FirstStep(int map)
{
  int numPts = this->InitialPd->GetNumberOfPoints();
  vtkNew(vtkFloatArray, laplacian);
  laplacian->SetNumberOfComponents(3);
  laplacian->Allocate(numPts, 10000);
  laplacian->SetNumberOfTuples(numPts);

  if (vtkSVGeneralUtils::ComputeMeshLaplacian(this->HarmonicMap[map], this->EdgeTable,
                                              this->EdgeWeights, this->EdgeNeighbors,
                                              laplacian, map) != SV_OK)
  {
    vtkErrorMacro("Error when computing laplacian");
    return SV_ERROR;
  }
  if (this->UpdateMap(laplacian, map, CG_NONE) != SV_OK)
  {
    vtkErrorMacro("Error when updating tutte map");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// WolfeLineSearch
// ----------------------
int vtkSVSphericalMapper::WolfeLineSearch(int map)
{
  int numPts = this->InitialPd->GetNumberOfPoints();

  vtkNew(vtkFloatArray, conjLaplacian);
  conjLaplacian->SetNumberOfComponents(3);
  conjLaplacian->Allocate(numPts, 10000);
  conjLaplacian->SetNumberOfTuples(numPts);
  vtkSVGeneralUtils::ComputeDataArrayLaplacian(this->ConjugateDir, this->HarmonicMap[map],
                                               this->EdgeTable,
                                               this->EdgeWeights, this->EdgeNeighbors,
                                               conjLaplacian, map);

  double numerator[3];
  double denominator[3];
  vtkSVMathUtils::VectorDotProduct(this->ConjugateDir, this->CurrDescent, numerator, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->ConjugateDir, conjLaplacian, denominator, numPts, 3);

  double maxstep = 100.0;
  for (int i=0; i<3; i++)
  {
    double alpha = numerator[i]/denominator[i];
    if (fabs(alpha) < maxstep)
    {
      maxstep = fabs(alpha);
    }
  }
  if (this->Verbose == 2)
  {
    vtkDebugMacro( "New Step Size: " <<  maxstep);
  }

  //Backtracking


  //vtkNew(vtkPolyData, tmpPoly);
  //tmpPoly->DeepCopy(this->HarmonicMap[map]);
  //double E0 = 0.0;
  //this->ComputeEnergy(tmpPoly, E0, map);
  //for (int i=0; i<10; i++)
  //{
  //  for (int j=0; j<numPts; j++)
  //  {
  //    double newDescent[3], ptVal[3];
  //    this->ConjugateDir->GetTuple(j, newDescent);
  //    this->HarmonicMap[map]->GetPoint(j, ptVal);
  //    for (int k=0; k<3; k++)
  //    {
  //      ptVal[k] = ptVal[k] + maxstep * newDescent[k];
  //    }
  //    vtkMath::Normalize(ptVal);
  //    tmpPoly->GetPoints()->SetPoint(j, ptVal);
  //  }
  //  double EStep = 0.0;
  //  this->ComputeEnergy(tmpPoly, EStep, map);

  //  if( E0 - EStep < 0.0)
  //  {
  //    maxstep = 0.9*maxstep;
  //    EStep = E0;
  //    vtkDebugMacro("IT HAPPPEENEEED: " <<  maxstep);
  //  }
  //  else
  //  {
  //    break;
  //  }
  //}
  this->TimeStep = maxstep;

  return SV_OK;
}

// ----------------------
// SphericalTutteMapping
// ----------------------
int vtkSVSphericalMapper::SphericalTutteMapping()
{
  vtkDebugMacro("SphericalTutteMapping...");
  int numPts = this->InitialPd->GetNumberOfPoints();
  int numTris = this->InitialPd->GetNumberOfCells();

  double E0 = 0.0;
  this->ComputeEnergy(this->HarmonicMap[TUTTE], this->EdgeTable,
                      this->EdgeWeights, E0, TUTTE);
  double R0 = 1000.0;

  vtkDebugMacro("Starting Tutte Iterations...");
  if (this->Verbose)
  {
    vtkDebugMacro("Initial Tutte Energy: " << E0);
  }
  if (this->FirstStep(TUTTE) != SV_OK)
  {
    vtkErrorMacro("Error during initial step");
    return SV_ERROR;
  }
  double EStep = 0.0;
  this->ComputeEnergy(this->HarmonicMap[TUTTE], this->EdgeTable,
                      this->EdgeWeights, EStep, TUTTE);
  double Ediff = E0-EStep;
  double RStep = 0.0;
  this->ComputeResidual(RStep);
  double Rdiff = R0-RStep;
  if (this->Verbose)
  {
    vtkDebugMacro("| Iter | 000000 | Tutte Energy | " << EStep << " | Res | " << Ediff << " |");
  }
  E0 = EStep;
  for (int iter=0; iter<this->MaxNumIterations; iter++)
  {
    vtkNew(vtkFloatArray, laplacian);
    laplacian->SetNumberOfComponents(3);
    laplacian->Allocate(numPts, 10000);
    laplacian->SetNumberOfTuples(numPts);
    if (vtkSVGeneralUtils::ComputeMeshLaplacian(this->HarmonicMap[TUTTE], this->EdgeTable,
                                                this->EdgeWeights, this->EdgeNeighbors,
                                                laplacian, TUTTE) != SV_OK)
    {
      vtkErrorMacro("Error when computing laplacian");
      return SV_ERROR;
    }

    if (this->UpdateMap(laplacian, TUTTE, this->CGUpdateMethod) != SV_OK)
    {
      vtkErrorMacro("Error when updating tutte map");
      return SV_ERROR;
    }

    this->ComputeEnergy(this->HarmonicMap[TUTTE], this->EdgeTable,
                        this->EdgeWeights, EStep, TUTTE);
    Ediff = E0-EStep;
    this->ComputeResidual(RStep);
    Rdiff = R0-RStep;

    if (this->Verbose)
    {
      vtkDebugMacro("| Iter | " << iter+1 << " | Tutte Energy | " << EStep << " | Res | " << Ediff << " |");
    }
    if (fabs(Ediff) < this->TutteEnergyCriterion)
    {
      vtkDebugMacro("Energy Criterion Met! " << EStep);
      break;
    }
    else
    {
      E0 = EStep;
      R0 = RStep;
    }
    if (this->Verbose == 3)
    {
      if (iter%this->NumSaveIterations == 0)
      {
        if (this->IterOutputFilename != NULL)
        {
          std::stringstream iterstr;
          iterstr << this->SaveIter++;
          std::string iterName = this->IterOutputFilename;
          std::string filename =  iterName+"_"+iterstr.str()+".vtp";
          vtkNew(vtkXMLPolyDataWriter, writer);
          writer->SetInputData(this->HarmonicMap[TUTTE]);
          writer->SetFileName(filename.c_str());
          writer->Write();
        }
      }
    }
  }

  vtkDebugMacro("Done with SphericalTutteMapping...");
  return SV_OK;
}

// ----------------------
// SphericalConformalMapper
// ----------------------
int vtkSVSphericalMapper::SphericalConformalMapper()
{
  vtkDebugMacro("SphericalConformalMapper...");
  int numPts = this->InitialPd->GetNumberOfPoints();
  int numTris = this->InitialPd->GetNumberOfCells();

  double E0 = 0.0;
  double R0 = 1000.0;
  this->ComputeEnergy(this->HarmonicMap[HARMONIC], this->EdgeTable,
                      this->EdgeWeights, E0, HARMONIC);

  vtkDebugMacro("Starting Harmonic Iterations...");
  if (this->Verbose)
  {
    vtkDebugMacro("Initial Harmonic Energy: " << E0);
  }
  if (this->FirstStep(HARMONIC) != SV_OK)
  {
    vtkErrorMacro("Error during initial step");
    return SV_ERROR;
  }
  double EStep = 0.0;
  this->ComputeEnergy(this->HarmonicMap[HARMONIC], this->EdgeTable,
                      this->EdgeWeights, EStep, HARMONIC);
  double Ediff = E0-EStep;
  double RStep = 0.0;
  this->ComputeResidual(RStep);
  double Rdiff = R0-RStep;
  if (this->Verbose)
  {
    vtkDebugMacro("| Iter | 000000 | Harmonic Energy | " << EStep << " | Res | " << Ediff << " |");
  }
  E0 = EStep;
  for (int iter=0; iter<this->MaxNumIterations; iter++)
  {
    vtkNew(vtkFloatArray, laplacian);
    laplacian->SetNumberOfComponents(3);
    laplacian->Allocate(numPts, 10000);
    laplacian->SetNumberOfTuples(numPts);
    if (vtkSVGeneralUtils::ComputeMeshLaplacian(this->HarmonicMap[HARMONIC], this->EdgeTable,
                                                this->EdgeWeights, this->EdgeNeighbors,
                                                laplacian, HARMONIC) != SV_OK)
    {
      vtkErrorMacro("Error when computing laplacian");
      return SV_ERROR;
    }

    if (this->UpdateMap(laplacian, HARMONIC, this->CGUpdateMethod) != SV_OK)
    {
      vtkErrorMacro("Error when updating tutte map");
      return SV_ERROR;
    }

    //Compute Mobius transformation
    if (this->NumBoundaries == 0)
    {
      if (this->ComputeMobiusTransformation() != SV_OK)
      {
        vtkErrorMacro("Error when computing the mobius transformation");
        return SV_ERROR;
      }
    }

    this->ComputeEnergy(this->HarmonicMap[HARMONIC], this->EdgeTable,
                        this->EdgeWeights, EStep, HARMONIC);
    Ediff = E0-EStep;
    this->ComputeResidual(RStep);
    Rdiff = R0-RStep;

    if (this->Verbose)
    {
      vtkDebugMacro("| Iter | " << iter+1 << " | Harmonic Energy | " << EStep << " | Res | " << Ediff << " |");
    }
    if (fabs(Ediff) < this->HarmonicEnergyCriterion)
    {
      vtkDebugMacro("Energy Criterion Met! " << EStep);
      break;
    }
    else
    {
      E0 = EStep;
      R0 = RStep;
    }
    if (this->Verbose == 3)
    {
      if (iter%this->NumSaveIterations == 0)
      {
        if (this->IterOutputFilename != NULL)
        {
          std::stringstream iterstr;
          iterstr << this->SaveIter++;
          std::string iterName = this->IterOutputFilename;
          std::string filename = iterName+"_"+iterstr.str()+".vtp";
          vtkNew(vtkXMLPolyDataWriter, writer);
          writer->SetInputData(this->HarmonicMap[HARMONIC]);
          writer->SetFileName(filename.c_str());
          writer->Write();
        }
      }
    }
  }

  vtkDebugMacro("Done with SphericalConformalMapper...");
  return SV_OK;
}

// ----------------------
// ComputeMobiusTransformation
// ----------------------
int vtkSVSphericalMapper::ComputeMobiusTransformation()
{
  int numPts = this->InitialPd->GetNumberOfPoints();

  double massCenter[3];
  vtkSVGeneralUtils::ComputeMassCenter(this->HarmonicMap[HARMONIC], massCenter);
  if (this->Verbose == 2)
  {
    vtkDebugMacro("Mass Center: " << massCenter[0] << " " <<  massCenter[1] << " " <<  massCenter[2]);
  }

  for (int i =0; i<numPts; i++)
  {
    double h[3];
    this->HarmonicMap[HARMONIC]->GetPoint(i, h);
    for (int j=0; j<3; j++)
    {
      h[j] = h[j] - massCenter[j];
    }
    double hNorm = vtkMath::Norm(h);
    for (int j=0; j<3; j++)
    {
      h[j] = h[j]/hNorm;
    }
    if (this->IsBoundary->GetValue(i) == 0)
    {
      this->HarmonicMap[HARMONIC]->GetPoints()->SetPoint(i, h);
    }
    this->MassCenter[0] = massCenter[0];
    this->MassCenter[1] = massCenter[1];
    this->MassCenter[2] = massCenter[2];
  }

  return SV_OK;
}

// ----------------------
// ComputeEnergy
// ----------------------
int vtkSVSphericalMapper::ComputeEnergy(vtkPolyData *pd,
                                                 vtkEdgeTable *edgeTable,
                                                 vtkFloatArray *edgeWeights,
                                                 double &harmonicEnergy,
                                                 int map)
{
  harmonicEnergy = 0.0;
  double compEnergy[3];
  compEnergy[0] = 0.0; compEnergy[1] = 0.0; compEnergy[2] =0.0;
  int numEdges = edgeTable->GetNumberOfEdges();

  edgeTable->InitTraversal();
  for (int i=0; i<numEdges; i++)
  {
    vtkIdType p0, p1;
    vtkIdType edgeId = edgeTable->GetNextEdge(p0, p1);
    double weight = edgeWeights->GetValue(edgeId);
    if (map == TUTTE)
    {
      weight = 1.0;
    }

    double h0[3];
    double h1[3];
    pd->GetPoint(p0, h0);
    pd->GetPoint(p1, h1);

    //Calculate String Energy!
    double edgeEnergy[3];
    vtkSVSphericalMapper::ComputeStringEnergy(h0, h1, weight, edgeEnergy);
    for (int j=0; j<3; j++)
    {
      compEnergy[j] += edgeEnergy[j];
    }
  }
  for (int i=0; i<3; i++)
  {
    harmonicEnergy += compEnergy[i];
  }

  return SV_OK;
}

// ----------------------
// ComputeStringEnergy
// ----------------------
int vtkSVSphericalMapper::ComputeStringEnergy(double e0[],
                                                      double e1[],
                                                      double weight,
                                                      double stringEnergy[])
{
  double edge[3];
  edge[0] = e0[0] - e1[0];
  edge[1] = e0[1] - e1[1];
  edge[2] = e0[2] - e1[2];

  for (int i=0; i<3; i++)
  {
    stringEnergy[i] = weight * pow(edge[i], 2);
  }

  return SV_OK;
}

// ----------------------
// ComputeResidualEnergy
// ----------------------
int vtkSVSphericalMapper::ComputeResidual(double &residual)
{
  int numPts = this->InitialPd->GetNumberOfPoints();
  double newRes[3];
  vtkSVMathUtils::VectorDotProduct(this->ConjugateDir, this->ConjugateDir, newRes,
                         numPts, 3);

  residual = 0.0;
  for (int i=0; i<3; i++)
  {
    newRes[i] = sqrt(newRes[i]);
    residual += newRes[i];
  }
  return SV_OK;
}

// ----------------------
// UpdateMap
// ----------------------
int vtkSVSphericalMapper::UpdateMap(vtkFloatArray *laplacian,
                                             int map,
                                             int cg_update)
{
  int numPts = this->HarmonicMap[map]->GetNumberOfPoints();

  for (int i=0; i<numPts; i++)
  {
    double pointLaplacian[3], pointNormal[3];
    laplacian->GetTuple(i, pointLaplacian);
    this->HarmonicMap[map]->GetPoint(i, pointNormal);

    double pointScalar = vtkMath::Dot(pointLaplacian, pointNormal);
    double pointLaplacianNormal[3];
    double pointLaplacianTangential[3];
    for (int j=0; j<3; j++)
    {
      pointLaplacianNormal[j] = pointScalar * pointNormal[j];
      pointLaplacianTangential[j] = -1.0*(pointLaplacian[j] - pointLaplacianNormal[j]);
    }
    if (cg_update == CG_NONE)
    {
      this->PrevDescent->SetTuple(i, pointLaplacianTangential);
    }
    else
    {
      this->CurrDescent->SetTuple(i, pointLaplacianTangential);
    }
  }

  if (this->StepForward(map, cg_update) != SV_OK)
  {
    vtkErrorMacro("Error when updating tutte map in CG step");
    return SV_ERROR;
  }
  return SV_OK;
}

// ----------------------
// StepForward
// ----------------------
int vtkSVSphericalMapper::StepForward(int map, int cg_update)
{

  if (cg_update == CG_NONE)
  {
    int numPts = this->InitialPd->GetNumberOfPoints();
    for (int i=0; i<numPts; i++)
    {
      double ptVal[3], descent[3];
      this->HarmonicMap[map]->GetPoint(i, ptVal);
      this->PrevDescent->GetTuple(i, descent);
      this->ConjugateDir->SetTuple(i, descent);
      if (this->IsBoundary->GetValue(i) == 0)
      {
        for (int j=0; j<3; j++)
        {
          ptVal[j] = ptVal[j] + this->InitialTimeStep * descent[j];
        }
        vtkMath::Normalize(ptVal);
      }
      this->HarmonicMap[map]->GetPoints()->SetPoint(i, ptVal);
    }
    return SV_OK;
  }
  else if (cg_update == CG_FLETCHER_REEVES)
  {
    this->FRUpdateMap(map);
  }
  else if (cg_update == CG_POLAK_RIBIERE)
  {
    this->PRUpdateMap(map);
  }
  else if (cg_update == CG_HESTENESS_STIEFEL)
  {
    this->HSUpdateMap(map);
  }
  else if (cg_update == CG_DAI_YUAN)
  {
    this->DYUpdateMap(map);
  }
  else
  {
    vtkErrorMacro("No correct option give");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// FRUpdateMap
// ----------------------
int vtkSVSphericalMapper::FRUpdateMap(int map)
{
  int numPts = this->PrevDescent->GetNumberOfTuples();
  double numerator[3], denominator[3];
  vtkSVMathUtils::VectorDotProduct(this->CurrDescent, this->CurrDescent,
                         numerator, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->PrevDescent, this->PrevDescent,
                         denominator, numPts, 3);

  double beta[3];
  for (int i=0; i<3; i++)
  {
    beta[i] = numerator[i]/denominator[i];
  }

  this->CGUpdateMap(map, beta);

  return SV_OK;
}

// ----------------------
// PRUpdateMap
// ----------------------
int vtkSVSphericalMapper::PRUpdateMap(int map)
{
  int numPts = this->PrevDescent->GetNumberOfTuples();
  double numerator[3], denominator[3];
  vtkNew(vtkFloatArray, difference);
  difference->SetNumberOfComponents(3);
  difference->Allocate(numPts, 10000);
  difference->SetNumberOfTuples(numPts);
  vtkSVMathUtils::VectorAdd(this->CurrDescent, this->PrevDescent, -1.0,
                               difference, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->CurrDescent, difference,
                                      numerator, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->PrevDescent, this->PrevDescent,
                                      denominator, numPts, 3);

  double beta[3];
  for (int i=0; i<3; i++)
  {
    beta[i] = svmaximum(0.0, numerator[i]/denominator[i]);
  }

  this->CGUpdateMap(map, beta);

  return SV_OK;
}

// ----------------------
// HSUpdateMap
// ----------------------
int vtkSVSphericalMapper::HSUpdateMap(int map)
{
  int numPts = this->PrevDescent->GetNumberOfTuples();
  double numerator[3], denominator[3];
  vtkNew(vtkFloatArray, difference);
  difference->SetNumberOfComponents(3);
  difference->Allocate(numPts, 10000);
  difference->SetNumberOfTuples(numPts);
  vtkSVMathUtils::VectorAdd(this->CurrDescent, this->PrevDescent, -1.0,
                               difference, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->CurrDescent, difference,
                                      numerator, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->ConjugateDir, difference,
                                      denominator, numPts, 3);

  double beta[3];
  for (int i=0; i<3; i++)
  {
    beta[i] = -1.0 *numerator[i]/denominator[i];
  }

  this->CGUpdateMap(map, beta);

  return SV_OK;
}

// ----------------------
// DYUpdateMap
// ----------------------
int vtkSVSphericalMapper::DYUpdateMap(int map)
{
  int numPts = this->PrevDescent->GetNumberOfTuples();
  double numerator[3], denominator[3];
  vtkNew(vtkFloatArray, difference);
  difference->SetNumberOfComponents(3);
  difference->Allocate(numPts, 10000);
  difference->SetNumberOfTuples(numPts);
  vtkSVMathUtils::VectorAdd(this->CurrDescent, this->PrevDescent, -1.0,
                  difference, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->CurrDescent, this->CurrDescent,
                                      numerator, numPts, 3);
  vtkSVMathUtils::VectorDotProduct(this->ConjugateDir, difference,
                                      denominator, numPts, 3);

  double beta[3];
  for (int i=0; i<3; i++)
  {
    beta[i] = -1.0 * numerator[i]/denominator[i];
  }

  this->CGUpdateMap(map, beta);

  return SV_OK;
}

// ----------------------
// CGUpdateMap
// ----------------------
int vtkSVSphericalMapper::CGUpdateMap(int map, double beta[])
{
  int numPts = this->InitialPd->GetNumberOfPoints();

  double descCond[3];
  vtkSVMathUtils::VectorDotProduct(this->CurrDescent, this->ConjugateDir, descCond,
                         numPts, 3);

  //If descent condition isn't satisfied, restart from new steepest descent dir
  for (int i=0; i<3; i++)
  {
    if (descCond[i] <= 0.0)
    {
      beta[0] = 0.0; beta[1] = 0.0; beta[2] = 0.0;
    }
  }

  for (int i=0; i<numPts; i++)
  {
    double conjdir[3], descent[3], newDescent[3];
    this->CurrDescent->GetTuple(i, descent);
    this->ConjugateDir->GetTuple(i, conjdir);
    for (int j=0; j<3; j++)
    {
      newDescent[j] = descent[j] + beta[j]*conjdir[j];
    }
    this->PrevDescent->SetTuple(i, descent);
    this->ConjugateDir->SetTuple(i, newDescent);
  }

  this->WolfeLineSearch(map);

  for (int i=0; i<numPts; i++)
  {
    double newDescent[3], ptVal[3];
    this->ConjugateDir->GetTuple(i, newDescent);
    this->HarmonicMap[map]->GetPoint(i, ptVal);
    if (this->IsBoundary->GetValue(i) == 0)
    {
      for (int j=0; j<3; j++)
      {
        {
          ptVal[j] = ptVal[j] + this->TimeStep * newDescent[j];
        }
      }
      vtkMath::Normalize(ptVal);
    }
    this->HarmonicMap[map]->GetPoints()->SetPoint(i, ptVal);
  }

  return SV_OK;
}

// ----------------------
// CalculateCircleLength
// ----------------------
int vtkSVSphericalMapper::CalculateCircleLength(vtkPolyData *lines,
                                                       double &length)
{
  int numLines = lines->GetNumberOfLines();

  length = 0.0;
  for (int i=0; i<numLines; i++)
  {
    vtkIdType npts, *pts;
    lines->GetCellPoints(i, npts, pts);

    double pt0[3], pt1[3];
    lines->GetPoint(pts[0], pt0);
    lines->GetPoint(pts[1], pt1);

    double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                       pow(pt0[1]-pt1[1], 2.0) +
                       pow(pt0[2]-pt1[2], 2.0));
    length += dist;
  }

  return SV_OK;
}

// ----------------------
// CalculateSquareEdgeLengths
// ----------------------
int vtkSVSphericalMapper::CalculateSquareEdgeLengths(vtkPolyData *lines,
                                                           vtkIntArray *markerPts,
                                                           double lengths[])
{
  vtkIntArray *pointIds = vtkIntArray::SafeDownCast(lines->GetPointData()->GetArray("PointIds"));
  int numLines = lines->GetNumberOfLines();

  for (int i=0; i<markerPts->GetNumberOfTuples(); i++)
  {
    lengths[i] = 0.0;
  }
  int currCell = 0;
  for (int i=0; i<markerPts->GetNumberOfTuples(); i++)
  {
    int lastPt  = markerPts->GetValue(i);
    vtkIdType npts, *pts;
    int checkPt = -1;
    while (checkPt != lastPt)
    {
      lines->GetCellPoints(currCell, npts, pts);
      double pt0[3], pt1[3];
      lines->GetPoint(pts[0], pt0);
      lines->GetPoint(pts[1], pt1);
      checkPt = pointIds->GetValue(pts[1]);

      double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                         pow(pt0[1]-pt1[1], 2.0) +
                         pow(pt0[2]-pt1[2], 2.0));
      lengths[i] += dist;

      currCell++;
    }
  }

  return SV_OK;
}

// ----------------------
// SetLoopOnUnitCircle
// ----------------------
int vtkSVSphericalMapper::SetLoopOnUnitCircle(vtkPolyData *lines,
                                                    double length,
                                                    double radius)
{
  vtkIntArray *pointIds = vtkIntArray::SafeDownCast(lines->GetPointData()->GetArray("PointIds"));
  int numLines = lines->GetNumberOfLines();

  double currLength = 0.0;
  for (int i=0; i<numLines; i++)
  {
    vtkIdType npts, *pts;
    lines->GetCellPoints(i, npts, pts);

    double pt0[3], pt1[3];
    lines->GetPoint(pts[0], pt0);
    lines->GetPoint(pts[1], pt1);

    double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                       pow(pt0[1]-pt1[1], 2.0) +
                       pow(pt0[2]-pt1[2], 2.0));
    currLength += dist;

    double angle = currLength/length * 2.0 * SV_PI;
    double x_val = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
    double y_val = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
    double z_val = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));

    int id = pointIds->GetValue(pts[1]);

    this->HarmonicMap[0]->GetPoints()->SetPoint(id, x_val, y_val, z_val);
  }

  return SV_OK;
}

// ----------------------
// SetCircleBoundary
// ----------------------
int vtkSVSphericalMapper::SetCircleBoundary(vtkPolyData *lines,
                                                   vtkIntArray *markerPts,
                                                   vtkIntArray *markerDirs,
                                                   double cubeStart[],
                                                   double lengths[],
                                                   double radius)
{
  vtkIntArray *pointIds = vtkIntArray::SafeDownCast(lines->GetPointData()->GetArray("PointIds"));
  int numLines = lines->GetNumberOfLines();

  double currCoords[3];
  for (int i=0; i<3; i++)
  {
    currCoords[i] = cubeStart[i];
  }

  double unitLength = SV_PI / 2.0;
  int currCell = 0;
  int checkPt = -1;
  for (int i=0; i<markerPts->GetNumberOfTuples(); i++)
  {
    double currLength = 0.0;
    int lastPt  = markerPts->GetValue(i);
    int dir     = markerDirs->GetValue(i);
    vtkIdType npts, *pts;
    while (checkPt != lastPt)
    {
      lines->GetCellPoints(currCell, npts, pts);
      double pt0[3], pt1[3];
      lines->GetPoint(pts[0], pt0);
      lines->GetPoint(pts[1], pt1);

      checkPt = pointIds->GetValue(pts[1]);

      double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                         pow(pt0[1]-pt1[1], 2.0) +
                         pow(pt0[2]-pt1[2], 2.0));
      currLength += dist;

      double boundaryVal[3];
      //if (dir == 7)
      //{
      //  double transPt[3];
      //  double angle = currLength/lengths[i] * unitLength + 5.0 * SV_PI/4.0;
      //  transPt[0] = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[1] = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[2] = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));
      //  vtkSVSphericalMapper::RotateByAngle(transPt, 180.0, boundaryVal);
      //}
      //else if (dir == 2)
      //{
      //  double transPt[3];
      //  double angle = currLength/lengths[i] * unitLength + 5.0 * SV_PI/4.0;
      //  transPt[0] = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[1] = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[2] = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));
      //  vtkSVSphericalMapper::RotateByAngle(transPt, 90.0, boundaryVal);
      //}
      //else if (dir == 3 && i != 2)
      //{
      //  double transPt[3];
      //  double angle = currLength/lengths[i] * unitLength + SV_PI/4.0;
      //  transPt[0] = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[1] = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[2] = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));
      //  vtkSVSphericalMapper::RotateByAngle(transPt, 180.0, boundaryVal);
      //}
      //else if (dir == 4)
      //{
      //  double transPt[3];
      //  double angle = currLength/lengths[i] * unitLength +  7.0 * SV_PI/4.0;
      //  transPt[0] = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[1] = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[2] = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));
      //  vtkSVSphericalMapper::RotateByAngle(transPt, 180.0, boundaryVal);
      //}
      //else if (dir == 5)
      //{
      //  double transPt[3];
      //  double angle = currLength/lengths[i] * unitLength +  SV_PI/4.0;
      //  transPt[0] = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[1] = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
      //  transPt[2] = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));
      //  vtkSVSphericalMapper::RotateByAngle(transPt, 90.0, boundaryVal);
      //}
      //else
      //{
        double angle = currLength/lengths[i] * unitLength + unitLength*i;// + unitLength/2.0;
        boundaryVal[0] = (2.0 * radius * std::cos(angle))/(1.0 + std::pow(radius, 2.0));
        boundaryVal[1] = (2.0 * radius * std::sin(angle))/(1.0 + std::pow(radius, 2.0));
        boundaryVal[2] = (-1.0 + std::pow(radius, 2.0))/(1.0 + std::pow(radius, 2.0));
      //}

      int id = pointIds->GetValue(pts[1]);

      this->HarmonicMap[0]->GetPoints()->SetPoint(id, boundaryVal);

      currCell++;
    }
  }

  return SV_OK;
}

// ----------------------
// RotateByAngle
// ----------------------
int vtkSVSphericalMapper::RotateByAngle(const double pt[3], const double angle,
                                               double returnPt[3])
{

  vtkNew(vtkTransform, transformer);
  transformer->RotateY(angle);
  transformer->TransformPoint(pt, returnPt);

  return SV_OK;
}

// ----------------------
// SetCubeBoundary
// ----------------------
int vtkSVSphericalMapper::SetCubeBoundary(vtkPolyData *lines,
                                                 vtkIntArray *markerPts,
                                                 vtkIntArray *markerDirs,
                                                 double cubeStart[],
                                                 double lengths[])
{
  vtkIntArray *pointIds = vtkIntArray::SafeDownCast(lines->GetPointData()->GetArray("PointIds"));
  int numLines = lines->GetNumberOfLines();

  double currCoords[3];
  for (int i=0; i<3; i++)
  {
    currCoords[i] = cubeStart[i];
  }
  double unitLength = 2.0;
  int currCell = 0;
  int checkPt = -1;
  for (int i=0; i<markerPts->GetNumberOfTuples(); i++)
  {
    double currLength = 0.0;
    int lastPt  = markerPts->GetValue(i);
    int dir     = markerDirs->GetValue(i);
    vtkIdType npts, *pts;
    while (checkPt != lastPt)
    {
      lines->GetCellPoints(currCell, npts, pts);
      double pt0[3], pt1[3];
      lines->GetPoint(pts[0], pt0);
      lines->GetPoint(pts[1], pt1);
      checkPt = pointIds->GetValue(pts[1]);

      double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                         pow(pt0[1]-pt1[1], 2.0) +
                         pow(pt0[2]-pt1[2], 2.0));
      currLength += dist;


      if (dir == 0)
      {
        currCoords[0] -= dist/lengths[i] * unitLength;
      }
      else if (dir == 1)
      {
        currCoords[1] -= dist/lengths[i] * unitLength;
      }
      else if (dir == 2)
      {
        currCoords[2] -= dist/lengths[i] * unitLength;
      }
      else if (dir == 3)
      {
        currCoords[0] += dist/lengths[i] * unitLength;
      }
      else if (dir == 4)
      {
        currCoords[1] += dist/lengths[i] * unitLength;
      }
      else if (dir == 5)
      {
        currCoords[2] += dist/lengths[i] * unitLength;
      }

      double sphereCoords[3];
      this->CubeBoundaryToSphere(currCoords, sphereCoords);

      int id = pointIds->GetValue(pts[1]);
      this->HarmonicMap[0]->GetPoints()->SetPoint(id, sphereCoords);

      currCell++;
    }
  }
  return SV_OK;
}

// ----------------------
// CubeBoundaryToSphere
// ----------------------
int vtkSVSphericalMapper::CubeBoundaryToSphere(double inCoords[], double outCoords[])
{
  double x2 = std::pow(inCoords[0], 2.0);
  double y2 = std::pow(inCoords[1], 2.0);
  double z2 = std::pow(inCoords[2], 2.0);

  outCoords[0] = inCoords[0] * sqrt(1 - (y2/2.0) - (z2/2.0) + (y2*z2/3.0));
  outCoords[1] = inCoords[1] * sqrt(1 - (x2/2.0) - (z2/2.0) + (x2*z2/3.0));
  outCoords[2] = inCoords[2] * sqrt(1 - (y2/2.0) - (x2/2.0) + (y2*x2/3.0));

  return SV_OK;
}

// ----------------------
// DetermineBoundaryPlan
// ----------------------
int vtkSVSphericalMapper::DetermineBoundaryPlan(int &numLoops, int bBool[])
{
  int t = this->BoundaryType;

  numLoops = 0;
  int opps = 0;
  int bounds = 0;
  if (t == 0)
  {
    return SV_OK;
  }

  //Determine number of boundaries and opposite boundaries
  if ((t & NORTH) == 0)
  {
    bounds += 1;
    bBool[0] = 1;
    if ((t & SOUTH) == 0)
    {
      opps += 1;
    }
  }
  if ((t & SOUTH) == 0)
  {
    bBool[1] = 1;
    bounds += 1;
  }
  if ((t & FRONT) == 0)
  {
    bBool[2] = 1;
    bounds += 1;
    if ((t & BACK) == 0)
    {
      opps += 1;
    }
  }
  if ((t & BACK) == 0)
  {
    bBool[3] = 1;
    bounds += 1;
  }
  if ((t & LEFT) == 0)
  {
    bBool[4] = 1;
    bounds += 1;
    if ((t & RIGHT) == 0)
    {
      opps += 1;
    }
  }
  if ((t & RIGHT) == 0)
  {
    bBool[5] = 1;
    bounds += 1;
  }

  //Set number of boundary loops and number of boundary points based
  //on information provided
  if (opps == 0)
  {
    numLoops = 1;
  }
  else if (opps == 1)
  {
    if (bounds == 2)
    {
      numLoops = 2;
    }
    else
    {
      numLoops = 1;
    }
  }
  else if (opps == 2)
  {
    if (bounds == 4)
    {
      numLoops = 2;
    }
    else
    {
      numLoops = 1;
    }
  }
  else
  {
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GetCubeStartPoint
// ----------------------
int vtkSVSphericalMapper::GetCubeStartPoint(int id, double startCoords[])
{
  if (id ==0)
  {
    startCoords[0] = 1.0; startCoords[1] = 1.0; startCoords[2] = 1.0;
  }
  else if (id == 1)
  {
    startCoords[0] = -1.0; startCoords[1] = 1.0; startCoords[2] = 1.0;
  }
  else if (id == 2)
  {
    startCoords[0] = -1.0; startCoords[1] = -1.0; startCoords[2] = 1.0;
  }
  else if (id == 3)
  {
    startCoords[0] = 1.0; startCoords[1] = -1.0; startCoords[2] = 1.0;
  }
  else if (id == 4)
  {
    startCoords[0] = 1.0; startCoords[1] = 1.0; startCoords[2] = -1.0;
  }
  else if (id == 5)
  {
    startCoords[0] = -1.0; startCoords[1] = 1.0; startCoords[2] = -1.0;
  }
  else if (id == 6)
  {
    startCoords[0] = -1.0; startCoords[1] = -1.0; startCoords[2] = -1.0;
  }
  else if (id == 7)
  {
    startCoords[0] = 1.0; startCoords[1] = -1.0; startCoords[2] = -1.0;
  }

  return SV_OK;
}
