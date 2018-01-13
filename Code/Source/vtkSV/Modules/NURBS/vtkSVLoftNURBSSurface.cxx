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

#include "vtkSVLoftNURBSSurface.h"

#include "vtkAlgorithmOutput.h"
#include "vtkCellData.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkSVGlobals.h"
#include "vtkSVNURBSUtils.h"
#include "vtkSVMathUtils.h"
#include "vtkTrivialProducer.h"

#include <string>
#include <sstream>
#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVLoftNURBSSurface);

// ----------------------
// Constructor
// ----------------------
vtkSVLoftNURBSSurface::vtkSVLoftNURBSSurface()
{
  this->UserManagedInputs = 0;
  this->UDegree = 2;
  this->VDegree = 2;

  this->PolyDataUSpacing = 0.1;
  this->PolyDataVSpacing = 0.1;

  this->StartUDerivatives = vtkDoubleArray::New();
  this->StartVDerivatives = vtkDoubleArray::New();
  this->EndUDerivatives   = vtkDoubleArray::New();
  this->EndVDerivatives   = vtkDoubleArray::New();

  this->StartUDerivatives->SetNumberOfComponents(3);
  this->StartUDerivatives->SetNumberOfTuples(1);
  this->StartVDerivatives->SetNumberOfComponents(3);
  this->StartVDerivatives->SetNumberOfTuples(1);
  this->EndUDerivatives->SetNumberOfComponents(3);
  this->EndUDerivatives->SetNumberOfTuples(1);
  this->EndVDerivatives->SetNumberOfComponents(3);
  this->EndVDerivatives->SetNumberOfTuples(1);

  this->Surface = vtkSVNURBSSurface::New();

  this->UKnotSpanType        = NULL;
  this->VKnotSpanType        = NULL;

  this->UParametricSpanType = NULL;
  this->VParametricSpanType = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVLoftNURBSSurface::~vtkSVLoftNURBSSurface()
{
  if (this->Surface != NULL)
  {
    this->Surface->Delete();
  }
  if (this->StartUDerivatives != NULL)
  {
    this->StartUDerivatives->Delete();
  }
  if (this->StartVDerivatives != NULL)
  {
    this->StartVDerivatives->Delete();
  }
  if (this->EndUDerivatives != NULL)
  {
    this->EndUDerivatives->Delete();
  }
  if (this->EndVDerivatives != NULL)
  {
    this->EndVDerivatives->Delete();
  }

  if (this->UKnotSpanType != NULL)
  {
    delete [] this->UKnotSpanType;
    this->UKnotSpanType = NULL;
  }
  if (this->VKnotSpanType != NULL)
  {
    delete [] this->VKnotSpanType;
    this->VKnotSpanType = NULL;
  }
  if (this->UParametricSpanType != NULL)
  {
    delete [] this->UParametricSpanType;
    this->UParametricSpanType = NULL;
  }
  if (this->VParametricSpanType != NULL)
  {
    delete [] this->VParametricSpanType;
    this->VParametricSpanType = NULL;
  }
}

// ----------------------
// AddInputData
// ----------------------
void vtkSVLoftNURBSSurface::AddInputData(vtkPolyData *ds)
{
  if (this->UserManagedInputs)
    {
    vtkErrorMacro(<<
      "AddInput is not supported if UserManagedInputs is true");
    return;
    }
  this->Superclass::AddInputData(ds);
}

// ----------------------
// RemoveInputData
// ----------------------
void vtkSVLoftNURBSSurface::RemoveInputData(vtkPolyData *ds)
{
  if (this->UserManagedInputs)
    {
    vtkErrorMacro(<<
      "RemoveInput is not supported if UserManagedInputs is true");
    return;
    }

  if (!ds)
    {
    return;
    }
  int numCons = this->GetNumberOfInputConnections(0);
  for(int i=0; i<numCons; i++)
    {
    if (this->GetInput(i) == ds)
      {
      this->RemoveInputConnection(0,
        this->GetInputConnection(0, i));
      }
    }
}

// ----------------------
// SetNumberOfInputs
// ----------------------
void vtkSVLoftNURBSSurface::SetNumberOfInputs(int num)
{
  if (!this->UserManagedInputs)
    {
    vtkErrorMacro(<<
      "SetNumberOfInputs is not supported if UserManagedInputs is false");
    return;
    }

  // Ask the superclass to set the number of connections.
  this->SetNumberOfInputConnections(0, num);
}

// ----------------------
// SetInputDataByNumber
// ----------------------
void vtkSVLoftNURBSSurface::
SetInputDataByNumber(int num, vtkPolyData* input)
{
  vtkTrivialProducer* tp = vtkTrivialProducer::New();
  tp->SetOutput(input);
  this->SetInputConnectionByNumber(num, tp->GetOutputPort());
  tp->Delete();
}

// ----------------------
// SetInputConnectionByNumber
// ----------------------
void vtkSVLoftNURBSSurface::
SetInputConnectionByNumber(int num,vtkAlgorithmOutput *input)
{
  if (!this->UserManagedInputs)
    {
    vtkErrorMacro(<<
      "SetInputConnectionByNumber is not supported if UserManagedInputs "<<
      "is false");
    return;
    }

  // Ask the superclass to connect the input.
  this->SetNthInputConnection(0, num, input);
}

// ----------------------
// RequestData
// ----------------------
int vtkSVLoftNURBSSurface::RequestData(
    vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the info object
  // get the ouptut
  vtkPolyData *output = vtkPolyData::GetData(outputVector, 0);

  // Get number of inputs
  int numInputs = inputVector[0]->GetNumberOfInformationObjects();

  // Set up input vector
  vtkPolyData** inputs = new vtkPolyData*[numInputs];
  for (int idx = 0; idx < numInputs; ++idx)
    {
    inputs[idx] = vtkPolyData::GetData(inputVector[0],idx);
    }

  // TODO: Need to make sure knot span and parameteric span types are set
  if (this->LoftNURBS(inputs,numInputs,output) != SV_OK)
  {
    vtkErrorMacro("Could not loft surface");
    delete [] inputs;
    return SV_ERROR;
  }

  delete [] inputs;
  return SV_OK;
}

// ----------------------
// RequestUpdateExtent
// ----------------------
int vtkSVLoftNURBSSurface::RequestUpdateExtent(
    vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the output info object
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  int piece, numPieces, ghostLevel;
  int idx;

  piece = outInfo->Get(
      vtkStreamingDemandDrivenPipeline::UPDATE_PIECE_NUMBER());
  numPieces = outInfo->Get(
      vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_PIECES());
  ghostLevel = outInfo->Get(
      vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS());

  // make sure piece is valid
  if (piece < 0 || piece >= numPieces)
    {
    return SV_ERROR;
    }

  int numInputs = this->GetNumberOfInputConnections(0);
  if (this->ParallelStreaming)
    {
    piece = piece * numInputs;
    numPieces = numPieces * numInputs;
    }

  vtkInformation *inInfo;
  // just copy the Update extent as default behavior.
  for (idx = 0; idx < numInputs; ++idx)
    {
    inInfo = inputVector[0]->GetInformationObject(idx);
    if (inInfo)
      {
      if (this->ParallelStreaming)
        {
        inInfo->Set(
	    vtkStreamingDemandDrivenPipeline::UPDATE_PIECE_NUMBER(),
	    piece + idx);
        inInfo->Set(
	    vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_PIECES(),
                    numPieces);
        inInfo->Set(
	    vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS(),
                    ghostLevel);
        }
      else
        {
        inInfo->Set(
	    vtkStreamingDemandDrivenPipeline::UPDATE_PIECE_NUMBER(),
                    piece);
        inInfo->Set(
	    vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_PIECES(),
                    numPieces);
        inInfo->Set(
	    vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS(),
                    ghostLevel);
        }
      }
    }

  return SV_OK;
}

// ----------------------
// GetInput
// ----------------------
vtkPolyData *vtkSVLoftNURBSSurface::GetInput(int idx)
{
  return vtkPolyData::SafeDownCast(
    this->GetExecutive()->GetInputData(0, idx));
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVLoftNURBSSurface::PrintSelf(ostream& os,
    vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << "ParallelStreaming:" << (this->ParallelStreaming?"On":"Off") << endl;
  os << "UserManagedInputs:" << (this->UserManagedInputs?"On":"Off") << endl;

  os << indent << "U Degree: " << this->UDegree << "\n";
  os << indent << "U Knot span type: " << this->UKnotSpanType << "\n";
  os << indent << "U Parametric values span type: " << this->UParametricSpanType << "\n";
  for (int i=0; i<this->StartUDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->StartUDerivatives->GetTuple(i, tup);
    os << indent << "Start U Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndUDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->EndUDerivatives->GetTuple(i, tup);
    os << indent << "End V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  os << "\n";

  os << indent << "V Degree: " << this->UDegree << "\n";
  os << indent << "V Knot span type: " << this->VKnotSpanType << "\n";
  os << indent << "V Parametric values span type: " << this->VParametricSpanType << "\n";
  for (int i=0; i<this->StartVDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->StartVDerivatives->GetTuple(i, tup);
    os << indent << "Start V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndVDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->EndVDerivatives->GetTuple(i, tup);
    os << indent << "End V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVLoftNURBSSurface::FillInputPortInformation(
    int port, vtkInformation *info)
{
  if (!this->Superclass::FillInputPortInformation(port, info))
    {
    return SV_ERROR;
    }
  info->Set(vtkAlgorithm::INPUT_IS_REPEATABLE(), 1);
  return SV_OK;
}

// ----------------------
// LoftNURBS
// ----------------------
int vtkSVLoftNURBSSurface::LoftNURBS(vtkPolyData *inputs[], int numInputs,
    vtkPolyData *outputPD)
{
  // Get number of control points and degree
  int nUCon = numInputs;
  int nVCon = inputs[0]->GetNumberOfPoints();
  int p     = this->UDegree;
  int q     = this->VDegree;

  // Get knot span and parametric span types
  std::string kutype = this->UKnotSpanType;
  std::string kvtype = this->VKnotSpanType;
  std::string putype = this->UParametricSpanType;
  std::string pvtype = this->VParametricSpanType;

  // Make sure input dimensions make sense
  for (int i=0; i<nUCon; i++)
  {
    if (nVCon != inputs[i]->GetNumberOfPoints())
    {
      vtkErrorMacro("Input segments do not have the same number of points, cannot loft");
      return SV_ERROR;
    }
  }

  // Check that the number of inputs enough for degree
  if (p > nUCon)
  {
    vtkErrorMacro("Need to either decrease degree given or number of inputs in U direction");
    return SV_ERROR;
  }
  if (q > nVCon)
  {
    vtkErrorMacro("Need to either decrease degree given or number of inputs in V direction");
    return SV_ERROR;
  }

  // Set the temporary control points
  vtkNew(vtkPoints, tmpPoints);
  tmpPoints->SetNumberOfPoints(nUCon);
  for (int i=0; i<nUCon; i++)
    tmpPoints->SetPoint(i, inputs[i]->GetPoint(0));

  // Get the input point set u representation
  vtkNew(vtkDoubleArray, U);
  if (vtkSVNURBSUtils::GetUs(tmpPoints, putype, U) != SV_OK)
  {
    return SV_ERROR;
  }
  //fprintf(stdout,"U:\n");
  //vtkSVNURBSUtils::PrintArray(U);

  // Get the knots in the u direction
  vtkNew(vtkDoubleArray, uKnots);
  if (vtkSVNURBSUtils::GetKnots(U, p, kutype, uKnots) != SV_OK)
  {
    fprintf(stderr,"Error getting knots\n");
    return SV_ERROR;
  }
  //fprintf(stdout,"X knots\n");
  //vtkSVNURBSUtils::PrintArray(uKnots);
  //
  // Get the input point set v representation
  vtkNew(vtkDoubleArray, V);
  if (vtkSVNURBSUtils::GetUs(inputs[0]->GetPoints(), pvtype, V) != SV_OK)
  {
    return SV_ERROR;
  }
  //fprintf(stdout,"V:\n");
  //vtkSVNURBSUtils::PrintArray(V);

  // Get the knots in the v direction
  vtkNew(vtkDoubleArray, vKnots);
  if (vtkSVNURBSUtils::GetKnots(V, q, kvtype, vKnots) != SV_OK)
  {
    fprintf(stderr,"Error getting knots\n");
    return SV_ERROR;
  }
  //fprintf(stdout,"Y knots\n");
  //vtkSVNURBSUtils::PrintArray(vKnots);

  // Convert input to structured grid
  vtkNew(vtkStructuredGrid, inputPoints);
  if (vtkSVNURBSUtils::PolyDatasToStructuredGrid(inputs, numInputs, inputPoints) != SV_OK)
    return SV_ERROR;

  // Get derivatives in fomrat we need
  vtkNew(vtkDoubleArray, DU0); DU0->DeepCopy(this->StartUDerivatives);
  vtkNew(vtkDoubleArray, DUN); DUN->DeepCopy(this->EndUDerivatives);
  if (!strncmp(kutype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DU0->GetNumberOfTuples() == 1 ||
        DUN->GetNumberOfTuples() == 1)
    {
      this->GetDefaultDerivatives(inputPoints, 0, DU0, DUN);
    }
  }

  // Get derivatives in fomrat we need
  vtkNew(vtkDoubleArray, DV0); DV0->DeepCopy(this->StartVDerivatives);
  vtkNew(vtkDoubleArray, DVN); DVN->DeepCopy(this->EndVDerivatives);
  if (!strncmp(kvtype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DV0->GetNumberOfTuples() == 1 ||
        DVN->GetNumberOfTuples() == 1)
    {
      this->GetDefaultDerivatives(inputPoints, 1, DV0, DVN);
    }
  }

  // Get the control points of surface, lengthy operation in vtkSVNURBSUtils
  vtkNew(vtkStructuredGrid, cPoints);
  vtkNew(vtkDoubleArray, uWeights);
  vtkNew(vtkDoubleArray, vWeights);
  if (vtkSVNURBSUtils::GetControlPointsOfSurface(inputPoints, U, V, uWeights, vWeights,
                                               uKnots, vKnots, p, q, kutype, kvtype,
                                               DU0, DUN, DV0, DVN, cPoints) != SV_OK)
  {
    return SV_ERROR;
  }

  // Set the knot vectors and control points
  this->Surface->SetKnotVector(uKnots, 0);
  this->Surface->SetKnotVector(vKnots, 1);
  this->Surface->SetControlPoints(cPoints);

  // Get the polydata representation from the NURBS Surface
  this->Surface->GeneratePolyDataRepresentation(this->PolyDataUSpacing, this->PolyDataVSpacing);
  outputPD->DeepCopy(this->Surface->GetSurfaceRepresentation());

  return SV_OK;
}

// ----------------------
// GetDefaultDerivatives
// ----------------------
int vtkSVLoftNURBSSurface::GetDefaultDerivatives(vtkStructuredGrid *input, const int comp, vtkDoubleArray *D0out, vtkDoubleArray *DNout)
{
  // Get dimensions
  int dim[3];
  input->GetDimensions(dim);

  // Get number of values and derivatives from dim
  int numVals   = dim[comp];
  int numDerivs = dim[-1*(comp-1)];

  // Set number of tuples for derivatives
  D0out->SetNumberOfTuples(numDerivs);
  DNout->SetNumberOfTuples(numDerivs);
  for (int i=0; i<numDerivs; i++)
  {
    int pos[3]; pos[2] = 0;
    pos[-1*(comp-1)] = i;

    // Get the point id
    double pt0[3]; pos[comp] = 0;
    int ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, pt0);

    // Get the point id
    double pt1[3]; pos[comp] = 1;
    ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, pt1);

    // Get the point id
    double ptnm1[3]; pos[comp] = numVals - 1;
    ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, ptnm1);

    // Get the point id
    double ptnm2[3]; pos[comp] = numVals - 2;
    ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, ptnm2);

    // From point ids, compute vectors at ends of data
    double D0[3], DN[3];
    vtkMath::Subtract(pt1, pt0, D0);
    vtkMath::Subtract(ptnm1, ptnm2, DN);

    // Set tuples
    D0out->SetTuple(i, D0);
    DNout->SetTuple(i, DN);
  }

  return SV_OK;
}
