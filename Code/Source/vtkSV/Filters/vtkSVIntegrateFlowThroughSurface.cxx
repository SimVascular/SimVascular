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

#include "vtkSVIntegrateFlowThroughSurface.h"

#include "vtkCellData.h"
#include "vtkCompositeDataIterator.h"
#include "vtkCompositeDataPipeline.h"
#include "vtkCompositeDataSet.h"
#include "vtkDataArray.h"
#include "vtkDataSet.h"
#include "vtkMultiBlockDataSet.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkSVIntegrateAttributes.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkSVSurfaceVectors.h"
#include "vtkUnstructuredGrid.h"
#include "vtkSmartPointer.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVIntegrateFlowThroughSurface);

// ----------------------
// Constructor
// ----------------------
vtkSVIntegrateFlowThroughSurface::vtkSVIntegrateFlowThroughSurface()
{
  // by default process active point vectors
  this->SetInputArrayToProcess(0,0,0,vtkDataObject::FIELD_ASSOCIATION_POINTS,
                               vtkDataSetAttributes::VECTORS);
}

// ----------------------
// Destructor
// ----------------------
vtkSVIntegrateFlowThroughSurface::~vtkSVIntegrateFlowThroughSurface()
{
}

// ----------------------
// RequestUpdateExtent
// ----------------------
int vtkSVIntegrateFlowThroughSurface::RequestUpdateExtent(
                                           vtkInformation * vtkNotUsed(request),
                                           vtkInformationVector **inputVector,
                                           vtkInformationVector *outputVector)
{
  // get the info objects
  vtkInformation* outInfo = outputVector->GetInformationObject(0);
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);

  inInfo->Set(vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS(),
              outInfo->Get(vtkStreamingDemandDrivenPipeline::
                           UPDATE_NUMBER_OF_GHOST_LEVELS()) + 1);

  return 1;
}

// ----------------------
// GenerateSurfaceVectors
// ----------------------
vtkDataSet* vtkSVIntegrateFlowThroughSurface::GenerateSurfaceVectors(
  vtkDataSet* input)
{
  vtkDataSet* inputCopy = input->NewInstance();
  inputCopy->CopyStructure(input);
  vtkDataArray *vectors = this->GetInputArrayToProcess(0, input);
  if (vectors == 0)
    {
    vtkErrorMacro("Missing Vectors.");
    inputCopy->Delete();
    return 0;
    }
  inputCopy->GetPointData()->SetVectors(vectors);
  inputCopy->GetCellData()->AddArray(
    input->GetCellData()->GetArray("vtkGhostLevels"));

  vtkSVSurfaceVectors* dot = vtkSVSurfaceVectors::New();
  dot->SetInputDataObject(inputCopy);
  dot->SetConstraintModeToPerpendicularScale();
  dot->Update();

  vtkDataSet* output = dot->GetOutput();
  vtkDataSet* outputCopy = output->NewInstance();
  outputCopy->ShallowCopy(output);

  dot->Delete();
  inputCopy->Delete();

  return outputCopy;
}

// ----------------------
// RequestData
// ----------------------
int vtkSVIntegrateFlowThroughSurface::RequestData(
  vtkInformation *request,
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the info objects
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  // get the input and output
  vtkSmartPointer<vtkDataObject> input = inInfo->Get(vtkDataObject::DATA_OBJECT());

  vtkDataSet *dsInput = vtkDataSet::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkUnstructuredGrid *output = vtkUnstructuredGrid::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  vtkSVIntegrateAttributes* integrate = vtkSVIntegrateAttributes::New();
  vtkCompositeDataSet *hdInput = vtkCompositeDataSet::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  if (hdInput)
    {
    vtkMultiBlockDataSet* hds = vtkMultiBlockDataSet::New();
    vtkCompositeDataIterator* iter = hdInput->NewIterator();
    iter->GoToFirstItem();
    while (!iter->IsDoneWithTraversal())
      {
      vtkDataSet* ds = vtkDataSet::SafeDownCast(iter->GetCurrentDataObject());
      if (ds)
        {
        vtkDataSet* intermData = this->GenerateSurfaceVectors(ds);
        if (intermData)
          {
          hds->SetBlock(hds->GetNumberOfBlocks(), intermData);
          intermData->Delete();
          }
        }
      iter->GoToNextItem();
      }
    iter->Delete();
    inInfo->Set(vtkDataObject::DATA_OBJECT(), hds);
    hds->Delete();
    }
  else if (dsInput)
    {
    vtkDataSet* intermData = this->GenerateSurfaceVectors(dsInput);
    if (!intermData)
      {
      return 0;
      }
    inInfo->Set(vtkDataSet::DATA_OBJECT(), intermData);
    intermData->Delete();
    }
  else
    {
    if (input)
      {
      vtkErrorMacro("This filter cannot handle input of type: "
                    << input->GetClassName());
      }
    return 0;
    }

  integrate->ProcessRequest(request, inputVector, outputVector);

  if (hdInput)
    {
    inInfo->Set(vtkDataObject::DATA_OBJECT(), hdInput);
    }
  else if (dsInput)
    {
    inInfo->Set(vtkDataObject::DATA_OBJECT(), dsInput);
    }

  vtkDataArray* flow = output->GetPointData()->GetArray("Perpendicular Scale");
  if (flow)
    {
    flow->SetName("Surface Flow");
    }

  integrate->Delete();
  integrate = 0;

  return 1;
}

// ----------------------
// CreateDefaultExecutive
// ----------------------
vtkExecutive* vtkSVIntegrateFlowThroughSurface::CreateDefaultExecutive()
{
  return vtkCompositeDataPipeline::New();
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVIntegrateFlowThroughSurface::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVIntegrateFlowThroughSurface::FillInputPortInformation(
  int port, vtkInformation* info)
{
  if(!this->Superclass::FillInputPortInformation(port, info))
    {
    return 0;
    }
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkDataObject");
  return 1;
}
