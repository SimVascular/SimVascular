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

#include "vtkSVIntegrateAttributes.h"

#include "vtkCellData.h"
#include "vtkCellType.h"
#include "vtkCompositeDataIterator.h"
#include "vtkCompositeDataPipeline.h"
#include "vtkCompositeDataSet.h"
#include "vtkDataSet.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#ifdef VTK_USE_PARALLEL
#include "vtkMultiProcessController.h"
#endif
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointData.h"
#include "vtkPolygon.h"
#include "vtkTriangle.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVIntegrateAttributes);

// ----------------------
// FieldList
// ----------------------
class vtkSVIntegrateAttributes::vtkFieldList :
  public vtkDataSetAttributes::FieldList
{
public:
  vtkFieldList(int numInputs) : vtkDataSetAttributes::FieldList(numInputs) { }
  // TODO: fix this
  // void SetFieldIndex(int i, int index)
  //     { this->vtkDataSetAttributes::FieldList::SetFieldIndex(i, index); }
};

// ----------------------
// Constructor
// ----------------------
vtkSVIntegrateAttributes::vtkSVIntegrateAttributes()
{
  this->IntegrationDimension = 0;
  this->Sum = 0.0;
  this->SumCenter[0] = this->SumCenter[1] = this->SumCenter[2] = 0.0;
  this->Controller = 0;

  this->PointFieldList = 0;
  this->CellFieldList = 0;
  this->FieldListIndex = 0;

#ifdef VTK_USE_PARALLEL
  SetController(vtkMultiProcessController::GetGlobalController());
#endif
}

// ----------------------
// Destructor
// ----------------------
vtkSVIntegrateAttributes::~vtkSVIntegrateAttributes()
{

#ifdef VTK_USE_PARALLEL
  if (this->Controller)
    {
    this->Controller->Delete();
    this->Controller = 0;
    }
#endif

}


// ----------------------
// SetController
// ----------------------
void vtkSVIntegrateAttributes::SetController(vtkMultiProcessController *controller)
{
#ifdef VTK_USE_PARALLEL
  if(this->Controller)
    {
    this->Controller->UnRegister(this);
    }

  this->Controller = controller;

  if(this->Controller)
    {
    this->Controller->Register(this);
    }
#endif
}


// ----------------------
// CreateDefaultExecutive
// ----------------------
vtkExecutive* vtkSVIntegrateAttributes::CreateDefaultExecutive()
{
  return vtkCompositeDataPipeline::New();
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVIntegrateAttributes::FillInputPortInformation(int port,
                                                     vtkInformation* info)
{
  if(!this->Superclass::FillInputPortInformation(port, info))
    {
    return 0;
    }
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkDataObject");
  return 1;
}

// ----------------------
// CompareIntegrationDimension
// ----------------------
int vtkSVIntegrateAttributes::CompareIntegrationDimension(vtkDataSet* output,
                                                        int dim)
{
  // higher dimension prevails
  if (this->IntegrationDimension < dim)
    { // Throw out results from lower dimension.
    this->Sum = 0;
    this->SumCenter[0] = this->SumCenter[1] = this->SumCenter[2] = 0.0;
    this->ZeroAttributes(output->GetPointData());
    this->ZeroAttributes(output->GetCellData());
    this->IntegrationDimension = dim;
    return 1;
    }
  // Skip this cell if we are inetrgrting a higher dimension.
  return (this->IntegrationDimension == dim);
}

// ----------------------
// ExecutiveBlock
// ----------------------
void vtkSVIntegrateAttributes::ExecuteBlock(
  vtkDataSet* input, vtkUnstructuredGrid* output,
  int fieldset_index,
  vtkSVIntegrateAttributes::vtkFieldList& pdList,
  vtkSVIntegrateAttributes::vtkFieldList& cdList)
{
  vtkDataArray* ghostLevelArray =
    input->GetCellData()->GetArray("vtkGhostLevels");

  // This is sort of a hack since it's incredibly painful to change all the
  // signatures to take the pdList, cdList and fieldset_index.
  this->PointFieldList = &pdList;
  this->CellFieldList = &cdList;
  this->FieldListIndex = fieldset_index;

  vtkIdList* cellPtIds = vtkIdList::New();
  vtkIdType numCells = input->GetNumberOfCells();
  vtkIdType cellId;
  vtkPoints *cellPoints = 0; // needed if we need to split 3D cells
  int cellType;
  for (cellId = 0; cellId < numCells; ++cellId)
    {
    cellType = input->GetCellType(cellId);
    // Make sure we are not integrating ghost cells.
    if (ghostLevelArray && ghostLevelArray->GetComponent(cellId,0) > 0.0)
      {
      continue;
      }

    switch (cellType)
      {
      // skip empty or 0D Cells
      case VTK_EMPTY_CELL:
      case VTK_VERTEX:
      case VTK_POLY_VERTEX:
        break;

      case VTK_POLY_LINE:
      case VTK_LINE:
      {
      if (this->CompareIntegrationDimension(output, 1))
        {
        input->GetCellPoints(cellId, cellPtIds);
        this->IntegratePolyLine(input, output, cellId, cellPtIds);
        }
      }
      break;

      case VTK_TRIANGLE:
      {
      if (this->CompareIntegrationDimension(output, 2))
        {
        input->GetCellPoints(cellId, cellPtIds);
        this->IntegrateTriangle(input,output,cellId,cellPtIds->GetId(0),
                                cellPtIds->GetId(1),cellPtIds->GetId(2));
        }
      }
      break;

      case VTK_TRIANGLE_STRIP:
      {
      if (this->CompareIntegrationDimension(output, 2))
        {
        input->GetCellPoints(cellId, cellPtIds);
        this->IntegrateTriangleStrip(input, output, cellId, cellPtIds);
        }
      }
      break;

      case VTK_POLYGON:
      {
      if (this->CompareIntegrationDimension(output, 2))
        {
        input->GetCellPoints(cellId, cellPtIds);
        this->IntegratePolygon(input, output, cellId, cellPtIds);
        }
      }
      break;

      case VTK_PIXEL:
      {
      if (this->CompareIntegrationDimension(output, 2))
        {
        input->GetCellPoints(cellId, cellPtIds);
        this->IntegratePixel(input, output, cellId, cellPtIds);
        }
      }
      break;

      case VTK_QUAD:
      {
      if (this->CompareIntegrationDimension(output, 2))
        {
        vtkIdType pt1Id, pt2Id, pt3Id;
        input->GetCellPoints(cellId, cellPtIds);
        pt1Id = cellPtIds->GetId(0);
        pt2Id = cellPtIds->GetId(1);
        pt3Id = cellPtIds->GetId(2);
        this->IntegrateTriangle(input, output, cellId, pt1Id, pt2Id, pt3Id);
        pt2Id = cellPtIds->GetId(3);
        this->IntegrateTriangle(input, output, cellId, pt1Id, pt2Id, pt3Id);
        }
      }
      break;

      case VTK_VOXEL:
      {
      if (this->CompareIntegrationDimension(output, 3))
        {
        input->GetCellPoints(cellId, cellPtIds);
        this->IntegrateVoxel(input, output, cellId, cellPtIds);
        }
      }
      break;

      case VTK_TETRA:
      {
      if (this->CompareIntegrationDimension(output, 3))
        {
        vtkIdType pt1Id, pt2Id, pt3Id, pt4Id;
        input->GetCellPoints(cellId, cellPtIds);
        pt1Id = cellPtIds->GetId(0);
        pt2Id = cellPtIds->GetId(1);
        pt3Id = cellPtIds->GetId(2);
        pt4Id = cellPtIds->GetId(3);
        this->IntegrateTetrahedron(input, output, cellId, pt1Id, pt2Id,
                                   pt3Id, pt4Id);
        }
      }
      break;

      default:
      {
      // We need to explicitly get the cell
      vtkCell *cell = input->GetCell(cellId);
      int cellDim = cell->GetCellDimension();
      if (cellDim == 0)
        {
        continue;
        }
      if (!this->CompareIntegrationDimension(output, cellDim))
        {
        continue;
        }

      // We will need a place to store points from the cell's
      // triangulate function
      if (!cellPoints)
        {
        cellPoints = vtkPoints::New();
        }

      cell->Triangulate(1, cellPtIds, cellPoints);
      switch (cellDim)
        {
        case 1:
          this->IntegrateGeneral1DCell(input, output, cellId, cellPtIds);
          break;
        case 2:
          this->IntegrateGeneral2DCell(input, output, cellId, cellPtIds);
          break;
        case 3:
          this->IntegrateGeneral3DCell(input, output, cellId, cellPtIds);
          break;
        default:
          vtkWarningMacro("Unsupported Cell Dimension = "
                          << cellDim);
        }
      }
      }
    }
  cellPtIds->Delete();
  if (cellPoints)
    {
    cellPoints->Delete();
    }

  this->PointFieldList = nullptr;
  this->CellFieldList = nullptr;
  this->FieldListIndex = 0;
}

// ----------------------
// RequestData
// ----------------------
int vtkSVIntegrateAttributes::RequestData(vtkInformation*,
                                        vtkInformationVector** inputVector,
                                        vtkInformationVector* outputVector)
{
  // Integration of imaginary attribute with constant value 1.
  this->Sum = 0;
  // For computation of point/vertext location.
  this->SumCenter[0] = this->SumCenter[1] = this->SumCenter[2] = 0.0;

  this->IntegrationDimension = 0;

  vtkInformation* info = outputVector->GetInformationObject(0);
  vtkUnstructuredGrid *output = vtkUnstructuredGrid::SafeDownCast(
    info->Get(vtkDataObject::DATA_OBJECT()));
  if (!output)
  {
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  vtkInformation* inInfo = inputVector[0]->GetInformationObject(0);

  vtkDataObject* input = inInfo->Get(vtkDataObject::DATA_OBJECT());
  vtkCompositeDataSet *compositeInput = vtkCompositeDataSet::SafeDownCast(input);
  vtkDataSet *dsInput = vtkDataSet::SafeDownCast(input);
  if (compositeInput)
    {
    vtkCompositeDataIterator* iter = compositeInput->NewIterator();
    int index = 0;
    // vtkFieldList needs to know num of inputs, so determine that first.
    for (iter->InitTraversal(); !iter->IsDoneWithTraversal();
      iter->GoToNextItem())
      {
      vtkDataObject* dobj = iter->GetCurrentDataObject();
      vtkDataSet* ds = vtkDataSet::SafeDownCast(dobj);
      if (ds)
        {
        if (ds->GetNumberOfPoints() == 0)
          {
          continue; // skip empty datasets.
          }
        index++;
        }
      else
        {
        if (dobj)
          {
          vtkWarningMacro("This filter cannot handle sub-datasets of type : "
            << dobj->GetClassName()
            << ". Skipping block");
          }
        }
      }

    // Create the intersection field list. This is list of arrays common
    // to all blocks in the input.
    vtkFieldList pdList(index);
    vtkFieldList cdList(index);
    index=0;
    for (iter->InitTraversal(); !iter->IsDoneWithTraversal();
      iter->GoToNextItem())
      {
      vtkDataObject* dobj = iter->GetCurrentDataObject();
      vtkDataSet* ds = vtkDataSet::SafeDownCast(dobj);
      if (ds)
        {
        if (ds->GetNumberOfPoints() == 0)
          {
          continue; // skip empty datasets.
          }
        if (index == 0)
          {
          pdList.InitializeFieldList(ds->GetPointData());
          cdList.InitializeFieldList(ds->GetCellData());
          }
        else
          {
          pdList.IntersectFieldList(ds->GetPointData());
          cdList.IntersectFieldList(ds->GetCellData());
          }
        index++;
        }
      else
        {
        if (dobj)
          {
          vtkWarningMacro("This filter cannot handle sub-datasets of type : "
                          << dobj->GetClassName()
                          << ". Skipping block");
          }
        }
      }

    // Now initialize the output for the intersected set of arrays.
    this->AllocateAttributes(pdList, output->GetPointData());
    this->AllocateAttributes(cdList, output->GetCellData());

    index = 0;
    // Now execute for each block.
    for (iter->InitTraversal(); !iter->IsDoneWithTraversal();
      iter->GoToNextItem())
      {
      vtkDataObject* dobj = iter->GetCurrentDataObject();
      vtkDataSet* ds = vtkDataSet::SafeDownCast(dobj);
      if (ds && ds->GetNumberOfPoints() > 0)
        {
        this->ExecuteBlock(ds, output, index, pdList, cdList);
        index++;
        }
      }
    iter->Delete();
    }
  else if (dsInput)
    {
    // Output will have all the same attribute arrays as input, but
    // only 1 entry per array, and arrays are double.
    // Set all values to 0.  All output attributes are type double.
    vtkFieldList pdList(1);
    vtkFieldList cdList(1);
    pdList.InitializeFieldList(dsInput->GetPointData());
    cdList.InitializeFieldList(dsInput->GetCellData());
    this->AllocateAttributes(pdList, output->GetPointData());
    this->AllocateAttributes(cdList, output->GetCellData());
    this->ExecuteBlock(dsInput, output, 0, pdList, cdList);
    }
  else
    {
    if (input)
      {
      vtkErrorMacro("This filter cannot handle data of type : "
                    << input->GetClassName());
      }
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
    }

  // Here is the trick:  The satellites need a point and vertex to
  // marshal the attributes.

  // Generate point and vertex.  Add extra attributes for area too.
  // Satellites do not need the area attribute, but it does not hurt.
  double pt[3];
  vtkPoints* newPoints = vtkPoints::New();
  newPoints->SetNumberOfPoints(1);
  // Get rid of the weight factors.
  if (this->Sum != 0.0)
    {
    pt[0] = this->SumCenter[0] / this->Sum;
    pt[1] = this->SumCenter[1] / this->Sum;
    pt[2] = this->SumCenter[2] / this->Sum;
    }
  else
    {
    pt[0] = this->SumCenter[0];
    pt[1] = this->SumCenter[1];
    pt[2] = this->SumCenter[2];
    }
  newPoints->InsertPoint(0, pt);
  output->SetPoints(newPoints);
  newPoints->Delete();
  newPoints = 0;

  output->Allocate(1);
  vtkIdType vertexPtIds[1];
  vertexPtIds[0] = 0;
  output->InsertNextCell(VTK_VERTEX, 1, vertexPtIds);

  // Create a new cell array for the total length, area or volume.
  vtkDoubleArray* sumArray = vtkDoubleArray::New();
  switch (this->IntegrationDimension)
    {
    case 1:
      sumArray->SetName("Length");
      break;
    case 2:
      sumArray->SetName("Area");
      break;
    case 3:
      sumArray->SetName("Volume");
      break;
    }
  sumArray->SetNumberOfTuples(1);
  sumArray->SetValue(0, this->Sum);
  output->GetCellData()->AddArray(sumArray);
  sumArray->Delete();

#ifdef VTK_USE_PARALLEL
  if (this->Controller->GetLocalProcessId() > 0)
    {
    double msg[5];
    msg[0] = (double)(this->IntegrationDimension);
    msg[1] = this->Sum;
    msg[2] = this->SumCenter[0];
    msg[3] = this->SumCenter[1];
    msg[4] = this->SumCenter[2];
    this->Controller->Send(msg, 5, 0, vtkSVIntegrateAttributes::IntegrateAttrInfo);
    this->Controller->Send(output, 0, vtkSVIntegrateAttributes::IntegrateAttrData);
    // Done sending.  Reset output so satellites will have empty data.
    output->Initialize();
    }
  else
    {
    int numProcs = this->Controller->GetNumberOfProcesses();
    for (int id = 1; id < numProcs; ++id)
      {
      double msg[5];
      this->Controller->Receive(msg,
                                5,
                                id,
                                vtkSVIntegrateAttributes::IntegrateAttrInfo);
      vtkUnstructuredGrid* tmp = vtkUnstructuredGrid::New();
      this->Controller->Receive(tmp,
                                id,
                                vtkSVIntegrateAttributes::IntegrateAttrData);
      if (this->CompareIntegrationDimension(output, (int)(msg[0])))
        {
        this->Sum += msg[1];
        this->SumCenter[0] += msg[2];
        this->SumCenter[1] += msg[3];
        this->SumCenter[2] += msg[4];
        this->IntegrateSatelliteData(tmp->GetPointData(),
                                     output->GetPointData());
        this->IntegrateSatelliteData(tmp->GetCellData(),
                                     output->GetCellData());
        }
      tmp->Delete();
      tmp = 0;
      }

    // now that we have all of the sums from each process
    // set the point location with the global value
    if (this->Sum != 0.0)
      {
      pt[0] = this->SumCenter[0] / this->Sum;
      pt[1] = this->SumCenter[1] / this->Sum;
      pt[2] = this->SumCenter[2] / this->Sum;
      }
    else
      {
      pt[0] = this->SumCenter[0];
      pt[1] = this->SumCenter[1];
      pt[2] = this->SumCenter[2];
      }
    output->GetPoints()->SetPoint(0, pt);

    if (output->GetPointData()->GetArray("vtkGhostLevels"))
      {
      output->GetPointData()->RemoveArray("vtkGhostLevels");
      }
    if (output->GetCellData()->GetArray("vtkGhostLevels"))
      {
      output->GetCellData()->RemoveArray("vtkGhostLevels");
      }
    }
#endif

  return SV_OK;
}

// ----------------------
// AllocateAttributes
// ----------------------
void vtkSVIntegrateAttributes::AllocateAttributes(
  vtkSVIntegrateAttributes::vtkFieldList& fieldList,
  vtkDataSetAttributes* outda)
{
  // TODO: fix this
  // int numArrays = fieldList.GetNumberOfArrays();
  // for (int i = 0; i < numArrays; ++i)
  //   {
  //   if (fieldList.GetFieldIndex(i) < 0)
  //     {
  //     continue;
  //     }
  //   int numComponents = fieldList.GetFieldComponents(i);
  //   // All arrays are allocated double with one tuple.
  //   vtkDoubleArray* outArray = vtkDoubleArray::New();
  //   outArray->SetNumberOfComponents(numComponents);
  //   outArray->SetNumberOfTuples(1);
  //   outArray->SetName(fieldList.GetFieldName(i));
  //   // It cannot hurt to zero the arrays here.
  //   for (int j = 0; j < numComponents; ++j)
  //     {
  //     outArray->SetComponent(0, j, 0.0);
  //     }
  //   fieldList.SetFieldIndex(i, outda->AddArray(outArray));
  //   outArray->Delete();
  //   // Should we set scalars, vectors ...
  //   }
}

// ----------------------
// ZeroAttributes
// ----------------------
void vtkSVIntegrateAttributes::ZeroAttributes(vtkDataSetAttributes* outda)
{
  int numArrays, i, numComponents, j;
  vtkDataArray* outArray;
  numArrays = outda->GetNumberOfArrays();
  for (i = 0; i < numArrays; ++i)
    {
    outArray = outda->GetArray(i);
    numComponents = outArray->GetNumberOfComponents();
    for (j = 0; j < numComponents; ++j)
      {
      outArray->SetComponent(0, j, 0.0);
      }
    }
}

// ----------------------
// IntegrateData1
// ----------------------
void vtkSVIntegrateAttributes::IntegrateData1(vtkDataSetAttributes* inda,
  vtkDataSetAttributes* outda,
  vtkIdType pt1Id, double k,
  vtkSVIntegrateAttributes::vtkFieldList& fieldList, int index)
{
  // TODO: fix this
  // int numArrays, i, numComponents, j;
  // vtkDataArray* inArray;
  // vtkDataArray* outArray;
  // numArrays = fieldList.GetNumberOfArrays();
  // double vIn1, dv, vOut;
  // for (i = 0; i < numArrays; ++i)
  //   {
  //   if (fieldList.GetFieldIndex(i) < 0)
  //     {
  //     continue;
  //     }
  //   // We could template for speed.
  //   inArray = inda->GetArray(fieldList.GetDSAIndex(index, i));
  //   outArray = outda->GetArray(fieldList.GetFieldIndex(i));
  //   numComponents = inArray->GetNumberOfComponents();
  //   for (j = 0; j < numComponents; ++j)
  //     {
  //     vIn1 = inArray->GetComponent(pt1Id, j);
  //     vOut = outArray->GetComponent(0, j);
  //     dv = vIn1;
  //     vOut += dv*k;
  //     outArray->SetComponent(0, j, vOut);
  //     }
  //   }
}

// ----------------------
// IntegrateData2
// ----------------------
void vtkSVIntegrateAttributes::IntegrateData2(vtkDataSetAttributes* inda,
  vtkDataSetAttributes* outda,
  vtkIdType pt1Id, vtkIdType pt2Id, double k,
  vtkSVIntegrateAttributes::vtkFieldList& fieldList, int index)
{
  // TODO: fix this
  // int numArrays, i, numComponents, j;
  // vtkDataArray* inArray;
  // vtkDataArray* outArray;
  // numArrays = fieldList.GetNumberOfArrays();
  // double vIn1, vIn2, dv, vOut;
  // for (i = 0; i < numArrays; ++i)
  //   {
  //   if (fieldList.GetFieldIndex(i) < 0)
  //     {
  //     continue;
  //     }
  //   // We could template for speed.
  //   inArray = inda->GetArray(fieldList.GetDSAIndex(index, i));
  //   outArray = outda->GetArray(fieldList.GetFieldIndex(i));
  //   numComponents = inArray->GetNumberOfComponents();
  //   for (j = 0; j < numComponents; ++j)
  //     {
  //     vIn1 = inArray->GetComponent(pt1Id, j);
  //     vIn2 = inArray->GetComponent(pt2Id, j);
  //     vOut = outArray->GetComponent(0, j);
  //     dv = 0.5*(vIn1+vIn2);
  //     vOut += dv*k;
  //     outArray->SetComponent(0, j, vOut);
  //     }
  //   }
}
// ----------------------
// IntegrateData3
// ----------------------
// Is the extra performance worth duplicating this code with IntergrateData2.
void vtkSVIntegrateAttributes::IntegrateData3(vtkDataSetAttributes* inda,
  vtkDataSetAttributes* outda,
  vtkIdType pt1Id, vtkIdType pt2Id,
  vtkIdType pt3Id, double k,
  vtkSVIntegrateAttributes::vtkFieldList& fieldList, int index)
{
  // TODO: fix this
  // int numArrays, i, numComponents, j;
  // vtkDataArray* inArray;
  // vtkDataArray* outArray;
  // numArrays = fieldList.GetNumberOfArrays();
  // double vIn1, vIn2, vIn3, dv, vOut;
  // for (i = 0; i < numArrays; ++i)
  //   {
  //   if (fieldList.GetFieldIndex(i) < 0)
  //     {
  //     continue;
  //     }
  //   // We could template for speed.
  //   inArray = inda->GetArray(fieldList.GetDSAIndex(index, i));
  //   outArray = outda->GetArray(fieldList.GetFieldIndex(i));
  //   numComponents = inArray->GetNumberOfComponents();
  //   for (j = 0; j < numComponents; ++j)
  //     {
  //     vIn1 = inArray->GetComponent(pt1Id, j);
  //     vIn2 = inArray->GetComponent(pt2Id, j);
  //     vIn3 = inArray->GetComponent(pt3Id, j);
  //     vOut = outArray->GetComponent(0, j);
  //     dv = (vIn1+vIn2+vIn3)/3.0;
  //     vOut += dv*k;
  //     outArray->SetComponent(0, j, vOut);
  //     }
  //   }
}

// ----------------------
// IntegrateData4
// ----------------------
// Is the extra performance worth duplicating this code with IntergrateData2.
void vtkSVIntegrateAttributes::IntegrateData4(vtkDataSetAttributes* inda,
  vtkDataSetAttributes* outda,
  vtkIdType pt1Id, vtkIdType pt2Id,
  vtkIdType pt3Id, vtkIdType pt4Id,
  double k,
  vtkSVIntegrateAttributes::vtkFieldList& fieldList, int index)
{
  // TODO: fix this
  // int numArrays, i, numComponents, j;
  // vtkDataArray* inArray;
  // vtkDataArray* outArray;
  // numArrays = fieldList.GetNumberOfArrays();
  // double vIn1, vIn2, vIn3, vIn4, dv, vOut;
  // for (i = 0; i < numArrays; ++i)
  //   {
  //   if (fieldList.GetFieldIndex(i) < 0)
  //     {
  //     continue;
  //     }
  //   // We could template for speed.
  //   inArray = inda->GetArray(fieldList.GetDSAIndex(index, i));
  //   outArray = outda->GetArray(fieldList.GetFieldIndex(i));
  //   numComponents = inArray->GetNumberOfComponents();
  //   for (j = 0; j < numComponents; ++j)
  //     {
  //     vIn1 = inArray->GetComponent(pt1Id, j);
  //     vIn2 = inArray->GetComponent(pt2Id, j);
  //     vIn3 = inArray->GetComponent(pt3Id, j);
  //     vIn4 = inArray->GetComponent(pt4Id, j);
  //     vOut = outArray->GetComponent(0, j);
  //     dv = (vIn1+vIn2+vIn3+vIn4) * 0.25;
  //     vOut += dv*k;
  //     outArray->SetComponent(0, j, vOut);
  //     }
  //   }
}

// ----------------------
// IntegrateSatelliteData
// ----------------------
// Used to sum arrays from all processes.
void vtkSVIntegrateAttributes::IntegrateSatelliteData(vtkDataSetAttributes* inda,
                                                    vtkDataSetAttributes* outda)
{
  if (inda->GetNumberOfArrays() != outda->GetNumberOfArrays())
    {
    return;
    }

  int numArrays, i, numComponents, j;
  vtkDataArray* inArray;
  vtkDataArray* outArray;
  numArrays = outda->GetNumberOfArrays();
  double vIn, vOut;
  for (i = 0; i < numArrays; ++i)
    {
    outArray = outda->GetArray(i);
    numComponents = outArray->GetNumberOfComponents();
    // Protect against arrays in a different order.
    const char* name = outArray->GetName();
    if (name && name[0] != '\0')
      {
      inArray = inda->GetArray(name);
      if (inArray && inArray->GetNumberOfComponents() == numComponents)
        {
        // We could template for speed.
        for (j = 0; j < numComponents; ++j)
          {
          vIn = inArray->GetComponent(0, j);
          vOut = outArray->GetComponent(0, j);
          outArray->SetComponent(0,j,vOut+vIn);
          }
        }
      }
    }
}

// ----------------------
// IntegratePolyLine
// ----------------------
void vtkSVIntegrateAttributes::IntegratePolyLine(vtkDataSet* input,
                                               vtkUnstructuredGrid* output,
                                               vtkIdType cellId,
                                               vtkIdList* ptIds)
{
  double length;
  double pt1[3], pt2[3], mid[3];
  vtkIdType numLines, lineIdx;
  vtkIdType pt1Id, pt2Id;

  numLines = ptIds->GetNumberOfIds()-1;
  for (lineIdx = 0; lineIdx < numLines; ++lineIdx)
    {
    pt1Id = ptIds->GetId(lineIdx);
    pt2Id = ptIds->GetId(lineIdx+1);
    input->GetPoint(pt1Id, pt1);
    input->GetPoint(pt2Id,pt2);

    // Compute the length of the line.
    length = sqrt(vtkMath::Distance2BetweenPoints(pt1, pt2));
    this->Sum += length;

    // Compute the middle, which is really just another attribute.
    mid[0] = (pt1[0]+pt2[0])*0.5;
    mid[1] = (pt1[1]+pt2[1])*0.5;
    mid[2] = (pt1[2]+pt2[2])*0.5;
    // Add weighted to sumCenter.
    this->SumCenter[0] += mid[0]*length;
    this->SumCenter[1] += mid[1]*length;
    this->SumCenter[2] += mid[2]*length;

    // Now integrate the rest of the attributes.
    this->IntegrateData2(input->GetPointData(), output->GetPointData(),
                         pt1Id, pt2Id, length,
                         *this->PointFieldList, this->FieldListIndex);
    this->IntegrateData1(input->GetCellData(), output->GetCellData(),
                         cellId, length,
                         *this->CellFieldList, this->FieldListIndex);
    }
}

// ----------------------
// IntegrateGeneral1DCell
// ----------------------
void vtkSVIntegrateAttributes::IntegrateGeneral1DCell(vtkDataSet* input,
                                               vtkUnstructuredGrid* output,
                                               vtkIdType cellId,
                                               vtkIdList* ptIds)
{
  // Determine the number of lines
  vtkIdType nPnts = ptIds->GetNumberOfIds();
  // There should be an even number of points from the triangulation
  if (nPnts % 2)
    {
    vtkWarningMacro("Odd number of points("
                    << nPnts << ")  encountered - skipping "
                    << " 1D Cell: " << cellId);
    return;
    }

  double length;
  double pt1[3], pt2[3], mid[3];
  vtkIdType pid=0;
  vtkIdType pt1Id, pt2Id;

  while (pid < nPnts)
    {
    pt1Id = ptIds->GetId(pid++);
    pt2Id = ptIds->GetId(pid++);
    input->GetPoint(pt1Id, pt1);
    input->GetPoint(pt2Id,pt2);

    // Compute the length of the line.
    length = sqrt(vtkMath::Distance2BetweenPoints(pt1, pt2));
    this->Sum += length;

    // Compute the middle, which is really just another attribute.
    mid[0] = (pt1[0]+pt2[0])*0.5;
    mid[1] = (pt1[1]+pt2[1])*0.5;
    mid[2] = (pt1[2]+pt2[2])*0.5;
    // Add weighted to sumCenter.
    this->SumCenter[0] += mid[0]*length;
    this->SumCenter[1] += mid[1]*length;
    this->SumCenter[2] += mid[2]*length;

    // Now integrate the rest of the attributes.
    this->IntegrateData2(input->GetPointData(), output->GetPointData(),
                         pt1Id, pt2Id, length,
                         *this->PointFieldList, this->FieldListIndex);
    this->IntegrateData1(input->GetCellData(), output->GetCellData(),
                         cellId, length,
                         *this->CellFieldList, this->FieldListIndex);
    }
}

// ----------------------
// IntegrateTriangleStrip
// ----------------------
void vtkSVIntegrateAttributes::IntegrateTriangleStrip(vtkDataSet* input,
                                                    vtkUnstructuredGrid* output,
                                                    vtkIdType cellId,
                                                    vtkIdList* ptIds)
{
  vtkIdType numTris, triIdx;
  vtkIdType pt1Id, pt2Id, pt3Id;

  numTris = ptIds->GetNumberOfIds()-2;
  for (triIdx = 0; triIdx < numTris; ++triIdx)
    {
    pt1Id = ptIds->GetId(triIdx);
    pt2Id = ptIds->GetId(triIdx+1);
    pt3Id = ptIds->GetId(triIdx+2);
    this->IntegrateTriangle(input, output, cellId, pt1Id, pt2Id, pt3Id);
    }
}

// ----------------------
// IntegratePolygon
// ----------------------
/// \details Works for convex polygons, and interpoaltion is not correct.
void vtkSVIntegrateAttributes::IntegratePolygon(vtkDataSet* input,
                                              vtkUnstructuredGrid* output,
                                              vtkIdType cellId,
                                              vtkIdList* ptIds)
{
  vtkIdType numTris, triIdx;
  vtkIdType pt1Id, pt2Id, pt3Id;

  numTris = ptIds->GetNumberOfIds()-2;
  pt1Id = ptIds->GetId(0);
  for (triIdx = 0; triIdx < numTris; ++triIdx)
    {
    pt2Id = ptIds->GetId(triIdx+1);
    pt3Id = ptIds->GetId(triIdx+2);
    this->IntegrateTriangle(input, output, cellId, pt1Id, pt2Id, pt3Id);
    }
}

// ----------------------
// IntegratePixel
// ----------------------
/// \details For axis alligned rectangular cells
void vtkSVIntegrateAttributes::IntegratePixel(vtkDataSet* input,
                                            vtkUnstructuredGrid* output,
                                            vtkIdType cellId,
                                            vtkIdList* cellPtIds)
{
  vtkIdType pt1Id, pt2Id, pt3Id, pt4Id;
  double pts[4][3];
  pt1Id = cellPtIds->GetId(0);
  pt2Id = cellPtIds->GetId(1);
  pt3Id = cellPtIds->GetId(2);
  pt4Id = cellPtIds->GetId(3);
  input->GetPoint(pt1Id,pts[0]);
  input->GetPoint(pt2Id,pts[1]);
  input->GetPoint(pt3Id,pts[2]);
  input->GetPoint(pt4Id,pts[3]);

  double l, w, a, mid[3];

  // get the lengths of its 2 orthogonal sides.  Since only 1 coordinate
  // can be different we can add the differences in all 3 directions
  l = (pts[0][0] - pts[1][0]) + (pts[0][1] - pts[1][1]) +
      (pts[0][2] - pts[1][2]);

  w = (pts[0][0] - pts[2][0]) + (pts[0][1] - pts[2][1]) +
      (pts[0][2] - pts[2][2]);

  a = fabs(l*w);
  this->Sum += a;
  // Compute the middle, which is really just another attribute.
  mid[0] = (pts[0][0]+pts[1][0]+pts[2][0]+pts[3][0])*0.25;
  mid[1] = (pts[0][1]+pts[1][1]+pts[2][1]+pts[3][1])*0.25;
  mid[2] = (pts[0][2]+pts[1][2]+pts[2][2]+pts[3][2])*0.25;
  // Add weighted to sumCenter.
  this->SumCenter[0] += mid[0]*a;
  this->SumCenter[1] += mid[1]*a;
  this->SumCenter[2] += mid[2]*a;

  // Now integrate the rest of the attributes.
  this->IntegrateData4(input->GetPointData(), output->GetPointData(),
                       pt1Id, pt2Id, pt3Id, pt4Id, a,
                       *this->PointFieldList, this->FieldListIndex);
  this->IntegrateData1(input->GetCellData(), output->GetCellData(), cellId, a,
    *this->CellFieldList, this->FieldListIndex);
}

// ----------------------
// IntegrateTriangle
// ----------------------
void vtkSVIntegrateAttributes::IntegrateTriangle(vtkDataSet* input,
                                               vtkUnstructuredGrid* output,
                                               vtkIdType cellId,
                                               vtkIdType pt1Id,
                                               vtkIdType pt2Id,
                                               vtkIdType pt3Id)
{
  double pt1[3], pt2[3], pt3[3];
  double mid[3], v1[3], v2[3];
  double cross[3];
  double k;

  input->GetPoint(pt1Id,pt1);
  input->GetPoint(pt2Id,pt2);
  input->GetPoint(pt3Id,pt3);

  // Compute two legs.
  v1[0] = pt2[0] - pt1[0];
  v1[1] = pt2[1] - pt1[1];
  v1[2] = pt2[2] - pt1[2];
  v2[0] = pt3[0] - pt1[0];
  v2[1] = pt3[1] - pt1[1];
  v2[2] = pt3[2] - pt1[2];

  // Use the cross product to compute the area of the parallelogram.
  vtkMath::Cross(v1,v2,cross);
  k = sqrt(cross[0]*cross[0] + cross[1]*cross[1] + cross[2]*cross[2]) * 0.5;

  if (k == 0.0)
    {
    return;
    }
  this->Sum += k;

  // Compute the middle, which is really just another attribute.
  mid[0] = (pt1[0]+pt2[0]+pt3[0])/3.0;
  mid[1] = (pt1[1]+pt2[1]+pt3[1])/3.0;
  mid[2] = (pt1[2]+pt2[2]+pt3[2])/3.0;
  // Add weighted to sumCenter.
  this->SumCenter[0] += mid[0]*k;
  this->SumCenter[1] += mid[1]*k;
  this->SumCenter[2] += mid[2]*k;

  // Now integrate the rest of the attributes.
  this->IntegrateData3(input->GetPointData(), output->GetPointData(),
                       pt1Id, pt2Id, pt3Id, k,
                       *this->PointFieldList, this->FieldListIndex);
  this->IntegrateData1(input->GetCellData(), output->GetCellData(), cellId, k,
    *this->CellFieldList, this->FieldListIndex);
}

// ----------------------
// IntegrateGeneral2DCell
// ----------------------
void vtkSVIntegrateAttributes::IntegrateGeneral2DCell(vtkDataSet* input,
                                               vtkUnstructuredGrid* output,
                                               vtkIdType cellId,
                                               vtkIdList* ptIds)
{
  vtkIdType nPnts = ptIds->GetNumberOfIds();
  // There should be a number of points that is a multiple of 3
  // from the triangulation
  if (nPnts % 3)
    {
    vtkWarningMacro("Number of points ("
                    << nPnts << ") is not divisiable by 3 - skipping "
                    << " 2D Cell: " << cellId);
    return;
    }

  vtkIdType triIdx = 0;
  vtkIdType pt1Id, pt2Id, pt3Id;

  while (triIdx < nPnts)
    {
    pt1Id = ptIds->GetId(triIdx++);
    pt2Id = ptIds->GetId(triIdx++);
    pt3Id = ptIds->GetId(triIdx++);
    this->IntegrateTriangle(input, output, cellId, pt1Id, pt2Id, pt3Id);
    }
}

// ----------------------
// IntegrateTetrahedron
// ----------------------
/// \details For Tetrahedral cells
void vtkSVIntegrateAttributes::IntegrateTetrahedron(vtkDataSet* input,
                                                  vtkUnstructuredGrid* output,
                                                  vtkIdType cellId,
                                                  vtkIdType pt1Id,
                                                  vtkIdType pt2Id,
                                                  vtkIdType pt3Id,
                                                  vtkIdType pt4Id)
{
  double pts[4][3];
  input->GetPoint(pt1Id,pts[0]);
  input->GetPoint(pt2Id,pts[1]);
  input->GetPoint(pt3Id,pts[2]);
  input->GetPoint(pt4Id,pts[3]);

  double a[3], b[3], c[3], n[3], v, mid[3];
  int i;
  // Compute the principle vectors around pt0 and the
  // centroid
  for (i = 0; i < 3; i++)
    {
    a[i] = pts[1][i] - pts[0][i];
    b[i] = pts[2][i] - pts[0][i];
    c[i] = pts[3][i] - pts[0][i];
    mid[i] = (pts[0][i]+pts[1][i]+pts[2][i]+pts[3][i])*0.25;
    }


  // Calulate the volume of the tet which is 1/6 * the box product
  vtkMath::Cross(a,b,n);
  v = vtkMath::Dot(c, n) / 6.0;
  this->Sum += v;

  // Add weighted to sumCenter.
  this->SumCenter[0] += mid[0]*v;
  this->SumCenter[1] += mid[1]*v;
  this->SumCenter[2] += mid[2]*v;

  // Integrate the attributes on the cell itself
  this->IntegrateData1(input->GetCellData(), output->GetCellData(), cellId, v,
    *this->CellFieldList, this->FieldListIndex);

  // Integrate the attributes associated with the points
  this->IntegrateData4(input->GetPointData(), output->GetPointData(),
                       pt1Id, pt2Id, pt3Id, pt4Id, v,
                       *this->PointFieldList, this->FieldListIndex);

}

// ----------------------
// IntegrateVoxel
// ----------------------
/// \details For axis alligned hexahedral cells
void vtkSVIntegrateAttributes::IntegrateVoxel(vtkDataSet* input,
                                            vtkUnstructuredGrid* output,
                                            vtkIdType cellId,
                                            vtkIdList* cellPtIds)
{
  vtkIdType pt1Id, pt2Id, pt3Id, pt4Id, pt5Id;
  double pts[5][3];
  pt1Id = cellPtIds->GetId(0);
  pt2Id = cellPtIds->GetId(1);
  pt3Id = cellPtIds->GetId(2);
  pt4Id = cellPtIds->GetId(3);
  pt5Id = cellPtIds->GetId(4);
  input->GetPoint(pt1Id,pts[0]);
  input->GetPoint(pt2Id,pts[1]);
  input->GetPoint(pt3Id,pts[2]);
  input->GetPoint(pt4Id,pts[3]);
  input->GetPoint(pt5Id,pts[4]);

  double l, w, h, v, mid[3];

  // Calulate the volume of the voxel
  l = pts[1][0] - pts[0][0];
  w = pts[2][1] - pts[0][1];
  h = pts[4][2] - pts[0][2];
  v = fabs(l*w*h);
  this->Sum += v;

  // Partially Compute the middle, which is really just another attribute.
  mid[0] = (pts[0][0]+pts[1][0]+pts[2][0]+pts[3][0])*0.125;
  mid[1] = (pts[0][1]+pts[1][1]+pts[2][1]+pts[3][1])*0.125;
  mid[2] = (pts[0][2]+pts[1][2]+pts[2][2]+pts[3][2])*0.125;

  // Integrate the attributes on the cell itself
  this->IntegrateData1(input->GetCellData(), output->GetCellData(), cellId, v,
    *this->CellFieldList, this->FieldListIndex);

  // Integrate the attributes associated with the points on the bottom face
  // note that since IntegrateData4 is going to weigh everything by 1/4
  // we need to pass down 1/2 the volume so they will be weighted by 1/8

  this->IntegrateData4(input->GetPointData(), output->GetPointData(),
                       pt1Id, pt2Id, pt3Id, pt4Id, v*0.5,
                       *this->PointFieldList, this->FieldListIndex);

  // Now process the top face points
  pt1Id = cellPtIds->GetId(5);
  pt2Id = cellPtIds->GetId(6);
  pt3Id = cellPtIds->GetId(7);
  input->GetPoint(pt1Id,pts[0]);
  input->GetPoint(pt2Id,pts[1]);
  input->GetPoint(pt3Id,pts[2]);
  // Finish Computing the middle, which is really just another attribute.
  mid[0] += (pts[0][0]+pts[1][0]+pts[2][0]+pts[4][0])*0.125;
  mid[1] += (pts[0][1]+pts[1][1]+pts[2][1]+pts[4][1])*0.125;
  mid[2] += (pts[0][2]+pts[1][2]+pts[2][2]+pts[4][2])*0.125;


  // Add weighted to sumCenter.
  this->SumCenter[0] += mid[0]*v;
  this->SumCenter[1] += mid[1]*v;
  this->SumCenter[2] += mid[2]*v;

  // Integrate the attributes associated with the points on the top face
  // note that since IntegrateData4 is going to weigh everything by 1/4
  // we need to pass down 1/2 the volume so they will be weighted by 1/8
  this->IntegrateData4(input->GetPointData(), output->GetPointData(),
                       pt1Id, pt2Id, pt3Id, pt5Id, v*0.5,
                       *this->PointFieldList, this->FieldListIndex);
}

// ----------------------
// IntegrateGeneral3DCell
// ----------------------
void vtkSVIntegrateAttributes::IntegrateGeneral3DCell(vtkDataSet* input,
                                               vtkUnstructuredGrid* output,
                                               vtkIdType cellId,
                                               vtkIdList* ptIds)
{

  vtkIdType nPnts = ptIds->GetNumberOfIds();
  // There should be a number of points that is a multiple of 4
  // from the triangulation
  if (nPnts % 4)
    {
    vtkWarningMacro("Number of points ("
                    << nPnts << ") is not divisiable by 4 - skipping "
                    << " 3D Cell: " << cellId);
    return;
    }

  vtkIdType tetIdx = 0;
  vtkIdType pt1Id, pt2Id, pt3Id, pt4Id;

  while (tetIdx < nPnts)
    {
    pt1Id = ptIds->GetId(tetIdx++);
    pt2Id = ptIds->GetId(tetIdx++);
    pt3Id = ptIds->GetId(tetIdx++);
    pt4Id = ptIds->GetId(tetIdx++);
    this->IntegrateTetrahedron(input, output, cellId, pt1Id, pt2Id, pt3Id,
                               pt4Id);
    }
}


// ----------------------
// PrintSelf
// ----------------------
void vtkSVIntegrateAttributes::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "IntegrationDimension: "
     << this->IntegrationDimension << endl;

}

