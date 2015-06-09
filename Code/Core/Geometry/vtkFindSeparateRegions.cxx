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

/** @file vtkFindSeparateRegions.cxx
 *  @brief This implements the vtkFindSeparateRegions filter as a class
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "vtkFindSeparateRegions.h"

#include "vtkFloatArray.h"
#include "vtkMath.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkCellArray.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkCellDataToPointData.h"
#include "vtkEdgeTable.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkPolygon.h"

#include <iostream>

vtkCxxRevisionMacro(vtkFindSeparateRegions, "$Revision: 0.0 $");
vtkStandardNewMacro(vtkFindSeparateRegions);

vtkFindSeparateRegions::vtkFindSeparateRegions()
{
//    this->intCellScalars = vtkIntArray::New();
    this->ArrayName = 0;
    this->OutPointArrayName = 0;

    this->targetCellIds = NULL;
}

vtkFindSeparateRegions::~vtkFindSeparateRegions()
{
  if (this->targetCellIds)
    this->targetCellIds->Delete();

//    if (this->intCellScalars)
//	this->intCellScalars->Delete();
}

void vtkFindSeparateRegions::PrintSelf(ostream& os, vtkIndent indent)
{
}

// Generate Separated Surfaces with Region ID Numbers
int vtkFindSeparateRegions::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
    // get the input and output
    vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
    vtkPolyData *output = vtkPolyData::GetData(outputVector);
    
    // Define variables used by the algorithm
    vtkSmartPointer<vtkPoints> inpts = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkCellArray> inPolys = vtkSmartPointer<vtkCellArray>::New();
    vtkIdType numPts, numPolys;
    vtkIdType newId, cellId,pointId;

    //Get input points, polys and set the up in the vtkPolyData mesh
    inpts = input->GetPoints();
    inPolys = input->GetPolys();

    //Get the number of Polys for scalar  allocation
    numPolys = input->GetNumberOfPolys();
    numPts = input->GetNumberOfPoints();

    //Check the input to make sure it is there
    if (numPolys < 1)               
    {
        vtkDebugMacro("No input!");
	return 1;
    }

    if (this->GetCellArray(input) != 1)
    {
      std::cout<<"No Cell Array Named "<<this->ArrayName<<" on surface"<<endl;
      return 0;
    }
    if (this->OutPointArrayName == 0)
    {
      std::cout<<"Need name for output point data information"<<endl;
      return 0;
    }

    input->BuildLinks();
    vtkSmartPointer<vtkIntArray> newPointArray =
      vtkSmartPointer<vtkIntArray>::New();
    vtkSmartPointer<vtkIdList> pointCells = 
      vtkSmartPointer<vtkIdList>::New();
    vtkSmartPointer<vtkIdList> checkList = 
      vtkSmartPointer<vtkIdList>::New();

    if (this->targetCellIds == NULL)
      this->SetAllCellIds();
    for (pointId = 0;pointId < numPts;pointId++)
    {
      checkList->Reset();
      int boundaryPoint = 0;
      input->GetPointCells(pointId,pointCells);
      for (int i=0;i<pointCells->GetNumberOfIds();i++)
      {
	cellId = pointCells->GetId(i);
	vtkIdType value = this->intCellScalars->GetValue(cellId);
	if (this->targetCellIds->IsId(value) != -1)
	{
	  vtkIdType check = checkList->InsertUniqueId(value);
	  if (check != 0)
	    boundaryPoint = 1;
	}
      }
      if (boundaryPoint)
	newPointArray->InsertValue(pointId,1);
      else
	newPointArray->InsertValue(pointId,0);
    }

    newPointArray->SetName(this->OutPointArrayName);
    output->DeepCopy(input);
    output->GetPointData()->RemoveArray(this->OutPointArrayName);
    output->GetPointData()->AddArray(newPointArray);
    output->GetPointData()->SetActiveScalars(this->OutPointArrayName);

    return 1;
}

int vtkFindSeparateRegions::GetCellArray(vtkPolyData *object)
{
  vtkIdType i;
  int exists = 0;
  int numArrays;

  numArrays = object->GetCellData()->GetNumberOfArrays();
  for (i=0;i<numArrays;i++)
  {
    if (!strcmp(object->GetCellData()->GetArrayName(i),
	  this->ArrayName))
    {
      exists = 1;
    }
  }

  if (exists)
  {
    this->intCellScalars = vtkIntArray::SafeDownCast(
	object->GetCellData()->GetArray(this->ArrayName));
  }

  return exists;
}

int vtkFindSeparateRegions::SetCellIds(vtkIdList *cellIds)
{
  this->targetCellIds = vtkIdList::New();
  this->targetCellIds->DeepCopy(cellIds);
  return 1;
}

int vtkFindSeparateRegions::SetAllCellIds()
{
  this->targetCellIds = vtkIdList::New();
  double range[2];
  this->intCellScalars->GetRange(range);

  int max = range[1];
  for (int i=0;i < max;i++)
    this->targetCellIds->InsertNextId(i);

  return 1;
}
