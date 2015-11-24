/*=========================================================================
 *
 * Copyright (c) 2015 The Regents of the University of California.
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

/** @file vtkBlendPolyData.cxx
 *  @brief This implements the vtkBlendPolyData filter as a class
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "vtkBlendPolyData.h"

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

//include local filter
#include "vtkLocalQuadricDecimation.h"
#include "vtkLocalSmoothPolyDataFilter.h"
#include "vtkLocalInterpolatingSubdivisionFilter.h"
#include "vtkLocalLinearSubdivisionFilter.h"

#include <iostream>

vtkCxxRevisionMacro(vtkBlendPolyData, "$Revision: 0.0 $");
vtkStandardNewMacro(vtkBlendPolyData);

vtkBlendPolyData::vtkBlendPolyData()
{
    this->PointArrayName = 0;
    this->CellArrayName = 0;

    this->NumBlendIterations = 1;
    this->NumSubBlendIterations = 1;
    this->NumSubdivisionIterations = 1;
    this->NumSmoothIterations = 1;
    this->SmoothRelaxationFactor = 0.01;
    this->DecimationTargetReduction = 0.005;

    this->UseCellArray = 1;
    this->UsePointArray = 0;
}

vtkBlendPolyData::~vtkBlendPolyData()
{
}

void vtkBlendPolyData::PrintSelf(ostream& os, vtkIndent indent)
{
}

// Generate Separated Surfaces with Region ID Numbers
int vtkBlendPolyData::RequestData(
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

    input->BuildLinks();

    vtkSmartPointer<vtkPolyData> workingPD =
      vtkSmartPointer<vtkPolyData>::New();
    workingPD->DeepCopy(input);

    for (int i=0;i<this->NumBlendIterations;i++)
    {
      std::cout<<"Starting Iteration "<<i+1<<endl;
      vtkSmartPointer<vtkLocalLinearSubdivisionFilter> subdivider =
	vtkSmartPointer<vtkLocalLinearSubdivisionFilter>::New();
      subdivider->SetInputData(workingPD);
      subdivider->SetNumberOfSubdivisions(this->NumSubdivisionIterations);
      subdivider->SetSubdivideCellArrayName(this->CellArrayName);
      subdivider->Update();

      workingPD->DeepCopy(subdivider->GetOutput(0));
      for (int j=0;j<this->NumSubBlendIterations;j++)
      {
	std::cout<<"Smoothing..."<<endl;
	vtkSmartPointer<vtkLocalSmoothPolyDataFilter> smoother =
	  vtkSmartPointer<vtkLocalSmoothPolyDataFilter>::New();
	smoother->SetInputData(workingPD);
	smoother->SetRelaxationFactor(this->SmoothRelaxationFactor);
	smoother->SetSmoothCellArrayName(this->CellArrayName);
	smoother->SetSmoothPointArrayName(this->PointArrayName);
	smoother->SetNumberOfIterations(this->NumSmoothIterations);
	smoother->SetUseCellArray(this->UseCellArray);
	smoother->SetUsePointArray(this->UsePointArray);
	smoother->FeatureEdgeSmoothingOff();
	smoother->BoundarySmoothingOff();
	smoother->Update();

	std::cout<<"Decimating..."<<endl;
	vtkSmartPointer<vtkLocalQuadricDecimation> decimator =
	  vtkSmartPointer<vtkLocalQuadricDecimation>::New();
	decimator->SetInputData(smoother->GetOutput(0));
	decimator->SetDecimateCellArrayName(this->CellArrayName);
	decimator->SetDecimatePointArrayName(this->PointArrayName);
	decimator->SetTargetReduction(this->DecimationTargetReduction);
	decimator->SetUsePointArray(this->UsePointArray);
	decimator->SetUseCellArray(this->UseCellArray);
	decimator->Update();

	workingPD->DeepCopy(decimator->GetOutput(0));
      }
    }

    output->DeepCopy(workingPD);

    return 1;
}


