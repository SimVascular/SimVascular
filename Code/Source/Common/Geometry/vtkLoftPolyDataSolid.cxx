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

/** @file vtkLoftPolyDataSolid.cxx
 *  @brief This is the filter to loft a solid from segmentation groups
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "vtkLoftPolyDataSolid.h"

#include "vtkAlgorithmOutput.h"
#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkDataSetAttributes.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkTrivialProducer.h"
#include "vtkSmartPointer.h"
#include "vtkIntArray.h"
#include "vtkCardinalSpline.h"
#include "vtkKochanekSpline.h"
#include "vtkParametricSpline.h"
#include "vtkSpline.h"

#include "cv_polydatasolid_utils.h"
#include <string>
#include <sstream>
#include <iostream>

vtkStandardNewMacro(vtkLoftPolyDataSolid);

//----------------------------------------------------------------------------
vtkLoftPolyDataSolid::vtkLoftPolyDataSolid()
{
  this->ParallelStreaming = 0;
  this->UserManagedInputs = 0;

  this->UseLinearSampleAlongLength = 1;
  this->UseFFT = 0;
  this->NumLinearPtsAlongLength = 600;
  this->NumModes = 20;

  this->NumOutPtsInSegs = 30;
  this->NumOutPtsAlongLength = 60;

  this->SplineType = 0;
  this->Tension = 0;
  this->Bias = 0;
  this->Continuity = 0;
}

//----------------------------------------------------------------------------
vtkLoftPolyDataSolid::~vtkLoftPolyDataSolid()
{
}

//----------------------------------------------------------------------------
// Add a dataset to the list of data to append.
void vtkLoftPolyDataSolid::AddInputData(vtkPolyData *ds)
{
  if (this->UserManagedInputs)
    {
    vtkErrorMacro(<<
      "AddInput is not supported if UserManagedInputs is true");
    return;
    }
  this->Superclass::AddInputData(ds);
}

//----------------------------------------------------------------------------
// Remove a dataset from the list of data to append.
void vtkLoftPolyDataSolid::RemoveInputData(vtkPolyData *ds)
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

//----------------------------------------------------------------------------
// make ProcessObject function visible
// should only be used when UserManagedInputs is true.
void vtkLoftPolyDataSolid::SetNumberOfInputs(int num)
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

//----------------------------------------------------------------------------
void vtkLoftPolyDataSolid::
SetInputDataByNumber(int num, vtkPolyData* input)
{
  vtkTrivialProducer* tp = vtkTrivialProducer::New();
  tp->SetOutput(input);
  this->SetInputConnectionByNumber(num, tp->GetOutputPort());
  tp->Delete();
}

//----------------------------------------------------------------------------
// Set Nth input, should only be used when UserManagedInputs is true.
void vtkLoftPolyDataSolid::
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

//Function to turn an integer into a string
std::string intToString(int i)
{
  std::stringstream out;
  out << i;
  return out.str();
}

//----------------------------------------------------------------------------
// This method is much too long, and has to be broken up!
// Append data sets into single polygonal data set.
int vtkLoftPolyDataSolid::RequestData(
    vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the info object
  // get the ouptut
  vtkPolyData *output = vtkPolyData::GetData(outputVector, 0);

  int numInputs = inputVector[0]->GetNumberOfInformationObjects();

  vtkPolyData** inputs = new vtkPolyData*[numInputs];
  for (int idx = 0; idx < numInputs; ++idx)
    {
    inputs[idx] = vtkPolyData::GetData(inputVector[0],idx);
    }

  this->LoftSolid(inputs,numInputs,output);

  delete [] inputs;
  return 1;
}

//----------------------------------------------------------------------------
int vtkLoftPolyDataSolid::RequestUpdateExtent(
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
    return 0;
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

  return 1;
}

//----------------------------------------------------------------------------
vtkPolyData *vtkLoftPolyDataSolid::GetInput(int idx)
{
  return vtkPolyData::SafeDownCast(
    this->GetExecutive()->GetInputData(0, idx));
}

//----------------------------------------------------------------------------
void vtkLoftPolyDataSolid::PrintSelf(ostream& os, 
    vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << "ParallelStreaming:" << (this->ParallelStreaming?"On":"Off") << endl;
  os << "UserManagedInputs:" << (this->UserManagedInputs?"On":"Off") << endl;
  os << "UseLinearSampleAlongLength:" << (this->UseLinearSampleAlongLength?"On":"Off") << endl;
  os << "UseFFT:" << (this->UseFFT?"On":"Off") << endl;
  os << "NumLinearPtsAlongLength: " << this->NumLinearPtsAlongLength <<endl;
  os << "NumModes: " << this->NumModes <<endl; 
  os << "NumOutPtsInSegs: " << this->NumOutPtsInSegs <<endl;
  os << "NumOutPtsAlongLength: " << this->NumOutPtsAlongLength <<endl;
}

//----------------------------------------------------------------------------
int vtkLoftPolyDataSolid::FillInputPortInformation(
    int port, vtkInformation *info)
{
  if (!this->Superclass::FillInputPortInformation(port, info))
    {
    return 0;
    }
  info->Set(vtkAlgorithm::INPUT_IS_REPEATABLE(), 1);
  return 1;
}

int vtkLoftPolyDataSolid::LoftSolid(vtkPolyData *inputs[], int numInputs,
    vtkPolyData *outputPD)
{
  vtkKochanekSpline *splineX;
  vtkKochanekSpline *splineY;
  vtkKochanekSpline *splineZ;
  //if (this->SplineType == 0)
  //{
  //  //vtkSmartPointer<vtkCardinalSpline> splineX = 
  //  //  vtkSmartPointer<vtkCardinalSpline>::New();
  //  //vtkSmartPointer<vtkCardinalSpline> splineY = 
  //  //  vtkSmartPointer<vtkCardinalSpline>::New();
  //  //vtkSmartPointer<vtkCardinalSpline> splineZ = 
  //  //  vtkSmartPointer<vtkCardinalSpline>::New();
  //  splineX = vtkCardinalSpline::New();
  //  splineY = vtkCardinalSpline::New();
  //  splineZ = vtkCardinalSpline::New();
  //}
  //else if (this->SplineType == 1) 
  //{
    //vtkSmartPointer<vtkKochanekSpline> splineX = 
    //  vtkSmartPointer<vtkKochanekSpline>::New();
    //vtkSmartPointer<vtkKochanekSpline> splineY = 
    //  vtkSmartPointer<vtkKochanekSpline>::New();
    //vtkSmartPointer<vtkKochanekSpline> splineZ = 
    //  vtkSmartPointer<vtkKochanekSpline>::New();
    splineX = vtkKochanekSpline::New(); 
    splineX->SetDefaultBias(this->Bias); 
    splineX->SetDefaultTension(this->Tension); 
    splineX->SetDefaultContinuity(this->Continuity);
    splineY = vtkKochanekSpline::New();
    splineY->SetDefaultBias(this->Bias); 
    splineY->SetDefaultTension(this->Tension); 
    splineY->SetDefaultContinuity(this->Continuity);
    splineZ = vtkKochanekSpline::New();
    splineZ->SetDefaultBias(this->Bias); 
    splineZ->SetDefaultTension(this->Tension); 
    splineZ->SetDefaultContinuity(this->Continuity);
  //}
       
  double t;
  double tmpPt[3];
  int lenpts=0;
  double **outPts = NULL;
  vtkPoints **sampledPts = new vtkPoints*[this->NumOutPtsAlongLength];
  for (int j=0;j<this->NumOutPtsAlongLength;j++)
  {
    sampledPts[j] = vtkPoints::New();
  }

  for (int i=0; i < this->NumOutPtsInSegs; i++)
  {
    splineX->RemoveAllPoints();
    splineY->RemoveAllPoints();
    splineZ->RemoveAllPoints();

    int numPts = this->NumOutPtsAlongLength;
    for (int n=0; n < numInputs;n++)
    {
      inputs[n]->GetPoint(i,tmpPt);
      splineX->AddPoint(n,tmpPt[0]);
      splineY->AddPoint(n,tmpPt[1]);
      splineZ->AddPoint(n,tmpPt[2]);
    }

    if (this->UseLinearSampleAlongLength == 0)
    {
      outPts = this->createArray(this->NumOutPtsAlongLength,3);
      for (int n=0;n < this->NumOutPtsAlongLength;n++)
      {
	t = (1.0*splineX->GetNumberOfPoints())/
	  (1.0*(this->NumOutPtsAlongLength-1))*n;
	
	outPts[n][0] = splineX->Evaluate(t);
	outPts[n][1] = splineY->Evaluate(t);
	outPts[n][2] = splineZ->Evaluate(t);
      }
    }
    else
    {
      double **pts = this->createArray(this->NumLinearPtsAlongLength,3);
      for (int n=0; n < this->NumLinearPtsAlongLength;n++)
      {
	t = (1.0*splineX->GetNumberOfPoints())/(1.0*(this->NumLinearPtsAlongLength-1))*n;

	pts[n][0] = splineX->Evaluate(t);
	pts[n][1] = splineY->Evaluate(t);
	pts[n][2] = splineZ->Evaluate(t);
      }

    if ((this->linearInterpolateCurve(pts,this->NumLinearPtsAlongLength,
              0,this->NumOutPtsAlongLength,&outPts)) != 1)
      {
      std::cout<<"error in linear interpolation"<<endl;
      this->deleteArray(pts,this->NumLinearPtsAlongLength,3);
      return 0;
      }
    this->deleteArray(pts,this->NumLinearPtsAlongLength,3);
    }

    if (this->UseFFT)
    {
      double firstPt[3]; 
      double lastPt[3];
      for (int r=0;r<3;r++)
      {  
	firstPt[r] = outPts[0][r];
	lastPt[r] = outPts[numPts-1][r];
      }
      int numSmoothPts = 2*this->NumOutPtsAlongLength;
      double **smoothOutPts = NULL;
      double **bigPts = this->createArray(numSmoothPts*2,3);
      for (int r = 0; r < numPts; r++)
      {
	for (int w=0;w<3;w++)
	  bigPts[r][w] = outPts[r][w];
      }
      int count = numPts;
      for (int r = numPts-1; r >= 0; r--)
      {
	for (int w=0;w<3;w++)
	  bigPts[count][w] = outPts[r][w];
	count++;
      }
      if ((this->smoothCurve(bigPts,numSmoothPts,0,this->NumModes,
	  numSmoothPts,&smoothOutPts)) != 1)
      {
	std::cout<<"error in smoothing"<<endl;
	this->deleteArray(outPts,numPts,3);
	this->deleteArray(bigPts,numSmoothPts,3);
	return 0;
      }

      for (int r=0;r<3;r++)
      {
        smoothOutPts[0][r] = firstPt[r];
        smoothOutPts[numPts-1][r] = lastPt[r];
      }

      this->deleteArray(outPts,numPts,3);
      this->deleteArray(bigPts,numSmoothPts,3);
      outPts = NULL;
      if ((this->linearInterpolateCurve(smoothOutPts,numPts,
              0,this->NumOutPtsAlongLength,&outPts)) != 1)
      {
	std::cout<<"error in linear interpolation"<<endl;
        this->deleteArray(smoothOutPts,numSmoothPts,3);
        return 0;
      }
      this->deleteArray(smoothOutPts,numSmoothPts,3);
    }

    for (int j=0;j<this->NumOutPtsAlongLength;j++)
    {
      sampledPts[j]->InsertNextPoint(outPts[j]);
    } 
  } //NumOutPtsInSegs

  vtkSmartPointer<vtkPoints> vPts = 
    vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkIdList> vconnA = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> vconnB = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkPolyData> vPD = 
    vtkSmartPointer<vtkPolyData>::New();

  vPts->Allocate(200,400);
  vconnA->Initialize();
  vconnA->Allocate(200,400);
  vconnB->Initialize();
  vconnB->Allocate(200,400);
  vPD->Initialize();
  vPD->Allocate(200,400);

  for (int i=0;i < this->NumOutPtsAlongLength;i++)
  {
    int numCurvePts = sampledPts[i]->GetNumberOfPoints();
    for (int j=0;j < numCurvePts;j++)
    {
      vPts->InsertNextPoint(sampledPts[i]->GetPoint(j));
      double mypt[3];
      sampledPts[i]->GetPoint(j,mypt);
    }
  }

  vPD->SetPoints(vPts);

  for (int i=0;i < this->NumOutPtsAlongLength-1;i++)
  {
    int numCurvePts = sampledPts[i]->GetNumberOfPoints();
    int offset = i*numCurvePts;
    for (int j=0;j < numCurvePts;j++)
    {
      vconnA->InsertNextId(j + offset);
      if (j == (numCurvePts - 1))
      {
	vconnA->InsertNextId(0 + offset);
	vconnA->InsertNextId(numCurvePts + offset);
	vconnB->InsertNextId(numCurvePts + offset);
      }
      else 
      {
	vconnA->InsertNextId(j + 1 + offset);
	vconnA->InsertNextId(numCurvePts + j + 1 + offset);
	vconnB->InsertNextId(numCurvePts + j + 1 + offset);
      }
      vconnB->InsertNextId(numCurvePts + j + offset);
      vconnB->InsertNextId(j + offset);
      vPD->InsertNextCell(VTK_TRIANGLE,vconnA);
      vPD->InsertNextCell(VTK_TRIANGLE,vconnB);
      vconnA->Initialize();
      vconnB->Initialize();
    }
  }

  outputPD->DeepCopy(vPD);
  outputPD->BuildLinks();

  for (int j=0;j<this->NumOutPtsAlongLength;j++)
  {
    sampledPts[j]->Delete();
  }
  delete [] sampledPts;
  splineX->Delete();
  splineY->Delete();
  splineZ->Delete();

  return 1;
}

// dynamically allocate a 2-dimensional array
double **vtkLoftPolyDataSolid::createArray(int a, int b) {
    double ** rtn = new double*[a+1];
    if (rtn == NULL) {
        printf("ERROR: Memory allocation error.\n");
        return NULL;
    }
    for (int i = 0; i < a+1; i++) {
        rtn[i] = new double[b+1];
        if (rtn[i] == NULL) {
            printf("ERROR:  Memory allocation error.\n");
            return NULL;
        }
        for (int j = 0; j < b + 1; j++) {
            rtn[i][j] = 0.0;
        }
    }
    return rtn;
}

// dynamically deallocate a 2-dimensional array
void vtkLoftPolyDataSolid::deleteArray(double **ptr, int a, int b) {
    for (int i = 0; i < a+1; i++) {
        delete ptr[i];
    }
    delete ptr;
}

int vtkLoftPolyDataSolid::linearInterpolate(double **orgPts, int numOrgPts, double t0, 
                            double dt, int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  Linear interpolation is used, and
    // the values outside of the range of orgPts are fixed to the values
    // at t_0 and t_n.

    int i,j;

    if (numOrgPts <= 0 || numOutPts <= 0) {
        return 0;
    }

    double **outPts = this->createArray(numOutPts,2);
    if (*outPts == NULL) {
        return 0;
    }

    double t;

    for (i=0; i < numOutPts; i++) {

        t = t0 + dt*i;

        outPts[i][0] = t;

        // if time is outside of data range, fix to values at beginning
        // and end of interval
        if (t <= orgPts[0][0]) {
          outPts[i][1] = orgPts[0][1];
          continue;
        } else if (t >= orgPts[numOrgPts-1][0]) {
          outPts[i][1] = orgPts[numOrgPts-1][1];
          continue;
         }

        // interpolate
        for (j = 1; j < numOrgPts; j++) {
          if (t < orgPts[j][0]) {
              double m = (orgPts[j][1]-orgPts[j-1][1])/(orgPts[j][0]-orgPts[j-1][0]);
              outPts[i][1] = m*(t - orgPts[j-1][0]) + orgPts[j-1][1];
              break;
          }   
      }

      if (j == numOrgPts) {
          fprintf(stdout,"Error interpolating point %i.\n",i);
          this->deleteArray(outPts,numOutPts,2);
          return 0;
      }
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1]);
    //}

    *rtnOutPts = outPts;

    return 1;

}

int vtkLoftPolyDataSolid::linearInterpolateCurve(double **orgPts, int numOrgPts, int closed, 
                                     int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  Linear interpolation between the points
    // of the 3D curve is used.

    if (numOrgPts <= 1 || numOutPts <= 2) {
        return 0;
    }

    // find the length of the curve
    double length = 0;
    curveLength(orgPts,numOrgPts,closed,&length);
 
    // now do linear interpolation of each coordinate
    double **xin = this->createArray(numOrgPts+1,2);
    double **yin = this->createArray(numOrgPts+1,2);
    double **zin = this->createArray(numOrgPts+1,2);   
 
    int i;
    double t = 0;
    for (i=0;i < numOrgPts;i++) {
        xin[i][0]=t;xin[i][1]=orgPts[i][0];
        yin[i][0]=t;yin[i][1]=orgPts[i][1];
        zin[i][0]=t;zin[i][1]=orgPts[i][2];
        int j = i+1;
        if (j == numOrgPts) {
            j = 0;
        }
        t += sqrt( (orgPts[j][0]-orgPts[i][0])*(orgPts[j][0]-orgPts[i][0]) +
                   (orgPts[j][1]-orgPts[i][1])*(orgPts[j][1]-orgPts[i][1]) +
                   (orgPts[j][2]-orgPts[i][2])*(orgPts[j][2]-orgPts[i][2]) ); 
    }

    int numPts = numOrgPts;
    double dt = length / (numOutPts-1);
    if (closed != 0) {
        xin[numOrgPts][0]=length;xin[numOrgPts][1]=orgPts[0][0];
        yin[numOrgPts][0]=length;yin[numOrgPts][1]=orgPts[0][1];
        zin[numOrgPts][0]=length;zin[numOrgPts][1]=orgPts[0][2];
        numPts++;
        dt = length / numOutPts;
    }

    // now do linear interpolation on each coordinate
    double **xout = this->createArray(numOutPts,2);
    double **yout = this->createArray(numOutPts,2);
    double **zout = this->createArray(numOutPts,2);

    if (linearInterpolate(xin, numPts, 0, dt, numOutPts, &xout) == 0) {
        this->deleteArray(xin,numOrgPts+1,2); this->deleteArray(xout,numOutPts,2);
        this->deleteArray(yin,numOrgPts+1,2); this->deleteArray(yout,numOutPts,2);
        this->deleteArray(zin,numOrgPts+1,2); this->deleteArray(zout,numOutPts,2);
        return 0;
    }
    if (linearInterpolate(yin, numPts, 0, dt, numOutPts, &yout) == 0) {
        this->deleteArray(xin,numOrgPts+1,2); this->deleteArray(xout,numOutPts,2);
        this->deleteArray(yin,numOrgPts+1,2); this->deleteArray(yout,numOutPts,2);
        this->deleteArray(zin,numOrgPts+1,2); this->deleteArray(zout,numOutPts,2);
        return 0;
    }
    if (linearInterpolate(zin, numPts, 0, dt, numOutPts, &zout) == 0) {
        this->deleteArray(xin,numOrgPts+1,2); this->deleteArray(xout,numOutPts,2);
        this->deleteArray(yin,numOrgPts+1,2); this->deleteArray(yout,numOutPts,2);
        this->deleteArray(zin,numOrgPts+1,2); this->deleteArray(zout,numOutPts,2);
        return 0;
    }

    // put it all back together
    double **outPts = this->createArray(numOutPts,3);
    if (*outPts == NULL) {
        this->deleteArray(xin,numOrgPts+1,2); this->deleteArray(xout,numOutPts,2);
        this->deleteArray(yin,numOrgPts+1,2); this->deleteArray(yout,numOutPts,2); 
        this->deleteArray(zin,numOrgPts+1,2); this->deleteArray(zout,numOutPts,2);
        return 0;
    }
    for (i = 0; i < numOutPts;i++) {
        outPts[i][0]=xout[i][1];
        outPts[i][1]=yout[i][1];
        outPts[i][2]=zout[i][1];
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1],outPts[i][2]);
    //}

    // clean up
    this->deleteArray(xin,numOrgPts+1,2); this->deleteArray(xout,numOutPts,2);
    this->deleteArray(yin,numOrgPts+1,2); this->deleteArray(yout,numOutPts,2); 
    this->deleteArray(zin,numOrgPts+1,2); this->deleteArray(zout,numOutPts,2);

    *rtnOutPts = outPts;

    return 1;

}

int vtkLoftPolyDataSolid::curveLength(double **pts, int numPts, int closed, double *length) {

    // This method takes an original set of points and returns the length
    // of the line 2-D line.  

    // If you specify closed == 1, the curve is assumed to be closed
    // and the distance between the last point and the first is included
    // in the value returned.

    if (numPts <= 1) {
        *length = 0;
        return 0;
    }

    int numSegments;
    if (closed == 0) {
        numSegments=numPts-1;
    } else {
        numSegments=numPts;
    }

    double result = 0;
    for (int i=0; i < numSegments;i++) {
        int j=i+1;
        if (j == numPts) {
            j = 0;
        }
        result += sqrt( (pts[j][0]-pts[i][0])*(pts[j][0]-pts[i][0]) + 
                        (pts[j][1]-pts[i][1])*(pts[j][1]-pts[i][1]) +
                        (pts[j][2]-pts[i][2])*(pts[j][2]-pts[i][2]) );
    }

    *length = result;
    return 1;
}

int vtkLoftPolyDataSolid::smoothCurve(double **orgPts, int numOrgPts, int closed, int keepNumModes, 
                                     int numOutPts, double ***rtnOutPts) {

    // This method takes an original set of points and returns a
    // newly allocated set of interpolated points (where the requested
    // number of points is numOutPts).  A this->FFT is performed on the points
    // and only the requested number of modes are maintained.

    if (numOrgPts <= 1 || numOutPts <= 2) {
        return 0;
    }

    if (keepNumModes < 1) {
        return 0;
    }

    // find the length of the curve
    double length = 0;
    this->curveLength(orgPts,numOrgPts,closed,&length);
 
    // now do linear interpolation of each coordinate
    double **xin = this->createArray(numOrgPts+1,2);
    double **yin = this->createArray(numOrgPts+1,2);
    double **zin = this->createArray(numOrgPts+1,2);   
 
    int i;
    double t = 0;
    for (i=0;i < numOrgPts;i++) {
        xin[i][0]=t;xin[i][1]=orgPts[i][0];
        yin[i][0]=t;yin[i][1]=orgPts[i][1];
        zin[i][0]=t;zin[i][1]=orgPts[i][2];
        int j = i+1;
        if (j == numOrgPts) {
            j = 0;
        }
        t += sqrt( (orgPts[j][0]-orgPts[i][0])*(orgPts[j][0]-orgPts[i][0]) +
                   (orgPts[j][1]-orgPts[i][1])*(orgPts[j][1]-orgPts[i][1]) +
                   (orgPts[j][2]-orgPts[i][2])*(orgPts[j][2]-orgPts[i][2]) ); 
    }

    int numPts = numOrgPts;
    double dt = length / (numOutPts-1);
    if (closed != 0) {
        xin[numOrgPts][0]=length;xin[numOrgPts][1]=orgPts[0][0];
        yin[numOrgPts][0]=length;yin[numOrgPts][1]=orgPts[0][1];
        zin[numOrgPts][0]=length;zin[numOrgPts][1]=orgPts[0][2];
        numPts++;
        dt = length / numOutPts;
    }

    // now do a this->FFT on each coordinate
    double **xmodes;
    double **ymodes;
    double **zmodes;
  
    // need to unhardcore this
    int numInterpPts = 2048;

    if (this->FFT(xin, numPts, numInterpPts, keepNumModes, &xmodes) == 0) {
        this->deleteArray(xin,numOrgPts+1,2);
        this->deleteArray(yin,numOrgPts+1,2);
        this->deleteArray(zin,numOrgPts+1,2);
        return 0;
    }
    this->deleteArray(xin,numOrgPts+1,2);
    if (this->FFT(yin, numPts, numInterpPts, keepNumModes, &ymodes) == 0) {
        this->deleteArray(xmodes,keepNumModes,2);
        this->deleteArray(yin,numOrgPts+1,2);
        this->deleteArray(zin,numOrgPts+1,2);
        return 0;
    }
    this->deleteArray(yin,numOrgPts+1,2);
    if (this->FFT(zin, numPts, numInterpPts, keepNumModes, &zmodes) == 0) {
        this->deleteArray(xmodes,keepNumModes,2);
        this->deleteArray(ymodes,keepNumModes,2);
        this->deleteArray(zin,numOrgPts+1,2);
        return 0;
    }
    this->deleteArray(zin,numOrgPts+1,2);

    double **xout;
    double **yout;
    double **zout;

    double t0 = 0;
    double Pi = 3.1415926535;
    double omega = 2.0*Pi/length;

    if (this->inverseFFT(xmodes, keepNumModes, t0, dt, omega, 
                   numOutPts, &xout) == 0) {
        this->deleteArray(xmodes,keepNumModes,2);
        this->deleteArray(ymodes,keepNumModes,2);
        this->deleteArray(zmodes,keepNumModes,2);
        return 0;
    }
    if (this->inverseFFT(ymodes, keepNumModes, t0, dt, omega, 
                   numOutPts, &yout) == 0) {
        this->deleteArray(xmodes,keepNumModes,2);this->deleteArray(xout,numOutPts,2);
        this->deleteArray(ymodes,keepNumModes,2);
        this->deleteArray(zmodes,keepNumModes,2);
        return 0;
    }
    if (this->inverseFFT(zmodes, keepNumModes, t0, dt, omega, 
                   numOutPts, &zout) == 0) {
        this->deleteArray(xmodes,keepNumModes,2);this->deleteArray(xout,numOutPts,2);
        this->deleteArray(ymodes,keepNumModes,2);this->deleteArray(yout,numOutPts,2);
        this->deleteArray(zmodes,keepNumModes,2);
        return 0;
    }

    // put it all back together
    double **outPts = this->createArray(numOutPts,3);
    if (*outPts == NULL) {
        this->deleteArray(xin,numOrgPts+1,2); this->deleteArray(xout,numOutPts,2);
        this->deleteArray(yin,numOrgPts+1,2); this->deleteArray(yout,numOutPts,2); 
        this->deleteArray(zin,numOrgPts+1,2); this->deleteArray(zout,numOutPts,2);
        return 0;
    }
    for (i = 0; i < numOutPts;i++) {
        outPts[i][0]=xout[i][1];
        outPts[i][1]=yout[i][1];
        outPts[i][2]=zout[i][1];
    }

    // debug
    //for (i = 0; i < numOutPts; i++) {
    //    fprintf(stdout,"%i: %8.3lf %8.3lf %8.3lf\n",i,outPts[i][0],outPts[i][1],outPts[i][2]);
    //}

    // clean up
    this->deleteArray(xout,numOutPts,2);
    this->deleteArray(yout,numOutPts,2); 
    this->deleteArray(zout,numOutPts,2);

    *rtnOutPts = outPts;

    return 1;

}

int vtkLoftPolyDataSolid::inverseFFT(double **terms, int numTerms, double t0, double dt, double omega, 
                         int numRtnPts, double ***rtnPts) {

  int i,j;
  double omega_t;

  double **pts = this->createArray(numRtnPts,2);
  if (pts == NULL) {
      return 0;
  }

  for (i=0;i<numRtnPts;i++) {
    pts[i][0] = t0+i*dt;
    omega_t = omega*i*dt;
    pts[i][1] = terms[0][0];
    for (j=1;j<numTerms;j++) {
      pts[i][1] += terms[j][0]*cos(j*omega_t) + terms[j][1]*sin(j*omega_t);
    }
  }

  *rtnPts = pts;

  return 1;

}

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void vtkLoftPolyDataSolid::FFT(double data[],int nn,int isign) {

	int n,mmax,m,j,istep,i;
	double wtemp,wr,wpr,wpi,wi,theta;
	double tempr,tempi;

	n=nn << 1;
	j=1;
	for (i=1;i<n;i+=2) {
		if (j > i) {
			SWAP(data[j-1],data[i-1]);
			SWAP(data[j+1-1],data[i+1-1]);
		}
		m=n >> 1;
		while (m >= 2 && j > m) {
			j -= m;
			m >>= 1;
		}
		j += m;
	}
	mmax=2;
	while (n > mmax) {
		istep=2*mmax;
		theta=6.28318530717959/(isign*mmax);
		wtemp=sin(0.5*theta);
		wpr = -2.0*wtemp*wtemp;
		wpi=sin(theta);
		wr=1.0;
		wi=0.0;
		for (m=1;m<mmax;m+=2) {
			for (i=m;i<=n;i+=istep) {
				j=i+mmax;
				tempr=wr*data[j-1]-wi*data[j+1-1];
				tempi=wr*data[j+1-1]+wi*data[j-1];
				data[j-1]=data[i-1]-tempr;
				data[j+1-1]=data[i+1-1]-tempi;
				data[i-1] += tempr;
				data[i+1-1] += tempi;
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
		}
		mmax=istep;
	}
}

#undef SWAP

int vtkLoftPolyDataSolid::FFT(double **pts, int numPts, int numInterpPts, int numDesiredTerms, double ***rtnterms) {

    int i;

    if (numInterpPts <= 0 || numDesiredTerms <= 0 || numPts <= 0) {
        return 0;
    }

    double **terms = this->createArray(numDesiredTerms,2);

    if (*terms == NULL) {
        return 0;
    }
   
    // here we calculate dt so that our time series will go from
    // 0 to T - dt.

    double t0 = pts[0][0];
    double dt = (pts[numPts-1][0]-t0)/numInterpPts;
    double **outPts = NULL;

    if (this->linearInterpolate(pts, numPts, t0, dt, numInterpPts, &outPts) == 0) {
        return 0;
    }

    // create a real-imaginary array to do fft
    double *data = new double [2*numInterpPts];
    for (i = 0; i < numInterpPts; i++) {
        data[2*i] = outPts[i][1];
        data[2*i+1] = 0.0;
    }
    this->deleteArray(outPts,numInterpPts,2);

    this->FFT(data,numInterpPts,1);

    terms[0][0] = data[0]/numInterpPts;
    terms[0][1] = data[1]/numInterpPts;

    for (i=1;i<numDesiredTerms;i++) {
      terms[i][0]=2.0*data[2*i]/numInterpPts;
      terms[i][1]=2.0*data[2*i+1]/numInterpPts;
    }

    delete data;

    *rtnterms = terms;

    return 1;

}

