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

#include "SimVascular.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "sv_VTK.h"
#include "sv_PolyData.h"
#include "sv_vtk_utils.h"

#include "sv2_CalculateTKE.h"

// --------------
// cvCalculateTKE
// --------------

cvCalculateTKE::cvCalculateTKE() {
    inputVectors_ = NULL;
    averageU_ = NULL;
    rms_ = NULL;
    KE_ = NULL;
    numInputArrays_ = 0;
    numArrayPts_ = 0;
}


// ---------------
// ~cvCalculateTKE
// ---------------

cvCalculateTKE::~cvCalculateTKE() {

    if (inputVectors_ != NULL) {
      delete [] inputVectors_;
    }

    if (averageU_ != NULL) {
        averageU_->Delete();
    }

    if (rms_ != NULL) {
        rms_->Delete();
    }

    if (KE_ != NULL) {
        KE_->Delete();
    }

}


// ------------
// SetInputData
// ------------

int cvCalculateTKE::SetInputData(int numPds, cvPolyData **inputPds) {

    int i = 0;

    fprintf(stdout,"numPds: %i\n",numPds);

    numInputArrays_ = numPds;
    // all of the shear pds must have the same num pts
    numArrayPts_ = inputPds[0]->GetVtkPolyData()->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numArrayPts_);

    // create a list of the shear vectors
    inputVectors_ = new vtkDataArray*[numInputArrays_];

    for (i = 0; i < numInputArrays_; i++) {
      inputVectors_[i]=inputPds[i]->GetVtkPolyData()->GetPointData()->GetVectors();
    }

    points_=inputPds[0]->GetVtkPolyData()->GetPoints();

    return SV_OK;

}


// ------------------------
// CalculateAverageVelocity
// ------------------------

int cvCalculateTKE::CalculateAverageVelocity() {

    int i = 0;
    int j = 0;
    vtkFloatingPointType vel[3];

    // create return vtk vector array
    averageU_ = vtkFloatingPointArrayType::New();
    averageU_->SetNumberOfComponents(3);
    averageU_->Allocate(numArrayPts_,1000);
    averageU_->Initialize();

    for (i = 0; i < numArrayPts_; i++) {
        double v0 = 0.0;
        double v1 = 0.0;
        double v2 = 0.0;
        for (j = 0; j < numInputArrays_; j++) {
            inputVectors_[j]->GetTuple(i,vel);
            v0 += vel[0];v1 += vel[1];v2 += vel[2];
        }
        v0 = v0/numInputArrays_;
        v1 = v1/numInputArrays_;
        v2 = v2/numInputArrays_;
        averageU_->InsertNextTuple3(v0,v1,v2);
    }

    return SV_OK;

}


// ------------
// CalculateTKE
// ------------

int cvCalculateTKE::CalculateTKE() {

    if (averageU_ == NULL) {
       this->CalculateAverageVelocity();
    }

    // create return vtk vector array
    rms_ = vtkFloatingPointArrayType::New();
    rms_->SetNumberOfComponents(3);
    rms_->Allocate(numArrayPts_,1000);
    rms_->Initialize();

    // create return vtk scalar array
    KE_ = vtkFloatingPointArrayType::New();
    KE_->SetNumberOfComponents(1);
    KE_->Allocate(numArrayPts_,1000);
    KE_->Initialize();

    int i = 0;
    int j = 0;
    vtkFloatingPointType avg[3];
    vtkFloatingPointType vel[3];

    for (i = 0; i < numArrayPts_; i++) {
        double v0 = 0.0;
        double v1 = 0.0;
        double v2 = 0.0;
        averageU_->GetTuple(i,avg);
        for (j = 0; j < numInputArrays_; j++) {
            inputVectors_[j]->GetTuple(i,vel);
            v0 = v0 + (vel[0]-avg[0])*(vel[0]-avg[0]);
            v1 = v1 + (vel[1]-avg[1])*(vel[1]-avg[1]);
            v2 = v2 + (vel[2]-avg[2])*(vel[2]-avg[2]);
        }
        v0 = sqrt(v0/numInputArrays_);
        v1 = sqrt(v1/numInputArrays_);
        v2 = sqrt(v2/numInputArrays_);
        rms_->InsertNextTuple3(v0,v1,v2);
        double s = 0.5*((v0*v0)+(v1*v1)+(v2*v2));
        KE_->InsertNextTuple1(s);
    }

    return SV_OK;

}


cvPolyData* cvCalculateTKE::GetAverageVelocityPolyData() {

  if (averageU_ == NULL) {
    this->CalculateAverageVelocity();
  }

  // create cvPolyData object to return
  vtkPolyData* pd = vtkPolyData::New();
  pd->SetPoints(points_);
  pd->GetPointData()->SetVectors(averageU_);
  cvPolyData* reposobj = new cvPolyData(pd);
  return reposobj;

}


cvPolyData* cvCalculateTKE::GetTKEPolyData() {

  if (rms_ == NULL) {
    this->CalculateTKE();
  }

  // create cvPolyData object to return
  vtkPolyData* pd = vtkPolyData::New();
  pd->SetPoints(points_);
  pd->GetPointData()->SetVectors(rms_);
  pd->GetPointData()->SetScalars(KE_);
  cvPolyData* reposobj = new cvPolyData(pd);
  return reposobj;

}

