/* Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
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
#include <math.h>
#include <stdlib.h>

#include "cvVTK.h"

int mr_decode (vtkStructuredPoints *phase,
               double venc,double vencscale,
               vtkStructuredPoints **vel) {

  double pi = 3.14159265358979323846;

  vtkDataArray *phaseScalars = phase->GetPointData()->GetScalars();

  int numScalars = phaseScalars->GetNumberOfTuples();

  vtkFloatArray *vScalars = vtkFloatArray::New();
  vScalars->Allocate(100,100);
  vScalars->SetNumberOfTuples(numScalars);

  vtkFloatingPointType v_i;

  for (int pixel=0;pixel < numScalars;pixel++) {
    vtkFloatingPointType phase_i = phaseScalars->GetTuple1(pixel);
    //fprintf(stdout,"venc: %lf vencscale: %lf  flow_i: %f\n",
    //               venc,vencscale,phase_i);
    v_i = phase_i * venc / (vencscale * pi);
    vScalars->InsertTuple1(pixel,v_i);
    //fprintf(stdout,"pixel %i: %f\n",pixel,v_i);
  }

  vtkStructuredPoints *v;
  v = vtkStructuredPoints::New();
  v->CopyStructure(phase);
  // not in vtk-6.0.0??  Do need this??  v->CopyInformation(phase);
  v->GetPointData()->SetScalars(vScalars);
  vtkFloatingPointType origin[3];
  vtkFloatingPointType spacing[3];
  phase->GetOrigin(origin);
  v->SetOrigin(origin);
  phase->GetSpacing(spacing);
  v->SetSpacing(spacing);

  *vel = v;

  return CV_OK;
}


int mr_decode_masked (vtkStructuredPoints *mag,
               vtkStructuredPoints *phase,
               double venc,double vencscale,
               vtkStructuredPoints **vel) {

  double pi = 3.14159265358979323846;

  vtkDataArray *magScalars = mag->GetPointData()->GetScalars();
  vtkDataArray *phaseScalars = phase->GetPointData()->GetScalars();

  int numScalars = magScalars->GetNumberOfTuples();

  vtkFloatArray *vScalars = vtkFloatArray::New();
  vScalars->Allocate(100,100);
  vScalars->SetNumberOfTuples(numScalars);

  vtkFloatingPointType v_i;

  for (int pixel=0;pixel < numScalars;pixel++) {
    vtkFloatingPointType phase_i = phaseScalars->GetTuple1(pixel);
    vtkFloatingPointType mag_i = magScalars->GetTuple1(pixel);
    //fprintf(stdout,"venc: %lf vencscale: %lf  flow_i: %f mag_i: %f\n",
    //               venc,vencscale,phase_i,mag_i);
    
    if (abs(mag_i) < 0.0001) {
        v_i = 0;
    } else {
        v_i = phase_i * venc / (vencscale * pi * mag_i);
    }
    vScalars->InsertTuple1(pixel,v_i);
    //fprintf(stdout,"pixel %i: %f\n",pixel,v_i);
  }

  vtkStructuredPoints *v;
  v = vtkStructuredPoints::New();
  v->CopyStructure(mag);
  // not in vtk-6.0.0, do we need this?  v->CopyInformation(mag);
  v->GetPointData()->SetScalars(vScalars);
  vtkFloatingPointType origin[3];
  vtkFloatingPointType spacing[3];
  mag->GetOrigin(origin);
  v->SetOrigin(origin);
  mag->GetSpacing(spacing);
  v->SetSpacing(spacing);

  *vel = v;

  return CV_OK;
}

