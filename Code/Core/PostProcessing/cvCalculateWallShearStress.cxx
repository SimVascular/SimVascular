/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h" 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "cvVTK.h"
#include "cvPolyData.h"
#include "cv_vtk_utils.h"

#include "cvCalculateWallShearStress.h"

// -----------------
// cvCalculateWallShearStress
// -----------------

cvCalculateWallShearStress::cvCalculateWallShearStress() {
    wallshear_ = NULL;
    surfaceMesh_ = NULL;
    tensors_ = NULL;
    
}


// --------------------
// ~cvConvertVisFiles
// --------------------

cvCalculateWallShearStress::~cvCalculateWallShearStress() {
    if (wallshear_ != NULL) {
        wallshear_->Delete();
    }
}


// ------------------
//   SetSurfaceMesh
// ------------------

int cvCalculateWallShearStress::SetSurfaceMesh(cvPolyData* surfaceMesh) {
    surfaceMesh_ = NULL;
    surfaceMesh_ = surfaceMesh->GetVtkPolyData();
    return CV_OK; 
}

// --------------
//   SetTensors
// --------------

int cvCalculateWallShearStress::SetTensors(cvPolyData* tensors) {
    tensors_ = NULL;
    tensors_ = tensors->GetVtkPolyData();
    return CV_OK; 
}

// ---------------
//   SetTractions
// ---------------

int cvCalculateWallShearStress::SetTractions(cvPolyData* tractions) {
    tractions_ = NULL;
    tractions_ = tractions->GetVtkPolyData();
    return CV_OK; 
}

// -----------------------------
//   CalcWallShearFromStresses
// -----------------------------

int cvCalculateWallShearStress::CalcWallShearFromStresses() {

    int i;

    int numPts = surfaceMesh_->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);
    // get the normals
    vtkDataArray* normals = surfaceMesh_->GetPointData()->GetNormals();
    // get the stress tensors
    vtkDataArray* stresses = tensors_->GetPointData()->GetTensors();

    // figure out which nodes are on surface
    int* ptOnSurf = new int[numPts];

    for (i = 0; i < numPts; i++) {
        ptOnSurf[i] = 0;
    }

    vtkCellArray* polys = surfaceMesh_->GetPolys();
    int numCells = polys->GetNumberOfCells();
    fprintf(stdout,"numCells: %i\n",numCells);

    polys->InitTraversal();
    vtkIdType celltype = 0;
    vtkIdType *ids;
    for (i = 0; i < numCells; i++) {
        polys->GetNextCell(celltype,ids);
        //fprintf(stdout,"checking cell: %i type: %i\n",i,celltype);
        if (celltype != 3) {
            return CV_ERROR;
        }
        ptOnSurf[ids[0]] = 1;
        ptOnSurf[ids[1]] = 1;
        ptOnSurf[ids[2]] = 1;
    } 
        
    int numPtsOnSurf = 0;
    for (i = 0; i < numPts; i++) {
        if (ptOnSurf[i] != 0) {
            numPtsOnSurf++;
        }
    }
    fprintf(stdout,"it appears %i pts are on the surface out of %i\n",numPtsOnSurf,numPts);

    // create return vtk vector
    wallshear_ = vtkFloatingPointArrayType::New();
    wallshear_->SetNumberOfComponents(3);
    wallshear_->Allocate(numPts,10000);
    wallshear_->Initialize();

    vtkFloatingPointType nrm[3];
    vtkFloatingPointType stress[9];
    double t[3];
    double t_dot_n = 0.0;    
    double twall[3];

    if (stresses->GetNumberOfComponents() != 9) {
        wallshear_->Delete();
        return CV_ERROR;
    }

    for (i = 0; i < numPts; i++) {
        if (ptOnSurf[i] == 0) {
           wallshear_->InsertNextTuple3(0.0,0.0,0.0);
        } else {
           // get outward normal
           normals->GetTuple(i,nrm);
           stresses->GetTuple(i,stress);
           t[0] = stress[0]*nrm[0]+stress[1]*nrm[1]+stress[2]*nrm[2];
           t[1] = stress[3]*nrm[0]+stress[4]*nrm[1]+stress[5]*nrm[2];
           t[2] = stress[6]*nrm[0]+stress[7]*nrm[1]+stress[8]*nrm[2];
           t_dot_n = t[0]*nrm[0]+t[1]*nrm[1]+t[2]*nrm[2];
           twall[0] = t[0] - t_dot_n*nrm[0];
           twall[1] = t[1] - t_dot_n*nrm[1];
           twall[2] = t[2] - t_dot_n*nrm[2]; 
           wallshear_->InsertNextTuple3(twall[0],twall[1],twall[2]);
        }
    }

    return CV_OK;

}


// -----------------------------
//   CalcWallShearFromTractions
// -----------------------------

int cvCalculateWallShearStress::CalcWallShearFromTractions() {

    int i;

    int numPts = surfaceMesh_->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);
    // get the normals
    vtkDataArray* normals = surfaceMesh_->GetPointData()->GetNormals();
    // get the tractions
    vtkDataArray* tractions = tractions_->GetPointData()->GetVectors();

    // figure out which nodes are on surface
    int* ptOnSurf = new int[numPts];

    for (i = 0; i < numPts; i++) {
        ptOnSurf[i] = 0;
    }

    vtkCellArray* polys = surfaceMesh_->GetPolys();
    int numCells = polys->GetNumberOfCells();
    fprintf(stdout,"numCells: %i\n",numCells);

    polys->InitTraversal();
    vtkIdType celltype = 0;
    vtkIdType *ids;
    for (i = 0; i < numCells; i++) {
        polys->GetNextCell(celltype,ids);
        //fprintf(stdout,"checking cell: %i type: %i\n",i,celltype);
        if (celltype != 3) {
            return CV_ERROR;
        }
        ptOnSurf[ids[0]] = 1;
        ptOnSurf[ids[1]] = 1;
        ptOnSurf[ids[2]] = 1;
    } 
        
    int numPtsOnSurf = 0;
    for (i = 0; i < numPts; i++) {
        if (ptOnSurf[i] != 0) {
            numPtsOnSurf++;
        }
    }
    fprintf(stdout,"it appears %i pts are on the surface out of %i\n",numPtsOnSurf,numPts);

    // create return vtk vector
    wallshear_ = vtkFloatingPointArrayType::New();
    wallshear_->SetNumberOfComponents(3);
    wallshear_->Allocate(numPts,10000);
    wallshear_->Initialize();

    vtkFloatingPointType nrm[3];
    vtkFloatingPointType tf[3];
    double t[3];
    double t_dot_n = 0.0;    
    double twall[3];

    for (i = 0; i < numPts; i++) {
        if (ptOnSurf[i] == 0) {
           wallshear_->InsertNextTuple3(0.0,0.0,0.0);
        } else {
           // get outward normal
           normals->GetTuple(i,nrm);
           tractions->GetTuple(i,tf);
           t[0] = tf[0];
           t[1] = tf[1];
           t[2] = tf[2];
           t_dot_n = t[0]*nrm[0]+t[1]*nrm[1]+t[2]*nrm[2];
           twall[0] = t[0] - t_dot_n*nrm[0];
           twall[1] = t[1] - t_dot_n*nrm[1];
           twall[2] = t[2] - t_dot_n*nrm[2]; 
           wallshear_->InsertNextTuple3(twall[0],twall[1],twall[2]);
        }
    }

    return CV_OK;

}


cvPolyData* cvCalculateWallShearStress::GetWallShear() {
    vtkPolyData* pd = vtkPolyData::New();
    pd->SetPoints(surfaceMesh_->GetPoints());
    //pd->CopyStructure(surfaceMesh_);
    pd->GetPointData()->SetVectors(wallshear_);
    cvPolyData* reposobj = new cvPolyData(pd);
    return reposobj;
}


cvPolyData* cvCalculateWallShearStress::CalcWallShearMean(int numPds, cvPolyData **shearPds) {

    int i = 0;
    int j = 0;
    vtkFloatingPointType shear[3];    

    fprintf(stdout,"numPds: %i\n",numPds);

    // all of the shear pds must have the same num pts
    int numPts = shearPds[0]->GetVtkPolyData()->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);

    // create a list of the shear vectors
    vtkDataArray **vectors = new vtkDataArray*[numPds];
    for (i = 0; i < numPds; i++) {
      vectors[i]=shearPds[i]->GetVtkPolyData()->GetPointData()->GetVectors();
    } 

    // create return vtk vector
    vtkFloatingPointArrayType *shearmean = vtkFloatingPointArrayType::New();
    shearmean->SetNumberOfComponents(1);
    shearmean->Allocate(numPts,10000);
    shearmean->Initialize();

    for (i = 0; i < numPts; i++) {
        double v0 = 0.0;
        double v1 = 0.0;
        double v2 = 0.0;
        for (j = 0; j < numPds; j++) {
            vectors[j]->GetTuple(i,shear);
            v0 += shear[0];v1 += shear[1];v2 += shear[2];
        }
        v0 = v0/numPds; v1 = v1/numPds; v2 = v2/numPds;
        vtkFloatingPointType mag = sqrt(v0*v0+v1*v1+v2*v2);
        shearmean->InsertNextTuple1(mag);
    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    if (surfaceMesh_ == NULL) {
      pd->SetPoints(shearPds[0]->GetVtkPolyData()->GetPoints());
    } else {
      pd->CopyStructure(surfaceMesh_);
    }
    pd->GetPointData()->SetScalars(shearmean);
    cvPolyData* reposobj = new cvPolyData(pd);
    return reposobj;

}

cvPolyData* cvCalculateWallShearStress::CalcWallShearPulse(int numPds, cvPolyData **shearPds){

    int i = 0;
    int j = 0;
    vtkFloatingPointType shear[3];    

    fprintf(stdout,"numPds: %i\n",numPds);

    // all of the shear pds must have the same num pts
    int numPts = shearPds[0]->GetVtkPolyData()->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);

    // create a list of the shear vectors
    vtkDataArray **vectors = new vtkDataArray*[numPds];
    for (i = 0; i < numPds; i++) {
      vectors[i]=shearPds[i]->GetVtkPolyData()->GetPointData()->GetVectors();
    } 

    // create return vtk vector
    vtkFloatingPointArrayType *shearpulse = vtkFloatingPointArrayType::New();
    shearpulse->SetNumberOfComponents(1);
    shearpulse->Allocate(numPts,10000);
    shearpulse->Initialize();

    for (i = 0; i < numPts; i++) {
        vtkFloatingPointType mag = 0.0;
        for (j = 0; j < numPds; j++) {
            vectors[j]->GetTuple(i,shear);
            mag += sqrt(shear[0]*shear[0]+shear[1]*shear[1]+shear[2]*shear[2]);
        }
        mag = mag/numPds;
        shearpulse->InsertNextTuple1(mag);
    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    if (surfaceMesh_ == NULL) {
      pd->SetPoints(shearPds[0]->GetVtkPolyData()->GetPoints());
    } else {
      pd->CopyStructure(surfaceMesh_);
    }
    pd->GetPointData()->SetScalars(shearpulse);
    cvPolyData* reposobj = new cvPolyData(pd);
    return reposobj;

}

cvPolyData* cvCalculateWallShearStress::CalcOSI(cvPolyData *shearMean, cvPolyData *shearPulse) {

    int i = 0;
    vtkFloatingPointType mean = 0.0;
    vtkFloatingPointType pulse = 0.0;

    // all of the shear pds must have the same num pts
    int numPts = shearMean->GetVtkPolyData()->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);

    // create a list of the shear vectors
    vtkDataArray *meanscalars = shearMean->GetVtkPolyData()->GetPointData()->GetScalars();
    vtkDataArray *pulsescalars = shearPulse->GetVtkPolyData()->GetPointData()->GetScalars(); 

    // create return vtk vector
    vtkFloatingPointArrayType *osiScalars = vtkFloatingPointArrayType::New();
    osiScalars->SetNumberOfComponents(1);
    osiScalars->Allocate(numPts,10000);
    osiScalars->Initialize();

    for (i = 0; i < numPts; i++) {
        vtkFloatingPointType osi = 0.0;
        mean  = 0;
        pulse  = 0;
        meanscalars->GetTuple(i,&mean);
        pulsescalars->GetTuple(i,&pulse);
        if (pulse <= 0.00001) {
            //fprintf(stderr,"warning: pulse set to 0.\n");
            osi = 0;
        } else {
            osi = 1.0/2.0*(1-mean/pulse);
        }
        osiScalars->InsertNextTuple1(osi);
    }

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    if (surfaceMesh_ == NULL) {
      pd->SetPoints(shearMean->GetVtkPolyData()->GetPoints());
    } else {
      pd->CopyStructure(surfaceMesh_);
    }
    pd->GetPointData()->SetScalars(osiScalars);
    cvPolyData* reposobj = new cvPolyData(pd);
    return reposobj;

}


cvPolyData* cvCalculateWallShearStress::CalcAvgPointData(int numPds, cvPolyData **inputPds) {

    int i = 0;
    int j = 0;
    vtkFloatingPointType vec[3];    
    vtkFloatingPointType scal=0;

    fprintf(stdout,"numPds: %i\n",numPds);

    // all of the shear pds must have the same num pts
    int numPts = inputPds[0]->GetVtkPolyData()->GetNumberOfPoints();
    fprintf(stdout,"numPts: %i\n",numPts);

    // create a list of the shear vectors
    vtkDataArray **vectors = new vtkDataArray*[numPds];
    vtkDataArray **scalars = new vtkDataArray*[numPds];

    for (i = 0; i < numPds; i++) {
      vectors[i]=inputPds[i]->GetVtkPolyData()->GetPointData()->GetVectors();
      scalars[i]=inputPds[i]->GetVtkPolyData()->GetPointData()->GetScalars();
    } 

    // create return vtk scalar array
    vtkFloatingPointArrayType *avgscalar = vtkFloatingPointArrayType::New();
    avgscalar->SetNumberOfComponents(1);
    avgscalar->Allocate(numPts,1000);
    avgscalar->Initialize();

    // create return vtk vector array
    vtkFloatingPointArrayType *avgvec = vtkFloatingPointArrayType::New();
    avgvec->SetNumberOfComponents(3);
    avgvec->Allocate(numPts,1000);
    avgvec->Initialize();

    for (i = 0; i < numPts; i++) {
        double v0 = 0.0;
        double v1 = 0.0;
        double v2 = 0.0;
        double s  = 0.0;
        for (j = 0; j < numPds; j++) {
            vectors[j]->GetTuple(i,vec);
            v0 += vec[0];v1 += vec[1];v2 += vec[2];
            s  += scalars[j]->GetTuple1(i);;
        }
        v0 = v0/numPds; v1 = v1/numPds; v2 = v2/numPds;
        s = s/numPds;
        avgscalar->InsertNextTuple1(s);
        avgvec->InsertNextTuple3(v0,v1,v2);
    }

    // free memory
    delete vectors;
    delete scalars;

    // create cvPolyData object to return
    vtkPolyData* pd = vtkPolyData::New();
    if (surfaceMesh_ == NULL) {
      pd->SetPoints(inputPds[0]->GetVtkPolyData()->GetPoints());
    } else {
      pd->CopyStructure(surfaceMesh_);
    }
    pd->GetPointData()->SetScalars(avgscalar);
    pd->GetPointData()->SetVectors(avgvec);
    cvPolyData* reposobj = new cvPolyData(pd);
    return reposobj;

}

