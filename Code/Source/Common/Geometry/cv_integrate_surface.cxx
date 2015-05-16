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
#include <math.h>

#include "cvVTK.h"
#include "cvPolyData.h"
#include "cv_vtk_utils.h"

#include "vtkIntegrateAttributes.h"
#include "vtkIntegrateFlowThroughSurface.h"

int gdscCalcU(double xx[3][5], double *d, double r, double s, double *u) {

  double uval;
  int i;
  double rp,sp,rm,sm;
  double h[5];

  // routine to calcuate the value of u at (r,s)

  rp = 1.0 + r;
  sp = 1.0 + s;
  rm = 1.0 - r;
  sm = 1.0 - s;
  
  // interpolation functions

  h[1] = 0.25 * rp * sp;
  h[2] = 0.25 * rm * sp;
  h[3] = 0.25 * rm * sm;
  h[4] = 0.25 * rp * sm;

  uval = 0.0;
  for (i = 1;i <= 4; i++) {
      uval = uval + h[i]*d[i];
  }

  *u = uval;

  return CV_OK;

}
    

int gdscCalcJacDet(double xx[3][5], double r, double s, double *determinant) {

  /**************************************************************
   *  The code below was adapted from Finite Element Procedures *
   *  by K. Bathe, 1996, pages 482-3.                           *
   **************************************************************/

  *determinant = 0.0;

  int i,j,k;
  double rp,sp,rm,sm;
  double h[5],p[3][5],xj[3][3];
  double dum,det;

  rp = 1.0 + r;
  sp = 1.0 + s;
  rm = 1.0 - r;
  sm = 1.0 - s;
  
  // interpolation functions

  h[1] = 0.25 * rp * sp;
  h[2] = 0.25 * rm * sp;
  h[3] = 0.25 * rm * sm;
  h[4] = 0.25 * rp * sm;

  //
  // natural coordinate derviatives of the interpolation functions
  //

  // with respect to r

  p[1][1] = 0.25 * sp;
  p[1][2] = -p[1][1];
  p[1][3] = -0.25 * sm;
  p[1][4] = -p[1][3];

  // with respect to s

  p[2][1] = 0.25 * rp;
  p[2][2] = 0.25 * rm;
  p[2][3] = -p[2][2];
  p[2][4] = -p[2][1];

  // evaluate the jacobian at point (r,s)

  for (i = 1; i <= 2; i++) {
    for (j = 1; j <= 2; j++) {
      dum = 0;
      for (k = 1; k <= 4; k++) {
          dum = dum + p[i][k]*xx[j][k];
      }
      xj[i][j]=dum;
    }
  }

  // compute the determinant of the jacobian at point (r,s)
  det = xj[1][1]*xj[2][2] - xj[2][1]*xj[1][2];
  if (det < 0.0) {
    //fprintf(stderr,"ERROR: Jacobian determinant negative! (%lf)\n"
    //     ,det);
    return CV_ERROR;
  }  

  *determinant = det;
  return CV_OK;

}


int gdscIntegrateSurfElem(vtkFloatingPointType crd[4][3], vtkFloatingPointType *uvalues, double *q) {

  // we map from the more intuitive 3-d coordinates of the
  // the cell as defined in vtk to the arrays used in the code
  // below

  int i,j;
  double qflow = 0.0;
  double xx[3][5],d[5];
  double u,det;

  // 2-point gaussian integration point locations
  // note that for 2 pt gauss integration the weights are 1.0
  double a[3];
  a[0] = 0.0;
  a[1] = 0.577350269189626;
  a[2] = -a[1];

  *q = qflow;

  for (i = 1; i <= 4; i++) {
      for (j = 1; j <= 2; j++) {
          xx[j][i]=crd[i-1][j-1];
      }
  }

  // note that all of the code for using shape functions assumes
  // the indexes vary from 1 to 4, while the input to this
  // routine goes from 0 to 3.

  for (i = 1; i <= 4; i++) {
      d[i] = uvalues[i-1];
  }

  // calculate a sample determinant, and rearrange points 
  // if det. is negative.
  if (gdscCalcJacDet(xx,a[1],a[1],&det) == CV_ERROR) {
      double swapd,swapxx[3];
      // swap point 1 -> 4
      swapxx[1] = xx[1][1] ; swapxx[2] = xx[2][1];   
      xx[1][1] = xx[1][4] ; xx[2][1] = xx[2][4];
      xx [1][4] = swapxx[1] ; xx[2][4] = swapxx[2];
      // swap point 2 -> 3
      swapxx[1] = xx[1][2] ; swapxx[2] = xx[2][2];   
      xx[1][2] = xx[1][3] ; xx[2][2] = xx[2][3];
      xx [1][3] = swapxx[1] ; xx[2][3] = swapxx[2];
      // swap d1 -> d4
      swapd = d[1];
      d[1] = d[4];
      d[4] = swapd;
      // swap d2 -> d3
      swapd = d[2];
      d[2] = d[3];
      d[3] = swapd;
      //fprintf(stderr,"Swapped points.\n");
  }

  qflow = 0.0;
  for (i = 1; i <= 2; i++) {
    for (j = 1; j <= 2; j++) {
      if (gdscCalcU(xx,d,a[i],a[j],&u) == CV_ERROR) {
        fprintf(stderr,"ERROR: Problem calculating u.\n");
        return CV_ERROR;
      }
      if (gdscCalcJacDet(xx,a[i],a[j],&det) == CV_ERROR) {
        fprintf(stderr,"ERROR: Jacobian determinant negative! (%lf)\n",det);
        return CV_ERROR;
      }
      if (det < 1E-6) {
          fprintf(stderr,"Warning: ignoring small element with det of (%e).\n",det);
          qflow = 0.0;
          *q = qflow;
          return CV_OK;
      } else {
        qflow = qflow + u*det;
      }
    }
  }
 
  *q = qflow;
  return CV_OK;

}


// -------------------------
// sys_geom_IntegrateSurface
// -------------------------

int sys_geom_IntegrateSurface( cvPolyData *src, int tensorType, double *nrm, double *q )
{

  int i,j;
  vtkPolyData *pd;
  vtkDataArray *scalars = NULL;
  vtkDataArray *vectors = NULL;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;
  double qflow;
  double qtotal = 0.0;
  *q = qtotal;
  vtkFloatingPointType crd[4][3];

  pd = src->GetVtkPolyData();

  if (tensorType < 0 || tensorType > 1) {
      fprintf(stderr,"ERROR:  Invalid tensorType (%i).\n",tensorType);
      return CV_ERROR;
  }
  if (tensorType == 0) {
    scalars = pd->GetPointData()->GetScalars();
    if (scalars == NULL) {
        fprintf(stderr,"ERROR: No scalars!\n");
        return CV_ERROR;
    } 
  } else {
    vectors = pd->GetPointData()->GetVectors();
    if (vectors == NULL) {
        fprintf(stderr,"ERROR: No vectors!\n");
        return CV_ERROR;
    } 
  }

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    return CV_ERROR;
  }

  int polyIndex = 0;
  int conn[4];

  for (i = 0; i < numPolys; i++) {
      int nElemNodes = polys[polyIndex++];
      if (nElemNodes < 3 || nElemNodes > 4) {
          fprintf(stderr,"ERROR:  Invalid number of nodes in element (%i).\n",nElemNodes);
          return CV_ERROR;
      }         

      conn[0]=polys[polyIndex++];
      conn[1]=polys[polyIndex++];
      conn[2]=polys[polyIndex++];
      if (nElemNodes == 3) {
          conn[3]=conn[2];
      } else {
          conn[3]=polys[polyIndex++];
      }

      for (j = 0; j < 4; j++) {
          crd[j][0]=pts[conn[j]*3+0];
          crd[j][1]=pts[conn[j]*3+1];
          crd[j][2]=pts[conn[j]*3+2];
      }

      vtkFloatingPointType uvalues[5];
      // if tensorType = 0, scalar is assumed to be through plane component
      if (tensorType == 0) {
          for (j = 0; j < 4; j++) {
              uvalues[j] = scalars->GetTuple1(conn[j]);
          }
      } else {
          // tensorType = 1, we need to dot the velocity vector with the surface normal
          for (j = 0; j < 4; j++) {
              vtkFloatingPointType v[3];
              vectors->GetTuple(conn[j],v);
              uvalues[j] = nrm[0]*v[0]+nrm[1]*v[1]+nrm[2]*v[2];
          }
      }

      qflow = 0.0;    
      if (gdscIntegrateSurfElem(crd, uvalues, &qflow) == CV_ERROR) {
          fprintf(stderr,"ERROR:  Problem calculating surface integral.\n");
          *q = 0.0;
          return CV_ERROR;
      }
      qtotal = qtotal + qflow;
  }

  *q = qtotal;

  return CV_OK;
}


// --------------------------
// sys_geom_IntegrateSurface2
// --------------------------

int sys_geom_IntegrateSurface2( cvPolyData *src, int tensorType, double *q, double *area )
{

  int i,j;
  vtkPolyData *pd;
  vtkDataArray *scalars = NULL;
  vtkDataArray *vectors = NULL;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;
  double qflow;
  double qtotal = 0.0;
  double areatotal = 0.0;
  *q = qtotal;
  *area = areatotal;

  pd = src->GetVtkPolyData();

  if (tensorType < 0 || tensorType > 1) {
      fprintf(stderr,"ERROR:  Invalid tensorType (%i).\n",tensorType);
      return CV_ERROR;
  }
  if (tensorType == 0) {
    scalars = pd->GetPointData()->GetScalars();
    if (scalars == NULL) {
        fprintf(stderr,"ERROR: No scalars!\n");
        return CV_ERROR;
    } 
  } else {
    vectors = pd->GetPointData()->GetVectors();
    if (vectors == NULL) {
        fprintf(stderr,"ERROR: No vectors!\n");
        return CV_ERROR;
    } 
  }

  // make sure we have normals on pd
  pd = src->GetVtkPolyData();

  vtkUnstructuredGrid* answer;

  if (tensorType == 1) {
    vtkIntegrateFlowThroughSurface* integrator = vtkIntegrateFlowThroughSurface::New();
    integrator->SetInputDataObject(pd);
    integrator->Update();
    answer = integrator->GetOutput();
    qtotal = ((answer->GetPointData())->GetArray("Surface Flow"))->GetTuple1(0);
    areatotal = ((answer->GetCellData())->GetArray("Area"))->GetTuple1(0);
    integrator->Delete();
  } else {
    vtkIntegrateAttributes* integrateAtts = vtkIntegrateAttributes::New();
    integrateAtts->SetInputDataObject(pd);
    integrateAtts->Update();
    answer = integrateAtts->GetOutput();
    if ( !((answer->GetPointData())->HasArray( ((pd->GetPointData())->GetScalars())->GetName()))) {
      fprintf(stderr,"ERROR:  no scalar point data!\n");
      return CV_ERROR;
    }
    if (!((answer->GetCellData())->HasArray("Area"))) {
      fprintf(stderr,"ERROR:  no area cell data!\n");
      return CV_ERROR;
    }
    qtotal = ((answer->GetPointData())->GetArray( ((pd->GetPointData())->GetScalars())->GetName()))->GetTuple1(0);
    areatotal = ((answer->GetCellData())->GetArray("Area"))->GetTuple1(0);
    integrateAtts->Delete();
  }

  *q = qtotal;
  *area = areatotal;

  return CV_OK;
}


// ----------------------------
// sys_geom_IntegrateScalarSurf
// ----------------------------

int sys_geom_IntegrateScalarSurf( cvPolyData *src, double *q )
{

  int i,j;
  vtkPolyData *pd;
  vtkDataArray *scalars = NULL;
  vtkIdType celltype = 0;
  vtkIdType *ids;
  int numPolys;
  double qflow;
  double qtotal = 0.0;
  *q = qtotal;

  int subId = 0;
  double pcoords[3];
  double x[3];
  double w[3];
  double F[3];
 
  // gaussian weights  (pg 467 Bathe)
  double r[3],s[3],gaussW[3];
  r[0] = 0.16666666666667;
  r[1] = 0.66666666666667;
  r[2] = 0.16666666666667;
  s[0] = 0.16666666666667;
  s[1] = 0.16666666666667;
  s[2] = 0.66666666666667;
  gaussW[0] = 0.3333333333333;
  gaussW[1] = 0.3333333333333;
  gaussW[2] = 0.3333333333333;

  pd = src->GetVtkPolyData();

  if (pd == NULL) {
    fprintf(stderr,"ERROR: No polydata!\n");
    return CV_ERROR;
  }

  scalars = pd->GetPointData()->GetScalars();

  if (scalars == NULL) {
    fprintf(stderr,"ERROR: No scalars!\n");
    return CV_ERROR;
  }

  numPolys = pd->GetNumberOfPolys();
  if (numPolys == 0) {
    fprintf(stderr,"ERROR: No polys!\n");
    return CV_ERROR;
  }

  vtkCellArray *polys = pd->GetPolys();

  int numCells = polys->GetNumberOfCells();
  if (numCells != numPolys) {
    fprintf(stderr,"ERROR: num cells not equal to num polys!\n");
    return CV_ERROR;
  }

  // dummy id list
  vtkIdList *myids = vtkIdList::New();
  myids->Allocate(10,10);
  myids->InsertNextId(0);myids->InsertNextId(1);myids->InsertNextId(2);
//  vtkGenericCell *mycell = vtkGenericCell::New();
//  mycell->SetCellTypeToTriangle();
  vtkTriangle *mycell = vtkTriangle::New();

  vtkPoints *mypts = vtkPoints::New();
  mypts->Allocate(100,100);
  mypts->SetNumberOfPoints(3);

  mycell->Points = mypts;
  mycell->PointIds = myids;
   
  polys->InitTraversal(); 
  for (i = 0; i < numPolys; i++) {
    polys->GetNextCell(celltype,ids);
    if (celltype != 3) {
      fprintf(stderr,"ERROR: invalid cell type (%i)\n",celltype);
      myids->Delete();
      mypts->Delete();
      return CV_ERROR;
    }

    double xx[3][3];
    for (j = 0; j < 3; j++) {
      double p[3];
      pd->GetPoints()->GetPoint(ids[j],p);
      pd->GetPoints()->GetPoint(ids[j],xx[j]);
      mypts->SetPoint(j,p);
    }

    // calculate area since determinant is 2*area for a tri
    double area = vtkTriangle::TriangleArea(xx[0],xx[1],xx[2]);
    double det = area*2.0;

    for (j = 0; j < 3; j++) {
      pcoords[0] = 1- r[j] - s[j];
      pcoords[1] = r[j];
      pcoords[2] = s[j];
      mycell->EvaluateLocation(subId,pcoords,x,w);
//      fprintf(stdout,"x: %lf %lf %lf\n",x[0],x[1],x[2]);
//      fprintf(stdout,"w: %lf %lf %lf\n",w[0],w[1],w[2]);
//      fprintf(stdout,"p1: %lf %lf %lf\n",pa[j][0],pa[j][1],pa[j][2]);
      F[j] = 0.0;
      for (int k = 0; k < 3; k++) {
//          fprintf(stdout,"scalar[%i] %lf\n",k,scalars->GetTuple1(ids[k]));
        F[j] += scalars->GetTuple1(ids[k])*w[k]*det;
      }
//      fprintf(stdout,"f[%i] = %lf  area = %lf\n",j,F[j],area);
    }

    qflow = 0.5*(F[0]*gaussW[0]+F[1]*gaussW[1]+F[2]*gaussW[2]);

//  using the average * area produces identical results for linear tri
//    qflow = area*(scalars->GetTuple1(ids[0])+scalars->GetTuple1(ids[1])+scalars->GetTuple1(ids[2]))/3.0;

    //fprintf(stdout,"cell %i  qflow  %lf  area %lf\n",i,qflow,vtkTriangle::TriangleArea(xx[0],xx[1],xx[2]));
                
    qtotal = qtotal + qflow;
  }

  *q = qtotal;

  myids->Delete();
  mypts->Delete();

  return CV_OK;
}


// ------------------------------
// sys_geom_IntegrateScalarThresh
// ------------------------------

int sys_geom_IntegrateScalarThresh( cvPolyData *src, double wssthresh, double *q, double *a )
{

  int i,j;
  vtkPolyData *pd;
  vtkDataArray *scalars = NULL;
  vtkIdType celltype = 0;
  vtkIdType *ids;
  int numPolys;
  double qflow;
  double qtotal = 0.0;
  double atotal = 0.0;
  *q = qtotal;
  *a = atotal;

  int subId = 0;
  double pcoords[3];
  double x[3];
  double w[3];
  double F[3];
 
  // gaussian weights  (pg 467 Bathe)
  double r[3],s[3],gaussW[3];
  r[0] = 0.16666666666667;
  r[1] = 0.66666666666667;
  r[2] = 0.16666666666667;
  s[0] = 0.16666666666667;
  s[1] = 0.16666666666667;
  s[2] = 0.66666666666667;
  gaussW[0] = 0.3333333333333;
  gaussW[1] = 0.3333333333333;
  gaussW[2] = 0.3333333333333;

  pd = src->GetVtkPolyData();

  if (pd == NULL) {
    fprintf(stderr,"ERROR: No polydata!\n");
    return CV_ERROR;
  }

  scalars = pd->GetPointData()->GetScalars();

  if (scalars == NULL) {
    fprintf(stderr,"ERROR: No scalars!\n");
    return CV_ERROR;
  }

  numPolys = pd->GetNumberOfPolys();
  if (numPolys == 0) {
    fprintf(stderr,"ERROR: No polys!\n");
    return CV_ERROR;
  }

  vtkCellArray *polys = pd->GetPolys();

  int numCells = polys->GetNumberOfCells();
  if (numCells != numPolys) {
    fprintf(stderr,"ERROR: num cells not equal to num polys!\n");
    return CV_ERROR;
  }

  // dummy id list
  vtkIdList *myids = vtkIdList::New();
  myids->Allocate(10,10);
  myids->InsertNextId(0);myids->InsertNextId(1);myids->InsertNextId(2);
  vtkTriangle *mycell = vtkTriangle::New();

  vtkPoints *mypts = vtkPoints::New();
  mypts->Allocate(100,100);
  mypts->SetNumberOfPoints(3);

  mycell->Points = mypts;
  mycell->PointIds = myids;
   
  polys->InitTraversal(); 
  for (i = 0; i < numPolys; i++) {
    polys->GetNextCell(celltype,ids);
    if (celltype != 3) {
      fprintf(stderr,"ERROR: invalid cell type (%i)\n",celltype);
      myids->Delete();
      mypts->Delete();
      return CV_ERROR;
    }

    double xx[3][3];
    for (j = 0; j < 3; j++) {
      double p[3];
      pd->GetPoints()->GetPoint(ids[j],p);
      pd->GetPoints()->GetPoint(ids[j],xx[j]);
      mypts->SetPoint(j,p);
    }

    // calculate area since determinant is 2*area for a tri
    double area = vtkTriangle::TriangleArea(xx[0],xx[1],xx[2]);
    double det = area*2.0;

    for (j = 0; j < 3; j++) {
      pcoords[0] = 1- r[j] - s[j];
      pcoords[1] = r[j];
      pcoords[2] = s[j];
      mycell->EvaluateLocation(subId,pcoords,x,w);
      F[j] = 0.0;
      for (int k = 0; k < 3; k++) {
        F[j] += scalars->GetTuple1(ids[k])*w[k]*det;
      }
    }

    qflow = 0.5*(F[0]*gaussW[0]+F[1]*gaussW[1]+F[2]*gaussW[2]);

//  using the average * area produces identical results for linear tri
//    qflow = area*(scalars->GetTuple1(ids[0])+scalars->GetTuple1(ids[1])+scalars->GetTuple1(ids[2]))/3.0;

//    fprintf(stdout,"cell %i  qflow  %lf  area %lf\n",i,qflow,vtkTriangle::TriangleArea(xx[0],xx[1],xx[2]));
    
//    double wssthresh = 1;
        if (scalars->GetTuple1(ids[0]) <= wssthresh & scalars->GetTuple1(ids[1]) <= wssthresh & scalars->GetTuple1(ids[2])<=wssthresh) {
            qtotal = qtotal + qflow;
            atotal = atotal + area;
        }           
  }     

  *q = qtotal;
  *a = atotal;

  myids->Delete();
  mypts->Delete();

  return CV_OK;
}


// ------------------------
// sys_geom_IntegrateEnergy
// ------------------------

int sys_geom_IntegrateEnergy ( cvPolyData *src, double rho, double *nrm, double *energy )
{

  int i,j;
  vtkPolyData *pd;
  vtkDataArray *scalars = NULL;
  vtkDataArray *vectors = NULL;
  int numPts, numPolys;
  vtkFloatingPointType *pts;
  vtkIdType *polys;
  double energyElem = 0.0;
  double energyTotal = 0.0;
  *energy = energyTotal;
  vtkFloatingPointType crd[4][3];

  pd = src->GetVtkPolyData();

  scalars = pd->GetPointData()->GetScalars();
  if (scalars == NULL) {
        fprintf(stderr,"ERROR: No scalars!\n");
        return CV_ERROR;
  }
  vectors = pd->GetPointData()->GetVectors();
  if (vectors == NULL) {
        fprintf(stderr,"ERROR: No vectors!\n");
        return CV_ERROR;
  }

  if ( VtkUtils_GetPointsFloat( pd, &pts, &numPts ) != CV_OK ) {
    printf("ERR: VtkUtils_GetPoints failed\n");
    return CV_ERROR;
  }
  if ( VtkUtils_GetAllPolys( pd, &numPolys, &polys ) != CV_OK ) {
    printf("ERR: VtkUtils_GetAllPolys failed\n");
    return CV_ERROR;
  }

  int polyIndex = 0;
  int conn[4];

  for (i = 0; i < numPolys; i++) {
      int nElemNodes = polys[polyIndex++];
      if (nElemNodes < 3 || nElemNodes > 4) {
          fprintf(stderr,"ERROR:  Invalid number of nodes in element (%i).\n",nElemNodes);
          return CV_ERROR;
      }         

      conn[0]=polys[polyIndex++];
      conn[1]=polys[polyIndex++];
      conn[2]=polys[polyIndex++];
      if (nElemNodes == 3) {
          conn[3]=conn[2];
      } else {
          conn[3]=polys[polyIndex++];
      }

      for (j = 0; j < 4; j++) {
          crd[j][0]=pts[conn[j]*3+0];
          crd[j][1]=pts[conn[j]*3+1];
          crd[j][2]=pts[conn[j]*3+2];
      }

      vtkFloatingPointType uvalues[5];
      vtkFloatingPointType v[3];
      double p, vmag, VdotN;

      for (j = 0; j < 4; j++) {
        p = scalars->GetTuple1(conn[j]);
        vectors->GetTuple(conn[j],v);
        vmag = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
        VdotN = nrm[0]*v[0]+nrm[1]*v[1]+nrm[2]*v[2];
        uvalues[j] = ( p + 0.5 * rho * vmag * vmag ) * VdotN;
      }

      energyElem = 0.0;    
      if (gdscIntegrateSurfElem(crd, uvalues, &energyElem) == CV_ERROR) {
          fprintf(stderr,"ERROR:  Problem calculating surface integral.\n");
          *energy = 0.0;
          return CV_ERROR;
      }
      energyTotal = energyTotal + energyElem;
  }

  *energy = energyTotal;

  return CV_OK;

}

