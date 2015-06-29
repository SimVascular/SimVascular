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

#include "cvTetAdaptCore.h"

#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkCellDataToPointData.h"
#include "vtkCellDerivatives.h"
#include "vtkGradientFilter.h"
#include "vtkCellArray.h"
#include "vtkDoubleArray.h"
#include "vtkSmartPointer.h"
#include "vtkPointLocator.h"
#include "vtkCellLocator.h"
#include "vtkGenericCell.h"
#include "vtkPointData.h"
#include "vtkCellData.h"
#include "vtkPolygon.h"
#include "vtkIdList.h"
#include "vtkTetra.h"

// Modify mesh-metric to take special care
// like in parallel plates (i.e, w=f(y)) 
// if a user wants different hmax in x-direction
// other examples can be situations where
// user don't want to have one mesh edge 
// connecting two geometric model edges (like no dofs)
void cvTetAdaptCore::ModifyMetric(vtkIdType vertex, double dir[3][3], double* h){

}

// -----------------------------
// SmoothHessians()
// -----------------------------
/** 
 * @brief simple average over a patch surrounding the vertex 
 * @note This smooths the hessians by patch method
 */
// 
void cvTetAdaptCore::SmoothHessians(vtkUnstructuredGrid *mesh)
{
  int i,j,k;
  int numVerts;
  int numSurroundingVerts;
  double newHessian;
  double averageHessian[6];
  double nodalHessian[6];
  vtkIdType checkPt;
  vtkIdType npts;
  vtkIdType *pts;
  vtkIdType p1,p2;
  vtkIdType pointId,cellId;
  vtkIdType surfacePt,volumePt;
  vtkIdType surfacePtId;

  vtkSmartPointer<vtkDoubleArray>  averageHessians = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray>  nodalHessians = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkIdList> patchElements = 
    vtkSmartPointer<vtkIdList>::New();

  numVerts = mesh->GetNumberOfPoints();
  bool *pointOnSurface = new bool[numVerts];
  
  averageHessians->SetNumberOfComponents(6);
  averageHessians->Allocate(numVerts,10000);
  averageHessians->SetNumberOfTuples(numVerts);
  averageHessians->SetName("averagehessians");

  nodalHessians = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray("hessians"));

  //Have no purpose for point mapping here
  getSurfaceBooleans(mesh,pointOnSurface,NULL);

  for (pointId=0;pointId<numVerts;pointId++)
  {
    numSurroundingVerts = 0;
    for (i=0;i<6;i++)
    {
      averageHessian[i] = 0.0;
      nodalHessian[i] = nodalHessians->GetComponent(pointId,i);
      averageHessian[i] = averageHessian[i] + nodalHessian[i];
    }
    mesh->GetPointCells(pointId,patchElements);
    
    vtkSmartPointer<vtkIdList> neighborPoints = 
      vtkSmartPointer<vtkIdList>::New();
    for (cellId=0;cellId<patchElements->GetNumberOfIds();cellId++)
    {
      mesh->GetCellPoints(patchElements->GetId(cellId),npts,pts);
      for (j=0;j<npts;j++)
      {
	if (pointOnSurface[pts[j]] == false)
	{
	  neighborPoints->InsertUniqueId(pts[j]);
	}
      }
    }
    numSurroundingVerts = neighborPoints->GetNumberOfIds();  

    if (numSurroundingVerts != 0)
    {
      for (i=0;i<numSurroundingVerts;i++)
      {
	checkPt = neighborPoints->GetId(i);
	for (j=0;j<6;j++)
	{
	  newHessian = nodalHessians->GetComponent(checkPt,j);
	  averageHessian[j] = averageHessian[j] + newHessian;
	}
      }
      for (i=0;i<6;i++)
      {
	averageHessian[i] = averageHessian[i]/numSurroundingVerts; 
      }
    }
    averageHessians->SetTuple(pointId,averageHessian);
  }

  mesh->GetPointData()->AddArray(averageHessians);
  mesh->GetPointData()->SetActiveScalars("averagehessians");

  delete [] pointOnSurface;
}

// -----------------------------
// getHessiansFromPhasta()
// -----------------------------
/** 
 * @brief extracts hessians from vector retrieved from restart and puts
 * it in an attachable format for vtk 
 * @param the vector from the restart containing all the hessians 
 * @param hessians this is the new stored array with the six components
 * of the symmetric hessian matrix
 */
// 
void cvTetAdaptCore::getHessiansFromPhasta(double *hessiansFromPhasta, 
      vtkUnstructuredGrid *mesh,int nvar, double *hessians)
{
  int i,j;
  int numPts = mesh->GetNumberOfPoints();

  for(int i=0; i <numPts; i++)
  {
   for(int j=0; j < 6; j++)
   {
     hessians[i*6+j] = hessiansFromPhasta[i*27+3*j];
   } 
  }

}
// 
// -----------------------------
// getHessian()
// -----------------------------
/** 
 * @brief returns a vector with hessian values from vtkDoubleArray 
 * @note The hessian returned is a 6 component symmetric matrix
 * @note u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
 * @note called in setSizeFieldUsingHessians (sizefield.cc)
 */
// 
void cvTetAdaptCore::getHessian(vtkDoubleArray *Hessians,vtkIdType v, double T[3][3])
{
  if (Hessians->GetNumberOfComponents() == 0)
  {
    cout<<"ERROR: No values in Hessian array"<<endl;
  }

  T[0][0] = Hessians->GetComponent(v,0);
  T[0][1] = T[1][0] = Hessians->GetComponent(v,1);
  T[0][2] = T[2][0] = Hessians->GetComponent(v,2);
  T[1][1] = Hessians->GetComponent(v,3);
  T[1][2] = T[2][1] = Hessians->GetComponent(v,4);
  T[2][2] = Hessians->GetComponent(v,5);
}

//
// -----------------------------
// attachArray()
// -----------------------------
/** 
 * @brief This attaches an array of doubles to a vtkUnstructuredGrid 
 * @param nVar = 5 (for flow problems) or 27 (for hessians)
 * @param poly is the polynomial order 
 * @note called in setSizeFieldUsingHessians (sizefield.cc)
 */
// 
void cvTetAdaptCore::attachArray( double *valueArray, 
		  vtkUnstructuredGrid *mesh, 
		  std::string dataName,
		  int nVar, 
		  int poly ) {
  int nem = (poly > 1) ? (poly - 1) : 0;
  int nfm = (poly > 3) ? ((poly-3)*(poly-2)/2.0) : 0;
  int nrm = (poly > 5) ? ((poly-4)*(poly-5)*(poly-3)/6.0) : 0;
  if(poly==3) nfm =1;

  int i,count;
  int numVerts,numCells,numEdges;
//  double data[nVar];
  vtkIdType pointId,cellId,edgeId;
  vtkSmartPointer<vtkDoubleArray> nodeDataArray = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> cellDataArray = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> edgeDataArray = 
    vtkSmartPointer<vtkDoubleArray>::New();

  numVerts = mesh->GetNumberOfPoints();
  numCells = mesh->GetNumberOfCells();
  numEdges = mesh->GetNumberOfCells();

  /* attach the vertex coefficients */
  nodeDataArray->SetNumberOfComponents(nVar);
  nodeDataArray->Allocate(numVerts,10000);
  nodeDataArray->SetNumberOfTuples(numVerts);
  nodeDataArray->SetName(dataName.c_str());
  count = 0;
  for (pointId = 0;pointId < numVerts;pointId++)
  {
    for (i=0;i<nVar;i++)
    { 
//      data[i] = valueArray[count++];
      nodeDataArray->InsertComponent(pointId,i,valueArray[count++]);
    }
//    nodeDataArray->SetTuple(pointId,data);
  }
  mesh->GetPointData()->AddArray(nodeDataArray);
  mesh->GetPointData()->SetActiveScalars(dataName.c_str());

}

// -----------------------------
// getAttachedArray()
// -----------------------------
/** 
 * @brief This gets an array that is attached to a vtkUnstructuredGrid
 * @param valueArray This is the array containing the extracted field
 * @param mesh this is the vtkUnstructured Grid with the data fields
 * @param dataName This is the name of the data field to be extraced
 * @param nVar This is the number of components of the array
 * @param poly This is the polynomial order of the data (currently only 1)
 */

void cvTetAdaptCore::getAttachedArray( double *&valueArray,
                       vtkUnstructuredGrid *mesh,
		       std::string dataName,
                       int nVar,
                       int poly)
{     
  if(poly!=1) {
      adaptSimLog << "\nError in getAttachedData()"<< endl;
      adaptSimLog << "Polynomial order [" << poly << "] NOT supported" << endl;
      exit(-1);
  }

  int i;
  int nshg;
  vtkIdType pointId;
  vtkSmartPointer<vtkDoubleArray> solutionArray = 
    vtkSmartPointer<vtkDoubleArray>::New();

  solutionArray = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray(dataName.c_str()));

  nshg = mesh->GetNumberOfPoints();
  valueArray = new double[nshg*nVar];

  for (pointId = 0;pointId<nshg;pointId++)
  {
    for (i=0;i<nVar;i++)
    {
      valueArray[pointId+i*nshg] = solutionArray->GetComponent(pointId,i);
    }
  } 
}

// -----------------------------
// phastaTransfer()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */
void cvTetAdaptCore::phastaTransfer( int mtype, int mco, void *userData)  {

}

// -----------------------------
// phastaTransfer()
// -----------------------------
/** 
 * @brief This function interpolates the solution from the old mesh onto the 
 * new mesh
 * @param inmesh This is the original mesh 
 * @param outmesh This is the new adapted mesh that needs solution information
 * @param nVar This is the number of components in the solutions array
 * @note This takes the solution from the closest node
 */
void cvTetAdaptCore::fix4SolutionTransfer(vtkUnstructuredGrid *inmesh,vtkUnstructuredGrid *outmesh,int nVar)
{
  int i;
  int numVerts;
  double xyz[3];
//  double solution[nVar];
  vtkIdType pointId;
  vtkIdType closestPoint;
  vtkSmartPointer<vtkDoubleArray> inSolution = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> outSolution = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkPointLocator> locator = 
    vtkSmartPointer<vtkPointLocator>::New();

  numVerts = outmesh->GetNumberOfPoints();

  outSolution->SetNumberOfComponents(nVar);
  outSolution->Allocate(numVerts,10000);
  outSolution->SetNumberOfTuples(numVerts);
  outSolution->SetName("solution");

  inSolution = vtkDoubleArray::SafeDownCast(inmesh->GetPointData()->GetArray("solution"));

  locator->SetDataSet(inmesh);
  locator->BuildLocator();

  for (pointId=0;pointId<numVerts;pointId++)
  {
    outmesh->GetPoint(pointId,xyz);
    closestPoint = locator->FindClosestPoint(xyz);

    for (i=0;i<nVar;i++)
    {
//      solution[i] = inSolution->GetComponent(closestPoint,i);
      outSolution->SetComponent(pointId,i,inSolution->GetComponent(closestPoint,i));
    }
//    outSolution->SetTuple(pointId,solution);
  }

  outmesh->GetPointData()->AddArray(outSolution);
  outmesh->GetPointData()->SetActiveScalars("solution");
}

void cvTetAdaptCore::modelFaceIDTransfer(vtkPolyData *inpd,vtkPolyData *outpd)
{
  int i,j,k; 
  int subId;    
  int maxIndex; 
  int temp;
  int flag = 1;  
  int count;
  int bigcount;
  vtkIdType npts;
  vtkIdType *pts;
  double distance;
  double mappingPt[3];
  double closestPt[3];
  double tolerance = 1.0;
  double minmax[2];
  double centroid[3];
  int range; 
  vtkIdType closestCell;
  vtkIdType cellId;
  vtkIdType currentValue;
  vtkIdType realValue;
  vtkSmartPointer<vtkCellLocator> locator = 
    vtkSmartPointer<vtkCellLocator>::New();
  vtkSmartPointer<vtkGenericCell> genericCell =
    vtkSmartPointer<vtkGenericCell>::New();
  vtkSmartPointer<vtkIntArray> currentRegionsInt = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> realRegions = 
    vtkSmartPointer<vtkIntArray>::New();

  outpd->BuildLinks();
  inpd->BuildLinks();
  locator->SetDataSet(inpd);
  locator->BuildLocator();

  realRegions = static_cast<vtkIntArray*>(inpd->GetCellData()->GetScalars("ModelFaceID"));

  range = minmax[1]-minmax[0];
  int *mapper;
  mapper = new int[1+range];
  
  for (i=0;i<range+1;i++)
  {
    mapper[i] = -1;
  }

  fprintf(stdout,"Mapping Cells\n");;
  for (cellId=0;cellId<outpd->GetNumberOfCells();cellId++)
  {
      outpd->GetCellPoints(cellId,npts,pts);
      vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
      vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
      for (i=0;i<npts;i++)
      {
	polyPtIds->InsertValue(i,i);
	polyPts->InsertNextPoint(outpd->GetPoint(pts[i]));
      }
      vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);

      locator->FindClosestPoint(centroid,closestPt,genericCell,closestCell,
	  subId,distance);
      currentRegionsInt->InsertValue(cellId,realRegions->GetValue(closestCell));
  }
  fprintf(stdout,"Done\n");;

  outpd->GetCellData()->RemoveArray("ModelFaceID");
  currentRegionsInt->SetName("ModelFaceID");
  outpd->GetCellData()->AddArray(currentRegionsInt);

  outpd->GetCellData()->SetActiveScalars("ModelFaceID");
  
  delete [] mapper;
}

// -----------------------------
// gradientsFromFilter()
// -----------------------------
/** 
 * @brief Filter to take the gradients of a scalar array, should be speed  
 * @param mesh The mesh with the attached field data information (speed)
 */

void cvTetAdaptCore::gradientsFromFilter(vtkUnstructuredGrid *mesh)
{ 
  int numPoints;
  vtkIdType vtkId;
  vtkSmartPointer<vtkGradientFilter> calcGradient = 
    vtkSmartPointer<vtkGradientFilter>::New();
  vtkSmartPointer<vtkDoubleArray> errorForGradient = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> newError = 
    vtkSmartPointer<vtkDoubleArray>::New();

  numPoints = mesh->GetNumberOfPoints();

  errorForGradient = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray("error"));

  //This is to contain the speed from the error array from restart
  newError->SetNumberOfComponents(1);
  newError->Allocate(numPoints,10000);
  newError->SetNumberOfTuples(numPoints);
  newError->SetName("errorforvtk");
  //The fifth component of the error array contains the speed info
  for (vtkId=0;vtkId<numPoints;vtkId++)
  {
    newError->SetTuple1(vtkId,errorForGradient->GetComponent(vtkId,4));
  }
  mesh->GetPointData()->AddArray(newError);
  mesh->GetPointData()->SetActiveScalars("errorforvtk");

  calcGradient->SetInputData(mesh);
  calcGradient->SetInputScalars(0,"errorforvtk");
  calcGradient->SetResultArrayName("gradients");
  calcGradient->Update();

  //The new mesh has gradient field data attached to it
  mesh->DeepCopy(calcGradient->GetOutput());
  mesh->GetCellData()->RemoveArray("errorforvtk");
}

// -----------------------------
// hessiansFromFilter()
// -----------------------------
/** 
 * @brief Filter to take the hessians of gradient array  
 * @param mesh The mesh with the attached gradient field data 
 */

void cvTetAdaptCore::hessiansFromFilter(vtkUnstructuredGrid *mesh)
{
  vtkSmartPointer<vtkGradientFilter> calcHessian = 
    vtkSmartPointer<vtkGradientFilter>::New();
  vtkSmartPointer<vtkCellDataToPointData> moveToPoints = 
    vtkSmartPointer<vtkCellDataToPointData>::New();
  vtkSmartPointer<vtkDoubleArray> Hessians = 
    vtkSmartPointer<vtkDoubleArray>::New();

  int numPts = mesh->GetNumberOfPoints();

  //This array will contain the 6 components of the hessian matrix.
  Hessians->SetNumberOfComponents(6);
  Hessians->Allocate(numPts,10000);
  Hessians->SetNumberOfTuples(numPts);
  Hessians->SetName("hessians");

  calcHessian->SetInputData(mesh);
  calcHessian->SetInputScalars(0,"gradients");
  calcHessian->SetResultArrayName("fullhessian");
  calcHessian->Update();

  //Copy the necessary components to the array to be attached to the mesh
  //Only need six components because symmetric matrix, see below
  Hessians->CopyComponent(0,calcHessian->GetOutput()->GetPointData()->GetArray("fullhessian"),0);
  Hessians->CopyComponent(1,calcHessian->GetOutput()->GetPointData()->GetArray("fullhessian"),1);
  Hessians->CopyComponent(2,calcHessian->GetOutput()->GetPointData()->GetArray("fullhessian"),2);
  Hessians->CopyComponent(3,calcHessian->GetOutput()->GetPointData()->GetArray("fullhessian"),4);
  Hessians->CopyComponent(4,calcHessian->GetOutput()->GetPointData()->GetArray("fullhessian"),5);
  Hessians->CopyComponent(5,calcHessian->GetOutput()->GetPointData()->GetArray("fullhessian"),8);

  mesh->GetPointData()->AddArray(Hessians);
  mesh->GetPointData()->SetActiveScalars("hessians");
}

// -----------------------------
// hessiansFromSolution()
// -----------------------------
/** 
 * @brief This function calls the gradient and hessian filters to get hessian
 * matrix in order to set the mesh size field
 * @note hessian  returned : 6-component (symmetric)
 * @note u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
 */
void cvTetAdaptCore::hessiansFromSolution(vtkUnstructuredGrid *mesh,int stepNumber)
{  
  // compute the hessain field from the solution

  //nodalgradientID  =  MD_newMeshDataId( "gradient");
  //nodalhessianID  =  MD_newMeshDataId( "hessian");
  
  // recover gradients from vtk filter
  // attaches gradient to vertices
  // gradient attached via nodalgradientID
  gradientsFromFilter(mesh);
  
  // recover hessians from vtk filter
  // attaches hessian to vertices
  // hessian attached via  nodalhessianID
  // hessian  attached : 6-component (symmetric)
  // u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
  hessiansFromFilter(mesh);

  SmoothHessians(mesh);
}

// this routine tags/marks the mesh entities for refinement (i.e., tag driven)
// as of now only tags the edges (later, may introduce other choices)
// tags entities for refinement which have error values greater than threshold
// as of now do not use hmin and hmax
// can introduce one more factor to compute threshold for coarsening
// -----------------------------
// applyMarkingStrategy()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */
int cvTetAdaptCore::applyMarkingStrategy(vtkUnstructuredGrid *mesh, int simAdapter,
		     double factor, double hmin, double hmax,
		     double &totalError, double &maxError, double &minError,
		     double &threshold, int option) 
{
  return CV_ERROR;
}
  

// -----------------------------
// getErrorThreshold()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */
double cvTetAdaptCore::getErrorThreshold(vtkUnstructuredGrid *mesh, double factor,
		  double &totalError, 
		  double &maxError, double &minError,
		  int option)
{
  return CV_ERROR;
} 

// option is to decide how to compute the error value
// (i.e., use 3 EI for flow problem or use 1 EI for scalar problem)
// -----------------------------
// getErrorValue()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */
double cvTetAdaptCore::getErrorValue(double *nodalValues, int option) {

  double errorValue = 0.;

  switch(option) {
  case 0:
    {
      double weight = 1.;
      errorValue = weight*nodalValues[0];  
    }
    break;
  case 1:
    {
      double weight[3]={1.,1.,1.};
      for (int i=0; i<3; i++) {
	errorValue += weight[i]*nodalValues[i];
      }
    }
    break;
  default :
    adaptSimLog<<"\nSpecify correct `option' to compute error value in getErrorValue(...)"<<endl;
    break;
  }
  return errorValue;
}

// -----------------------------
// setSizeFieldUsingHessians()
// -----------------------------
/** 
 * @brief This function takes the hessian field data and computes a mesh 
 * size field for the mesh adaptation/refinement
 * @param mesh This is the mesh to be adapted
 * @param inmesh This is the tetgen mesh object to be constructucted 
 * @param factor This is the ratio refinement factor
 * @param hmax This is the maximum edge length acceptable for the mesh
 * @param hmin This is the minimum edge length acceptable for the mesh
 */
void cvTetAdaptCore::setSizeFieldUsingHessians(vtkUnstructuredGrid *mesh,
			       tetgenio *inmesh,
			       double factor,
			       double hmax,
			       double hmin)
{
  int i,j,k;
  int nshg;
  int ierr;
  int three = 3;
  int bdryNumNodes = 0;
  double xyz[3];
  double T[3][3];
  double tol=1.e-12;
  double maxEigenval=0;
  double eloc;  	  // local error at a vertex
  double etot=0.;	  // total error for all vertices
  double emean; 	  // emean = etot / nv
  double elocmax=0.;	  // max local error
  double elocmin=1.e20;   // min local error
  double z[3][3];
  vtkIdType pointId,averageHessian;

  vtkSmartPointer<vtkDoubleArray> averageHessians = 
    vtkSmartPointer<vtkDoubleArray>::New();

  nshg = mesh->GetNumberOfPoints();
  averageHessians = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray("averagehessians"));

  // struct Hessian contains decomposed values
  // mesh sizes and directional information
  Hessian *hess = new Hessian[nshg];

  inmesh->numberofpointmtrs = 3;
  inmesh->pointmtrlist = new REAL[nshg*inmesh->numberofpointmtrs];

  i=0;
  for (pointId=0;pointId<nshg;pointId++)
  {
    mesh->GetPoint(pointId,xyz);

    getHessian(averageHessians,pointId,T);

    double eigenVals[3];
    double e[3];
    double Tfoo[3][3];

    // copy T into temporary buffer
    for (j=0;j<3;j++)
    {
      for (k=0;k<3;k++)
      {
	Tfoo[j][k] = T[j][k];
      }
    }
    
    mytred(&three,&three,Tfoo,eigenVals,e,z);

    tql2(&three,&three,eigenVals,e,z,&ierr);

    for (j=0;j<3;j++)
    {
      hess[i].h[j] = eigenVals[j];
      for (k=0;k<3;k++)
      {
	hess[i].dir[j][k]=z[j][k];
      }
    }

    hess[i].h[0] = ABS(hess[i].h[0]);
    hess[i].h[1] = ABS(hess[i].h[1]);
    hess[i].h[2] = ABS(hess[i].h[2]);

    if( MAX(hess[i].h[0],MAX(hess[i].h[1],hess[i].h[2])) < tol ) {
      printf("Warning: zero maximum eigenvalue for node %d !!!\n",i);
      printf("       %f %f %f\n", hess[i].h[0],
             hess[i].h[1],hess[i].h[2]);
      continue;
    }

    // estimate relative interpolation error
    // needed for scaling metric field (mesh size field)
    // to get an idea refer Appendix A in Li's thesis
    eloc=maxLocalError(mesh,pointId,T);
    etot += eloc;
    if( eloc>elocmax )  elocmax=eloc;
    if( eloc<elocmin )  elocmin=eloc;

    i++;
  }

  printf("Info: Reading hessian... done...\n");

  emean =  etot / nshg;
  printf("\n Info on relative interpolation error: ");
  printf("   total: %f\n",etot);
  printf("   mean:  %f\n",emean);
  printf("   max local: %f\n",elocmax);
  printf("   min local: %f\n",elocmin);

  eloc = emean*factor;

  printf("\n towards uniform local error distribution of %f\n", eloc);
  printf("   with max edge length=%f; min edge length=%f\n\n",hmax,hmin);

  adaptSimLog<<"Strategy chosen is isotropic adaptation, i.e., size-field driven"<<endl;
  adaptSimLog<<"Info on relative interpolation error :"<<endl;
  adaptSimLog<<"total : "<<etot<<endl;
  adaptSimLog<<"mean : "<<emean<<endl;
  adaptSimLog<<"factor : "<<factor<<endl;
  adaptSimLog<<"min. local : "<<elocmin<<endl;
  adaptSimLog<<"max. local : "<<elocmax<<endl;
  adaptSimLog<<"towards uniform local error distribution of "<<eloc<<endl;
  adaptSimLog<<"with min. edge length : "<<hmin<<endl;
  adaptSimLog<<"with max. edge length : "<<hmax<<endl;

  int foundHmin = 0;
  int foundHmax = 0;
  int hminCount = 0;
  int hmaxCount = 0;
  int bothHminHmaxCount = 0;
  int insideSphereCount = 0;

  double tol2, tol3;

  i=0;
  for (pointId=0;pointId<nshg;pointId++)
  {
    tol2 = 0.01*hmax;
    tol3 = 0.01*hmin;
    foundHmin = 0;
    foundHmax = 0;
    for( j=0; j<3; j++ ) {
      if( hess[i].h[j] < tol )
	hess[i].h[j] = hmax;
      else {
	hess[i].h[j] = sqrt(eloc/hess[i].h[j]);
	if( hess[i].h[j] > hmax )
	  hess[i].h[j] = hmax;
	if( hess[i].h[j] < hmin )
	  hess[i].h[j] = hmin;
      }
    }

    for(j=0; j<3; j++) {
      if(ABS(hess[i].h[j]-hmax) <= tol2)
	foundHmax = 1;
      if(ABS(hess[i].h[j]-hmin) <= tol3)
	foundHmin = 1;
    }
    if(foundHmin)
      hminCount++;
    if(foundHmax)
      hmaxCount++;
    if(foundHmin && foundHmax)
      bothHminHmaxCount++;

    double vxyz[3];
    mesh->GetPoint(pointId,vxyz);

    // set the data in directions
    for (int jRow=0; jRow<3; jRow++) {
      for(int iDir=0; iDir<3; iDir++) {
	hess[i].dir[jRow][iDir]=hess[i].dir[jRow][iDir]*hess[i].h[jRow];
      }
    }

    for (j=0;j<3;j++)
    {
      inmesh->pointmtrlist[i*3+j] = ABS(hess[i].h[j]);
    }
    
    i++;
  }
  adaptSimLog<<"Nodes with hmin into effect : "<<hminCount<<endl;
  adaptSimLog<<"Nodes with hmax into effect : "<<hmaxCount<<endl;
  adaptSimLog<<"Nodes with both hmin/hmax into effect : "<<bothHminHmaxCount<<endl;
  adaptSimLog<<"Nodes ignored in boundary layer : "<<bdryNumNodes<<endl<<endl;

  delete [] hess;
}

// max relative interpolation error at a vertex
// -----------------------------
// getErrorValue()
// -----------------------------
/** 
 * @brief This returns the maximum relative interpolation error at a vertex
 * @param vertex This is the vertex where the max local error is calculated
 * @param H This is the Hessian matrix that the error is calculated for
 */
double cvTetAdaptCore::maxLocalError(vtkUnstructuredGrid *mesh,vtkIdType vertex, double H[3][3])
{     
  int i;
  int listCount=0;
  double locE;
  double xyz[2][3];
  double maxLocE=0;
  vtkIdType cellId,pointId;
  vtkIdType npts;
  vtkIdType *pts;
  vtkSmartPointer<vtkIdList> attachedCells = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> firstPointList = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> fullPointList = 
    vtkSmartPointer<vtkIdList>::New();

  mesh->GetPoint(vertex,xyz[0]);
  mesh->GetPointCells(vertex,attachedCells);


  for (cellId=0;cellId<attachedCells->GetNumberOfIds();cellId++)
  {
    mesh->GetCellPoints(attachedCells->GetId(cellId),npts,pts);
    for (i=0;i<npts;i++)
    {
      if(firstPointList->InsertUniqueId(pts[i]) != listCount)
      {
	fullPointList->InsertUniqueId(pts[i]);
      }
      else
      {
	listCount++;
      }
    }
  }

  for (pointId = 0;pointId<fullPointList->GetNumberOfIds();pointId++)
  {
    mesh->GetPoint(fullPointList->GetId(pointId),xyz[1]);
    locE = E_error(xyz,H);
    if ( locE > maxLocE )
    {
      maxLocE=locE;
    }
  }

  return maxLocE;
}

// relative interpolation error along an edge
// -----------------------------
// E_error()
// -----------------------------
/** 
 * @brief This is called by maxLocalError. It gets the error along an edge
 * @param xyz contains the x, y, and z coordinates for the two points on an
 * edge
 * @param H This contains the Hessian information
 */
double cvTetAdaptCore::E_error(double xyz[2][3], double H[3][3])
{
  int i,j;
  double locE=0;
  double vec[3];

  vec[0] = xyz[1][0] - xyz[0][0];
  vec[1] = xyz[1][1] - xyz[0][1];
  vec[2] = xyz[1][2] - xyz[0][2];

  for( i=0; i<3; i++ )
    for( j=0; j<3; j++ ) 
      locE += H[i][j]*vec[i]*vec[j];

  return ABS(locE);
}

// -----------------------------
// setIsotopicSizeField()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */

void cvTetAdaptCore::setIsotropicSizeField(vtkUnstructuredGrid *mesh,
			   int simAdapter,
			   double factor,
			   double hmax, 
			   double hmin,
			   int option)
{
}

// -----------------------------
// setManualSizeField()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */

void cvTetAdaptCore::setManualSizeField(vtkUnstructuredGrid *mesh,
		   int simAdapter, 
        	   int strategy) {
}


// tag the entities to be refinement (for isotropic refinement)
// factor is used to evaluate the threshold for refinement
// as of now do not use hmin and hmax 
// -----------------------------
// tagEntitiesForRefinement()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */
void cvTetAdaptCore::tagEntitiesForRefinement(vtkUnstructuredGrid *mesh,
			      int simAdapter,
			      double factor,
			      double hmax, 
			      double hmin,
			      int option)
{
}

////////////////////////////////////////////
// write in MEDIT format                  //
// for visualization of mesh-metric field //
////////////////////////////////////////////
// -----------------------------
// writeMEDITSizeField()
// -----------------------------
/** 
 * @brief
 * @param 
 * @param
 * @param
 */
void cvTetAdaptCore::writeMEDITSizeField(Hessian* hess, vtkUnstructuredGrid *mesh)
{
}

// -----------------------------
// convertToTetGen()
// -----------------------------
/** 
 * @brief Function to convert the current mesh to a tetgen mesh object to be
 * able to remesh
 * @param mesh This is the full mesh to be remeshed
 * @param surfaceMesh This is the intial mesh; If we don't need the final 
 * mesh regions, then we don't have to actually use this
 * @param inmesh This is the tegen mesh object to be transferred to
 */

int cvTetAdaptCore::convertToTetGen(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *inmesh)
{
  int numTets,numPolys;
  int numPoints,numSurfacePoints;
  double tetPts[3];
  tetgenio::facet *f;
  tetgenio::polygon *p;
  vtkIdType i,j;
  vtkIdType npts = 0;
  vtkIdType *pts = 0;
  vtkIdType cellId;
  vtkSmartPointer<vtkPoints> uPoints = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> pPolys = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkCellArray> uTets = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkIntArray> boundaryScalars = 
	  vtkSmartPointer<vtkIntArray>::New();

  mesh->BuildLinks();
  numTets = mesh->GetNumberOfCells();
  numPoints = mesh->GetNumberOfPoints();
  uPoints = mesh->GetPoints();
  uTets = mesh->GetCells();

  numSurfacePoints = surfaceMesh->GetNumberOfPoints();
  numPolys = surfaceMesh->GetNumberOfPolys();
  pPolys = surfaceMesh->GetPolys();
  boundaryScalars = vtkIntArray::SafeDownCast(surfaceMesh->GetCellData()->GetArray("ModelFaceID"));

  cout<<"Num Cells "<<numTets<<endl;
  cout<<"Num Points "<<numPoints<<endl;
  inmesh->numberofcorners = 4;
  inmesh->numberoftetrahedra = numTets;
  inmesh->numberofpoints = numPoints;
  inmesh->pointlist = new double[numPoints*3];
  inmesh->tetrahedronlist = new int[numTets*4];

  cout<<"Converting to Adapt Points..."<<endl;
  for (i = 0; i < numPoints; i++)
  {
    uPoints->GetPoint(i,tetPts);
    inmesh->pointlist[i*3] = tetPts[0];
    inmesh->pointlist[i*3+1] = tetPts[1];
    inmesh->pointlist[i*3+2] = tetPts[2];
  }

  cout<<"Converting to Adapt Tets..."<<endl;
  for (i=0,uTets->InitTraversal();uTets->GetNextCell(npts,pts);i++)
  {
    for (j = 0;j < npts;j++)
    {
      inmesh->tetrahedronlist[i*npts+j] = pts[j];
    }
  }
  
  return CV_OK;

}
// -----------------------------
// getSurfaceBooleans()
// -----------------------------
/** 
 * @brief This is a function to determine what points of a mesh on on the 
 * surface. This is done by applying node ids, extracting the surface, and 
 * then checking for matching node ids
 * @param mesh This is the mesh to get the booleans for
 * @param pointOnSurface This is a boolean vector with information on whether
 * or not a point is on the surface of the mesh
 * @param pointMapper This is a vector containing the mappings between 
 * the surface mesh and the volumetric mesh
 */

int cvTetAdaptCore::getSurfaceBooleans(vtkUnstructuredGrid *mesh,bool *pointOnSurface,int *pointMapper)
{
  int vCount = 0;
  int numVerts;
  vtkSmartPointer<vtkDataSetSurfaceFilter> extractor = 
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  vtkSmartPointer<vtkIntArray> surfaceNodeArray = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> volumeNodeArray = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkIdType pointId;
  vtkIdType volumePt,surfacePt;
  vtkIdType surfacePtId;

  numVerts = mesh->GetNumberOfPoints();
  extractor->SetInputData(mesh);
  extractor->Update();
  surfaceNodeArray = vtkIntArray::SafeDownCast(extractor->GetOutput()->GetPointData()->GetArray("GlobalNodeID"));
  volumeNodeArray = vtkIntArray::SafeDownCast(mesh->GetPointData()->GetArray("GlobalNodeID"));

  //Get Point mapping for boundary nodes
  for(pointId=0;pointId<numVerts;pointId++)
  {
    pointOnSurface[pointId] = false;
    volumePt = volumeNodeArray->GetValue(pointId);
    for (surfacePtId=0;surfacePtId<surfaceNodeArray->GetNumberOfTuples();surfacePtId++)
    {
      if (pointMapper != NULL)
      {
        pointMapper[surfacePtId] = -1;
      }
      surfacePt = surfaceNodeArray->GetValue(surfacePtId);
      if (volumePt == surfacePt)
      {
	pointOnSurface[pointId] = true;
	if (pointMapper != NULL)
	{
	  pointMapper[surfacePtId] = pointId;
	}
	vCount++;
      }
    }
  }

  return 1;
}

// -----------------------------
// runAdaptor()
// -----------------------------
/** 
 * @brief This is a function to run tetgen on the mesh
 * @param inmesh This is the original mesh as a tetgen object
 * @param outmesh This is the new adapted mesh as a tetgen object
 */

int cvTetAdaptCore::runAdaptor(tetgenio *inmesh,tetgenio *outmesh)
{
  cout<<"Starting Adaptive Mesh..."<<endl;

  tetgenbehavior* newtgb = new tetgenbehavior;

  newtgb->refine=1;
  newtgb->metric=1;
  newtgb->quality=1;
  newtgb->neighout=2;
  newtgb->verbose=1;
  //newtgb->coarsen=1;
  //newtgb->coarsen_param=8;
  //newtgb->coarsen_percent=1;
#if USE_TETGEN143
  newtgb->goodratio = 4.0;
  newtgb->goodangle = 0.88;
  newtgb->useshelles = 1;
#endif

  tetrahedralize(newtgb, inmesh, outmesh);

  cout<<"Done with Adaptive Mesh..."<<endl;

  return CV_OK;

}

// -----------------------------
// convertToVTK()
// -----------------------------
/** 
 * @brief This is a function to convert back to vtk structured from a tetgen
 * mesh
 * @param mesh This is the vtk object to contain the new mesh
 * @param surfaceMesh This is the vtk object to contain the new surface mesh
 * @param outmesh This is the tetgen object returned from tetgen
 */

int cvTetAdaptCore::convertToVTK(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *outmesh)
{
  int count;
  int numAdaptPts;
  int numAdaptTets;
  int numAdaptFaces;
  vtkIdType i,j;
  vtkIdType globalId = 1;
  vtkIdType vtkId;
  vtkSmartPointer<vtkPoints> vtpAdaptPoints = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkPoints> adaptPoints = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> adaptTets = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkCellArray> adaptFaces = vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkIntArray> adaptGlobalNodeIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> adaptGlobalElementIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIdList> adaptTetPointIds = vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIntArray> vtpAdaptPointIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> vtpAdaptFaceIds = vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIdList> facePointIds = vtkSmartPointer<vtkIdList>::New();

  numAdaptPts = outmesh->numberofpoints;
  numAdaptTets = outmesh->numberoftetrahedra;
  numAdaptFaces = outmesh->numberoftrifaces;

  bool *pointOnSurface = new bool[numAdaptPts];
  int *pointMapping = new int[numAdaptPts];

  std::cout<<"Converting Points to adapt VTK Structures..."<<endl;
  globalId = 1;
  adaptPoints->SetNumberOfPoints(numAdaptPts);
  for (i=0;i< numAdaptPts; i++)
  {
    adaptPoints->SetPoint(i,outmesh->pointlist[i*3],outmesh->pointlist[i*3+1],outmesh->pointlist[i*3+2]);
    adaptGlobalNodeIds->InsertValue(i,globalId);
    pointOnSurface[i] = false;
    globalId++;
  }

  //Save all point information in a vtkPoints list
  count = 0;
  for (i=0;i<numAdaptFaces;i++)
  {
    for (j=0;j<3;j++)
    {
      if (pointOnSurface[outmesh->trifacelist[3*i+j]] == false)
      {
	pointOnSurface[outmesh->trifacelist[3*i+j]] = true;
	pointMapping[outmesh->trifacelist[3*i+j]] = count++;
      }
    }
  }

  vtpAdaptPoints->SetNumberOfPoints(count);
  //Create face point list
  count=0;
  for (i=0;i<numAdaptPts;i++)
  {
    if (pointOnSurface[i] == true)
    {
      vtpAdaptPoints->SetPoint(pointMapping[i],outmesh->pointlist[i*3],outmesh->pointlist[i*3+1],outmesh->pointlist[i*3+2]);
      vtpAdaptPointIds->InsertValue(pointMapping[i],i+1);
    }
  }

  std::cout<<"Converting Elements to Adapt VTK Structures..."<<endl;
  adaptTetPointIds->SetNumberOfIds(4);
  globalId=1;
  for (i=0;i< numAdaptTets;i++)
  {
    for (j=0; j< outmesh->numberofcorners;j++)
    {
      vtkId = outmesh->tetrahedronlist[i*outmesh->numberofcorners+j];
      adaptTetPointIds->SetId(j,vtkId);
    }

    adaptGlobalElementIds->InsertValue(i,globalId);
    globalId++;
    adaptTets->InsertNextCell(adaptTetPointIds);
  }

  mesh->SetPoints(adaptPoints);
  mesh->SetCells(VTK_TETRA, adaptTets);

  adaptGlobalNodeIds->SetName("GlobalNodeID");
  mesh->GetPointData()->AddArray(adaptGlobalNodeIds);
  mesh->GetPointData()->SetActiveScalars("GlobalNodeID");

  adaptGlobalElementIds->SetName("GlobalElementID");
  mesh->GetCellData()->AddArray(adaptGlobalElementIds);
  mesh->GetCellData()->SetActiveScalars("GlobalElementID");

  fprintf(stderr,"Converting Faces to VTK Structures...\n");
  facePointIds->SetNumberOfIds(3);
  
  count=0;
  for (i=0;i< numAdaptFaces;i++)
  {
    for (j=0; j<3;j++)
    {
      facePointIds->SetId(j,pointMapping[outmesh->trifacelist[i*3+j]]);
    }

    adaptFaces->InsertNextCell(facePointIds);

    if (outmesh->adjtetlist[2*i] >= numAdaptTets || outmesh->adjtetlist[2*i] <= 0)
    {
      vtpAdaptFaceIds->InsertValue(i,adaptGlobalElementIds->GetValue(outmesh->adjtetlist[2*i+1]));
      count++;
    }
    else if (outmesh->adjtetlist[2*i+1] >= numAdaptTets || outmesh->adjtetlist[2*i+1] <= 0)
    {
      vtpAdaptFaceIds->InsertValue(i,adaptGlobalElementIds->GetValue(outmesh->adjtetlist[2*i]));
      count++;
    }

    else
    {
      vtpAdaptFaceIds->InsertValue(i,adaptGlobalElementIds->GetValue(outmesh->adjtetlist[2*i+1]));
    }
  }

  //Create a polydata grid and link scalar information to nodes and elements
  surfaceMesh->SetPoints(vtpAdaptPoints);
  surfaceMesh->SetPolys(adaptFaces);

  vtpAdaptPointIds->SetName("GlobalNodeID");
  surfaceMesh->GetPointData()->AddArray(vtpAdaptPointIds);
  surfaceMesh->GetPointData()->SetActiveScalars("GlobalNodeID");
												      
  vtpAdaptFaceIds->SetName("GlobalElementID");
  surfaceMesh->GetCellData()->AddArray(vtpAdaptFaceIds);
  surfaceMesh->GetCellData()->SetActiveScalars("GlobalElementID");

  delete [] pointMapping;
  delete [] pointOnSurface;

  return CV_OK;

}

