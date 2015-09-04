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

/** @file cv_adapt_utils.cxx
 *  @brief The implementations of functions in cv_adapt_utils
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "SimVascular.h"

#include "cv_adapt_utils.h"

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

#include "eispack.h"

#include "cvSolverIO.h"

#include <sys/stat.h>
// -----------------------------
// AdaptUtils_file_exists()
// -----------------------------
/** 
 * @brief function to check files existence before loading
 */
// 
bool AdaptUtils_file_exists(const std::string& name) {
  struct stat buffer;   
  return (stat (name.c_str(), &buffer) == 0); 
}

// -----------------------------
// SmoothHessians()
// -----------------------------
/** 
 * @brief simple average over a patch surrounding the vertex 
 * @note This smooths the hessians by patch method
 */
// 
int AdaptUtils_SmoothHessians(vtkUnstructuredGrid *mesh)
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
  AdaptUtils_getSurfaceBooleans(mesh,pointOnSurface,NULL);

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

  return CV_OK;
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
int AdaptUtils_getHessiansFromPhasta(double *hessiansFromPhasta, 
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

  return CV_OK;
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
int AdaptUtils_getHessian(vtkDoubleArray *Hessians,vtkIdType v, double T[3][3])
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

  return CV_OK;
}

// ----------------------------
// averageSolutionsOnMesh()
// ---------------------------
/**
 * @brief This averages the solutions on the mesh into within the given
 * steps and increment into one solution. 
 * @param begin first step to use
 * @param end last step to use
 * @param incr increment to avg with
 */

int AdaptUtils_averageSolutionsOnMesh(vtkUnstructuredGrid *mesh, int begin,
    int end, int incr)
{
  //Essentially creating our own ybar array, 
  //Component 1: vel_x
  //Component 2: vel_y
  //Component 3: vel_z
  //Component 4: Pressure
  //Component 5: vel_magnitude
  int numPoints = mesh->GetNumberOfPoints();

  vtkSmartPointer<vtkDoubleArray> averageArray = 
    vtkSmartPointer<vtkDoubleArray>::New(); 
  averageArray->SetNumberOfComponents(5);
  averageArray->Allocate(numPoints,10000);
  averageArray->SetNumberOfTuples(numPoints);
  averageArray->SetName("avg_sols");
  for (int i=0; i<numPoints;i++)
  {
    for (int j=0;j<5;j++)
    {
      averageArray->InsertComponent(i,j,0);
    }
  }
  
  int numArrays = 0;
  for (int step_num = begin;step_num <= end;step_num += incr)
  {
    vtkDoubleArray *tmpVelArray;
    vtkDoubleArray *tmpPressureArray;
    vtkSmartPointer<vtkDoubleArray> tmpArray = 
      vtkSmartPointer<vtkDoubleArray>::New();;
    tmpArray->SetNumberOfComponents(5);
    tmpArray->Allocate(numPoints,10000);
    tmpArray->SetNumberOfTuples(numPoints);
    char vel_step[80];
    char press_step[80];
    sprintf(vel_step,"%s_%05i","velocity",step_num);
    sprintf(press_step,"%s_%05i","pressure",step_num);
    if (AdaptUtils_checkArrayExists(mesh,0,vel_step) != CV_OK)
    {
      fprintf(stderr,"Array %s not existent on mesh\n",vel_step);
      return CV_ERROR;
    }
    tmpVelArray = vtkDoubleArray::SafeDownCast(
	mesh->GetPointData()->GetArray(vel_step));
    if (AdaptUtils_checkArrayExists(mesh,0,press_step) != CV_OK)
    {
      fprintf(stderr,"Array %s not existent on mesh\n",press_step);
      return CV_ERROR;
    }
    tmpPressureArray = vtkDoubleArray::SafeDownCast(
	mesh->GetPointData()->GetArray(press_step));
    for (int i = 0;i < numPoints;i++)
    {
      double vel_x = tmpVelArray->GetComponent(i,0);
      tmpArray->InsertComponent(i,0,vel_x);

      double vel_y = tmpVelArray->GetComponent(i,0);
      tmpArray->InsertComponent(i,1,vel_y);

      double vel_z = tmpVelArray->GetComponent(i,0);
      tmpArray->InsertComponent(i,2,vel_z);

      double pressure = tmpPressureArray->GetValue(i);
      tmpArray->InsertComponent(i,3,pressure);

      double vel_mag = sqrt(pow(vel_x,2) + pow(vel_y,2) + pow(vel_z,2));
      tmpArray->InsertComponent(i,4,vel_mag);

      for (int j=0;j <5;j++)
      {
        double tmp = averageArray->GetComponent(i,j);
        averageArray->InsertComponent(i,j,tmp+tmpArray->GetComponent(i,j));
      }
    }
    numArrays++;
  }
  
  for (int i=0; i<numPoints;i++)
  {
    for (int j=0;j <5;j++)
    {
      double tmp = averageArray->GetComponent(i,j);
      averageArray->InsertComponent(i,j,tmp/numArrays);
    }
  }

  mesh->GetPointData()->AddArray(averageArray);
  return CV_OK;
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
int AdaptUtils_attachArray( double *valueArray, 
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

  if (AdaptUtils_checkArrayExists(mesh,0,dataName))
    mesh->GetPointData()->RemoveArray(dataName.c_str());

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

  return CV_OK;
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

int AdaptUtils_getAttachedArray( double *&valueArray,
                       vtkUnstructuredGrid *mesh,
		       std::string dataName,
                       int nVar,
                       int poly)
{     
  if(poly!=1) {
      fprintf(stderr,"\nError in getAttachedData()\n");
      fprintf(stderr,"Polynomial order %d NOT supported\n",poly);
      return CV_ERROR;
  }

  int i;
  int nshg;
  vtkIdType pointId;
  vtkSmartPointer<vtkDoubleArray> solutionArray = 
    vtkSmartPointer<vtkDoubleArray>::New();

  solutionArray = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray(dataName.c_str()));

  nshg = mesh->GetNumberOfPoints();
  valueArray = new double[nshg*nVar];

  int count = 0;
  for (pointId = 0;pointId<nshg;pointId++)
  {
    for (i=0;i<nVar;i++)
    {
      //valueArray[pointId+i*nshg] = solutionArray->GetComponent(pointId,i);
      valueArray[count++] = solutionArray->GetComponent(pointId,i);
    }
  } 
  return CV_OK;
}

// -----------------------------
// fix4SolutionTransfer()
// -----------------------------
/** 
 * @brief This function interpolates the solution from the old mesh onto the 
 * new mesh
 * @param inmesh This is the original mesh 
 * @param outmesh This is the new adapted mesh that needs solution information
 * @param nVar This is the number of components in the solutions array
 * @note This takes the solution from the closest node
 */
int AdaptUtils_fix4SolutionTransfer(vtkUnstructuredGrid *inmesh,vtkUnstructuredGrid *outmesh,int outstep)
{
  int i;
  int numVerts;
  double xyz[3];
//  double solution[nVar];
  vtkIdType pointId;
  vtkIdType closestPoint;
  vtkDoubleArray *inVel; 
  vtkDoubleArray *inPress; 
  vtkSmartPointer<vtkDoubleArray> outSol = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkPointLocator> locator = 
    vtkSmartPointer<vtkPointLocator>::New();

  numVerts = outmesh->GetNumberOfPoints();

  char vel[80];
  char press[80];
  sprintf(vel,"%s_%05i","velocity",outstep);
  sprintf(press,"%s_%05i","pressure",outstep);
  if (AdaptUtils_checkArrayExists(inmesh,0,vel) != CV_OK)
  {
    fprintf(stderr,"Array %s does not exist on mesh\n");
    return CV_ERROR;
  }
  if (AdaptUtils_checkArrayExists(inmesh,0,press) != CV_OK)
  {
    fprintf(stderr,"Array %s does not exist on mesh\n");
    return CV_ERROR;
  }
  outSol->SetNumberOfComponents(5);
  outSol->Allocate(numVerts,10000);
  outSol->SetNumberOfTuples(numVerts);
  outSol->SetName("solution");

  inVel = vtkDoubleArray::SafeDownCast(inmesh->GetPointData()->GetArray(vel));
  inPress = vtkDoubleArray::SafeDownCast(inmesh->GetPointData()->GetArray(press));

  locator->SetDataSet(inmesh);
  locator->BuildLocator();

  for (pointId=0;pointId<numVerts;pointId++)
  {
    outmesh->GetPoint(pointId,xyz);
    closestPoint = locator->FindClosestPoint(xyz);

    double vel[3];
    for (i=0;i<3;i++)
    {
      vel[i] = inVel->GetComponent(closestPoint,i);
      outSol->SetComponent(pointId,i,vel[i]);
    }
    outSol->SetComponent(pointId,3,inPress->GetValue(closestPoint));
    outSol->SetComponent(pointId,4,sqrt(pow(vel[0],2)+pow(vel[1],2)+pow(vel[3],2)));
  }

  outmesh->GetPointData()->AddArray(outSol);
  outmesh->GetPointData()->SetActiveScalars("solution");

  return CV_OK;
}

int AdaptUtils_modelFaceIDTransfer(vtkPolyData *inpd,vtkPolyData *outpd)
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

  outpd->GetCellData()->RemoveArray("ModelFaceID");
  currentRegionsInt->SetName("ModelFaceID");
  outpd->GetCellData()->AddArray(currentRegionsInt);

  outpd->GetCellData()->SetActiveScalars("ModelFaceID");
  
  delete [] mapper;

  return CV_OK;
}

// -----------------------------
// splitSpeedFromYbar()
// -----------------------------
/** 
 * @brief Filter to split speed from the ybar field  
 * @param mesh The mesh with the attached field data information (avg_sols))
 */

int AdaptUtils_splitSpeedFromYbar(vtkUnstructuredGrid *mesh)
{ 
  int numPoints;
  vtkIdType vtkId;
  vtkSmartPointer<vtkDoubleArray> avg_sols = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> speed = 
    vtkSmartPointer<vtkDoubleArray>::New();

  numPoints = mesh->GetNumberOfPoints();

  if (AdaptUtils_checkArrayExists(mesh,0,"avg_sols") != CV_OK)
  {
    fprintf(stderr,"Array named avg_sols is not on mesh\n");
    return CV_ERROR;
  }

  avg_sols = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray("avg_sols"));

  //This is to contain the speed from the ybar array from restart
  speed->SetNumberOfComponents(1);
  speed->Allocate(numPoints,10000);
  speed->SetNumberOfTuples(numPoints);
  speed->SetName("average_speed");
  //The fifth component of the ybar array contains the speed info
  for (vtkId=0;vtkId<numPoints;vtkId++)
  {
    speed->SetTuple1(vtkId,avg_sols->GetComponent(vtkId,4));
  }
  mesh->GetPointData()->AddArray(speed);
  mesh->GetPointData()->SetActiveScalars("average_speed");

  return CV_OK;
}

// -----------------------------
// gradientsFromFilter()
// -----------------------------
/** 
 * @brief Filter to take the gradients of a scalar array, should be speed  
 * @param mesh The mesh with the attached field data information (speed)
 */

int AdaptUtils_gradientsFromFilter(vtkUnstructuredGrid *mesh)
{ 
  int numPoints;
  vtkSmartPointer<vtkGradientFilter> calcGradient = 
    vtkSmartPointer<vtkGradientFilter>::New();

  numPoints = mesh->GetNumberOfPoints();

  if (AdaptUtils_checkArrayExists(mesh,0,"average_speed") != CV_OK)
  {
    fprintf(stderr,"Array named average_speed is not on mesh\n");
  }

  mesh->GetPointData()->SetActiveScalars("average_speed");

  calcGradient->SetInputData(mesh);
  calcGradient->SetInputScalars(0,"average_speed");
  calcGradient->SetResultArrayName("gradients");
  calcGradient->Update();

  //The new mesh has gradient field data attached to it
  mesh->GetCellData()->RemoveArray("average_speed");
  mesh->DeepCopy(calcGradient->GetOutput());

  return CV_OK;
}

// -----------------------------
// hessiansFromFilter()
// -----------------------------
/** 
 * @brief Filter to take the hessians of gradient array  
 * @param mesh The mesh with the attached gradient field data 
 */

int AdaptUtils_hessiansFromFilter(vtkUnstructuredGrid *mesh)
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

  return CV_OK;
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
int AdaptUtils_hessiansFromSolution(vtkUnstructuredGrid *mesh)
{  
  // compute the hessain field from the solution

  //nodalgradientID  =  MD_newMeshDataId( "gradient");
  //nodalhessianID  =  MD_newMeshDataId( "hessian");
  
  if (AdaptUtils_splitSpeedFromYbar(mesh) != CV_OK)
  {
    fprintf(stderr,"Error in setting getting speed array\n");
    return CV_ERROR;
  }
  // recover gradients from vtk filter
  // attaches gradient to vertices
  // gradient attached via nodalgradientID
  if (AdaptUtils_gradientsFromFilter(mesh) != CV_OK)
  {
    fprintf(stderr,"Error in setting gradients\n");
    return CV_ERROR;
  }
  
  // recover hessians from vtk filter
  // attaches hessian to vertices
  // hessian attached via  nodalhessianID
  // hessian  attached : 6-component (symmetric)
  // u_xx, u_xy, u_xz, u_yy, u_yz, u_zz
  if (AdaptUtils_hessiansFromFilter(mesh) != CV_OK)
  {
    fprintf(stderr,"Error in setting hessians\n");
    return CV_ERROR;
  }

  if (AdaptUtils_SmoothHessians(mesh) != CV_OK)
  {
    fprintf(stderr,"Error in setting hessians\n");
    return CV_ERROR;
  }

  return CV_OK;
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
double AdaptUtils_getErrorValue(double *nodalValues, int option) {

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
    fprintf(stderr,"\nSpecify correct `option' to compute error value in getErrorValue(...)\n");
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
int AdaptUtils_setSizeFieldUsingHessians(vtkUnstructuredGrid *mesh,
			       double factor,
			       double hmax,
			       double hmin,
                               double sphere[5],
			       int strategy)
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
  //double z[3][3];
  double z[9];
  vtkIdType pointId,averageHessian;

  vtkSmartPointer<vtkDoubleArray> averageHessians = 
    vtkSmartPointer<vtkDoubleArray>::New();

  nshg = mesh->GetNumberOfPoints();
  averageHessians = vtkDoubleArray::SafeDownCast(mesh->GetPointData()->GetArray("averagehessians"));

  // struct Hessian contains decomposed values
  // mesh sizes and directional information
  Hessian *hess = new Hessian[nshg];

  vtkSmartPointer<vtkDoubleArray> errorMetricArray = 
    vtkSmartPointer<vtkDoubleArray>::New();
  if (strategy == 1)
    errorMetricArray->SetNumberOfComponents(1);
  else if (strategy == 2)
    errorMetricArray->SetNumberOfComponents(9);
  else
  {
    fprintf(stderr,"Strategy does not exist\n");
    return CV_ERROR;
  }
  errorMetricArray->Allocate(nshg,10000);
  errorMetricArray->SetNumberOfTuples(nshg);
  errorMetricArray->SetName("errormetric");

  i=0;
  for (pointId=0;pointId<nshg;pointId++)
  {
    mesh->GetPoint(pointId,xyz);

    if (AdaptUtils_getHessian(averageHessians,pointId,T) != CV_OK)
    {
      fprintf(stderr,"Error when getting hessian\n");
      return CV_OK;
    }

    double eigenVals[3];
    double e[3];
    //double Tfoo[3][3];
    double Tfoo[9];

    //copy T into temporary buffer
    //for (j=0;j<3;j++)
    //{
    //  for (k=0;k<3;k++)
    //  {
    //    Tfoo[j][k] = T[j][k];
    //  }
    //}
    for (j=0;j<3;j++)
    {
      for (k=0;k<3;k++)
      {
        Tfoo[j*3+k] = T[j][k];
      }
    }
    
    //mytred(&three,&three,Tfoo,eigenVals,e,z);
    tred2(three,Tfoo,eigenVals,e,z);

    //tql2(&three,&three,eigenVals,e,z,&ierr);
    ierr = tql2(three,eigenVals,e,z);

    for (j=0;j<3;j++)
    {
      hess[i].h[j] = eigenVals[j];
      for (k=0;k<3;k++)
      {
	//hess[i].dir[j][k]=z[j][k];
	hess[i].dir[j][k]=z[j*3+k];
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
    eloc=AdaptUtils_maxLocalError(mesh,pointId,T);
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

  fprintf(stdout,"Strategy chosen is isotropic adaptation, i.e., size-field driven\n");
  fprintf(stdout,"Info on relative interpolation error :\n");
  fprintf(stdout,"total : %.4f\n",etot);
  fprintf(stdout,"mean : %.4f\n",emean);
  fprintf(stdout,"factor : %.4f\n",factor);
  fprintf(stdout,"min. local : %.4f\n",elocmin);;
  fprintf(stdout,"max. local : %.4f\n",elocmax);
  fprintf(stdout,"towards uniform local error distribution of %.4f\n",eloc);
  fprintf(stdout,"with min. edge length : %.4f\n",hmin);
  fprintf(stdout,"with max. edge length : %.4f\n",hmax);

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
    // check if inside of sphere radius
    double r = sqrt ((vxyz[0] - sphere[1])*(vxyz[0] - sphere[1]) + 
		     (vxyz[1] - sphere[2])*(vxyz[1] - sphere[2]) +
		     (vxyz[2] - sphere[3])*(vxyz[2] - sphere[3]));

    if (r < sphere[0]) {

      hess[i].h[0] = sphere[4];
      hess[i].h[1] = sphere[4];
      hess[i].h[2] = sphere[4];

      insideSphereCount++;

    }

    // set the data in directions
    for (int jRow=0; jRow<3; jRow++) {
      for(int iDir=0; iDir<3; iDir++) {
	hess[i].dir[jRow][iDir]=hess[i].dir[jRow][iDir]*hess[i].h[jRow];
      }
    }

    if (strategy == 1)
    {
      double value = 0;
      for (j=0;j<3;j++)
      {
	 value += ABS(hess[i].h[j]);
      }
      value = value/3;
      errorMetricArray->SetValue(i,value);
    }
    else if (strategy == 2)
    {
      //fprintf(stdout,"\nHessian for node %d is:\n",i);
      for (j=0;j<3;j++)
      {
	for (k=0;k<3;k++)
	{  
	  //fprintf(stdout,"%.4f ",hess[i].dir[j][k]);
	  errorMetricArray->SetComponent(i,j*3+k,hess[i].dir[j][k]);
	}
	//fprintf(stdout,"\n");
      }
      //fprintf(stdout,"\n");
    }
    
    i++;
  }
  fprintf(stdout,"Nodes with hmin into effect : %d\n",hminCount);
  fprintf(stdout,"Nodes with hmax into effect : %d\n",hmaxCount);
  fprintf(stdout,"Nodes with both hmin/hmax into effect : %d\n",bothHminHmaxCount);
  fprintf(stdout,"Nodes within sphere : %d\v",insideSphereCount);
  fprintf(stdout,"Nodes ignored in boundary layer : %d\n",bdryNumNodes);;

  delete [] hess;

  mesh->GetPointData()->AddArray(errorMetricArray);
  mesh->GetPointData()->SetActiveScalars("errormetric");

  return CV_OK;
}

// max relative interpolation error at a vertex
// -----------------------------
// maxLocalError()
// -----------------------------
/** 
 * @brief This returns the maximum relative interpolation error at a vertex
 * @param vertex This is the vertex where the max local error is calculated
 * @param H This is the Hessian matrix that the error is calculated for
 */
double AdaptUtils_maxLocalError(vtkUnstructuredGrid *mesh,vtkIdType vertex, double H[3][3])
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
    locE = AdaptUtils_E_error(xyz,H);
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
double AdaptUtils_E_error(double xyz[2][3], double H[3][3])
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

int AdaptUtils_getSurfaceBooleans(vtkUnstructuredGrid *mesh,bool *pointOnSurface,int *pointMapper)
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

int AdaptUtils_convertToVTK(vtkUnstructuredGrid *mesh,vtkPolyData *surfaceMesh,tetgenio *outmesh)
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

// to read parameters from a phasta file (filename)
// parameters correspond to nshg & nvar, i.e., size of field-array
// these parameters are used as reference values 
// (sometimes needed before reading the field-array)
void AdaptUtils_readParametersFromFile(char *filename,
			    char *fieldName,
			    int &nshg, 
			    int &numVars) {

  // read file (i.e., restart, error etc.)
  // fileDescriptor
  int restart;
  // format of the file
  char* iformat = "binary";

  openfile_( filename, "read",  &restart );

  // contains: nshg,numVars,lstep
  int iarray[4];
  // don't know what is this for
  // think it loops over token `<>' 
  // inside phastaIO.cc in readHeader(...)
  int isize = 3;
  
  readheader_( &restart, fieldName, iarray,
	       &isize, "double", iformat );

  // nshg * numVars
  nshg=iarray[0];
  numVars=iarray[1];

  closefile_(&restart, "read");
}

// to read array from a phasta file (filename)
// memory is allocated HERE for 'valueArray'
// `fieldName' tells which block to read like solution, error etc.
void AdaptUtils_readArrayFromFile( char *filename,
			char *fieldName,
			double *&valueArray) {
  // read file (i.e., restart, error etc.)
  // fileDescriptor
  int restart;
  // format of the file
  char* iformat = "binary";

  openfile_( filename, "read",  &restart );

  // contains: nshg,numVars,lstep
  int iarray[4];
  // don't know what is this for
  // think it loops over token `<>' 
  // inside phastaIO.cc in readHeader(...)
  int isize = 3;
  
  readheader_( &restart, fieldName, iarray,
	       &isize, "double", iformat );

  // nshg * numVars
  int nshg=iarray[0];
  int numVars=iarray[1];
  isize = iarray[0]*iarray[1];

  double* q = new double[nshg*numVars];

  readdatablock_( &restart, fieldName, q, &isize,
		  "double" , iformat );

  valueArray = new double[nshg*numVars];

  for(int i = 0; i< nshg; i++){
    for( int j=0; j< numVars; j++){
      valueArray[i*numVars+j] = q[j*nshg+i];
    }
  }
  
  delete [] q;

  closefile_(&restart, "read");
}

// to write array to a phasta file (filename)
// NOTE: array should be transposed!!!
// `fieldName' tells in which block to write like solution, error etc.
// `outputFormat' tells in which format to write, i.e., binary/ascii
// `mode' : "write", "appeand" etc.
void AdaptUtils_writeArrayToFile( char *filename,
		       char *fieldName,
		       char *outputFormat,
		       char *mode,
		       int nshg,
		       int numVars,
		       int stepNumber,
		       double *valueArray) {
 
  int restart;
  char fname[256];
  int iarray[10];
  int size, nitems;
  
  openfile_( filename, mode, &restart );

  writestring_( &restart,"# PHASTA Input File Version 2.0\n");
  writestring_( &restart, "# Byte Order Magic Number : 362436 \n");

  fname[0]='\0';
  sprintf(fname,"# Output generated by phAdapt version: 0 \n");
  writestring_( &restart, fname );

  time_t timenow = time ( &timenow);
  fname[0]='\0';
  sprintf(fname,"# %s\n", ctime( &timenow ));
  writestring_( &restart, fname );

  size = 1;
  nitems = 1;
  iarray[0] = 1;
  int magic_number = 362436;
  int* mptr = &magic_number;

  writeheader_( &restart, "byteorder magic number ",
		(void*)iarray, &nitems, &size, "integer", outputFormat );

  writedatablock_( &restart, "byteorder magic number ",
		   (void*)mptr, &nitems, "integer", outputFormat );          

  bzero( (void*)fname, 256 );
  sprintf(fname,"number of modes : < 0 > %d\n", nshg);
  writestring_( &restart, fname );
    
  bzero( (void*)fname, 256 );
  sprintf(fname,"number of variables : < 0 > %d\n", numVars);
  writestring_( &restart, fname );
    
  size =  nshg*numVars;
  nitems = 3; // length of array
  iarray[0] = nshg;
  iarray[1] = numVars;
  iarray[2] = stepNumber;

  writeheader_( &restart, fieldName,
		( void* )iarray, &nitems, &size,"double", outputFormat );

  nitems = nshg*numVars; // length of array
  writedatablock_( &restart, fieldName,
		   ( void* )(valueArray), &nitems, "double", outputFormat );

  closefile_( &restart, mode);
}

// -------------------
// AdaptUtils_checkArrayExists
// -------------------
/** 
 * @brief Function to check is array with name exists in cell or point data
 * @param object this is the object to check if the array exists
 * @param datatype this is point or cell. point =0,cell=1
 * @param arrayname this is the name of the array to check
 * @reutrn this returns 1 if the array exists and zero if it doesn't
 * or the function does not return properly.
 */
int AdaptUtils_checkArrayExists(vtkUnstructuredGrid *object,int datatype,std::string arrayname)
{
  vtkIdType i;
  int numArrays;
  int exists =0;

  if (datatype == 0)
  {
    numArrays = object->GetPointData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetPointData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }
  else 
  {
    numArrays = object->GetCellData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetCellData()->GetArrayName(i),arrayname.c_str()))
      {
	exists =1;
      }
    }
  }

  if (exists == 1)
  {
    return CV_OK;
  }
  else
  {
    return CV_ERROR;
  }
}
