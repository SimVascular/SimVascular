/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
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
 *
 *=========================================================================*/

#include "SimVascular.h" 

#include "cvAdaptObject.h"
#include "cv_misc_utils.h"

#include <string.h>
#include <assert.h>

#include "cv_globals.h"

KernelType cvAdaptObject::gCurrentKernel = KERNEL_TETGEN;
cvFactoryRegistrar cvAdaptObject::gRegistrar;

cvAdaptObject* cvAdaptObject::DefaultInstantiateAdaptObject( Tcl_Interp *interp )
{
  cvFactoryRegistrar* adaptObjectRegistrar;
  if (interp == NULL) {
    fprintf(stdout,"WARNING:  Null interpreter passed to SolidModel.  Overriding with default.\n");
    fflush(stdout);
  }
  Tcl_Interp* myinterp = NULL;
  myinterp = gVtkTclInterp;
  assert(myinterp);

  adaptObjectRegistrar = (cvFactoryRegistrar *) Tcl_GetAssocData( myinterp, "AdaptObjectRegistrar", NULL);

  cvAdaptObject* adaptor = NULL;
  if (cvAdaptObject::gCurrentKernel == KERNEL_TETGEN ||
      cvAdaptObject::gCurrentKernel == KERNEL_MESHSIM) {
    adaptor = (cvAdaptObject *) (adaptObjectRegistrar->UseFactoryMethod( cvAdaptObject::gCurrentKernel ));
    if (adaptor == NULL) {
      fprintf(stdout, "Unable to create adaptor of kernel type (%i)\n",cvAdaptObject::gCurrentKernel);
    }
  } else {
    fprintf(stdout, "current kernel is not valid (%i)\n",cvAdaptobject::gCurrentKernel);
  }
  return adaptor;
}

// ----------
// cvAdaptObject
// ----------

cvAdaptObject::cvAdaptObject( KernelType t )
  : cvRepositoryData( KERNEL_TETGEN )
{
  kernel_ = t;

  inmesh_ = NULL;
  outmesh_ = NULL;
  insurface_mesh_ = NULL;
  outsurface_mesh_ = NULL;

  sol_ = NULL;
  error_indicator_ = NULL;
  hessians_ = NULL;
}

cvAdaptObject::~cvAdaptObject() 
{
  ;
}

// -----------------------
// SetHessians
// -----------------------
int cvAdaptObject::SetHessians()
{
  if (inmesh_ == NULL)
  {
    fprint(stderr,"Error: Mesh must be loaded to set hessians\n");
    return CV_ERROR;
  }
  vtkIdType numPoints = inmesh_->GetNumberOfPoints();

  switch(strategy) {
  //this code processes for both if strategy == 1 || strategy ==2
  //Right now the only implemented adaptation is for isotropic meshing. 
  //TetGen only has the ability to specify one size metric at each node 
  //within the mesh, so anisotropic meshing is not capable at this moment.
  //Strategies 1 and 2 implement isotropic adaptation 
  case 1 :
  case 2 : { //isotropic adaptation
    cout<<"\nStrategy chosen for ANISOTROPIC adaptation : size-field driven"<<endl;
    
    char error_tag[28];
    if(strategy == 1) {
      cout<<"\nUsing ybar to compute hessians...\n"<<endl;
      sprintf(error_tag,"ybar");
    }
    else if (strategy == 2) {
      cout<<"\nUsing numerical/computed hessians (i.e, from phasta)...\n"<<endl;
      sprintf(error_tag,"hessains");
    }
    
    cout<<"\n Reading file:"<<endl;
    cout<<" ..."<<solution_file_<<" (for \"solution and error\")"<<endl;
    //cout<<" ..."<<error_indicator_file_<<" (for \""<<error_tag<<"\")"<<endl;

    if(strategy==1) {
      // attaching the solution to the original mesh
      if (this->LoadFromRestart(solution_file_,"solution",&sol_))
      {
        fprint(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }
      if (this->AttachArray(sol_,inmesh_,name,nvar_,poly_) != CV_OK)
      {
        fprint(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }

      // read ybar (and compute/use hessians of ybar) 
      if (this->LoadFromRestart(solution_file_,"error",&error_indicator_) != CV_OK)
      {
        fprint(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }
      if (this->AttachArray(error_indicator_,inmesh_,name,nvar_,poly_) != CV_OK)
      {
        fprint(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }
      // calculating hessians for ybar field
      // first reconstruct gradients and then the hessians 
      // also deals with boundary issues &
      // applies smoothing procedure for hessians
      // (simple average : arithmetic mean)
      hessiansFromSolution(inputug,lstep);
    }
    else if (strategy == 2) { // cannot use analytic hessian in this case
      // use the hessians computed from phasta
      double *hessians;
      if (this->LoadFromRestart(solution_file_,error_tag,&hessians))
      {
        fprint(stderr,"Error: Error when attaching solution to mesh\n");
        return CV_ERROR;
      }
      if (hessians_ != NULL)
	delete [] hessians_;
      double *hessians_ = new double[nshg*6];
      getHessiansFromPhasta(hessians,inputug,nvar,hessians_);
      delete [] hessians;
      if (this->AttachArray(hessians_,inmesh_,"hessians",6,poly_) != CV_OK)
      return 0;
    }
    setSizeFieldUsingHessians(inputug,&inmesh,factor,hmax,hmin);
  }
  break;
  case 3:
  case 4: { // anisotropic adaptation (tag driven)
    cout<<"Strategy has not been implemented"<<endl;
    return 0;
  }
  break;
  case 5:
  case 6: { //anisotropic adaptation (size-field driven)
    cout<<"Strategy has not been implemented"<<endl;
    return 0;
  }
  break;
  default : {
    if(strategy<0) {
      cout<<"This is default case but has not been implemented"<<endl;

    }
    else {
      cout<<"\nSpecify a correct (adaptation) strategy (adapt.cc)"<<endl;
      exit(-1);
    }
  }
  break;
  }

  return CV_OK;
}

// -----------------------
// LoadFromRestart
// -----------------------
cvAdaptObject::LoadFromRestart(char *fileName,char *name,double **array)
{
  if (*array != NULL)
    delete [] *array;
  if (inmesh_ == NULL)
  {
    fprint(stderr,"Error: Mesh must be loaded to load and attach the solution from restart\n");
    return CV_ERROR;
  }

  readArrayFromFile(fileName,name,*array);

  return CV_OK;
}


// -----------------------
// LoadHessianFromRestart
// -----------------------
cvAdaptObject::LoadHessianFromRestart(char *fileName)
{
  if (sol != NULL)
    delete [] sol;
  if (inmesh_ == NULL)
  {
    fprint(stderr,"Error: Mesh must be loaded to load and attach the solution from restart\n");
    return CV_ERROR;
  }

  readArrayFromFile(fileName,"solution",sol);
  if (this->AttachArray(sol,inmesh_,"solution",nvar_,poly_) != CV_OK)
  {
    fprint(stderr,"Error: Error when attaching sollution to mesh\n");
    return CV_ERROR;
  }

  return CV_OK;
}

int cvAdaptObject::AttachArray(double *valueArray, 
    vtkUnstructuredGrid *mesh,std::string dataName, int nVar, int poly)
{
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

  return CV_OK;
}


