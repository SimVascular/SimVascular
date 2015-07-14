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

}

cvAdaptObject::~cvAdaptObject() 
{
  ;
}

cvAdaptObject::SetKernel(cvAdaptObjec::KernelType kernel_type)
{
  switch (kernel_type) {
    case cvAdaptObject::KERNEL_TETGEN:
      gCurrentKernel = cvAdaptObject::KERNEL_TETGEN;

    case cvAdaptObject::KERNEL_MESHSIM:
      gCurrentKernel = cvAdaptObject::KERNEL_MESHSIM;
      
     default:
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


