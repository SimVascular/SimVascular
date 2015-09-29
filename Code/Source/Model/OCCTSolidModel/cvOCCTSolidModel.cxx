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

/** @file cvOCCTSolidModel.cxx
 *  @brief The implementations of functions in OCCTSolidModel
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#include "SimVascular.h" 

#include "cvOCCTSolidModel.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "cv_get_tcl_interp_init.h"
#include "cv_polydatasolid_utils.h"
#include "cv_misc_utils.h"
#include "cv_sys_geom.h"
#include <string.h>
#include <assert.h>

#include "gp_Pnt.hxx"
#include "gp_Ax2.hxx"
#include "gp_Dir.hxx"
#include "gp_Vec.hxx"
#include "BRepPrimAPI_MakeBox.hxx"
#include "BRepPrimAPI_MakeSphere.hxx"
#include "BRepPrimAPI_MakeCylinder.hxx"

#include "IVtkOCC_Shape.hxx"
#include "IVtk_IShapeData.hxx"
#include "IVtk_IShapeMesher.hxx"
#include "IVtkVTK_ShapeData.hxx"
#include "IVtkOCC_ShapeMesher.hxx"

// ----------
// OCCTSolidModel
// ----------
/** 
 * @brief Constructor for OCCTSolidModel (Should never be called directly) 
 */

cvOCCTSolidModel::cvOCCTSolidModel()
  : cvSolidModel( SM_KT_OCCT)
{
/** 
 * @brief Data Member is a vtkPolyData. It is initiated as NULL. When a 
 * solid is loaded, a new PolyData is created
 */
  numBoundaryRegions = 0;
  geom_ = NULL;
}

// -----------
// ~cvOCCTSolidModel
// -----------
/** 
 * @brief Destructor for cvOCCTSolidModel 
 */

cvOCCTSolidModel::~cvOCCTSolidModel() 
{
  if (geom_ != NULL)
  {
    delete geom_;
  }
}

// -----------
// ~Copy( const cvOCCTSolidModel& sm)
// -----------
/** 
 * @brief Copy Constructor for cvOCCTSolidModel 
 */
cvOCCTSolidModel::cvOCCTSolidModel( const cvOCCTSolidModel& sm)
	: cvSolidModel( SM_KT_OCCT)
{
  geom_ = NULL;
  Copy( sm );
}

// -----------
// ~Copy( const cvSolidModel& src )
// -----------
/** 
 * @brief Copy for cvSolidModel 
 */

int cvOCCTSolidModel::Copy(const cvSolidModel& src )
{
  cvOCCTSolidModel *solidPtr;

  if (geom_ != NULL) {
    return CV_ERROR;
  }
  if (src.GetKernelT() != SM_KT_OCCT) {
    return CV_ERROR;
  }

  solidPtr = (cvOCCTSolidModel *)( &src );
  if ( solidPtr->geom_ == NULL ) {
    return CV_OK;
  }

  return CV_OK;
}

// ----
// Copy
// ----

cvSolidModel *cvOCCTSolidModel::Copy() const
{
  cvOCCTSolidModel *result = new cvOCCTSolidModel(*this);
  return result;
}


// ----------
// MakeBox3d
// ----------

int cvOCCTSolidModel::MakeBox3d( double dims[], double ctr[])
{
  if(geom_ != NULL)
    delete geom_;

  gp_Pnt center(ctr[0],ctr[1],ctr[2]);
  BRepPrimAPI_MakeBox boxmaker(center,dims[0],dims[1],dims[2]);

  boxmaker.Build();
  geom_ = new TopoDS_Shape;
  *geom_ = boxmaker.Shape();

  return CV_OK;
}

// -----------
// MakeSphere
// -----------

int cvOCCTSolidModel::MakeSphere( double r, double ctr[])
{
  if (geom_ == NULL)
    delete geom_;

  gp_Pnt center(ctr[0],ctr[1],ctr[2]);
  BRepPrimAPI_MakeSphere spheremaker(center,r);

  spheremaker.Build();
  geom_ = new TopoDS_Shape;
  *geom_ = spheremaker.Shape();

  return CV_OK;
}

// -----------
// MakeCylinder
// -----------

int cvOCCTSolidModel::MakeCylinder( double r, double length, double ctr[],
    					double axis[])
{
  if (geom_ == NULL)
    delete geom_;

  gp_Pnt center(ctr[0],ctr[1],ctr[2]);
  gp_Vec vector(axis[0],axis[1],axis[2]);
  gp_Dir direction(vector);
  gp_Ax2 cylaxis(center,direction);
  BRepPrimAPI_MakeCylinder cylindermaker(cylaxis,r,length);

  cylindermaker.Build();
  geom_ = new TopoDS_Shape;
  *geom_ = cylindermaker.Shape();

  return CV_OK;
}

// ------------
// GetPolyData
// ------------
cvPolyData *cvOCCTSolidModel::GetPolyData(int useMaxDist, double max_dist) const
{
  if (geom_ == NULL)
  {
    fprintf(stderr,"Solid is null\n");
    return CV_ERROR;
  }
  cvPolyData *result;
  vtkPolyData *pd;

  IVtkOCC_Shape::Handle aShapeImpl = new IVtkOCC_Shape(*geom_);
  //IVtk_IShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  IVtkVTK_ShapeData::Handle aDataImpl = new IVtkVTK_ShapeData();
  IVtk_IShapeMesher::Handle aMesher = new IVtkOCC_ShapeMesher();
  aMesher->Build(aShapeImpl,aDataImpl);

  pd = vtkPolyData::New();
  pd->DeepCopy(aDataImpl->getVtkPolyData());

  result = new cvPolyData(pd);
  pd->Delete();
  return result;
}
