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

// The functions defined here implement the SV Python API 'imaging' module 'Image' class.
//
//     image = imaging.Image()
//
// The Python Image class is implemented using the PyImage struct defined in Imaging_PyModule.h.
//
// The 'Image' class is used to create an image from DICOM and VTK .vti image files. The image
// data can then be operated using various methods.

#include "sv4gui_VtkUtils.h"
#include "sv3_SegmentationUtils.h"

#include <mitkExtractSliceFilter.h>
#include "mitkPoint.h"
#include "mitkPlaneGeometry.h"
#include "mitkSlicedGeometry3D.h"
#include <mitkVtkResliceInterpolationProperty.h>

#include <tinyxml.h>

#include <vtkDoubleArray.h>
#include <vtkImageData.h>
#include <vtkPointData.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkXMLImageDataWriter.h>


//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

bool CheckImageData(PyUtilApiFunction& api, PyImage* self)
{
  if (self->image_node == nullptr) {
      api.error("The Image object does not have image data.");
      return false;
  }

  return true;
}

//-----------------
// GetvtkTransform
//-----------------
// Compute the transformation to orient the plane with a local coordinates system
// given by tangent and normal.
//
vtkTransform * 
GetvtkTransform(double pos[3], double nrm[3], double xhat[3])
{
  using util = sv3::SegmentationUtils; 

  /*
  double pos[3],nrm[3],xhat[3];

  pos[0]=pathPoint.pos[0];
  pos[1]=pathPoint.pos[1];
  pos[2]=pathPoint.pos[2];

  nrm[0]=pathPoint.tangent[0];
  nrm[1]=pathPoint.tangent[1];
  nrm[2]=pathPoint.tangent[2];

  xhat[0]=pathPoint.rotation[0];
  xhat[1]=pathPoint.rotation[1];
  xhat[2]=pathPoint.rotation[2];
  */

  double zhat[3]={0,0,1};
  double theta = util::math_radToDeg(util::math_angleBtw3DVectors(zhat,nrm));

  double axis[3];
  util::math_cross(axis,zhat,nrm);

  vtkTransform* tmpTr = vtkTransform::New();
  tmpTr->Identity();
  tmpTr->RotateWXYZ(theta,axis);

  vtkPoints* tmpPt = vtkPoints::New();
  tmpPt->InsertNextPoint(1, 0, 0);

  vtkPolyData* tmpPd = vtkPolyData::New();
  tmpPd->SetPoints(tmpPt);

  vtkTransformPolyDataFilter* tmpTf = vtkTransformPolyDataFilter::New();
  tmpTf->SetInputDataObject(tmpPd);
  tmpTf->SetTransform(tmpTr);
  tmpTf->Update();
  double pt[3];
  tmpTf->GetOutput()->GetPoint(0,pt);

  tmpTr->Delete();
  tmpPt->Delete();
  tmpPd->Delete();
  tmpTf->Delete();

  double rot = util::math_radToDeg(util::math_angleBtw3DVectors(pt,xhat));

  double x[3];
  util::math_cross(x,pt,xhat);
  double d = util::math_dot(x,nrm);
  if (d < 0.0) {
        rot=-rot;
  }

  vtkTransform* tr = vtkTransform::New();
  tr->Identity();
  tr->Translate(pos);
  tr->RotateWXYZ(rot,nrm);
  tr->RotateWXYZ(theta,axis);

  return tr;
}

//---------------------
// ComputePlaneSpacing 
//---------------------
// Compute the plane spacing used when slicing an image.
//
// The spacing is used to sample the image.
//
mitk::Vector3D 
ComputePlaneSpacing(mitk::Image* image, double planeSize, double pos[3], double tangent[3], double rotation[3])
{
  // Compute the transformation to orient the plane.
  vtkTransform* tr = GetvtkTransform(pos, tangent, rotation);
  mitk::PlaneGeometry::Pointer planeGeometry = mitk::PlaneGeometry::New();
  planeGeometry->SetIndexToWorldTransformByVtkMatrix(tr->GetMatrix());

  mitk::Vector3D right = planeGeometry->GetAxisVector(0);
  mitk::Vector3D bottom = planeGeometry->GetAxisVector(1);
  mitk::Vector3D planeNormal = planeGeometry->GetNormal();

  right.Normalize();
  bottom.Normalize();
  planeNormal.Normalize();

  right = planeSize * right;
  bottom = planeSize * bottom;

  mitk::Vector3D rightInIndex, bottomInIndex, normalInIndex;
  image->GetTimeGeometry()->GetGeometryForTimeStep(0)->WorldToIndex(right, rightInIndex);
  image->GetTimeGeometry()->GetGeometryForTimeStep(0)->WorldToIndex(bottom, bottomInIndex);
  image->GetTimeGeometry()->GetGeometryForTimeStep(0)->WorldToIndex(planeNormal, normalInIndex);

  mitk::Vector3D planeSpacing;
  planeSpacing[0] = planeSize / rightInIndex.GetNorm();
  planeSpacing[1] = planeSize / bottomInIndex.GetNorm();
  planeSpacing[2] = 1.0 / normalInIndex.GetNorm();

  return planeSpacing;
}

//---------------------
// CreatePlaneGeometry
//---------------------
//
mitk::PlaneGeometry::Pointer
CreatePlaneGeometry(mitk::Image* image, double planeSize, double pos[3], double tangent[3], double rotation[3])
{
  std::cout << "========== PyImage.CreatePlaneGeometry ==========" << std::endl;
   
  // Compute plane spacing.
  auto planeSpacing = ComputePlaneSpacing(image, planeSize, pos, tangent, rotation);
  std::cout << "[PyImage.CreatePlaneGeometry] planeSpacing: " <<planeSpacing[0]<<" "<<planeSpacing[1]<<" "<<planeSpacing[2]<<std::endl;

  // Compute the transformation to orient the plane.
  vtkTransform* tr = GetvtkTransform(pos, tangent, rotation);
  mitk::PlaneGeometry::Pointer planeGeometry = mitk::PlaneGeometry::New();
  planeGeometry->SetIndexToWorldTransformByVtkMatrix(tr->GetMatrix());

  mitk::Vector3D right,bottom;
  right.SetVnlVector(planeGeometry->GetIndexToWorldTransform()->GetMatrix().GetVnlMatrix().get_column(0) );
  bottom.SetVnlVector(planeGeometry->GetIndexToWorldTransform()->GetMatrix().GetVnlMatrix().get_column(1) );

  mitk::Point3D origin;
  origin[0] = pos[0] - right[0]*planeSize/2.0 - bottom[0]*planeSize/2.0;
  origin[1] = pos[1] - right[1]*planeSize/2.0 - bottom[1]*planeSize/2.0;
  origin[2] = pos[2] - right[2]*planeSize/2.0 - bottom[2]*planeSize/2.0;
  std::cout << "[PyImage.CreatePlaneGeometry] Plane origin: " <<origin[0]<<" " << origin[1]<<" " <<origin[2] << std::endl;
    
  planeGeometry->SetOrigin(origin);
  planeGeometry->SetSpacing(planeSpacing);
    
  double width = planeSize/ planeSpacing[0];
  double height = planeSize / planeSpacing[1];
  mitk::ScalarType bounds[6] = { 0, width, 0, height, 0, 1 };
  planeGeometry->SetBounds(bounds);

  planeGeometry->SetReferenceGeometry(image->GetTimeGeometry()->GetGeometryForTimeStep(0));
  planeGeometry->SetImageGeometry(true);

  mitk::Vector3D normal;
  normal = planeGeometry->GetNormal();
  std::cout << "[PyImage.CreatePlaneGeometry] Plane normal: " << normal[0] << " " << normal[1] << " " << normal[2] << std::endl;

  return planeGeometry;
}

//----------
// ReadFile
//----------
// Read image data from a file and store it the returned MITK data node.
//
mitk::DataNode::Pointer
ReadFile(const std::string& fileName)
{
  return sv4guiProjectManager::LoadDataNode(fileName);
}

//--------------------
// ReadImageTransform
//--------------------
// Read an image transformation from a file and apply it.
//
// This sets the image orientation, it does not set its origin.
//
// Note: This reproduces the SV sv4guiProjectManager::setTransform() method.
//
void ReadImageTransform(PyImage* self, const std::string& fileName)
{
  TiXmlDocument document;
  if (!document.LoadFile(fileName)) {
      throw std::runtime_error("Unable to load the file named '" + fileName + "'.");
  }

  auto root = document.FirstChildElement("Transform");
  auto xformElement = root->FirstChildElement("transform");
  auto transform = self->image_data->GetGeometry()->GetVtkMatrix();

  for (int j = 0; j < 3; j++){
    for (int i = 0; i < 3; i++){
      auto label = "t" + std::to_string(i) + std::to_string(j);
      float value;
      if (xformElement->QueryFloatAttribute(label.c_str(), &value) != TIXML_SUCCESS) {
        throw std::runtime_error("No '" + label + "' element found.");
      }
      transform->SetElement(i,j,value);
    }
  }

 self->image_data->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(transform);
 self->image_data->UpdateOutputInformation();
}

//--------------
// ResliceImage
//--------------
//
// Note: This is taken from sv4guiSegmentationUtils::GetSlicevtkImage() 
// but does not work here, slice is off.
//
vtkImageData *
ResliceImage(mitk::Image* image, double planeSize, double pos[3], double tangent[3], double rotation[3])
{
  vtkTransform* tr = GetvtkTransform(pos, tangent, rotation);
  vtkImageReslice* rs = vtkImageReslice::New();

  auto vtkImage = image->GetVtkImageData();
  double spacing[3];
  vtkImage->GetSpacing(spacing);

  // This does not make sense, just need std::min(spacing[0],spacing[1]).
  double vmin = std::min(spacing[0], std::min(spacing[0],spacing[1]));

  int width = planeSize / vmin;
  int height = planeSize / vmin;
  double pdimx = width * vmin;
  double pdimy = height * vmin;

  double origin[3];
  origin[0] = -0.5 * pdimx;
  origin[1] = -0.5 * pdimy;
  origin[2] = 0.0;

  rs->SetInputDataObject(vtkImage);

  rs->SetResliceTransform(tr);
  rs->SetOutputSpacing(vmin,vmin,vmin);
  rs->SetOutputOrigin(origin);
  rs->SetOutputExtent(0, width-1, 0, height-1, 0, 0);
  rs->InterpolateOn();
  rs->Update();

  return rs->GetOutput();
}

//------------
// ScaleImage 
//-------------
// Scale an image.
//
// Note: This is taken from the SV sv4guiProjectManager::AddImage() code. 
//
void ScaleImage(PyImage* self, double scale)
{
  auto image = self->image_data;
  mitk::Point3D origin = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetOrigin();
  mitk::Vector3D spacing = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetSpacing();
  origin[0] *= scale;
  origin[1] *= scale;
  origin[2] *= scale;
  image->SetOrigin(origin);
  image->SetSpacing(scale * spacing);
  image->UpdateOutputInformation();
}

//------------
// WriteImage
//------------
// Write the image to a VTK .vti file. 
//
void WriteImage(PyImage* self, const std::string& fileName)
{
  vtkImageData* vtkImg = sv4guiVtkUtils::MitkImage2VtkImage(self->image_data);

  if (vtkImg == nullptr) {
    throw std::runtime_error("Unable to get VTK image data.");
  }

  vtkSmartPointer<vtkXMLImageDataWriter> writer = vtkSmartPointer<vtkXMLImageDataWriter>::New();
  writer->SetFileName(fileName.c_str());
  writer->SetInputData(vtkImg);
  writer->Write();
}

//---------------------
// WriteImageTransform
//---------------------
// Write the image transformation.
//
// Note: This reproduces the SV sv4guiProjectManager::writeTransformFile() method.
//
bool WriteImageTransform(PyImage* self, const std::string& fileName)
{
  //std::cout << "========== WriteImageTransform ==========" << std::endl;
  TiXmlDocument document;
  auto decl = new TiXmlDeclaration( "1.0", "UTF-8", "" );
  document.LinkEndChild( decl );

  auto  root = new TiXmlElement("Transform");
  document.LinkEndChild(root);

  auto xformElement = new TiXmlElement("transform");
  auto transform = self->image_data->GetGeometry()->GetVtkMatrix();

  for (int j = 0; j < 3; j++){
      for (int i = 0; i < 3; i++){
          auto value = transform->GetElement(i,j);
          auto label = "t" + std::to_string(i) + std::to_string(j);
          //std::cout << "[WriteImageTransform] label: " << label << "  value: " << value << std::endl;
          xformElement->SetDoubleAttribute(label, value);
      }
  }

  root->LinkEndChild(xformElement);

  return document.SaveFile(fileName);
}

//----------------
// GetPathElement
//----------------
// Get the path PathElement object.
//
/*
static PathElement*
GetPathElement(PyUtilApiFunction& api, PyPath* self)
{
  auto path = self->path;
  if (path == NULL) {
      api.error("The path element data has not be created.");
      return nullptr;
  }
  return path;
}
*/

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'Image' class methods.

//----------------
// get_dimensions 
//----------------
//
PyDoc_STRVAR(Image_extract_slice_doc,
  "extract_slice() \n\
   \n\
   Extract a slice from the image.  \n\
   \n\
");


static PyObject *
Image_extract_slice(PyImage* self, PyObject* args)
{
  std::cout << std::endl;
  std::cout << "========== Image_extract_slice ==========" << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  if (!CheckImageData(api, self)) {
      return nullptr;
  }

  auto image = self->image_data;
  auto dimensions = image->GetDimensions();
  auto imageGeometry = image->GetGeometry();
  auto origin = image->GetGeometry()->GetOrigin();

  /*
  double pos[3] = { 2.438, 5.200, 82.219};
  double tangent[3] = { -0.047, -0.121, -0.992};
  double rotation[3] = {0.000, 0.993, -0.121};

  double pos[3] = { 0.412,0.016,50.294 }; 
  double tangent[3] = { -0.094,-0.242,-0.966 };
  double rotation[3] = { 0.000,0.970,-0.243 };
  double planeSize = 80.0;

  double pos[3] = {2.824, -5.721, 30.409 }; 
  double tangent[3] = {0.223, -0.223, -0.949 };
  double rotation[3] = { 0.000, 0.973, -0.229 };

  // Saggital
  double pos[3] = {2.824, -5.721, 30.409 }; 
  double tangent[3] = {1.0, 0.0, 0.0 };
  double rotation[3] = { 0.0, 1.0, 0.0 };

  // Coronal, (239, 252, 31)
  double pos[3] = {0.88, -12.8, -16.2 }; 
  double tangent[3] = {0.0, -1.0, 0.0 };
  double rotation[3] = { 1.0, 0.0, 0.0 };
  */

  // Axial (239, 252, 31) but LR switched 
  double pos[3] = {0.88, -12.8, -16.2 }; 
  double tangent[3] = {0.0, 0.0, -1.0 };
  double rotation[3] = { -1.0, 0.0, 0.0 };

  double planeSize = 80.0;

  std::cout << "[Image_extract_slice] Pos: " << pos[0] << " " << pos[1] << " " << pos[2] << std::endl;
  std::cout << "[Image_extract_slice] Tangent: " << tangent[0] << " " << tangent[1] << " " << tangent[2] << std::endl;
  std::cout << "[Image_extract_slice] Rotation: " << rotation[0] << " " << rotation[1] << " " << rotation[2] << std::endl;

  // Create an oriented plane used to slice the image.
  auto planeGeometry = CreatePlaneGeometry(image, planeSize, pos, tangent, rotation);

  mitk::SlicedGeometry3D::Pointer slicedGeo3D=mitk::SlicedGeometry3D::New();
  slicedGeo3D->SetEvenlySpaced(false);
  slicedGeo3D->InitializeSlicedGeometry(1);
  slicedGeo3D->SetPlaneGeometry(planeGeometry,0);

  auto geometry = image->GetTimeGeometry()->GetGeometryForTimeStep(0);
  slicedGeo3D->SetReferenceGeometry(geometry);
  slicedGeo3D->SetBounds(geometry->GetBounds());
  slicedGeo3D->SetOrigin(geometry->GetOrigin());
  slicedGeo3D->SetIndexToWorldTransform(geometry->GetIndexToWorldTransform());

  mitk::VtkResliceInterpolationProperty::Pointer interProp = mitk::VtkResliceInterpolationProperty::New();
  interProp->SetInterpolationToNearest();

  mitk::ExtractSliceFilter::Pointer slicer = mitk::ExtractSliceFilter::New();
  slicer->SetInput(image);
  slicer->SetTimeStep(0);
  slicer->SetWorldGeometry(planeGeometry);
  slicer->SetResliceTransformByGeometry(image->GetTimeGeometry()->GetGeometryForTimeStep(0));
  slicer->SetVtkOutputRequest(true);
  slicer->Modified();
  slicer->Update();

  //vtkSmartPointer<vtkImageData> slice = vtkSmartPointer<vtkImageData>::New();
  auto slice = slicer->GetVtkOutput();


  double* slice_origin = slice->GetOrigin();
  int* slice_extent = slice->GetExtent();
  double* slice_spacing = slice->GetSpacing();

  std::cout << "[Image_extract_slice] Slice: "<<std::endl;
  std::cout << "[Image_extract_slice]   Min scalar: " << slice->GetScalarRange()[0] << std::endl;
  std::cout << "[Image_extract_slice]   Max scalar: " << slice->GetScalarRange()[1] << std::endl;
  std::cout << "[Image_extract_slice]   Orign: "<<slice_origin[0]<<","<<slice_origin[1]<<","<<slice_origin[2]<<std::endl;
  std::cout << "[Image_extract_slice]   Extent: " << slice_extent[0] << ", " << slice_extent[1] << "; " << slice_extent[2] << ", " << slice_extent[3] << "; " << slice_extent[4] << ", " << slice_extent[5] << std::endl;
  std::cout << "[Image_extract_slice]   Spacing: " << slice_spacing[0] << ", " << slice_spacing[1] << ", " << slice_spacing[2] << std::endl;

  std::cout<<"[Image_extract_slice] Structured points: "<<std::endl;
  auto vtkStructPts = sv3::SegmentationUtils::vtkImageData2cvStrPts(slice);
  auto slicePts = vtkStructPts->GetVtkStructuredPoints();

  int* dims = slicePts->GetDimensions();
  std::cout << "[Image_extract_slice]   Dims: " << " x: " << dims[0] << " y: " << dims[1] << " z: " << dims[2] << std::endl;
  
  std::cout << "[Image_extract_slice]   Number of points: " << slicePts->GetNumberOfPoints() << std::endl;
  std::cout << "[Image_extract_slice]   Number of cells: " << slicePts->GetNumberOfCells() << std::endl;
  std::cout << "[Image_extract_slice]   Min scalar: " << slicePts->GetScalarRange()[0] << std::endl;
  std::cout << "[Image_extract_slice]   Max scalar: " << slicePts->GetScalarRange()[1] << std::endl;
  
  for (int z = 0; z < dims[2]; z++) {
    for (int y = 0; y < dims[1]; y++) {
      for (int x = 0; x < dims[0]; x++) {
        double* pixel = static_cast<double*>(slicePts->GetScalarPointer(x,y,z));
        }
      }
    }

  return vtkPythonUtil::GetObjectFromPointer(slicePts);
  //return vtkPythonUtil::GetObjectFromPointer(slice);
}


static PyObject *
Image_extract_slice_1(PyImage* self, PyObject* args)
{
  std::cout << std::endl;
  std::cout << "========== Image_extract_slice ==========" << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  if (!CheckImageData(api, self)) {
      return nullptr;
  }

  auto dimensions = self->image_data->GetDimensions();
  auto imageGeometry = self->image_data->GetGeometry();
  auto origin = self->image_data->GetGeometry()->GetOrigin();

  /*
  auto currentGeometry = mitk::BaseGeometry::ConstPointer();
  currentGeometry = self->image_data->GetGeometry();

  bool top = false;
  bool frontside = false;
  bool rotated = false;

  auto slicedWorldGeometry = mitk::SlicedGeometry3D::New();
  slicedWorldGeometry->InitializePlanes(currentGeometry, mitk::PlaneGeometry::Axial, top, frontside, rotated);
  */

  auto slicedGeom = mitk::SlicedGeometry3D::New();
  slicedGeom->SetEvenlySpaced(true);
  slicedGeom->InitializeSlicedGeometry(1);

  // Define a plane to slice the 3D image.
  //
  int sliceIndex = 30; 
  bool isFrontside = true;
  bool isRotated = false;
  mitk::PlaneGeometry::Pointer plane = mitk::PlaneGeometry::New();
  plane->InitializeStandardPlane(imageGeometry, mitk::PlaneGeometry::Frontal, sliceIndex, isFrontside, isRotated);
  //plane->InitializeStandardPlane(imageGeometry, mitk::PlaneGeometry::Axial, sliceindex, isFrontside, isRotated);
  plane->SetOrigin(origin);

  mitk::Vector3D normal;
  normal = plane->GetNormal();
  std::cout << "[Image_extract_slice] Plane normal: " << normal[0] << " " << normal[1] << " " << normal[2] << std::endl;

  // Extract slice.
  //
  //vtkSmartPointer<mitkVtkImageOverwrite> resliceIdx = vtkSmartPointer<mitkVtkImageOverwrite>::New();
  mitk::ExtractSliceFilter::Pointer slicer = mitk::ExtractSliceFilter::New();
  //mitk::ExtractSliceFilter::Pointer slicer = mitk::ExtractSliceFilter::New(resliceIdx);
  slicer->SetInput(self->image_data);
  slicer->SetWorldGeometry(plane);
  slicer->SetVtkOutputRequest(true);
  slicer->Modified();
  slicer->Update();

  //vtkSmartPointer<vtkImageData> slice = vtkSmartPointer<vtkImageData>::New();
  auto slice = slicer->GetVtkOutput();

  /*
  double valuesRange[2];
  vtkDoubleArray::SafeDownCast(slice->GetPointData()->GetArray("ImageScalars"))->GetValueRange(valuesRange);
  std::cout << "valuesRange = " << valuesRange[0] << " " << valuesRange[1] << std::endl;
  */
  std::cout << "Min scalar: " << slice->GetScalarRange()[0] << std::endl;
  std::cout << "Max scalar: " << slice->GetScalarRange()[1] << std::endl;

  int sdims[3];
  slice->GetDimensions(sdims);

  /*
  for (int z = 0; z < sdims[2]; z++) {
    for (int y = 0; y < sdims[1]; y++) {
      for (int x = 0; x < sdims[0]; x++) {
        double* pixel = static_cast<double*>(slice->GetScalarPointer(x,y,z));
        printf("%g \n", pixel[0]);
        }
      std::cout << std::endl;
      }
    std::cout << std::endl;
  }
  */
  

  return vtkPythonUtil::GetObjectFromPointer(slice);
}

//----------------
// get_dimensions 
//----------------
//
PyDoc_STRVAR(Image_get_dimensions_doc,
  "get_dimensions() \n\
   \n\
   Get the image dimensions.  \n\
   \n\
");

static PyObject *
Image_get_dimensions(PyImage* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  if (!CheckImageData(api, self)) {
      return nullptr;
  } 

  auto dimensions = self->image_data->GetDimensions();
  return Py_BuildValue("[i, i, i]", dimensions[0], dimensions[1], dimensions[2]);
}

//------------
// get_origin 
//------------
//
PyDoc_STRVAR(Image_get_origin_doc,
  "get_origin() \n\
   \n\
   Get the image origin.  \n\
   \n\
");

static PyObject *
Image_get_origin(PyImage* self, PyObject* args)
{ 
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  if (!CheckImageData(api, self)) {
      return nullptr;
  } 

  auto origin = self->image_data->GetGeometry()->GetOrigin();
  return Py_BuildValue("[d, d, d]", origin[0], origin[1], origin[2]);
}

//-------------------
// Image_get_spacing 
//-------------------
//
PyDoc_STRVAR(Image_get_spacing_doc,
  "get_spacing() \n\
   \n\
   Get the image spacing.  \n\
   \n\
");

static PyObject *
Image_get_spacing(PyImage* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  if (!CheckImageData(api, self)) {
      return nullptr;
  } 

  auto spacing = self->image_data->GetGeometry()->GetSpacing();
  return Py_BuildValue("[d, d, d]", spacing[0], spacing[1], spacing[2]);
}

//-------------
// Image_read  
//-------------
//
PyDoc_STRVAR(Image_read_doc,
  "read(file_name, scale=None) \n\
   \n\
   Create image data from a file. \n\
   \n\
   \n\
   Args:                                    \n\
     file_name (str): The name of the file to read image data from. \n\
     scale (Optional[float]): The value used to scale the image data. \n\
");

static PyObject *
Image_read(PyImage* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", "scale_factor", NULL};
  char* fileName = NULL;
  PyObject* scaleObj = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName, &PyFloat_Type, &scaleObj)) {
    return api.argsError();
  }

  self->image_node = ReadFile(std::string(fileName));
  self->image_data  = dynamic_cast<mitk::Image*>(self->image_node->GetData());

  // Process 'scale' argument.
  if (scaleObj != nullptr) {
      double scale = PyFloat_AsDouble(scaleObj);
      if (PyErr_Occurred()) {
          return nullptr;
      }

      if (scale <= 0.0) {
          api.error("The 'scale ' argument must be >= 0.0.");
          return nullptr;
      }

      ScaleImage(self, scale);
  }

  Py_RETURN_NONE;
}

//---------------------------
// Image_read_transformation 
//---------------------------
//
PyDoc_STRVAR(Image_read_transformation_doc,
  "read_transformation(file_name) \n\
   \n\
   Rean an image transformation from a file. \n\
   \n\
   \n\
   Args:                                    \n\
     file_name (str): The name of the image transformation file to read. \n\
");

static PyObject *
Image_read_transformation(PyImage* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileName = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }

  if (!CheckImageData(api, self)) {
      return nullptr;
  }

  // Read in the transformtion XML file and apply it to the image.
  //
  try {
      ReadImageTransform(self, std::string(fileName));

  } catch (std::exception &e) {
      api.error(e.what());
      return nullptr;
  }

  Py_RETURN_NONE;
}

//-------------
// Image_scale 
//-------------
//
PyDoc_STRVAR(Image_scale_doc,
  "scale(scale_factor) \n\
   \n\
   Scale the image by a scaling factor.  \n\
   \n\
   \n\
   Args:                                    \n\
     scale_factor (float): The scaling factor. \n\
");

static PyObject *
Image_scale(PyImage* self, PyObject* args, PyObject* kwargs)
{ 
  auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);
  static char *keywords[] = {"scale_factor", NULL};
  double scale;
  
  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &scale)) {
    return api.argsError();
  }

  if (scale <= 0.0) {
      api.error("The 'scale_factor' argument must be >= 0.0.");
      return nullptr;
  }

  ScaleImage(self, scale);

  Py_RETURN_NONE;
}

//--------------
// Image_origin 
//--------------
//
PyDoc_STRVAR(Image_set_origin_doc,
  "set_origin(origin) \n\
   \n\
   Set the image origin.  \n\
   \n\
   \n\
   Args:                                    \n\
     origin (list([float,float,float]): The 3D point to set the image origin to.  \n\
");

static PyObject *
Image_set_origin(PyImage* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"origin", NULL};
  PyObject* originArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &originArg)) {
    return api.argsError();
  }

  std::string emsg;
  std::array<double,3> origin;
  if (!PyUtilGetPointData(originArg, emsg, origin.data())) {
      api.error("The 'origin' argument " + emsg);
      return nullptr;
  }

  mitk::Point3D mitkOrigin = {origin.data()};
  self->image_data->SetOrigin(mitkOrigin);

  Py_RETURN_NONE;
}

PyDoc_STRVAR(Image_set_spacing_doc,
  "set_spacing(origin) \n\
   \n\
   Set the image spacing.  \n\
   \n\
   \n\
   Args:                                    \n\
     spacing (list([float,float,float]): The three values used to set the image spacing.  \n\
");

static PyObject *
Image_set_spacing(PyImage* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"spacing", NULL};
  PyObject* spacingArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &spacingArg)) {
    return api.argsError();
  }

  std::string emsg;
  std::array<double,3> spacing;
  if (!PyUtilGetPointData(spacingArg, emsg, spacing.data())) {
      api.error("The 'spacing' argument " + emsg);
      return nullptr;
  }

  for (auto value : spacing) {
      if (value <= 0.0) {
          api.error("The 'spacing' argument must contain values that are > 0.0.");
          return nullptr;
      }
  }

  mitk::Vector3D mitkSpacing = {spacing.data()};
  self->image_data->SetSpacing(mitkSpacing);

  Py_RETURN_NONE;
}

//-----------------
// Image_transform 
//-----------------
//
PyDoc_STRVAR(Image_transform_doc,
  "transform(matrix) \n\
   \n\
   Transform the image using a 4x4 transformation matrix. \n\
   \n\
   \n\
   Args:                                    \n\
     matrix (vtkMatrix4x4): The 4x4 transformation matrix. \n\
");

static PyObject *
Image_transform(PyImage* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  static char *keywords[] = {"matrix", NULL};
  PyObject* matrixArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &matrixArg)) {
    return api.argsError();
  }

  // Get the vtkMatrix4x4 object.
  auto matrix = (vtkMatrix4x4*)vtkPythonUtil::GetPointerFromObject(matrixArg, "vtkMatrix4x4");
  if (matrix == nullptr) { 
      api.error("The 'matrix' argument is not a vtkMatrix4x4 object.");
      return nullptr;
  }
  std::cout << "[Image_transform] ---------- matrix ----------" << std::endl;
  vtkIndent indent;
  matrix->PrintSelf(std::cout, indent);
  std::cout << "[Image_transform] ----------------------------" << std::endl;

  auto transform = self->image_data->GetGeometry()->GetVtkMatrix();
  std::cout << "[Image_transform] ---------- transform ----------" << std::endl;
  transform->PrintSelf(std::cout, indent);
  std::cout << "[Image_transform] ----------------------------" << std::endl;

  // Transform the image.
  self->image_data->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(matrix);
  self->image_data->UpdateOutputInformation();

  Py_RETURN_NONE;
}

//-------------
// Image_write
//------------
//
PyDoc_STRVAR(Image_write_doc,
  "write(file_name) \n\
   \n\
   Write the image to a VTK VTI format file. \n\
   \n\
   \n\
   Args:                                    \n\
     file_name (str): The name of the file to write the image to. \n\
");

static PyObject *
Image_write(PyImage* self, PyObject* args, PyObject* kwargs)
{ 
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileName = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }

  if (!CheckImageData(api, self)) {
      return nullptr;
  } 

  // Check that you can write to the file.
  ofstream cfile;
  cfile.open(fileName);
  if (!cfile.is_open()) {
      api.error("Unable to write the image to the file named '" + std::string(fileName) + "'.");
      return nullptr;
  } else {
    cfile.close();
  }

  try {
      WriteImage(self, std::string(fileName));
  } catch (std::exception &e) {
      api.error(e.what());
      return nullptr;
  }

  Py_RETURN_NONE;
}

//----------------------------
// Image_write_transformation 
//----------------------------
//
PyDoc_STRVAR(Image_write_transformation_doc,
  "write_transformation(file_name) \n\
   \n\
   Write the image transformation to a file. \n\
   \n\
   \n\
   Args:                                    \n\
     file_name (str): The name of the file to write the image transformation to. \n\
");

static PyObject *
Image_write_transformation(PyImage* self, PyObject* args, PyObject* kwargs)
{ 
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileName = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }

  if (!CheckImageData(api, self)) {
      return nullptr;
  } 

  // Check that you can write to the file.
  ofstream cfile;
  cfile.open(fileName);
  if (!cfile.is_open()) {
      api.error("Unable to write the image transformatio to the file named '" + std::string(fileName) + "'.");
      return nullptr;
  } else {
    cfile.close();
  }

  if (!WriteImageTransform(self, std::string(fileName))) {
      api.error("Unable to write the image transformatio to the file named '" + std::string(fileName) + "'.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* IMAGE_CLASS = "Image";
// Dotted name that includes both the module name and
// the name of the type within the module.
static char* IMAGING_MODULE_CLASS = "imaging.Image";

//----------------
// ImageClass_doc
//----------------
// Define the Image class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ImageClass_doc,
   "The Image class provides methods for                                       \n\
   \n\
");

//----------------
// PyImageMethods
//----------------
// Image class methods.
//
static PyMethodDef PyImageMethods[] = {

  {"extract_slice", (PyCFunction)Image_extract_slice, METH_VARARGS|METH_KEYWORDS, Image_extract_slice_doc },
  {"get_dimensions", (PyCFunction)Image_get_dimensions, METH_NOARGS, Image_get_dimensions_doc },
  {"get_origin", (PyCFunction)Image_get_origin, METH_NOARGS, Image_get_origin_doc},
  {"get_spacing", (PyCFunction)Image_get_spacing, METH_NOARGS, Image_get_spacing_doc },
  {"read", (PyCFunction)Image_read, METH_VARARGS|METH_KEYWORDS, Image_read_doc },
  {"read_transformation", (PyCFunction)Image_read_transformation, METH_VARARGS|METH_KEYWORDS, Image_read_transformation_doc},
  {"scale", (PyCFunction)Image_scale, METH_VARARGS|METH_KEYWORDS, Image_scale_doc},
  {"set_origin", (PyCFunction)Image_set_origin, METH_VARARGS|METH_KEYWORDS, Image_set_origin_doc},
  {"set_spacing", (PyCFunction)Image_set_spacing, METH_VARARGS|METH_KEYWORDS, Image_set_spacing_doc},
  {"transform", (PyCFunction)Image_transform, METH_VARARGS|METH_KEYWORDS, Image_transform_doc },
  {"write", (PyCFunction)Image_write, METH_VARARGS|METH_KEYWORDS, Image_write_doc},
  {"write_transformation", (PyCFunction)Image_write_transformation, METH_VARARGS|METH_KEYWORDS, Image_write_transformation_doc},

  {NULL,NULL}
};

//-------------
// PyImageType
//-------------
// Define the Python type object that stores Image data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyImageType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  IMAGING_MODULE_CLASS,
  sizeof(PyImage)
};

//-------------
// PyImageInit
//-------------
// This is the __init__() method for the Image class.
//
// This function is used to initialize an object after it is created.
//
static int
PyImageInit(PyImage* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  std::cout << "[PyImageInit] New Path object: " << numObjs << std::endl;
  //self->path = new PathElement();
  self->id = numObjs;
  numObjs += 1;
  return 0;
}

//------------
// PyImageNew
//------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyImageNew(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
  static char *keywords[] = {"file_name", "scale_factor", NULL};
  auto api = PyUtilApiFunction("|sO!", PyRunTimeErr, "imaging.Image");
  char* fileNameArg = nullptr; 
  PyObject* scaleObj = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileNameArg, &PyFloat_Type, &scaleObj)) {
      return api.argsError();
  }

  std::cout << "[PyImageNew] PyImageNew " << std::endl;
  auto self = (PyImage*)type->tp_alloc(type, 0);
  if (self != NULL) {
      self->id = 1;
  }

  if (fileNameArg != nullptr) {
      std::cout << "[PyImageNew] fileNameArg: " << fileNameArg << std::endl;
      self->image_node = ReadFile(std::string(fileNameArg));
      self->image_data  = dynamic_cast<mitk::Image*>(self->image_node->GetData());
  }

  // Process 'scale' argument.
  if (scaleObj != nullptr) {
      double scale = PyFloat_AsDouble(scaleObj);
      if (PyErr_Occurred()) {
          return nullptr;
      }

      if (scale <= 0.0) {
          api.error("The 'scale ' argument must be >= 0.0.");
          return nullptr;
      }

      std::cout << "[PyImageNew] Scale image: " << scale << std::endl;
      ScaleImage(self, scale);
  }

  return (PyObject*)self;
}

//----------------
// PyImageDealloc
//----------------
//
static void
PyImageDealloc(PyImage* self)
{
  //std::cout << "[PyImageDealloc] Free PyImage" << std::endl;
  //delete self->path;
  Py_TYPE(self)->tp_free(self);
}

//--------------------
// SetImageTypeFields
//--------------------
// Set the Python type object fields that stores Image data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetPyImageTypeFields(PyTypeObject& imageType)
{
  // Doc string for this type.
  imageType.tp_doc = ImageClass_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  imageType.tp_new = PyImageNew;
  imageType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  imageType.tp_init = (initproc)PyImageInit;
  imageType.tp_dealloc = (destructor)PyImageDealloc;
  imageType.tp_methods = PyImageMethods;
}

//---------------
// CreatePyImage
//---------------
// Create a PyImage object.
//
PyObject *
CreatePyImage()
//CreatePyImage(PathElement* path)
{
  //std::cout << "[CreatePyImage] Create Path object ... " << std::endl;
  auto imageObj = PyObject_CallObject((PyObject*)&PyImageType, NULL);
  auto pyImage = (PyImage*)imageObj;

  /*
  if (path != nullptr) {
      delete pyPath->path;
      pyPath->path = path;
  }
  */
  //std::cout << "[CreatePyImage] pyPath id: " << pyPath->id << std::endl;
  //std::cout << "[CreatePyImage] pathObj ref count: " << Py_REFCNT(pathObj) << std::endl;
  return imageObj;
}

