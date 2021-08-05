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

#include "sv4gui_SegmentationUtils.h"
#include "sv4gui_VtkUtils.h"

#include "SimVascular.h"
#include "sv_StrPts.h"
#include "sv3_ITKLset_ITKUtils.h"
#include "sv_sys_geom.h"
#include "sv_vtk_utils.h"
#include "sv3_ITKLevelSet.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include "mitkProperties.h"
#include <mitkInteractionConst.h>
#include <mitkPointOperation.h>
#include <mitkOperationEvent.h>
#include <mitkUndoController.h>
#include <mitkNodePredicateDataType.h>
#include <mitkStandaloneDataStorage.h>
#include <mitkImage.h>
#include <mitkLookupTable.h>
#include <mitkLookupTableProperty.h>
#include <mitkExtractSliceFilter.h>

#include <vtkPoints.h>
#include <vtkPolyData.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkImageReslice.h>
#include <vtkLookupTable.h>
#include <vtkTexture.h>
#include <vtkPlaneSource.h>
#include <vtkTextureMapToPlane.h>
#include <vtkDataSetMapper.h>
#include <vtkActor.h>
#include <vtkOutlineFilter.h>
#include <vtkPolyDataMapper.h>
#include <vtkRendererCollection.h>
#include <vtkProperty.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkContourFilter.h>
#include <vtkPolyDataConnectivityFilter.h>

#include <vtkXMLImageDataWriter.h>

#include "sv4gui_ImageProcessingUtils.h"
#include <mitkImageCast.h>

#include <vtkPlaneSource.h>
#include "vtkXMLPolyDataWriter.h"

#include <iostream>
using namespace std;

double SV_PI=3.1415926535;

double math_radToDeg(double rad){
    return rad*180.0/SV_PI;
}

double math_dot(double vecA[3], double vecB[3])
{
    return vecA[0]*vecB[0]+vecA[1]*vecB[1]+vecA[2]*vecB[2];
}

void math_cross(double cross[3], double vecA[3], double vecB[3])
{
    cross[0]=vecA[1]*vecB[2]-vecA[2]*vecB[1];
    cross[1]=vecA[2]*vecB[0]-vecA[0]*vecB[2];
    cross[2]=vecA[0]*vecB[1]-vecA[1]*vecB[0];
}

double math_magnitude(double vecA[3])
{
    return sqrt(math_dot(vecA,vecA));
}

double math_angleBtw3DVectors(double vecA[3], double vecB[3])
{
    double dot=math_dot(vecA, vecB);
    double magA=math_magnitude(vecA);
    double magB=math_magnitude(vecB);
    double cosTheta=dot / (magA * magB);
    if (cosTheta >= 1) {
        cosTheta=1;
    }
    return acos(cosTheta);
}

sv4guiSegmentationUtils::sv4guiSegmentationUtils()
{
}

sv4guiSegmentationUtils::~sv4guiSegmentationUtils()
{
}

//-----------------
// GetvtkTransform
//-----------------
// Compute the transformation used to translate and rotate a plane with 
// origin = [0,0,0] and normal = [0,0,1] to the 'pathPoint' point and normal.
//
// Since we are transforming from the standard basis just set the columns
// of the rotation matrix with the path point local basis.
//
vtkSmartPointer<vtkTransform> 
sv4guiSegmentationUtils::GetvtkTransform(sv4guiPathElement::sv4guiPathPoint pathPoint)
{
    double pos[3], tangent[3], normal[3], binormal[3];

    // Define path local basis.
    for (int i = 0; i < 3; i++) {
        pos[i] = pathPoint.pos[i];
        tangent[i] = pathPoint.tangent[i];
        normal[i] = pathPoint.rotation[i];
    }

    binormal[0] = tangent[1]*normal[2] - tangent[2]*normal[1];
    binormal[1] = tangent[2]*normal[0] - tangent[0]*normal[2];
    binormal[2] = tangent[0]*normal[1] - tangent[1]*normal[0];

    // Define rotation matrix.
    auto matrix = vtkSmartPointer<vtkMatrix4x4>::New();
    matrix->Identity();
    int a0 = 2, a1 = 0, a2 = 1;

    for (int i = 0; i < 3; i++) {
        matrix->SetElement(i, a0, tangent[i]);
        matrix->SetElement(i, a1, normal[i]);
        matrix->SetElement(i, a2, binormal[i]);
    }

    // Create transformation to rotate and then translate.
    auto transform = vtkSmartPointer<vtkTransform>::New();
    transform->Identity();
    transform->Translate(pos[0], pos[1], pos[2]);
    transform->Concatenate(matrix);

    return transform;
}

vtkTransform* sv4guiSegmentationUtils::GetvtkTransformBox(sv4guiPathElement::sv4guiPathPoint pathPoint,double boxHeight)
{
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

    double zhat[3]={0,0,1};
    double theta=math_radToDeg(math_angleBtw3DVectors(zhat,nrm));
    double axis[3];
    math_cross(axis,zhat,nrm);

    vtkTransform* tmpTr=vtkTransform::New();
    tmpTr->Identity();
    tmpTr->RotateWXYZ(theta,axis);

    vtkPoints* tmpPt=vtkPoints::New();
    tmpPt->InsertNextPoint(1, 0, 0);

    vtkPolyData* tmpPd=vtkPolyData::New();
    tmpPd->SetPoints(tmpPt);

    vtkTransformPolyDataFilter* tmpTf=vtkTransformPolyDataFilter::New();
    tmpTf->SetInputDataObject(tmpPd);
    tmpTf->SetTransform(tmpTr);
    tmpTf->Update();
    double pt[3];
    tmpTf->GetOutput()->GetPoint(0,pt);

    tmpTr->Delete();
    tmpPt->Delete();
    tmpPd->Delete();
    tmpTf->Delete();

    double rot=math_radToDeg(math_angleBtw3DVectors(pt,xhat));

    double x[3];
    math_cross(x,pt,xhat);
    double d=math_dot(x,nrm);
    if (d < 0.0) {
        rot=-rot;
    }

    vtkTransform* tr=vtkTransform::New();
    tr->Identity();
    tr->Translate(pos);
    tr->RotateWXYZ(rot,nrm);
    tr->RotateWXYZ(theta,axis);
    tr->Translate(0,0,boxHeight/4.0);

    return tr;
}

//---------------------
// CreatePlaneGeometry
//---------------------
// Create a plane geometry used to extact a 2D slice from a 3D image.
//
// The plane position and orientation is determined by 'pathPoint'.
//
mitk::PlaneGeometry::Pointer 
sv4guiSegmentationUtils::CreatePlaneGeometryFromSpacing(sv4guiPathElement::sv4guiPathPoint pathPoint, 
    mitk::Vector3D spacing, double size)
{
    auto tr=GetvtkTransform(pathPoint);
    mitk::PlaneGeometry::Pointer planegeometry = mitk::PlaneGeometry::New();
    planegeometry->SetIndexToWorldTransformByVtkMatrix(tr->GetMatrix());

    mitk::Vector3D right,bottom;
    right.SetVnlVector( planegeometry->GetIndexToWorldTransform()->GetMatrix().GetVnlMatrix().get_column(0) );
    bottom.SetVnlVector( planegeometry->GetIndexToWorldTransform()->GetMatrix().GetVnlMatrix().get_column(1) );

    mitk::Point3D pos;
    pos[0]=pathPoint.pos[0]-right[0]*size/2.0-bottom[0]*size/2.0;
    pos[1]=pathPoint.pos[1]-right[1]*size/2.0-bottom[1]*size/2.0;
    pos[2]=pathPoint.pos[2]-right[2]*size/2.0-bottom[2]*size/2.0;

    planegeometry->SetOrigin(pos);
    planegeometry->SetSpacing(spacing);

    double width=size/spacing[0];
    double height=size/spacing[1];

    mitk::ScalarType bounds[6] = { 0, width, 0, height, 0, 1 };
    planegeometry->SetBounds( bounds );

    mitk::Vector3D normal;
    normal = planegeometry->GetNormal();

    return planegeometry;
}

//---------------------
// CreatePlaneGeometry
//---------------------
// Create an oriented plane used to slice an image.
//
// The plane position and orientation is determined by the input sv4guiPathPoint.
//
mitk::PlaneGeometry::Pointer 
sv4guiSegmentationUtils::CreatePlaneGeometry(sv4guiPathElement::sv4guiPathPoint pathPoint, 
    mitk::BaseData* baseData, double size, bool useOnlyMinimumSpacing)
{
    mitk::Vector3D newSpacing;
    mitk::Image* image = dynamic_cast<mitk::Image*>(baseData);

    if (image) {
        mitk::Vector3D imageSpacing = image->GetTimeGeometry()->GetGeometryForTimeStep(0)->GetSpacing();
        if (useOnlyMinimumSpacing) {
            double minSpacing = std::min(imageSpacing[0],std::min(imageSpacing[1],imageSpacing[2]));
            newSpacing.Fill(minSpacing);
        } else {
            auto tr = GetvtkTransform(pathPoint);
            mitk::PlaneGeometry::Pointer planeGeometry1 = mitk::PlaneGeometry::New();
            planeGeometry1->SetIndexToWorldTransformByVtkMatrix(tr->GetMatrix());

            // Use the same way as mitk::ExtractSliceFilter to calculate spacing
            mitk::Vector3D right = planeGeometry1->GetAxisVector(0);
            mitk::Vector3D bottom = planeGeometry1->GetAxisVector(1);
            mitk::Vector3D normal = planeGeometry1->GetNormal();

            right.Normalize();
            bottom.Normalize();
            normal.Normalize();

            right=size*right;
            bottom=size*bottom;

            mitk::Vector3D rightInIndex, bottomInIndex,normalInIndex;
            image->GetTimeGeometry()->GetGeometryForTimeStep(0)->WorldToIndex( right, rightInIndex );
            image->GetTimeGeometry()->GetGeometryForTimeStep(0)->WorldToIndex( bottom, bottomInIndex );
            image->GetTimeGeometry()->GetGeometryForTimeStep(0)->WorldToIndex( normal, normalInIndex );
            newSpacing[0] = size/rightInIndex.GetNorm();
            newSpacing[1] = size/bottomInIndex.GetNorm();
            newSpacing[2] = 1.0/normalInIndex.GetNorm();
        }

    } else {
        newSpacing[0] = 0.1;
        newSpacing[1] = 0.1;
        newSpacing[2] = 0.1;
    }

    mitk::PlaneGeometry::Pointer planeGeometry = CreatePlaneGeometryFromSpacing(pathPoint, newSpacing, size);

    if (baseData) {
        planeGeometry->SetReferenceGeometry(baseData->GetTimeGeometry()->GetGeometryForTimeStep(0));
    }

    return planeGeometry;
}

//----------------------
// CreateSlicedGeometry
//----------------------
//
mitk::SlicedGeometry3D::Pointer sv4guiSegmentationUtils::CreateSlicedGeometry(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints, mitk::BaseData* baseData, double size, bool useOnlyMinimumSpacing)
{
    mitk::SlicedGeometry3D::Pointer slicedGeo3D=mitk::SlicedGeometry3D::New();
    slicedGeo3D->SetEvenlySpaced(false);
    slicedGeo3D->InitializeSlicedGeometry(pathPoints.size());
    mitk::Image* image=dynamic_cast<mitk::Image*>(baseData);

    for (int i = 0; i < pathPoints.size(); i++) {
        mitk::PlaneGeometry::Pointer planegeometry = CreatePlaneGeometry(pathPoints[i], baseData,
          size, useOnlyMinimumSpacing);

        if (image) {
            planegeometry->SetImageGeometry(true);
         }

        slicedGeo3D->SetPlaneGeometry(planegeometry,i);
    }

    if (baseData) {
        auto geometry = baseData->GetTimeGeometry()->GetGeometryForTimeStep(0);
        slicedGeo3D->SetReferenceGeometry(geometry);
        slicedGeo3D->SetBounds(geometry->GetBounds());
        slicedGeo3D->SetOrigin(geometry->GetOrigin());
        slicedGeo3D->SetIndexToWorldTransform(geometry->GetIndexToWorldTransform());
    }
    return slicedGeo3D;
}

//---------------
// GetSliceImage
//---------------
//
mitk::Image::Pointer sv4guiSegmentationUtils::GetSliceImage(const mitk::PlaneGeometry* planeGeometry, const mitk::Image* image, unsigned int timeStep)
{
    if ( !image || !planeGeometry ) return NULL;

    //    //Make sure that for reslicing and overwriting the same alogrithm is used. We can specify the mode of the vtk reslicer
    //    vtkSmartPointer<mitkVtkImageOverwrite> reslice = vtkSmartPointer<mitkVtkImageOverwrite>::New();
    //    //set to false to extract a slice
    //    reslice->SetOverwriteMode(false);
    //    reslice->Modified();

    //use ExtractSliceFilter with our specific vtkImageReslice for overwriting and extracting
    //    mitk::ExtractSliceFilter::Pointer extractor =  mitk::ExtractSliceFilter::New(reslice);

    mitk::ExtractSliceFilter::Pointer extractor =  mitk::ExtractSliceFilter::New();
    extractor->SetInput( image );
    extractor->SetTimeStep( timeStep );
    extractor->SetWorldGeometry( planeGeometry );
    extractor->SetVtkOutputRequest(false);
    extractor->SetResliceTransformByGeometry( image->GetTimeGeometry()->GetGeometryForTimeStep( timeStep ) );
    //    extractor->SetInPlaneResampleExtentByGeometry(true);
    extractor->Modified();
    extractor->Update();

    mitk::Image::Pointer slice = extractor->GetOutput();

    return slice;
}

//------------------
// GetSlicevtkImage
//------------------
// [TODO:DaveP] This is never used but keep it around, may be useful 
// to see this code but not sure if it even works.
/*
vtkImageData * 
sv4guiSegmentationUtils::GetSlicevtkImage(const mitk::PlaneGeometry* planeGeometry, const mitk::Image* image, unsigned int timeStep)
{
    if ( !image || !planeGeometry ) return NULL;

    //    //Make sure that for reslicing and overwriting the same alogrithm is used. We can specify the mode of the vtk reslicer
    //    vtkSmartPointer<mitkVtkImageOverwrite> reslice = vtkSmartPointer<mitkVtkImageOverwrite>::New();
    //    //set to false to extract a slice
    //    reslice->SetOverwriteMode(false);
    //    reslice->Modified();

    //use ExtractSliceFilter with our specific vtkImageReslice for overwriting and extracting
    //    mitk::ExtractSliceFilter::Pointer extractor =  mitk::ExtractSliceFilter::New(reslice);
    mitk::ExtractSliceFilter::Pointer extractor =  mitk::ExtractSliceFilter::New();
    extractor->SetInput( image );
    extractor->SetTimeStep( timeStep );
    extractor->SetWorldGeometry( planeGeometry );
    extractor->SetResliceTransformByGeometry( image->GetTimeGeometry()->GetGeometryForTimeStep( timeStep ) );
    //    extractor->SetInPlaneResampleExtentByGeometry(true);
    //    extractor->SetInterpolationMode(mitk::ExtractSliceFilter::RESLICE_NEAREST);
    extractor->SetVtkOutputRequest(true);

    //    extractor->SetOutputDimensionality( 2 );
    //    extractor->SetOutputSpacingZDirection(1.0);
    //    extractor->SetOutputExtentZDirection( 0, 0 );

    extractor->Modified();
    //start the pipeline with updating the largest possible, needed if the geometry of the input has changed
    //    extractor->UpdateLargestPossibleRegion();
    extractor->Update();

    //    mitk::Image::Pointer slice = extractor->GetOutput();

    //    return slice->GetVtkImageData();

    //    vtkImageData* vtkimage=extractor->GetVtkOutput();
    //    int extent[6];
    //    vtkimage->GetExtent(extent);
    //    vtkimage->SetDimensions(extent[1]-extent[0]+1,extent[3]-extent[2]+1,extent[5]-extent[4]+1);

    //    return vtkimage;

    vtkImageData* vtkimage=vtkImageData::New();
    vtkimage->DeepCopy(extractor->GetVtkOutput());

    double* origin2=vtkimage->GetOrigin();
    int* extent2=vtkimage->GetExtent();
    double* spacing2=vtkimage->GetSpacing();

    cout<<"vtk image"<<endl;
    cout<<"orign: "<<origin2[0]<<","<<origin2[1]<<","<<origin2[2]<<endl;
    cout<<"extent: "<<extent2[0]<<","<<extent2[1]<<";"<<extent2[2]<<","<<extent2[3]<<";"<<extent2[4]<<","<<extent2[5]<<endl;
    cout<<"spacing: "<<spacing2[0]<<","<<spacing2[1]<<","<<spacing2[2]<<endl;

    //    return extractor->GetVtkOutput();
    return vtkimage;
}
*/

//------------------------
// GetImageTransformation
//------------------------
// Get the transformation used to transform geometry defined in mitk::Image 
// coordinate system to unoriented vtkImageData coordinate system. 
//
// The direction cosine matrix is the direction of the image axis in row major order.
//
vtkSmartPointer<vtkTransform> 
sv4guiSegmentationUtils::GetImageTransformation(mitk::Image* image)
{ 
  sv4guiImageProcessingUtils::itkImPoint itkImage = sv4guiImageProcessingUtils::itkImageType::New();
  mitk::CastToItkImage(image, itkImage);
  auto itkDirCos = itkImage->GetDirection();

  // Set a 4x4 matrix with the image direction cosines.
  auto matrix = vtkSmartPointer<vtkMatrix4x4>::New();
  matrix->Identity();
  for (int i = 0; i < 3; i++) { 
    for (int j = 0; j < 3; j++) {
      matrix->SetElement(i, j, itkDirCos[i][j]);
    }
  }
  matrix->Invert();
 
  //double origin[3];
  auto origin = image->GetGeometry()->GetOrigin();

  // Create a transformation to rotatate about the origin.
  auto imageTransform = vtkSmartPointer<vtkTransform>::New();
  imageTransform->Identity();
  imageTransform->Translate(origin[0], origin[1], origin[2]);
  imageTransform->Concatenate(matrix);
  imageTransform->Translate(-origin[0], -origin[1], -origin[2]);

  return imageTransform;
}

//------------------
// GetSlicevtkImage
//------------------
// Old version no longer used. 
//
cvStrPts * 
sv4guiSegmentationUtils::GetSlicevtkImage_old(sv4guiPathElement::sv4guiPathPoint pathPoint, vtkImageData* volumeImage, double size)
{
    // Compute a slice transformation from the 'pathPoint' path point, normal and tangent.
    //
    auto sliceTransform = GetvtkTransform(pathPoint);

    double spacing[3];
    volumeImage->GetSpacing(spacing);
    double sliceSpacing = std::min(spacing[0],std::min(spacing[0],spacing[1]));

    int dims[3];
    volumeImage->GetDimensions(dims);

    int sliceWidth = size / sliceSpacing;
    int sliceHeight = size / sliceSpacing;
    double pdimx = sliceWidth * sliceSpacing;
    double pdimy = sliceHeight * sliceSpacing;

    double sliceOrigin[3];
    sliceOrigin[0] = -0.5*pdimx;
    sliceOrigin[1] = -0.5*pdimy;
    sliceOrigin[2] = 0.0;

    // Extract a slice from the image volume.
    vtkImageReslice* imageReslice = vtkImageReslice::New();
    imageReslice->SetInputDataObject(volumeImage);
    imageReslice->SetResliceTransform(sliceTransform);
    imageReslice->SetOutputSpacing(sliceSpacing, sliceSpacing, sliceSpacing);
    imageReslice->SetOutputOrigin(sliceOrigin);
    imageReslice->SetOutputExtent(0, sliceWidth-1, 0, sliceHeight-1, 0, 0);
    imageReslice->InterpolateOn();
    imageReslice->Update();

    return vtkImageData2cvStrPts(imageReslice->GetOutput());
}

//------------------
// GetSlicevtkImage
//------------------
// Extract a 2D slice from a 3D volume using a path point.
//
// An image slice is extracted using 'vtkImageReslice' from a 'vtkImageData' image volume. 
// However, 'vtkImageData' does not store a rotation matrix and so supports only axis-parallel 
// 
// Path data (points, tangents and normals) are defined in a possibly transformed mitk image 
// coordinate system so we must transform a slice obtained from path data into the 'vtkImageData' 
// coordinate system. 
//
cvStrPts * 
sv4guiSegmentationUtils::GetSlicevtkImage(sv4guiPathElement::sv4guiPathPoint pathPoint, vtkImageData* volumeImage, 
    double size, vtkTransform* imageTransform) 
{
    // Compute the transformation to define a slice plane aligned with
    // the 'pathPoint' point, normal and tangent.
    //
    auto sliceTransform = GetvtkTransform(pathPoint);

    // Transform to vtkImageData system.
    //
    sliceTransform->PostMultiply();
    sliceTransform->Concatenate(imageTransform);

    // Set the spacing and dimensions of the plane used
    // to extract a slice from a image volume.
    //
    double spacing[3];
    volumeImage->GetSpacing(spacing);
    double sliceSpacing = std::min(spacing[0],std::min(spacing[0],spacing[1]));

    int dims[3];
    volumeImage->GetDimensions(dims);

    int sliceWidth = size / sliceSpacing;
    int sliceHeight = size / sliceSpacing;

    double pdimx = sliceWidth * sliceSpacing;
    double pdimy = sliceHeight * sliceSpacing;

    double sliceOrigin[3];
    sliceOrigin[0] = -0.5*pdimx;
    sliceOrigin[1] = -0.5*pdimy;
    sliceOrigin[2] = 0.0;

    // Extract a slice from the image volume.
    vtkImageReslice* imageReslice = vtkImageReslice::New();
    imageReslice->SetInputDataObject(volumeImage);
    imageReslice->SetResliceTransform(sliceTransform);
    imageReslice->SetOutputSpacing(sliceSpacing, sliceSpacing, sliceSpacing);
    imageReslice->SetOutputOrigin(sliceOrigin);
    imageReslice->SetOutputExtent(0, sliceWidth-1, 0, sliceHeight-1, 0, 0);
    imageReslice->InterpolateOn();
    imageReslice->Update();

    /*
    auto writer = vtkXMLImageDataWriter::New();
    writer->SetFileName("GetSlicevtkImage.vti");
    writer->SetInputData(imageReslice->GetOutput());
    writer->Write();
    */

    return vtkImageData2cvStrPts(imageReslice->GetOutput());
}

cvStrPts* sv4guiSegmentationUtils::image2cvStrPts(mitk::Image* image)
{
    vtkImageData* vtkImg=sv4guiVtkUtils::MitkImage2VtkImage(image);

    vtkStructuredPoints *mysp = vtkStructuredPoints::New();
    mysp->ShallowCopy(vtkImg);

    cvStrPts *sp;
    sp = new cvStrPts (mysp);

    return sp;
}

cvStrPts* sv4guiSegmentationUtils::vtkImageData2cvStrPts(vtkImageData* vtkImg)
{
    vtkStructuredPoints *mysp = vtkStructuredPoints::New();
    mysp->ShallowCopy(vtkImg);

    int whole[6];
    //    int extent[6];
    double *spacing, origin[3];

    vtkImg->GetExtent(whole);

    spacing = vtkImg->GetSpacing();
    vtkImg->GetOrigin(origin);

    origin[0] += spacing[0] * whole[0];
    origin[1] += spacing[1] * whole[2];
    whole[1] -= whole[0];
    whole[3] -= whole[2];
    whole[0] = 0;
    whole[2] = 0;
    // shift Z origin for 3-D images
    //    if (whole[4] > 0 && whole[5] > 0) {
    origin[2] += spacing[2] * whole[4];
    whole[5] -= whole[4];
    whole[4] = 0;
    //    }
    mysp->SetExtent(whole);
    mysp->SetOrigin(origin);
    mysp->SetSpacing(spacing);

    cvStrPts *sp;
    sp = new cvStrPts (mysp);

    //    mysp->Delete();

    return sp;
}

//-----------------
// CreateLSContour
//-----------------
// Extract a level set contour from a 2D image slice.
//
sv4guiContour* 
sv4guiSegmentationUtils::CreateLSContour(sv4guiPathElement::sv4guiPathPoint pathPoint, vtkImageData* volumeImage, 
    svLSParam* param, double size, vtkTransform* imageTransform, bool forceClosed)
{
    ////////////////////////////
    //  Stage 1 computation  //
    ///////////////////////////

    // Create level set object set some parameters. 
    cvITKLevelSet *ls;
    ls = new cvITKLevelSet;
    ls->SetDebug(false);
    ls->SetUseInputImageAsFeature(false);
    ls->SetMaxIterations(param->maxIter1);
    ls->SetMaxRMSError(param->maxErr1);
    ls->SetAdvectionScaling(1.0);
    ls->SetCurvatureScaling(1.0);

    if(param->sigmaFeat1 >= 0) {
        ls->SetSigmaFeature(param->sigmaFeat1);
    }
    if(param->sigmaAdv1 >= 0) {
        ls->SetSigmaAdvection(param->sigmaAdv1);
    }

    // Create seed curve.
    cvPolyData *seedPd = NULL;
    double center[3];
    center[0] = param->ctrx;
    center[1] = param->ctry;
    center[2] = 0.0;
    cvITKLSUtil::vtkGenerateCircle(param->radius, center, 50, &seedPd);

    // Extract a 2D slice from the image volume.
    cvStrPts* strPts = GetSlicevtkImage(pathPoint, volumeImage, size, imageTransform);
    ls->SetInputImage(strPts);
    ls->SetSeed(seedPd);

    // Phase 1 (whatever that is ...)
    ls->ComputePhaseOneLevelSet(param->kc, param->expFactorRising, param->expFactorFalling);

    cvPolyData *front1;
    front1 = ls->GetFront();

    ////////////////////////////
    //  Stage 2 computation  //
    ///////////////////////////

    cvITKLevelSet *ls2;
    ls2 = new cvITKLevelSet;

    ls2->SetDebug(false);
    ls2->SetUseInputImageAsFeature(false);

    ls2->SetMaxIterations(param->maxIter2);
    ls2->SetMaxRMSError(param->maxErr2);
    ls2->SetAdvectionScaling(1.0);
    ls2->SetCurvatureScaling(1.0);

    cvStrPts* strPts2 = GetSlicevtkImage(pathPoint, volumeImage,  size, imageTransform);
    ls2->SetInputImage(strPts2);
    ls2->SetSeed(front1);

    if(param->sigmaFeat2 >= 0) {
        ls2->SetSigmaFeature(param->sigmaFeat2);
    }
    if(param->sigmaAdv2 >= 0) {
        ls2->SetSigmaAdvection(param->sigmaAdv2);
    }

    ls2->ComputePhaseTwoLevelSet(param->kupp,param->klow);

    cvPolyData *front2;
    front2=ls2->GetFront();

    cvPolyData *dst;
    double tol=0.001;
    dst=sys_geom_MergePts_tol(front2, tol );

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

    cvPolyData *dst2;

    sys_geom_OrientProfile(dst, pos, nrm, xhat, &dst2);

    sv4guiContour* contour=new sv4guiContour();
    contour->SetPathPoint(pathPoint);
    contour->SetPlaced(true);
    contour->SetMethod("LevelSet");

    std::vector<mitk::Point3D> contourPoints;

    vtkPolyData* pd=dst2->GetVtkPolyData();
    bool ifClosed;
    std::deque<int> IDList=GetOrderedPtIDs(pd->GetLines(),ifClosed);
    double point[3];
    mitk::Point3D pt;
    for(int i=0;i<IDList.size();i++)
    {
        pd->GetPoint(IDList[i],point);
        pt[0]=point[0];
        pt[1]=point[1];
        pt[2]=point[2];
        contourPoints.push_back(pt);
    }

    contour->SetClosed(ifClosed||forceClosed);
    contour->SetContourPoints(contourPoints);

    return contour;
}

vtkPolyData* sv4guiSegmentationUtils::orientBack(vtkPolyData* srcPd, mitk::PlaneGeometry* planeGeometry)
{

    vtkCellArray *lines;
    vtkPoints *pts = vtkPoints::New();
    vtkPolyData *pd = vtkPolyData::New();
    int i, numPts;
    //    vtkFloatingPointType origpt[3];
    //    vtkFloatingPointType newpt[3];
    //    vtkFloatingPointType trans[2];
    double origpt[3];
    double newpt[3];

    lines = VtkUtils_DeepCopyCells( srcPd->GetLines() );
    numPts = srcPd->GetNumberOfPoints();
    for ( i = 0; i < numPts; i++ ) {
        srcPd->GetPoint( i, origpt );
        mitk::Point2D pt2d;
        mitk::Point3D pt3d;
        pt2d[0]=origpt[0];
        pt2d[1]=origpt[1];
        planeGeometry->Map(pt2d,pt3d);
        newpt[0]=pt3d[0];
        newpt[1]=pt3d[1];
        newpt[2]=pt3d[2];
        pts->InsertNextPoint( newpt );
    }

    pd->SetPoints( pts );
    pd->SetLines( lines );
    pts->Delete();
    lines->Delete();

    return pd;
}

std::vector<mitk::Point3D> sv4guiSegmentationUtils::GetThresholdContour(vtkImageData* imageSlice, double thresholdValue, sv4guiPathElement::sv4guiPathPoint pathPoint, bool& ifClosed, double seedPoint[3])
{
    vtkSmartPointer<vtkContourFilter> contourFilter=vtkSmartPointer<vtkContourFilter>::New();
    contourFilter->SetInputDataObject(imageSlice);
    contourFilter->SetValue(0,thresholdValue);
    contourFilter->Update();

    vtkSmartPointer<vtkPolyData> uncleanContour=contourFilter->GetOutput();
    cvPolyData* cvUncleanContour=new cvPolyData(uncleanContour);

    //    cvPolyData *cvCleanContour;
    //    double tol=0.001;
    //    cvCleanContour=sys_geom_MergePts_tol(cvUncleanContour, tol );
    //    vtkPolyData* cleanContour=cvCleanContour->GetVtkPolyData();

    vtkSmartPointer<vtkPolyDataConnectivityFilter> connectFilter=vtkSmartPointer<vtkPolyDataConnectivityFilter>::New();
    //    connectFilter->SetInputData(cleanContour);
    connectFilter->SetInputData(uncleanContour);
    //    connectFilter->SetExtractionModeToLargestRegion();
    connectFilter->SetExtractionModeToClosestPointRegion();
    connectFilter->SetClosestPoint(seedPoint);
    connectFilter->Update();

    vtkSmartPointer<vtkPolyData> selectedContour=connectFilter->GetOutput();

    cvPolyData *cvSelectedContour=new cvPolyData(selectedContour);

    cvPolyData *dst;

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

    sys_geom_OrientProfile(cvSelectedContour, pos, nrm, xhat, &dst);

    std::vector<mitk::Point3D> contourPoints;

    vtkPolyData* pd=dst->GetVtkPolyData();
    std::deque<int> IDList=GetOrderedPtIDs(pd->GetLines(),ifClosed);
    double point[3];
    mitk::Point3D pt;
    for(int i=0;i<IDList.size();i++)
    {
        pd->GetPoint(IDList[i],point);
        pt[0]=point[0];
        pt[1]=point[1];
        pt[2]=point[2];
        contourPoints.push_back(pt);
    }

    delete cvUncleanContour;
    delete cvSelectedContour;

    return contourPoints;
}

//------------------------
// CreateThresholdContour
//------------------------
//
sv4guiContour* sv4guiSegmentationUtils::CreateThresholdContour(sv4guiPathElement::sv4guiPathPoint pathPoint, vtkImageData* volumeimage, double thresholdValue, double size, vtkTransform* imageTransform, bool forceClosed)
{
    sv4guiContour* contour=new sv4guiContour();
    contour->SetPlaced(true);
    contour->SetMethod("Threshold");
    contour->SetPathPoint(pathPoint);

    cvStrPts*  strPts = GetSlicevtkImage(pathPoint, volumeimage,  size, imageTransform);

    bool ifClosed;
    double point[3]={0};
    std::vector<mitk::Point3D> contourPoints=GetThresholdContour(strPts->GetVtkStructuredPoints(), thresholdValue, pathPoint, ifClosed, point);

    contour->SetClosed(ifClosed||forceClosed);
    contour->SetContourPoints(contourPoints);

    return contour;

}

std::deque<int> sv4guiSegmentationUtils::GetOrderedPtIDs(vtkCellArray* lines, bool& ifClosed)
{

    vtkIdType *ptIds;
    vtkIdType npts;

    lines->InitTraversal();

    std::vector<std::vector<int>> lineList;

    std::deque<int> linkedPtList;

    while ( lines->GetNextCell( npts, ptIds ) ) {

        if(npts!=2) break;

        std::vector<int> ids;
        ids.push_back(ptIds[0]);
        ids.push_back(ptIds[1]);

        lineList.push_back(ids);
    }

    bool firstTime=true;

    while(lineList.size()>0)
    {
        if(firstTime)
        {
            linkedPtList.push_back(lineList[0][0]);
            linkedPtList.push_back(lineList[0][1]);

            lineList.erase(lineList.begin()+0);
            firstTime=false;
        }
        else
        {

            int first=linkedPtList.front();
            int last=linkedPtList.back();

            bool firstLinked=false;
            bool lastLinked=false;

            for(int i=0;i<lineList.size();i++){

                if(firstLinked&&lastLinked) break;

                int id1=lineList[i][0];
                int id2=lineList[i][1];

                if(!firstLinked&&(first==id1||first==id2))
                {
                    if(first==id1){
                        linkedPtList.push_front(id2);
                    }else{
                        linkedPtList.push_front(id1);
                    }
                    firstLinked=true;
                    lineList.erase(lineList.begin()+i);
                    i--;
                    continue;
                } else if(!lastLinked&&(last==id1||last==id2))
                {
                    if(last==id1){
                        linkedPtList.push_back(id2);
                    }else{
                        linkedPtList.push_back(id1);
                    }
                    lastLinked=true;
                    lineList.erase(lineList.begin()+i);
                    i--;
                    continue;
                }

            }

            if(!firstLinked&&!lastLinked) break;
        }

    }

    ifClosed=false;
    if(linkedPtList.size()>0&&linkedPtList.front()==linkedPtList.back())
    {
        ifClosed=true;
        linkedPtList.pop_back();
    }

    return linkedPtList;

}

