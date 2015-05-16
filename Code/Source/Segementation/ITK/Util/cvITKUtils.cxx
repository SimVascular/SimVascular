/*=========================================================================
 *
 * Copyright (c) 2014 The Regents of the University of California.
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
/*
 * cvLevelSetUtils.cxx
 *
 *  Created on: May 22, 2014
 *      Author: jmerkow
 */

#include "cvITKUtils.h"
#include <iostream>
#include <sstream>
#include <math.h>
#include <vtkContourFilter.h>
#include <vtkPolyDataToImageStencil.h>
#include <vtkImageStencil.h>
#include <vtkImageChangeInformation.h>
#include <vtkImageCast.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkMetaImageWriter.h>


namespace cvITKLSUtil {

ImgInfo DefaultImgInfo;
bool Debug = false;

void vtkChangeImageInformation(vtkStructuredPoints* vtkImage,
		ImgInfo* refInfo)
{
	vtkImage->GetOrigin();
	if(Debug){
		std::cout << "vtkSetInfo2Reference: in " <<
				vtkImage->GetScalarTypeAsString() <<std::endl;
	}
	vtkStructuredPoints* temp = vtkStructuredPoints::New();
	temp->SetScalarType(vtkImage->GetScalarType(),vtkImage->GetInformation());
	temp->DeepCopy(vtkImage);
	if(Debug) {
		std::cout << "vtkSetInfo2Reference: temp " <<
				temp->GetScalarTypeAsString() <<std::endl;
	}
	vtkSmartPointer<vtkImageChangeInformation> changer =
			vtkSmartPointer<vtkImageChangeInformation>::New();
	changer->SetInputData(temp);
	changer->SetOutputOrigin(refInfo->GetOrigin());
	changer->SetOutputSpacing(refInfo->GetSpacing());
	changer->Update();

	vtkSmartPointer<vtkImageCast> castFilter =
			vtkSmartPointer<vtkImageCast>::New();
	castFilter->SetInputConnection(changer->GetOutputPort());
	castFilter->SetOutputScalarTypeToShort();
	castFilter->Update();


	vtkImage->DeepCopy(changer->GetOutput());
	vtkImage->SetScalarType(temp->GetScalarType(),temp->GetInformation());

	if(Debug){
		std::cout << "vtkSetInfo2Reference: " <<
				vtkImage->GetScalarTypeAsString() <<std::endl;
	}
	temp->Delete();
}

void vtkPolyDataTo2DImage(vtkPolyData* pd,vtkStructuredPoints* vtkImg,
		ImgInfo *extInfo)
{
	vtkSmartPointer<vtkImageData> img = vtkSmartPointer<vtkImageData>::New();
	double bounds[6];
	pd->GetBounds(bounds);

	double spacing[3];
	extInfo->GetSpacing(spacing);
	// compute dimensions
	int dim[3];
	for (int i = 0; i < 3; i++)
	{
		dim[i] = static_cast<int>(ceil((bounds[i * 2 + 1] - bounds[i * 2])
				/ spacing[i]));
	}
	int realdim = std::max(dim[0],dim[1]);

	img->SetSpacing(extInfo->GetSpacing());
	img->SetOrigin(extInfo->GetOrigin());
	img->SetDimensions(dim);
	img->SetExtent(extInfo->GetExtent());

	if(Debug)
	{
		std::cout <<  "vtkPolyDataToImageData: ";
		//MyUt::printArrays(extent,spacing,origin,center);
		std::cout << "dim ";
		//MyUt::printArray(dim,3);
	}

	img->AllocateScalars(VTK_SHORT,1);
	// fill the image with foreground voxels:q
	short inval = 255;
	short outval = 0;
	vtkIdType count = img->GetNumberOfPoints();
	for (vtkIdType i = 0; i < count; ++i)
	{
		img->GetPointData()->GetScalars()->SetTuple1(i, inval);
	}

	// polygonal data --> image stencil:
	vtkSmartPointer<vtkPolyDataToImageStencil> pol2stenc =
			vtkSmartPointer<vtkPolyDataToImageStencil>::New();

	pol2stenc->SetInputData(pd);
	pol2stenc->SetOutputOrigin(extInfo->GetOrigin());
	pol2stenc->SetOutputSpacing(extInfo->GetSpacing());
	pol2stenc->SetOutputWholeExtent(img->GetExtent());
	pol2stenc->Update();

	vtkSmartPointer<vtkImageStencil> imgstenc =
			vtkSmartPointer<vtkImageStencil>::New();
	imgstenc->SetInputData(img);
	imgstenc->SetStencilConnection(pol2stenc->GetOutputPort());
	imgstenc->ReverseStencilOff();
	imgstenc->SetBackgroundValue(outval);
	imgstenc->Update();

	vtkImg->DeepCopy((vtkStructuredPoints*)imgstenc->GetOutput());


}

void vtkPolyDataToVolume(vtkPolyData* pd,vtkStructuredPoints* vtkImg,
		ImgInfo *extInfo)
{
	vtkSmartPointer<vtkImageData> img = vtkSmartPointer<vtkImageData>::New();
	double bounds[6];
	pd->GetBounds(bounds);

	img->SetSpacing(extInfo->GetSpacing());
	img->SetOrigin(extInfo->GetOrigin());
	img->SetExtent(extInfo->GetExtent());

	if(Debug)
	{
		std::cout <<  "vtkPolyDataToImageData: ";
		//MyUt::printArrays(extent,spacing,origin,center);
		std::cout << "dim ";
		//MyUt::printArray(dim,3);
	}

	img->AllocateScalars(VTK_SHORT,1);
	// fill the image with foreground voxels:
	short inval = 255;
	short outval = 0;
	vtkIdType count = img->GetNumberOfPoints();
	for (vtkIdType i = 0; i < count; ++i)
	{
		img->GetPointData()->GetScalars()->SetTuple1(i, inval);
	}

	// polygonal data --> image stencil:
	vtkSmartPointer<vtkPolyDataToImageStencil> pol2stenc =
			vtkSmartPointer<vtkPolyDataToImageStencil>::New();

	pol2stenc->SetInputData(pd);
	pol2stenc->SetOutputOrigin(img->GetOrigin());
	pol2stenc->SetOutputSpacing(img->GetSpacing());
	pol2stenc->SetOutputWholeExtent(img->GetExtent());
	pol2stenc->Update();

	vtkSmartPointer<vtkImageStencil> imgstenc =
			vtkSmartPointer<vtkImageStencil>::New();
	imgstenc->SetInputData(img);
	imgstenc->SetStencilConnection(pol2stenc->GetOutputPort());
	imgstenc->ReverseStencilOff();
	imgstenc->SetBackgroundValue(outval);
	imgstenc->Update();

	vtkImg->DeepCopy((vtkStructuredPoints*)imgstenc->GetOutput());


}


void vtkGenerateCircle(double radius,double center[3],
		int numPoints,vtkPolyData* circle)
{
	vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
	vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();
	vtkIdType* lineIndices = new vtkIdType[numPoints+1];

	vtkSmartPointer<vtkPolyData> tmpc = vtkSmartPointer<vtkPolyData>::New();

	for( unsigned int i = 0; i < numPoints; i++ )
	{
		const double angle = 2.0 * vtkMath::Pi() *
				static_cast< double >( i ) / static_cast< double >( numPoints );

		points->InsertPoint( static_cast< vtkIdType >( i ),
				radius * cos( angle ),radius * sin( angle ), 0. );

		lineIndices[i] = static_cast< vtkIdType >( i );
	}
	lineIndices[numPoints] = 0;
	lines->InsertNextCell( numPoints+1, lineIndices );
	delete[] lineIndices;

	tmpc->SetPoints( points );
	tmpc->SetLines( lines );

	vtkSmartPointer<vtkTransform> translation =
			vtkSmartPointer<vtkTransform>::New();
	translation->Translate(center[0], center[1], center[3]);


	vtkSmartPointer<vtkTransformPolyDataFilter> transformFilter =
			vtkSmartPointer<vtkTransformPolyDataFilter>::New();
	transformFilter->SetInputData(tmpc);
	transformFilter->SetTransform(translation);
	transformFilter->Update();

	circle->DeepCopy(transformFilter->GetOutput());
}



void WriteImage(vtkStructuredPoints* image,string FilenameBase) {
	vtkSmartPointer<vtkTIFFWriter> writer =
			vtkSmartPointer<vtkTIFFWriter>::New();
	std::stringstream sstm;
	sstm  << FilenameBase;
	sstm  << "-vtk";
	sstm << ".tiff";
	writer->SetFileName(sstm.str().c_str());
	writer->SetInputData(image);
	writer->Write();
}

void vtkWriteImage2(vtkStructuredPoints* image,string FilenameBase) {
	vtkSmartPointer<vtkMetaImageWriter> writer =
			vtkSmartPointer<vtkMetaImageWriter>::New();
	std::stringstream sstm;
	sstm  << FilenameBase;
	writer->SetFileName(sstm.str().c_str());
	writer->SetInputData(image);
	writer->Write();
}

void WritePerciseVtkImage(vtkStructuredPoints* image,string FilenameBase)
{
	vtkSmartPointer<vtkImageCast> caster = vtkSmartPointer<vtkImageCast>::New();
	caster->SetInputData(image);
	caster->SetOutputScalarTypeToFloat();
	caster->ClampOverflowOff();
	caster->Update();

	WriteImage((vtkStructuredPoints *)caster->GetOutput(),FilenameBase);

}


} //namespace


