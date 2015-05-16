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

/*
 * cvITKMacros.h
 *
 *  Created on: May 21, 2014
 *      Author: jmerkow
 */

#ifndef CVITKMACROS_H_
#define CVITKMACROS_H_

#define MyUtGenerateITK2VTKBridge(imagetype,name)															\
		typedef itk::VTKImageExport< imagetype > itkExport##name##Type;											\
		itkExport##name##Type::Pointer itkExporter##name = itkExport##name##Type::New();								\
		vtkSmartPointer<vtkImageImport> vtkImporter##name = vtkSmartPointer<vtkImageImport>::New();	\
		ConnectITKToVTK(itkExporter##name.GetPointer(),vtkImporter##name.GetPointer());						\

#define MyUtGenerateVTK2ITKBridge(imagetype,name)	\
		typedef itk::VTKImageImport< imagetype > itkImport##name##Type; \
		itkImport##name##Type::Pointer itkImporter##name = itkImport##name##Type::New(); 								\
		vtkSmartPointer<vtkImageExport> vtkExporter##name = vtkSmartPointer<vtkImageExport>::New();	\
		ConnectVTKToITK(vtkExporter##name.GetPointer(),itkImporter##name.GetPointer());						\

#define MyUtGenerateITKTemplate2VTKBridge(imagetype,name)															\
		typedef itk::VTKImageExport< imagetype > itkExport##name##Type;											\
		typename itkExport##name##Type::Pointer itkExporter##name = itkExport##name##Type::New();								\
		vtkSmartPointer<vtkImageImport> vtkImporter##name = vtkSmartPointer<vtkImageImport>::New();	\
		ConnectITKToVTK(itkExporter##name.GetPointer(),vtkImporter##name.GetPointer());						\

#define MyUtGenerateVTK2ITKTemplateBridge(imagetype,name)	\
		typedef itk::VTKImageImport< imagetype > itkImport##name##Type; \
		typename itkImport##name##Type::Pointer itkImporter##name = itkImport##name##Type::New(); 								\
		vtkSmartPointer<vtkImageExport> vtkExporter##name = vtkSmartPointer<vtkImageExport>::New();	\
		ConnectVTKToITK(vtkExporter##name.GetPointer(),itkImporter##name.GetPointer());						\


#define cvITKGenerateCastFilter(inputImageType,outputImageType,name) \
		typedef itk::CastImageFilter< inputImageType, outputImageType> CastFilter##name##Type; \
		CastFilter##name##Type::Pointer cast##name = CastFilter##name##Type::New(); \

#define cvITKConnectCast2Import(name) \
		cast##name##->SetInput(itkImporter##name##->GetOutput()); \

#define cvITKInternalTypedefs(type,dim)															\
		typedef type InternalPixelType;															\
		typedef itk::Image<InternalPixelType,dim> InternalImageType;							\

#define cvITKNewMacro(type,name)															\
		type::Pointer name = type::New();													\

#define cvITKTemplateNewMacro(type,name)													\
		typename cvITKNewMacro(type,name)												\

#endif /* CVITKMACROS_H_ */
