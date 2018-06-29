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

#ifndef __ConnectVTKITK_h
#define __ConnectVTKITK_h

#include "itkVTKImageExport.h"
#include "vtkImageImport.h"

#include "itkVTKImageImport.h"
#include "vtkImageExport.h"

template <typename ImageType>
void ConnectITKToVTK(itk::VTKImageExport<ImageType>* in, vtkImageImport* out)
{
  out->SetUpdateInformationCallback(in->GetUpdateInformationCallback());
  out->SetPipelineModifiedCallback(in->GetPipelineModifiedCallback());
  out->SetWholeExtentCallback(in->GetWholeExtentCallback());
  out->SetSpacingCallback(in->GetSpacingCallback());
  out->SetOriginCallback(in->GetOriginCallback());
  out->SetScalarTypeCallback(in->GetScalarTypeCallback());
  out->SetNumberOfComponentsCallback(in->GetNumberOfComponentsCallback());
  out->SetPropagateUpdateExtentCallback(in->GetPropagateUpdateExtentCallback());
  out->SetUpdateDataCallback(in->GetUpdateDataCallback());
  out->SetDataExtentCallback(in->GetDataExtentCallback());
  out->SetBufferPointerCallback(in->GetBufferPointerCallback());
  out->SetCallbackUserData(in->GetCallbackUserData());
};


template <typename ImageType>
void ConnectVTKToITK(vtkImageExport* in, itk::VTKImageImport<ImageType>* out)
{
  out->SetUpdateInformationCallback(in->GetUpdateInformationCallback());
  out->SetPipelineModifiedCallback(in->GetPipelineModifiedCallback());
  out->SetWholeExtentCallback(in->GetWholeExtentCallback());
  out->SetSpacingCallback(in->GetSpacingCallback());
  out->SetOriginCallback(in->GetOriginCallback());
  out->SetScalarTypeCallback(in->GetScalarTypeCallback());
  out->SetNumberOfComponentsCallback(in->GetNumberOfComponentsCallback());
  out->SetPropagateUpdateExtentCallback(in->GetPropagateUpdateExtentCallback());
  out->SetUpdateDataCallback(in->GetUpdateDataCallback());
  out->SetDataExtentCallback(in->GetDataExtentCallback());
  out->SetBufferPointerCallback(in->GetBufferPointerCallback());
  out->SetCallbackUserData(in->GetCallbackUserData());
};

#endif
