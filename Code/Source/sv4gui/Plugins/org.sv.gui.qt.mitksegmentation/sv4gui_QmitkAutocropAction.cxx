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
 
/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/
#include "sv4gui_QmitkAutocropAction.h"

#include "mitkAutoCropImageFilter.h"
#include "mitkImageCast.h"
#include "mitkImageWriteAccessor.h"
#include "mitkRenderingManager.h"
#include "mitkProgressBar.h"

#include <itkConstantPadImageFilter.h>

//needed for qApp
#include <qcoreapplication.h>

QmitkAutocropAction::QmitkAutocropAction()
{
}

QmitkAutocropAction::~QmitkAutocropAction()
{
}

void QmitkAutocropAction::Run( const QList<mitk::DataNode::Pointer> &selectedNodes )
{
  foreach ( mitk::DataNode::Pointer node, selectedNodes )
  {
    if (node)
    {
      mitk::Image::Pointer image = dynamic_cast<mitk::Image*>( node->GetData() );
      if (image.IsNull()) return;

      mitk::ProgressBar::GetInstance()->AddStepsToDo(10);
      mitk::ProgressBar::GetInstance()->Progress(2);

      qApp->processEvents();

      mitk::AutoCropImageFilter::Pointer cropFilter = mitk::AutoCropImageFilter::New();
      cropFilter->SetInput( image );
      cropFilter->SetBackgroundValue( 0 );
      try
      {
        cropFilter->Update();

        image = cropFilter->GetOutput();

        if (image.IsNotNull())
        {

          if (image->GetDimension() == 4)
          {
            MITK_INFO << "4D AUTOCROP DOES NOT WORK AT THE MOMENT";
            throw "4D AUTOCROP DOES NOT WORK AT THE MOMENT";

            unsigned int timesteps = image->GetDimension(3);
            for (unsigned int i = 0; i < timesteps; i++)
            {
              mitk::ImageTimeSelector::Pointer imageTimeSelector = mitk::ImageTimeSelector::New();
              imageTimeSelector->SetInput(image);
              imageTimeSelector->SetTimeNr(i);
              imageTimeSelector->UpdateLargestPossibleRegion();

              // We split a long nested code line into separate calls for debugging:
              mitk::ImageSource::OutputImageType *_3dSlice = imageTimeSelector->GetOutput();
              mitk::Image::Pointer _cropped3dSlice = this->IncreaseCroppedImageSize(_3dSlice);

              // +++ BUG +++ BUG +++ BUG +++ BUG +++ BUG +++ BUG +++ BUG +++
              mitk::ImageWriteAccessor imAccess(_cropped3dSlice);
              void *_data = imAccess.GetData();

              // <ToBeRemoved>
              // We write some stripes into the image
              if ((i & 1) == 0)
              {
              int depth = _cropped3dSlice->GetDimension(2);
              int height = _cropped3dSlice->GetDimension(1);
              int width = _cropped3dSlice->GetDimension(0);

              for (int z = 0; z < depth; ++z)
                for (int y = 0; y < height; ++y)
                  for (int x = 0; x < width; ++x)
                    reinterpret_cast<unsigned char *>(_data)[(width * height * z) + (width * y) + x] = x & 1;
              // </ToBeRemoved>
              }

              image->SetVolume(_data, i);
            }
            node->SetData( image ); // bug fix 3145
          }
          else
          {
            node->SetData( this->IncreaseCroppedImageSize(image) ); // bug fix 3145
          }
          // Reinit node
          mitk::RenderingManager::GetInstance()->InitializeViews(
            node->GetData()->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );
          mitk::RenderingManager::GetInstance()->RequestUpdateAll();

        }
      }
      catch(...)
      {
        MITK_ERROR << "Cropping image failed...";
      }
      mitk::ProgressBar::GetInstance()->Progress(8);
    }
    else
    {
      MITK_INFO << "   a nullptr node selected";
    }
  }
}

mitk::Image::Pointer QmitkAutocropAction::IncreaseCroppedImageSize( mitk::Image::Pointer image )
{
  typedef itk::Image< short, 3 > ImageType;
  typedef itk::Image< unsigned char, 3 > PADOutputImageType;

  ImageType::Pointer itkTransformImage = ImageType::New();
  mitk::CastToItkImage( image, itkTransformImage );

  typedef itk::ConstantPadImageFilter< ImageType, PADOutputImageType > PadFilterType;
  PadFilterType::Pointer padFilter = PadFilterType::New();

  itk::SizeValueType upperPad[3];
  itk::SizeValueType lowerPad[3];
  int borderLiner = 3;

  mitk::Point3D mitkOriginPoint;
  double origin[3];
  origin[0]=0;
  origin[1]=0;
  origin[2]=0;
  itkTransformImage->SetOrigin(origin);

  lowerPad[0]=borderLiner;
  lowerPad[1]=borderLiner;
  lowerPad[2]=borderLiner;

  upperPad[0]=borderLiner;
  upperPad[1]=borderLiner;
  upperPad[2]=borderLiner;

  padFilter->SetInput(itkTransformImage);
  padFilter->SetConstant(0);
  padFilter->SetPadUpperBound(upperPad);
  padFilter->SetPadLowerBound(lowerPad);
  padFilter->UpdateLargestPossibleRegion();


  mitk::Image::Pointer paddedImage = mitk::Image::New();
  paddedImage->InitializeByItk(padFilter->GetOutput());
  mitk::CastToMitkImage(padFilter->GetOutput(), paddedImage);

  //calculate translation according to padding to get the new origin
  mitk::Point3D paddedOrigin = image->GetGeometry()->GetOrigin();
  mitk::Vector3D spacing = image->GetGeometry()->GetSpacing();
  paddedOrigin[0] -= (borderLiner)*spacing[0];
  paddedOrigin[1] -= (borderLiner)*spacing[1];
  paddedOrigin[2] -= (borderLiner)*spacing[2];

  paddedImage->GetGeometry()->SetOrigin( paddedOrigin );

  return paddedImage;
}

void QmitkAutocropAction::SetSmoothed(bool /*smoothed*/)
{
 //not needed
}

void QmitkAutocropAction::SetDecimated(bool /*decimated*/)
{
  //not needed
}

void QmitkAutocropAction::SetDataStorage(mitk::DataStorage* /*dataStorage*/)
{
  //not needed
}

void QmitkAutocropAction::SetFunctionality(berry::QtViewPart* /*view*/)
{
  //not needed
}
