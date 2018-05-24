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

#include "SimVascular.h"

#include "sv2_LevelSetVelocityImage.h"
#include "sv2_LevelSet.h"


// ------
// cvLevelSetVelocityImage
// ------

cvLevelSetVelocityImage::cvLevelSetVelocityImage()
{
  image_ = NULL;
}


// -------
// ~cvLevelSetVelocityImage
// -------

cvLevelSetVelocityImage::~cvLevelSetVelocityImage()
{
  ClearImage();
}


// --------
// SetImage
// --------

int cvLevelSetVelocityImage::SetImage( Image_T *i, int closed )
{
  if ( image_ != NULL ) {
    return SV_ERROR;
  } else if ( i == NULL ) {
    return SV_ERROR;
  } else {
    image_ = i;
    ComputeImageGrad( image_ );
    SetImageClosed( image_, closed );
    PostSetImageAction();
    return SV_OK;
  }
}


// --------
// SetImage
// --------

int cvLevelSetVelocityImage::SetImage( char *filebase, int fileNumRange[], int imgDims[],
		      double pixDims[], int closed )
{
  Image_T *tmp = NULL;

  tmp = ReadImage( filebase, fileNumRange, "-short", imgDims, pixDims );
  if ( tmp == NULL ) {
    return SV_ERROR;
  }
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return SV_OK;
}


// --------
// SetImage
// --------
// Being able to set image directly with a pre-existing array of data
// allows us to interface to other systems that work with imaging data
// (e.g. vtk).  CreateImage COPIES the given data array, which means
// the copy passed in to this method still belongs to the caller.

int cvLevelSetVelocityImage::SetImage( short *imgData, int numData, int imgDims[],
		      double pixDims[], double origin[], int closed )
{
  Image_T *tmp = NULL;

  tmp = CreateImage( imgData, numData, "-short", imgDims, pixDims );
  if ( tmp == NULL ) {
    return SV_ERROR;
  }
  SetLowerLeft( tmp, origin );
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return SV_OK;
}


// --------
// SetImage
// --------

int cvLevelSetVelocityImage::SetImage( double *imgData, int numData, int imgDims[],
		      double pixDims[], double origin[], int closed )
{
  Image_T *tmp = NULL;

  tmp = CreateImage( imgData, numData, "-double", imgDims, pixDims );
  if ( tmp == NULL ) {
    return SV_ERROR;
  }
  SetLowerLeft( tmp, origin );
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return SV_OK;
}


// --------
// SetImage
// --------

int cvLevelSetVelocityImage::SetImage( float *imgData, int numData, int imgDims[],
		      double pixDims[], double origin[], int closed )
{
  Image_T *tmp = NULL;

  tmp = CreateImage( (void*)imgData, numData, "-float", imgDims, pixDims );
  if ( tmp == NULL ) {
    return SV_ERROR;
  }
  SetLowerLeft( tmp, origin );
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return SV_OK;
}


// --------
// GetImage
// --------

int cvLevelSetVelocityImage::GetImage( Image_T **i )
{
  if ( image_ == NULL ) {
    return SV_ERROR;
  } else {
    *i = image_;
    return SV_OK;
  }
}


// ---------------
// GetMagGradRange
// ---------------

int cvLevelSetVelocityImage::GetMagGradRange( double rng[] )
{
  if ( image_ == NULL ) {
    return SV_ERROR;
  } else {
    Img_GetMagGradRange( image_, rng );
    return SV_OK;
  }
}


// -----------------
// GetXYMagGradRange
// -----------------

int cvLevelSetVelocityImage::GetXYMagGradRange( double rng[] )
{
  if ( image_ == NULL ) {
    return SV_ERROR;
  } else {
    Img_GetXYMagGradRange( image_, rng );
    return SV_OK;
  }
}


// ----------------
// GetZMagGradRange
// ----------------

int cvLevelSetVelocityImage::GetZMagGradRange( double rng[] )
{
  if ( image_ == NULL ) {
    return SV_ERROR;
  } else {
    Img_GetZMagGradRange( image_, rng );
    return SV_OK;
  }
}


// -----------------
// GetIntensityRange
// -----------------

int cvLevelSetVelocityImage::GetIntensityRange( double rng[] )
{
  if ( image_ == NULL ) {
    return SV_ERROR;
  } else {
    Img_GetIntensityRange( image_, rng );
    return SV_OK;
  }
}


// ----------
// ClearImage
// ----------

int cvLevelSetVelocityImage::ClearImage()
{
  if ( image_ != NULL ) {
    Image_Delete( image_ );
  }
  image_ = NULL;
  return SV_OK;
}
