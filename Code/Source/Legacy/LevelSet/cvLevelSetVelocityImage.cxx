/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
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
 */

#include "SimVascular.h" 

#include "cvLevelSetVelocityImage.h"
#include "cvLevelSet.h"


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
    return CV_ERROR;
  } else if ( i == NULL ) {
    return CV_ERROR;
  } else {
    image_ = i;
    ComputeImageGrad( image_ );
    SetImageClosed( image_, closed );
    PostSetImageAction();
    return CV_OK;
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
    return CV_ERROR;
  }
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return CV_OK;
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
    return CV_ERROR;
  }
  SetLowerLeft( tmp, origin );
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return CV_OK;
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
    return CV_ERROR;
  }
  SetLowerLeft( tmp, origin );
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return CV_OK;
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
    return CV_ERROR;
  }
  SetLowerLeft( tmp, origin );
  if ( image_ ) {
    ClearImage();
  }
  image_ = tmp;
  ComputeImageGrad( image_ );
  SetImageClosed( image_, closed );
  PostSetImageAction();
  return CV_OK;
}


// --------
// GetImage
// --------

int cvLevelSetVelocityImage::GetImage( Image_T **i )
{
  if ( image_ == NULL ) {
    return CV_ERROR;
  } else {
    *i = image_;
    return CV_OK;
  }
}


// ---------------
// GetMagGradRange
// ---------------

int cvLevelSetVelocityImage::GetMagGradRange( double rng[] )
{
  if ( image_ == NULL ) {
    return CV_ERROR;
  } else {
    Img_GetMagGradRange( image_, rng );
    return CV_OK;
  }
}


// -----------------
// GetXYMagGradRange
// -----------------

int cvLevelSetVelocityImage::GetXYMagGradRange( double rng[] )
{
  if ( image_ == NULL ) {
    return CV_ERROR;
  } else {
    Img_GetXYMagGradRange( image_, rng );
    return CV_OK;
  }
}


// ----------------
// GetZMagGradRange
// ----------------

int cvLevelSetVelocityImage::GetZMagGradRange( double rng[] )
{
  if ( image_ == NULL ) {
    return CV_ERROR;
  } else {
    Img_GetZMagGradRange( image_, rng );
    return CV_OK;
  }
}


// -----------------
// GetIntensityRange
// -----------------

int cvLevelSetVelocityImage::GetIntensityRange( double rng[] )
{
  if ( image_ == NULL ) {
    return CV_ERROR;
  } else {
    Img_GetIntensityRange( image_, rng );
    return CV_OK;
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
  return CV_OK;
}
