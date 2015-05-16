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
 * itkVascularPhaseTwoLevelSetImageFilter.hxx
 *
 *  Created on: Nov 19, 2013
 *      Author: jmerkow
 */
#ifndef __itkVascularPhaseTwoLevelSetImageFilter_hxx
#define __itkVascularPhaseTwoLevelSetImageFilter_hxx

#include "itkVascularPhaseTwoLevelSetImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputType >
VascularPhaseTwoLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::VascularPhaseTwoLevelSetImageFilter()
 {
	/* Instantiate a geodesic active contour function and set it as the
    segmentation function. */
	m_VascularPhaseTwoLevelSetFunction = VascularPhaseTwoLevelSetFunctionType::New();

	this->SetVascularSegmentationFunction(m_VascularPhaseTwoLevelSetFunction);
	/* Turn off interpolation. */
	this->InterpolateSurfaceLocationOff();
	this->SetDebug(0);
 }

template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
VascularPhaseTwoLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::GenerateData()
 {
	// Make sure the SpeedImage is setup for the case when PropagationScaling
	// is zero
	if(this->GetDebug() > 1)
		std::cout << "Image Filter -> Generating Data" <<std::endl;

	if ( this->GetVascularSegmentationFunction()
			&& this->GetVascularSegmentationFunction()->GetPropagationWeight() == 0 )
	{
		this->GetVascularSegmentationFunction()->AllocateSpeedImage();
		this->GetVascularSegmentationFunction()->CalculateSpeedImage();
		this->GetVascularSegmentationFunction()->AllocateAdvectionImage();
		this->GetVascularSegmentationFunction()->CalculateAdvectionImage();
	}

	// Continue with Superclass implementation
	Superclass::GenerateData();
 }


template< typename TInputImage, typename TFeatureImage, typename TOutputType >
void
VascularPhaseTwoLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::PrintSelf(std::ostream & os, Indent indent) const
 {
	Superclass::PrintSelf(os, indent);
	os << "VascularPhaseTwoLevelSetFunction: " << m_VascularPhaseTwoLevelSetFunction.GetPointer();
 }


}// namespace

#endif

