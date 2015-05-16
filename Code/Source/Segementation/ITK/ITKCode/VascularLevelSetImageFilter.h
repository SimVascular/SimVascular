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
 * VascularLevelsetImageFilter.h
 *
 *  Created on: Nov 25, 2013
 *      Author: jmerkow
 */

#ifndef __VascularLevelSetImageFilter_H_
#define __VascularLevelSetImageFilter_H_


#include "itkSegmentationLevelSetImageFilter.h"
#include "VascularLevelSetFunction.h"

namespace itk
{
template<	typename TInputImage,
typename TFeatureImage,
typename TOutputPixelType = float >
class ITK_EXPORT VascularLevelSetImageFilter:
public SegmentationLevelSetImageFilter<TInputImage, TFeatureImage,
TOutputPixelType >
{
public:
	/* Standard */
	typedef VascularLevelSetImageFilter Self;
	typedef SegmentationLevelSetImageFilter< TInputImage, TFeatureImage,
			TOutputPixelType > Superclass;

	typedef SmartPointer< Self >       Pointer;
	typedef SmartPointer< const Self > ConstPointer;

	/** Inherited typedef from the superclass. */
	typedef typename Superclass::ValueType        ValueType;
	typedef typename Superclass::OutputImageType  OutputImageType;
	typedef typename Superclass::FeatureImageType FeatureImageType;

	/** Type of the segmentation function */
	typedef VascularLevelSetFunction< OutputImageType,
			FeatureImageType > VascularLevelSetFunctionType;
	typedef typename VascularLevelSetFunctionType::Pointer
			VascularLevelSetFunctionPointer;

	/** Run-time type information (and related methods). */
	itkTypeMacro(VascularLevelSetImageFilter, SegmentationLevelSetImageFilter);

	/** Method for creation through the object factory */
	itkNewMacro(Self);

	int GetDebug() const
	{
		if ( m_VascularLevelSetFunction == 0 )
		{
			itkExceptionMacro("No finite difference function was specified.");
		}
		return m_VascularLevelSetFunction->GetDebug(); }

	void SetDebug(int _arg){
		if ( this->GetDebug() != _arg ){
			m_VascularLevelSetFunction->SetDebug(_arg);
			this->Modified();
		}
	}

	typedef typename VascularLevelSetFunctionType::ImageType CurrentImageType;
	const CurrentImageType * GetCurrentCurvatureImage() const
	{
		return m_VascularLevelSetFunction->GetCurrentCurvatureImage();
	}

	void GenerateCurrentCurvatureImage();


	const CurrentImageType * GetCurrentAdvectionImage() const
	{
		return m_VascularLevelSetFunction->GetCurrentAdvectionImage();
	}

	void GenerateCurrentAdvectionImage();


	const CurrentImageType * GetCurrentUpdateImage() const
	{
		return m_VascularLevelSetFunction->GetCurrentUpdateImage();
	}

	const OutputImageType * GetCurrentOutputImage() const
	{
		return this->m_OutputImage;
	}
	void GenerateCurrentUpdateImage();
	virtual void SetVascularSegmentationFunction(VascularLevelSetFunctionType *s)
	{
		m_VascularLevelSetFunction = s;

		this->Superclass::SetSegmentationFunction(m_VascularLevelSetFunction);
		this->Modified();
	}
	virtual VascularLevelSetFunctionType * GetVascularSegmentationFunction()
	{
		return m_VascularLevelSetFunction;
	}
	void ClearCurrentCurvatureImage()
	{
		m_VascularLevelSetFunction->ClearCurrentCurvatureImage();
	}

protected:
	~VascularLevelSetImageFilter() {}
	VascularLevelSetImageFilter();

	virtual void PrintSelf(std::ostream & os, Indent indent) const;

	VascularLevelSetImageFilter(const Self &); // purposely not implemented
	void operator=(const Self &);

	void GenerateData();

private:
	VascularLevelSetFunctionType * m_VascularLevelSetFunction;

}; // Class Declaration

template< class TInputImage, class TFeatureImage, class TOutputPixelType >
VascularLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::VascularLevelSetImageFilter()
 {
	m_VascularLevelSetFunction = 0;
 }


template< class TInputImage, class TFeatureImage, class TOutputPixelType >
void
VascularLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType >
::GenerateData(){
	if ( m_VascularLevelSetFunction == 0 )
	{
		itkExceptionMacro("No finite difference function was specified.");
	}
	else
	{
		this->GetVascularSegmentationFunction()->AllocateCurrentCurvatureImage();
		this->GetVascularSegmentationFunction()->AllocateCurrentAdvectionImage();
		this->GetVascularSegmentationFunction()->AllocateCurrentUpdateImage();
	}


	// Continue with Superclass implementation
	Superclass::GenerateData();
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType >
void
VascularLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
 {
	Superclass::PrintSelf(os, indent);
	os << indent << "m_VascularLevelSetFunction = " << m_VascularLevelSetFunction << std::endl;
 }





}
#endif /* VASCULARLEVELSETIMAGEFILTER_H_ */
