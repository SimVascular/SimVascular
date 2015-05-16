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
 * itkVascularPhaseTwoLevelsetImageFilter.h
 *
 *  Created on: Nov 19, 2013
 *      Author: jmerkow
 */

#ifndef __itkVascularPhaseTwoLevelSetImageFilter_h
#define __itkVascularPhaseTwoLevelSetImageFilter_h

#include "VascularLevelSetImageFilter.h"
#include "itkVascularPhaseTwoLevelSetFunction.h"

namespace itk
{
template<	typename TInputImage,
typename TFeatureImage,
typename TOutputPixelType = float >
class ITK_EXPORT VascularPhaseTwoLevelSetImageFilter:
public VascularLevelSetImageFilter< TInputImage, TFeatureImage,
TOutputPixelType >
{

public:
	/* Standard */
	typedef VascularPhaseTwoLevelSetImageFilter Self;
	typedef VascularLevelSetImageFilter< TInputImage, TFeatureImage,
			TOutputPixelType > Superclass;

	typedef SmartPointer< Self >       Pointer;
	typedef SmartPointer< const Self > ConstPointer;

	/** Inherited typedef from the superclass. */
	typedef typename Superclass::ValueType        ValueType;
	typedef typename Superclass::OutputImageType  OutputImageType;
	typedef typename Superclass::FeatureImageType FeatureImageType;
	typedef typename Superclass::CurrentImageType CurrentImageType;

	/** Type of the segmentation function */
	typedef VascularPhaseTwoLevelSetFunction< OutputImageType,
			FeatureImageType > VascularPhaseTwoLevelSetFunctionType;
	typedef typename VascularPhaseTwoLevelSetFunctionType::Pointer
			VascularPhaseTwoLevelSetFunctionPointer;

	/** Run-time type information (and related methods). */
	itkTypeMacro(VascularPhaseTwoLevelSetImageFilter, VascularLevelSetImageFilter);

	/** Method for creation through the object factory */
	itkNewMacro(Self);

	void SetAdvectionDerivativeSigma(float value)
	{if ( value != m_VascularPhaseTwoLevelSetFunction->GetAdvectionDerivativeSigma() )
	{m_VascularPhaseTwoLevelSetFunction->SetAdvectionDerivativeSigma(value);
	this->Modified();}}

	/** Get the value of sigma used to compute the edge potential map derivatives.*/
	float GetAdvectionDerivativeSigma() const
	{ return m_VascularPhaseTwoLevelSetFunction->GetAdvectionDerivativeSigma(); }

	void SetSpeedDerivativeSigma(float value)
	{if ( value != m_VascularPhaseTwoLevelSetFunction->GetSpeedDerivativeSigma() )
	{m_VascularPhaseTwoLevelSetFunction->SetSpeedDerivativeSigma(value);
	this->Modified();}}
	float GetSpeedDerivativeSigma() const
	{ return m_VascularPhaseTwoLevelSetFunction->GetSpeedDerivativeSigma(); }


	void SetCurvataureLowerThreshold(const double v)
	{ m_VascularPhaseTwoLevelSetFunction->SetCurvataureLowerThreshold(v);
	this->Modified();}
	double GetCurvataureLowerThreshold()
	{ return m_VascularPhaseTwoLevelSetFunction->GetCurvataureLowerThreshold(); }

	void SetCurvataureUpperThreshold(const double v)
	{ m_VascularPhaseTwoLevelSetFunction->SetCurvataureUpperThreshold(v);
	this->Modified();}
	double GetCurvataureUpperThreshold()
	{ return m_VascularPhaseTwoLevelSetFunction->GetCurvataureUpperThreshold();}



protected:
	~VascularPhaseTwoLevelSetImageFilter() {}
	VascularPhaseTwoLevelSetImageFilter();

	virtual void PrintSelf(std::ostream & os, Indent indent) const;

	VascularPhaseTwoLevelSetImageFilter(const Self &); // purposely not
	// implemented
	void operator=(const Self &);

	void GenerateData();

private:
	VascularPhaseTwoLevelSetFunctionPointer m_VascularPhaseTwoLevelSetFunction;


};

}//namespace




#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVascularPhaseTwoLevelSetImageFilter.hxx"
#endif

#endif
