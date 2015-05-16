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
 * itkVascularPhaseOneLevelSetImageFilter.h
 *
 *  Created on: Nov 12, 2013
 *      Author: root
 */
#ifndef __itkVascularPhaseOneLevelSetImageFilter_h
#define __itkVascularPhaseOneLevelSetImageFilter_h

#include "VascularLevelSetImageFilter.h"
#include "itkVascularPhaseOneLevelSetFunction.h"


namespace itk
{
template<	typename TInputImage,
typename TFeatureImage,
typename TOutputPixelType = float >
class ITK_EXPORT VascularPhaseOneLevelSetImageFilter:
public VascularLevelSetImageFilter< TInputImage, TFeatureImage,
TOutputPixelType >
{
public:
	/* Standard */
	typedef VascularPhaseOneLevelSetImageFilter Self;
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
	typedef VascularPhaseOneLevelSetFunction< OutputImageType,
			FeatureImageType > VascularPhaseOneLevelSetFunctionType;
	typedef VascularPhaseOneLevelSetFunction< OutputImageType,
			FeatureImageType > LevelSetFunctionType;
	typedef typename VascularPhaseOneLevelSetFunctionType::Pointer
			VascularPhaseOneLevelSetFunctionPointer;

	/** Run-time type information (and related methods). */
	itkTypeMacro(VascularPhaseOneLevelSetImageFilter,
			VascularLevelSetImageFilter);

	/** Method for creation through the object factory */
	itkNewMacro(Self);


	void SetAdvectionDerivativeSigma(float value)
	{if ( value != m_VascularPhaseOneLevelSetFunction->GetAdvectionDerivativeSigma() )
	{m_VascularPhaseOneLevelSetFunction->SetAdvectionDerivativeSigma(value);
	this->Modified();}}

	/** Get the value of sigma used to compute the edge potential map derivatives.
	 */
	float SetAdvectionDerivativeSigma() const
	{ return m_VascularPhaseOneLevelSetFunction->GetAdvectionDerivativeSigma(); }


	void SetSpeedDerivativeSigma(float value)
	{if ( value != m_VascularPhaseOneLevelSetFunction->GetSpeedDerivativeSigma() )
	{m_VascularPhaseOneLevelSetFunction->SetSpeedDerivativeSigma(value);
	this->Modified();}}

	/** Get the value of the rising
	 */
	float GetSpeedDerivativeSigma() const
	{ return m_VascularPhaseOneLevelSetFunction->GetSpeedDerivativeSigma(); }


	void SetRisingVelocityDecayModifier(float value)
	{if ( value != m_VascularPhaseOneLevelSetFunction->GetRisingVelocityDecayModifier() )
	{m_VascularPhaseOneLevelSetFunction->SetRisingVelocityDecayModifier(value);
	this->Modified();}}

	/** Get the value of the value Falling Modifier
	 */
	float GetRisingVelocityDecayModifier() const
	{ return m_VascularPhaseOneLevelSetFunction->GetRisingVelocityDecayModifier(); }


	void SetFallingVelocityDecayModifier(float value)
	{if ( value != m_VascularPhaseOneLevelSetFunction->GetFallingVelocityDecayModifier() )
	{m_VascularPhaseOneLevelSetFunction->SetFallingVelocityDecayModifier(value);
	this->Modified();}}
	float GetFallingVelocityDecayModifier() const
	{ return m_VascularPhaseOneLevelSetFunction->GetFallingVelocityDecayModifier(); }

	float GetDecayModiferRatio()
	{return this->m_VascularPhaseOneLevelSetFunction->GetDecayModiferRatio();}

	void SetEquilibriumCurvature(float value)
	{if ( value != m_VascularPhaseOneLevelSetFunction->GetEquilibriumCurvature() )
	{m_VascularPhaseOneLevelSetFunction->SetEquilibriumCurvature(value);
	this->Modified();}}
	float GetEquilibriumCurvature() const
	{ return m_VascularPhaseOneLevelSetFunction->GetEquilibriumCurvature(); }


	void PrintShort(std::ostream & os) const
	{
		m_VascularPhaseOneLevelSetFunction->PrintShort(os);
	}


protected:
	~VascularPhaseOneLevelSetImageFilter() {}
	VascularPhaseOneLevelSetImageFilter();

	virtual void PrintSelf(std::ostream & os, Indent indent) const;

	VascularPhaseOneLevelSetImageFilter(const Self &); // purposely not
	// implemented
	void operator=(const Self &);

	void GenerateData();
private:
	VascularPhaseOneLevelSetFunctionPointer m_VascularPhaseOneLevelSetFunction;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVascularPhaseOneLevelSetImageFilter.hxx"
#endif

#endif /* ITKVASCULARPHASEONELEVELSETIMAGEFILTER_H_ */
