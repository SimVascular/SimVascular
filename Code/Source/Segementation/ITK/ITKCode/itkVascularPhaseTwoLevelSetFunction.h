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
 * itkVascularPhaseTwoLevelSetFunction.h
 *
 *  Created on: Nov 19, 2013
 *      Author: jmerkow
 */

#ifndef __itkVascularPhaseTwoLevelSetFunction_h
#define __itkVascularPhaseTwoLevelSetFunction_h

#include "VascularLevelSetFunction.h"

namespace itk
{

template< typename TImageType, typename TFeatureImageType = TImageType >
class VascularPhaseTwoLevelSetFunction:
		public VascularLevelSetFunction< TImageType, TFeatureImageType >
{
public:
	typedef VascularPhaseTwoLevelSetFunction Self;
	typedef VascularLevelSetFunction< TImageType, TFeatureImageType >
	Superclass;
	typedef SmartPointer< Self >       Pointer;
	typedef SmartPointer< const Self > ConstPointer;
	typedef TFeatureImageType          FeatureImageType;

	/** Method for creation through the object factory. */
	itkNewMacro(Self);

	/** Run-time type information (and related methods) */
	itkTypeMacro(VascularPhaseTwoLevelSetFunction, VascularLevelSetFunction);

	/** Extract some parameters from the superclass. */
	typedef typename Superclass::ImageType         ImageType;
	typedef typename Superclass::PixelType         PixelType;
	typedef typename Superclass::NeighborhoodType  NeighborhoodType;
	typedef typename Superclass::NeighborhoodScalesType  NeighborhoodScalesType;
	typedef typename Superclass::ScalarValueType   ScalarValueType;
	typedef typename Superclass::FeatureScalarType FeatureScalarType;
	typedef typename Superclass::RadiusType        RadiusType;
	typedef typename Superclass::FloatOffsetType   FloatOffsetType;
	typedef typename Superclass::VectorType   	   VectorType;
	typedef typename Superclass::VectorImageType   VectorImageType;
	typedef typename Superclass::GlobalDataStruct  GlobalDataStruct;
	typedef typename Superclass::VectorInterpolatorType VectorInterpolatorType;
	typedef typename Superclass::TimeStepType TimeStepType;
	typedef typename Superclass::IndexType IndexType;

	/** Extract some parameters from the superclass. */
	itkStaticConstMacro(ImageDimension, unsigned int,
			Superclass::ImageDimension);


	typedef itk::Vector<ScalarValueType,ImageDimension> NormalVectorType;
	typedef typename NeighborhoodType::SizeValueType    NeighborhoodSizeValueType;


	/** Compute speed image from feature image.*/
	virtual void CalculateSpeedImage();

	/** Compute the advection field from feature image.*/
	virtual void CalculateAdvectionImage();

	//void GenerateGImageFromFeature(typename ExpNegativeImageFilter<FeatureImageType,FeatureImageType>::Pointer filter,double sigma,double expfactor);


	virtual ScalarValueType CurvatureSpeed(const NeighborhoodType & neighborhood,
			const FloatOffsetType & offset, GlobalDataStruct *gd) const
	{
		return this->PropagationSpeed(neighborhood, offset, gd);//TODO: Change this?
	}


	virtual void Initialize(const RadiusType & r)
	{
		Superclass::Initialize(r);

		this->SetAdvectionWeight(NumericTraits< ScalarValueType >::One);
		this->SetPropagationWeight(NumericTraits< ScalarValueType >::One);
		this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);
	}


	/* Getters and setters */
	void SetAdvectionDerivativeSigma(const double v)
	{ m_AdvectionDerivativeSigma = v; }
	double GetAdvectionDerivativeSigma()
	{ return m_AdvectionDerivativeSigma; }

	void SetSpeedDerivativeSigma(const double v)
	{ m_SpeedDerivativeSigma = v; }
	double GetSpeedDerivativeSigma()
	{ return m_SpeedDerivativeSigma; }

	void SetCurvataureLowerThreshold(const double v)
	{ m_CurvataureLowerThreshold = v; }
	double GetCurvataureLowerThreshold()
	{ return m_CurvataureLowerThreshold; }

	void SetCurvataureUpperThreshold(const double v)
	{ m_CurvataureUpperThreshold = v; }
	double GetCurvataureUpperThreshold()
	{ return m_CurvataureUpperThreshold; }


protected:

	VascularPhaseTwoLevelSetFunction()
{
		this->SetAdvectionWeight(NumericTraits< ScalarValueType >::One);
		this->SetPropagationWeight(NumericTraits< ScalarValueType >::One);
		this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);

		this->SetUseMinimalCurvature(false);

		m_AdvectionDerivativeSigma = 1.0;
		m_SpeedDerivativeSigma = 1.0;
		m_CurvataureLowerThreshold = 0.0;
		m_CurvataureUpperThreshold = 1;
}
	virtual ~VascularPhaseTwoLevelSetFunction() {}
	VascularPhaseTwoLevelSetFunction(const Self &); //purposely not
	// implemented
	void operator=(const Self &);                        //purposely not
	// implemented

	void PrintSelf(std::ostream & os, Indent indent) const
	{

		Superclass::PrintSelf(os, indent);
		//this->PrintShort(os);

	}


private:
	float m_AdvectionDerivativeSigma;
	float m_SpeedDerivativeSigma;
	float m_CurvataureLowerThreshold;
	float m_CurvataureUpperThreshold;


	//Functions that need to be overloaded.
public:

	virtual PixelType ComputeUpdate(const NeighborhoodType & it, void *globalData,
			const FloatOffsetType & offset);

	virtual ScalarValueType ComputeCurvatureTerm(const NeighborhoodType &,
			const FloatOffsetType &,
			GlobalDataStruct *gd = 0
	);

	virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const;

};
}


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVascularPhaseTwoLevelSetFunction.hxx"
#endif

#endif
