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
 * itkVascularPhaseOneLevelSetFunction.h
 *
 *  Created on: Nov 12, 2013
 *      Author: root
 */
#ifndef __itkVascularPhaseOneLevelSetFunction_h
#define __itkVascularPhaseOneLevelSetFunction_h


#include "VascularLevelSetFunction.h"
#include "itkExpNegativeImageFilter.h"

namespace itk
{

template< typename TImageType, typename TFeatureImageType = TImageType >
class VascularPhaseOneLevelSetFunction:
		public VascularLevelSetFunction< TImageType, TFeatureImageType >
{
public:
	typedef VascularPhaseOneLevelSetFunction Self;
	typedef VascularLevelSetFunction< TImageType, TFeatureImageType >
	Superclass;
	typedef SmartPointer< Self >       Pointer;
	typedef SmartPointer< const Self > ConstPointer;
	typedef TFeatureImageType          FeatureImageType;
	typedef typename FeatureImageType::Pointer FeatureImageTypePointer;

	/** Method for creation through the object factory. */
	itkNewMacro(Self);

	/** Run-time type information (and related methods) */
	itkTypeMacro(VascularPhaseOneLevelSetFunction, VascularLevelSetFunction);

	//itkSetMacro(Debug,bool);

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
	typedef typename Superclass::InterpolatorType InterpolatorType;
	typedef typename ImageType::IndexType         IndexType;
	typedef typename InterpolatorType::ContinuousIndexType ContinuousIndexType;




	/** Extract some parameters from the superclass. */
	itkStaticConstMacro(ImageDimension, unsigned int,
			Superclass::ImageDimension);

	typedef itk::Vector<ScalarValueType,ImageDimension> NormalVectorType;
	typedef typename NeighborhoodType::SizeValueType    NeighborhoodSizeValueType;


	/** Compute speed image from feature image.
	 * V0.0.3 SpeedImage = V = exp(-e|dG/d|) (smoothed)
	 * Exp negative of magnitude of the gradient of the feature image*/
	virtual void CalculateSpeedImage();

	/** Compute the advection field from feature image.
	 *  V0.0.3 AdvectionImage = A = gradient[exp(-e|dG/d|)] (smoothed)
	 *  Gradient of the speed image*/
	virtual void CalculateAdvectionImage();

	virtual ScalarValueType CurvatureSpeed(const NeighborhoodType & neighborhood,
			const FloatOffsetType & offset, GlobalDataStruct *gd) const
	{
		return this->PropagationSpeed(neighborhood, offset, gd);//TODO: Change this?
	}

	/** Set/Get the sigma for the Gaussian kernel used to compute the gradient
	 * of the feature image needed for the advection term of the equation. */
	void SetAdvectionDerivativeSigma(const double v)
	{ m_AdvectionDerivativeSigma = v; }
	double GetAdvectionDerivativeSigma()
	{ return m_AdvectionDerivativeSigma; }

	/** Set/Get the sigma for the Gaussian kernel used to compute the gradient
	 * of the feature image needed for the speed term of the equation. */
	void SetSpeedDerivativeSigma(const double v)
	{ m_SpeedDerivativeSigma = v; }

	double GetSpeedDerivativeSigma()
	{ return m_SpeedDerivativeSigma; }

	void SetRisingVelocityDecayModifier(const double v)
	{ m_RisingVelocityDecayModifier = v; }
	double GetRisingVelocityDecayModifier()
	{ return m_RisingVelocityDecayModifier; }

	void SetFallingVelocityDecayModifier(const double v)
	{ m_FallingVelocityDecayModifier = v; }
	double GetFallingVelocityDecayModifier()
	{ return m_FallingVelocityDecayModifier; }

	double GetDecayModiferRatio()
	{return m_FallingVelocityDecayModifier/m_RisingVelocityDecayModifier;}

	void SetEquilibriumCurvature(const double v)
	{ m_EquilibriumCurvature = v; }
	double GetEquilibriumCurvature()
	{ return m_EquilibriumCurvature; }


	virtual void Initialize(const RadiusType & r)
	{
		Superclass::Initialize(r);

		this->SetAdvectionWeight(NumericTraits< ScalarValueType >::One);
		this->SetPropagationWeight(NumericTraits< ScalarValueType >::One);
		this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);
	}

protected:

	VascularPhaseOneLevelSetFunction()
{
		this->SetAdvectionWeight(NumericTraits< ScalarValueType >::One);
		this->SetPropagationWeight(NumericTraits< ScalarValueType >::One);
		this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);

		this->SetUseMinimalCurvature(false);


		m_AdvectionDerivativeSigma = 1.0;
		m_SpeedDerivativeSigma = 1.0;
		m_RisingVelocityDecayModifier = 2.0;
		m_FallingVelocityDecayModifier = 1.0;
		m_EquilibriumCurvature = 0.0;
		m_Debug = 0;


		m_CurrentCurvatureImage = ImageType::New();
		m_CurrentAdvectionImage = ImageType::New();
		m_CurrentUpdateImage = ImageType::New();

		m_GradientImage = VectorImageType::New();
		m_VectorGradientInterpolator = VectorInterpolatorType::New();
}

	//typename VectorInterpolatorType::Pointer m_VectorInterpolator;

	virtual ~VascularPhaseOneLevelSetFunction() {}
	VascularPhaseOneLevelSetFunction(const Self &); //purposely not
	// implemented
	void operator=(const Self &);                        //purposely not
	// implemented

	void PrintSelf(std::ostream & os, Indent indent) const
	{

		Superclass::PrintSelf(os, indent);
		this->PrintShort(os);

	}



public:
	void PrintShort(std::ostream & os) const
	{
		os << "AdvectionDerivativeSigma: " << m_AdvectionDerivativeSigma << std::endl;
		os << "SpeedDerivativeSigma: " << m_SpeedDerivativeSigma << std::endl;
		os << "RisingVelocityDecayModifier: " << m_RisingVelocityDecayModifier << std::endl;
		os << "FallingVelocityDecayModifier: " << m_FallingVelocityDecayModifier << std::endl;
		os << "EquilibriumCurvature: " << m_EquilibriumCurvature << std::endl;
		os << "Debug: " << m_Debug << std::endl;
	}
private:
	double m_AdvectionDerivativeSigma;
	double m_SpeedDerivativeSigma;
	double m_RisingVelocityDecayModifier; //+
	double m_FallingVelocityDecayModifier; //-
	double m_EquilibriumCurvature; //Kc
	typename ImageType::Pointer m_CurrentCurvatureImage;
	typename ImageType::Pointer m_CurrentAdvectionImage;
	typename ImageType::Pointer m_CurrentUpdateImage;
	int m_Debug;

public:
	virtual PixelType ComputeUpdate(const NeighborhoodType & it, void *globalData,
			const FloatOffsetType & offset);

	virtual ScalarValueType ComputeCurvatureTerm(const NeighborhoodType &,
			const FloatOffsetType &,
			GlobalDataStruct *gd = 0
	);

	//typedef typename Superclass::GlobalDataStruct GlobalDataStruct;
	struct VascularPhaseOneGlobalDataStruct:public GlobalDataStruct {
		ScalarValueType m_GradientProjection;
	};

	/** Returns a pointer to a global data structure for computing time step. */
	virtual void * GetGlobalDataPointer() const
	{
		VascularPhaseOneGlobalDataStruct *ans = new VascularPhaseOneGlobalDataStruct();

		ans->m_MaxAdvectionChange   = NumericTraits< ScalarValueType >::Zero;
		ans->m_MaxPropagationChange = NumericTraits< ScalarValueType >::Zero;
		ans->m_MaxCurvatureChange   = NumericTraits< ScalarValueType >::Zero;
		ans->m_GradientProjection  = NumericTraits< ScalarValueType >::Zero;
		return ans;
	}

	/** Release the global data structure. */
	virtual void ReleaseGlobalDataPointer(void *GlobalData) const
	{ delete (VascularPhaseOneGlobalDataStruct *)GlobalData; }


	private:
	/** The image holding the gradient field */
	typename VectorImageType::Pointer m_GradientImage;
	typename VectorInterpolatorType::Pointer m_VectorGradientInterpolator;


	//Added for debug, and evolution
	public:


	//	/** Allocates the image that will be used for the level set function's
	//	 * Gradient field term. */
	virtual void AllocateGradientImage();

	virtual void CalculateGradientImage();

	/** ADDED for rising/falling calculation!*/
	virtual VectorImageType * GetGradientImage()
	{ return m_GradientImage.GetPointer(); }
	void SetGradientImage(VectorImageType *s);

	virtual VectorType GradientField(const NeighborhoodType &,
			const FloatOffsetType &, GlobalDataStruct *gd) const;





};

} //namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVascularPhaseOneLevelSetFunction.hxx"
#endif

#endif
