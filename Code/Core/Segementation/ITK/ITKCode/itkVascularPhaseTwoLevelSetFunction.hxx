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
 * itkVascularPhaseTwoLevelSetFunction.hxx
 *
 *  Created on: Nov 19, 2013
 *      Author: jmerkow
 */
#ifndef __itkVascularPhaseTwoLevelSetFunction_hxx
#define __itkVascularPhaseTwoLevelSetFunction_hxx

#include "itkVascularPhaseTwoLevelSetFunction.h"

#include "itkImageRegionIterator.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImageAlgorithm.h"

#include"vnl/vnl_math.h"


namespace itk
{


/*
 *
template< typename TImageType, typename TFeatureImageType >
void VascularPhaseTwoLevelSetFunction< TImageType, TFeatureImageType >
::
 */


template< typename TImageType, typename TFeatureImageType >
void VascularPhaseTwoLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
 {
	ImageAlgorithm::Copy( this->GetFeatureImage(),
			this->GetSpeedImage(),
			this->GetFeatureImage()->GetRequestedRegion(),
			this->GetFeatureImage()->GetRequestedRegion() );

 }


template< typename TImageType, typename TFeatureImageType >
void VascularPhaseTwoLevelSetFunction< TImageType, TFeatureImageType >
::CalculateAdvectionImage()
 {
	typename VectorImageType::Pointer gradientImage;

	if ( m_AdvectionDerivativeSigma != NumericTraits< float >::Zero )
	{
		typedef GradientRecursiveGaussianImageFilter< FeatureImageType, VectorImageType >
		DerivativeFilterType;

		typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
		derivative->SetInput( this->GetFeatureImage());
		derivative->SetSigma(m_AdvectionDerivativeSigma);
		derivative->Update();

		gradientImage = derivative->GetOutput();
	}
	else
	{
		typedef GradientImageFilter< FeatureImageType > DerivativeFilterType;

		typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
		derivative->SetInput( this->GetFeatureImage());
		derivative->SetUseImageSpacingOn();
		derivative->Update();

		typedef typename DerivativeFilterType::OutputImageType                      DerivativeOutputImageType;
		typedef VectorCastImageFilter< DerivativeOutputImageType, VectorImageType > GradientCasterType;

		typename GradientCasterType::Pointer caster = GradientCasterType::New();
		caster->SetInput( derivative->GetOutput() );
		caster->Update();

		gradientImage = caster->GetOutput();
	}

	this->GetAdvectionImage()->CopyInformation( this->GetFeatureImage() );
	/* copy negative gradient into the advection image. */
	ImageRegionIterator< VectorImageType >
	dit( gradientImage, this->GetAdvectionImage()->GetRequestedRegion() );
	ImageRegionIterator< VectorImageType >
	ait( this->GetAdvectionImage(), this->GetAdvectionImage()->GetRequestedRegion() );

	for ( dit.GoToBegin(), ait.GoToBegin(); !dit.IsAtEnd(); ++dit, ++ait )
	{
		typename VectorImageType::PixelType v = dit.Get();
		for ( unsigned int j = 0; j < ImageDimension; j++ )
		{
			v[j] *= 1.0L;
		}
		ait.Set(v);
	}

	if(this->GetDebug() > 2)
		std::cout << "Seg Function -> Advection Image Calculated"  << std::endl;

 }


template< typename TImageType, typename TFeatureImageType >
typename VascularPhaseTwoLevelSetFunction<TImageType,TFeatureImageType >::ScalarValueType
VascularPhaseTwoLevelSetFunction< TImageType, TFeatureImageType >
::ComputeCurvatureTerm(const NeighborhoodType & it,const FloatOffsetType & offset, GlobalDataStruct *globalData)
 {

	ScalarValueType curvature_term = this->Superclass::ComputeCurvatureTerm(it, offset,globalData);
	//std::cout << "Update1 K: "<<curvature_term << "Index: " << it.GetIndex() << std::endl;



	return curvature_term;
 }

template< typename TImageType, typename TFeatureImageType >
typename VascularPhaseTwoLevelSetFunction<TImageType,TFeatureImageType >::PixelType
VascularPhaseTwoLevelSetFunction< TImageType, TFeatureImageType >
::ComputeUpdate(const NeighborhoodType & it, void *globalData,
		const FloatOffsetType & offset)
		{
	const ScalarValueType ZERO = NumericTraits< ScalarValueType >::Zero;
	ScalarValueType Klow = this->GetCurvataureLowerThreshold();
	ScalarValueType Kupp = this->GetCurvataureUpperThreshold();


	ScalarValueType  x_energy,curvature_term, advection_term, curvature_term2;
	VectorType advection_field;

	ScalarValueType AdvectionWeight = this->GetAdvectionWeight();
	ScalarValueType CurvatureWeight = this->GetCurvatureWeight();

	//need to updage gd
	this->Superclass::ComputeUpdate(it,globalData,offset);

	GlobalDataStruct *gd = (GlobalDataStruct *)globalData;



	// curvature_term = this->Superclass::ComputeCurvatureTerm(it, offset, gd);
	//	* CurvatureWeight
	//	* this->Superclass::CurvatureSpeed(it, offset);

	bool smooth = false;
	ScalarValueType normvec_curvature = this->Superclass::ComputeCurvatureFromNormalVector(it);

	curvature_term = normvec_curvature;


	curvature_term2 = ZERO;
	if(curvature_term > Kupp)
	{
		curvature_term2 = -(Kupp - curvature_term);
		smooth = true;
	}
	else if(curvature_term < Klow)
	{
		curvature_term2 = -(Klow - curvature_term);
		smooth = true;
	}



	curvature_term2 =curvature_term2*smooth*CurvatureWeight
			* this->Superclass::CurvatureSpeed(it, offset);;

	gd->m_MaxCurvatureChange = vnl_math_max( gd->m_MaxCurvatureChange,
			vnl_math_abs(curvature_term2) );
	ScalarValueType advection_term2 = ZERO;
	unsigned int i;
	if(AdvectionWeight != ZERO)
	{
		// if not smoothing do advection, edge attraction

		advection_field = this->AdvectionField(it, offset, gd);
		advection_term = ZERO;

		for ( i = 0; i < ImageDimension; i++ )
		{
			x_energy = AdvectionWeight * advection_field[i];

			if ( x_energy > ZERO )
			{
				advection_term += advection_field[i] * gd->m_dx_backward[i];
			}
			else
			{
				advection_term += advection_field[i] * gd->m_dx_forward[i];
			}


		}

		advection_term2 = (!smooth)*AdvectionWeight*advection_term;
		gd->m_MaxAdvectionChange =
				vnl_math_max( gd->m_MaxAdvectionChange, vnl_math_abs(advection_term2) );

	}
	else
	{
		advection_term2 = ZERO;
	}


	PixelType update = (PixelType)(curvature_term2
			-advection_term2);

	this->GetCurrentCurvatureImage()->SetPixel(it.GetIndex(),vnl_math_min(vnl_math_max(curvature_term,(float)-1000.0),(float)1000.0));
	this->GetCurrentAdvectionImage()->SetPixel(it.GetIndex(),advection_term);
	this->GetCurrentUpdateImage()->SetPixel(it.GetIndex(),update);

	if(0){
		std::cout << "Smooth: " << smooth << std::endl
				<< "Norm Curvature "<< normvec_curvature <<" K: "<< curvature_term << " Kout: "<< curvature_term2 << std::endl;
		std::cout <<" advection : "<< advection_term << std::endl;
		std::cout <<" update : "<< update << std::endl;
	}


	return update; //(gradient is negative!)


		}
template< typename TImageType, typename TFeatureImageType >
typename VascularPhaseTwoLevelSetFunction<TImageType,TFeatureImageType >::TimeStepType
VascularPhaseTwoLevelSetFunction< TImageType, TFeatureImageType >
::ComputeGlobalTimeStep(void *GlobalData) const
 {
	TimeStepType dt;

	GlobalDataStruct *d = (GlobalDataStruct *)GlobalData;
	double m_WaveDT = this->Superclass::GetMaximumPropagationTimeStep();
	double m_DT = this->Superclass::GetMaximumCurvatureTimeStep();
	d->m_MaxAdvectionChange += d->m_MaxPropagationChange;

	if ( vnl_math_abs(d->m_MaxCurvatureChange) > 0.0 )
	{
		if ( d->m_MaxAdvectionChange > 0.0 )
		{
			dt = vnl_math_min( ( m_WaveDT / d->m_MaxAdvectionChange ),
					(    m_DT / d->m_MaxCurvatureChange ) );
		}
		else
		{
			dt = m_DT / d->m_MaxCurvatureChange;
		}
	}
	else
	{
		if ( d->m_MaxAdvectionChange > 0.0 )
		{
			dt = m_WaveDT / d->m_MaxAdvectionChange;
		}
		else
		{
			dt = 0.0;
		}
	}

	double maxScaleCoefficient = 0.0;
	for ( unsigned int i = 0; i < ImageDimension; i++ )
	{
		maxScaleCoefficient = vnl_math_max(this->m_ScaleCoefficients[i], maxScaleCoefficient);
	}
	dt /= maxScaleCoefficient;
	//std::cout << "dt: " << dt << std::endl;
	// reset the values
	d->m_MaxAdvectionChange   = NumericTraits< ScalarValueType >::Zero;
	d->m_MaxPropagationChange = NumericTraits< ScalarValueType >::Zero;
	d->m_MaxCurvatureChange   = NumericTraits< ScalarValueType >::Zero;

	return dt;
 }



} //namespace



#endif
