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
 * itkVascularPhaseOneLevelSetFunction.hxx
 *
 *  Created on: Nov 13, 2013
 *      Author: root
 */

#ifndef __itkVascularPhaseOneLevelSetFunction_hxx
#define __itkVascularPhaseOneLevelSetFunction_hxx

#include "itkVascularPhaseOneLevelSetFunction.h"
#include <math.h>


#include "itkImageRegionIterator.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkImageAlgorithm.h"

namespace itk
{

template< typename TImageType, typename TFeatureImageType >
void VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
 {

	typedef ExpNegativeImageFilter<FeatureImageType,ImageType> ExpNegativeFilterType;
	typedef typename ExpNegativeFilterType::Pointer ExpNegativeFilterTypePointer;
	ExpNegativeFilterTypePointer expFilter = ExpNegativeFilterType::New();


	expFilter->SetFactor(m_RisingVelocityDecayModifier);
	expFilter->SetInput(this->GetFeatureImage());

	expFilter->Update();


	this->GetSpeedImage()->CopyInformation( this->GetFeatureImage() );

	ImageRegionIterator <ImageType>
	dit(expFilter->GetOutput(),this->GetSpeedImage()->GetRequestedRegion());

	ImageRegionIterator <ImageType>
	ait(this->GetSpeedImage(),this->GetSpeedImage()->GetRequestedRegion());
	for ( dit.GoToBegin(), ait.GoToBegin(); !dit.IsAtEnd(); ++dit, ++ait )
	{
		typename ImageType::PixelType v = dit.Get();
		ait.Set(v);
	}

 }

template< typename TImageType, typename TFeatureImageType >
void VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::CalculateAdvectionImage()
 {

	typedef ExpNegativeImageFilter<FeatureImageType,ImageType> ExpNegativeFilterType;
	typedef typename ExpNegativeFilterType::Pointer ExpNegativeFilterTypePointer;
	ExpNegativeFilterTypePointer expFilter = ExpNegativeFilterType::New();


	expFilter->SetFactor(m_RisingVelocityDecayModifier);
	expFilter->SetInput(this->GetFeatureImage());
	expFilter->Update();

	if(this->GetDebug() > 2)
		std::cout << "Seg Function -> Calculating Advection Image" << std::endl;
	/* compute the gradient of the feature image. */

	typename VectorImageType::Pointer vecgradientImage;

	if ( m_AdvectionDerivativeSigma != NumericTraits< float >::Zero )
	{
		typedef GradientRecursiveGaussianImageFilter< ImageType, VectorImageType >
		DerivativeFilterType;

		typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
		derivative->SetInput( expFilter->GetOutput() );
		derivative->SetSigma(m_AdvectionDerivativeSigma);
		derivative->Update();

		vecgradientImage = derivative->GetOutput();
	}
	else
	{
		typedef GradientImageFilter< ImageType > DerivativeFilterType;

		typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
		derivative->SetInput( expFilter->GetOutput()  );
		derivative->SetUseImageSpacingOff();
		derivative->Update();

		typedef typename DerivativeFilterType::OutputImageType                      DerivativeOutputImageType;
		typedef VectorCastImageFilter< DerivativeOutputImageType, VectorImageType > GradientCasterType;

		typename GradientCasterType::Pointer caster = GradientCasterType::New();
		caster->SetInput( derivative->GetOutput() );
		caster->Update();

		vecgradientImage = caster->GetOutput();
	}

	this->GetAdvectionImage()->CopyInformation( this->GetFeatureImage() );
	/* copy negative gradient into the advection image. */
	ImageRegionIterator< VectorImageType >
	dit( vecgradientImage, this->GetAdvectionImage()->GetRequestedRegion() );
	ImageRegionIterator< VectorImageType >
	ait( this->GetAdvectionImage(), this->GetAdvectionImage()->GetRequestedRegion() );

	for ( dit.GoToBegin(), ait.GoToBegin(); !dit.IsAtEnd(); ++dit, ++ait )
	{
		typename VectorImageType::PixelType v = dit.Get();
		for ( unsigned int j = 0; j < ImageDimension; j++ )
		{
			v[j] *= -1.0L;
		}
		ait.Set(v);
	}

	if(this->GetDebug() > 2)
		std::cout << "Seg Function -> Advection Image Calculated"  << std::endl;
 }

template< typename TImageType, typename TFeatureImageType >
void VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::CalculateGradientImage()
 {
	if(this->GetDebug() > 2)
		std::cout << "Seg Function -> Calculating Gradient Image" << std::endl;
	/* compute the gradient of the feature image. */

	typename VectorImageType::Pointer gradientImage;

	if ( m_AdvectionDerivativeSigma != NumericTraits< float >::Zero )
	{
		typedef GradientRecursiveGaussianImageFilter< FeatureImageType, VectorImageType >
		DerivativeFilterType;

		typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
		derivative->SetInput( this->GetFeatureImage() );
		derivative->SetSigma(m_AdvectionDerivativeSigma);
		derivative->Update();

		gradientImage = derivative->GetOutput();
	}
	else
	{
		typedef GradientImageFilter< FeatureImageType > DerivativeFilterType;

		typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
		derivative->SetInput( this->GetFeatureImage() );
		derivative->SetUseImageSpacingOff();
		derivative->Update();

		typedef typename DerivativeFilterType::OutputImageType                      DerivativeOutputImageType;
		typedef VectorCastImageFilter< DerivativeOutputImageType, VectorImageType > GradientCasterType;

		typename GradientCasterType::Pointer caster = GradientCasterType::New();
		caster->SetInput( derivative->GetOutput() );
		caster->Update();

		gradientImage = caster->GetOutput();
	}

	/* copy negative gradient into the advection image. */
	ImageRegionIterator< VectorImageType >
	dit( gradientImage, this->GetFeatureImage()->GetRequestedRegion() );
	ImageRegionIterator< VectorImageType >
	ait( this->GetGradientImage(), this->GetFeatureImage()->GetRequestedRegion() );

	for ( dit.GoToBegin(), ait.GoToBegin(); !dit.IsAtEnd(); ++dit, ++ait )
	{
		typename VectorImageType::PixelType v = dit.Get();
		for ( unsigned int j = 0; j < ImageDimension; j++ )
		{
			v[j] *= -1.0L;
		}
		ait.Set(v);
	}
	if(this->GetDebug() > 2)
		std::cout << "Seg Function -> Gradient Image Calculated" << std::endl;
 }

// Getting Into Risky Business now....

template< typename TImageType, typename TFeatureImageType >
typename VascularPhaseOneLevelSetFunction<TImageType,TFeatureImageType >::PixelType
VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::ComputeUpdate(const NeighborhoodType & it, void *globalData,
		const FloatOffsetType & offset)
		{

	unsigned int          i, j;
	const ScalarValueType ZERO = NumericTraits< ScalarValueType >::Zero;
	const ScalarValueType center_value  = it.GetCenterPixel();

	const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();

	ScalarValueType laplacian, x_energy, laplacian_term, propagation_term,
	curvature_term, advection_term, phi_gradient;
	VectorType advection_field;

	// Global data structure
	GlobalDataStruct *gd = (GlobalDataStruct *)globalData;
	VascularPhaseOneGlobalDataStruct *gdv = (VascularPhaseOneGlobalDataStruct *)globalData;

	// Compute the Hessian matrix and various other derivatives.  Some of these
	// derivatives may be used by overloaded virtual functions.
	//	OffsetValueType Center =  it.Size() / 2;
	//	OffsetValueType xStride[itkGetStaticConstMacro(ImageDimension)];
	//	for ( unsigned int i = 0; i < ImageDimension; i++ )
	//	{
	//		xStride[i] = it.GetStride(i);
	//	}
	//	gd->m_GradMagSqr = 1.0e-6;
	//	for ( i = 0; i < ImageDimension; i++ )
	//	{
	//		const unsigned int positionA =
	//				static_cast< unsigned int >( Center + xStride[i] );
	//		const unsigned int positionB =
	//				static_cast< unsigned int >( Center - xStride[i] );
	//
	//		gd->m_dx[i] = 0.5 * ( it.GetPixel(positionA)
	//				- it.GetPixel(positionB) ) * neighborhoodScales[i];
	//		gd->m_dxy[i][i] = ( it.GetPixel(positionA)
	//				+ it.GetPixel(positionB) - 2.0 * center_value )
	//				* vnl_math_sqr(neighborhoodScales[i]);
	//
	//		gd->m_dx_forward[i]  = ( it.GetPixel(positionA) - center_value ) * neighborhoodScales[i];
	//
	//		gd->m_dx_backward[i] = ( center_value - it.GetPixel(positionB) ) * neighborhoodScales[i];
	//
	//		gd->m_GradMagSqr += gd->m_dx[i] * gd->m_dx[i];
	//
	//		for ( j = i + 1; j < ImageDimension; j++ )
	//		{
	//			const unsigned int positionAa = static_cast< unsigned int >(
	//					Center - xStride[i] - xStride[j] );
	//			const unsigned int positionBa = static_cast< unsigned int >(
	//					Center - xStride[i] + xStride[j] );
	//			const unsigned int positionCa = static_cast< unsigned int >(
	//					Center + xStride[i] - xStride[j] );
	//			const unsigned int positionDa = static_cast< unsigned int >(
	//					Center + xStride[i] + xStride[j] );
	//
	//			gd->m_dxy[i][j] = gd->m_dxy[j][i] = 0.25 * ( it.GetPixel(positionAa)
	//					- it.GetPixel(positionBa)
	//					- it.GetPixel(positionCa)
	//					+ it.GetPixel(positionDa) )
	//					* neighborhoodScales[i] * neighborhoodScales[j];
	//		}
	//	}

	PixelType sup_update = this->Superclass::ComputeUpdate(it,globalData,offset);
	ScalarValueType tmp;
	if ( this->GetCurvatureWeight() != ZERO )
	{
		//this invokes a a new term in gd...
		curvature_term = this->ComputeCurvatureTerm(it, offset, gd) * this->GetCurvatureWeight();
		//curvature_term = this->Superclass::ComputeCurvatureTerm(it, offset, gd) * this->GetCurvatureWeight();
		tmp = this->CurvatureSpeed(it, offset,gd);
		if(gdv->m_GradientProjection < ZERO){
			tmp = (ScalarValueType)std::pow((double)tmp,(double)this->GetDecayModiferRatio());
		}

		curvature_term *= curvature_term*tmp*-1;


		//gd->m_MaxCurvatureChange = vnl_math_max( gd->m_MaxCurvatureChange,
		//		vnl_math_abs(curvature_term) );


		phi_gradient = ZERO;

		if ( curvature_term > ZERO )
		{
			for ( i = 0; i < ImageDimension; i++ )
			{
				phi_gradient += vnl_math_sqr( vnl_math_max(gd->m_dx_backward[i], ZERO) )
		                                		+ vnl_math_sqr( vnl_math_min(gd->m_dx_forward[i],  ZERO) );
			}
		}
		else
		{
			for ( i = 0; i < ImageDimension; i++ )
			{
				phi_gradient += vnl_math_sqr( vnl_math_min(gd->m_dx_backward[i], ZERO) )
		                                		+ vnl_math_sqr( vnl_math_max(gd->m_dx_forward[i],  ZERO) );
			}
		}


		// Collect energy change from propagation term.  This will be used in
		// calculating the maximum time step that can be taken for this iteration.
		gd->m_MaxCurvatureChange =
				vnl_math_max( gd->m_MaxCurvatureChange,
						vnl_math_abs(curvature_term) );

		curvature_term *= vcl_sqrt(phi_gradient);



	}
	else
	{
		curvature_term = ZERO;
		tmp = ZERO;
	}

	// Calculate the advection term.
	//  $\alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi $
	//
	// Here we can use a simple upwinding scheme since we know the
	// sign of each directional component of the advective force.
	//
	if ( this->GetAdvectionWeight() != ZERO )
	{
		advection_field = this->AdvectionField(it, offset, gd);
		advection_term = ZERO;

		for ( i = 0; i < ImageDimension; i++ )
		{
			x_energy = this->GetAdvectionWeight() * advection_field[i];

			if ( x_energy > ZERO )
			{
				advection_term += advection_field[i] * gd->m_dx_backward[i];
			}
			else
			{
				advection_term += advection_field[i] * gd->m_dx_forward[i];
			}

			gd->m_MaxAdvectionChange =
					vnl_math_max( gd->m_MaxAdvectionChange, vnl_math_abs(x_energy) );
		}
		advection_term *= this->GetAdvectionWeight();
	}
	else
	{
		advection_term = ZERO;
	}


	// Return the combination of all the terms.
	PixelType update =  (PixelType)( curvature_term
			- advection_term);

	if(0){
		std::cout << "Speed: " << this->CurvatureSpeed(it, offset,gd) << std::endl;
		std::cout << "super update: " << sup_update <<  std::endl;
		std::cout << update << " = " << this->ComputeCurvatureTerm(it, offset, gd) << " * " << this->GetCurvatureWeight() << " * " << tmp <<
				" - " << (double)advection_term/(double)this->GetAdvectionWeight() <<  " * " <<  this->GetAdvectionWeight() << std::endl;

	}
	this->GetCurrentCurvatureImage()->SetPixel(it.GetIndex(),curvature_term);
	this->GetCurrentAdvectionImage()->SetPixel(it.GetIndex(),advection_term);
	this->GetCurrentUpdateImage()->SetPixel(it.GetIndex(),update);

	return update;
		}



template< typename TImageType, typename TFeatureImageType >
typename VascularPhaseOneLevelSetFunction<TImageType,TFeatureImageType >::ScalarValueType
VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::ComputeCurvatureTerm(const NeighborhoodType & it,const FloatOffsetType & offset,
		GlobalDataStruct *globalData)
		{

	VascularPhaseOneGlobalDataStruct *gdv = (VascularPhaseOneGlobalDataStruct *)globalData;
	VectorType gradient_field = this->GradientField(it, offset, (GlobalDataStruct *)globalData);
	const ScalarValueType ZERO = NumericTraits< ScalarValueType >::Zero;
	ScalarValueType projection_term = ZERO;

	NormalVectorType normalvector = this->ComputeNormalVector(it);

	for (unsigned int i = 0; i < ImageDimension; i++ )
	{
		if(gradient_field[i] >ZERO)
			projection_term +=gradient_field[i] * gdv->m_dx_backward[i];
		else
			projection_term +=gradient_field[i] * gdv->m_dx_forward[i];
	}

	gdv->m_GradientProjection = projection_term;


	ScalarValueType Kc = this->GetEquilibriumCurvature();
	ScalarValueType ct = this->Superclass::ComputeCurvatureFromNormalVector(it);
	//ScalarValueType ct = this->Superclass::ComputeCurvatureTerm(it,offset,globalData);

	if(0){
		std::cout << "real curvature term: " << ct << std::endl;
	}



	return (ScalarValueType)(ct-Kc);
		}


// Gradient Image Stuff
template< typename TImageType, typename TFeatureImageType >
void VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::SetGradientImage(VectorImageType *s)
 {
	m_GradientImage = s;
	m_VectorGradientInterpolator->SetInputImage(m_GradientImage);
 }



template< typename TImageType, typename TFeatureImageType >
void VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::AllocateGradientImage()
 {
	m_GradientImage->SetRequestedRegion( this->GetFeatureImage()->GetRequestedRegion() );
	m_GradientImage->SetBufferedRegion( this->GetFeatureImage()->GetBufferedRegion() );
	m_GradientImage->SetLargestPossibleRegion(this->GetFeatureImage()->GetLargestPossibleRegion() );
	m_GradientImage->Allocate();
	m_VectorGradientInterpolator->SetInputImage(m_GradientImage);
 }



template< class TImageType, class TFeatureImageType >
typename VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >::VectorType
VascularPhaseOneLevelSetFunction< TImageType, TFeatureImageType >
::GradientField(const NeighborhoodType & neighborhood,
		const FloatOffsetType & offset, GlobalDataStruct *)  const
		{
	IndexType           idx = neighborhood.GetIndex();
	ContinuousIndexType cdx;

	for ( unsigned i = 0; i < ImageDimension; ++i )
	{
		cdx[i] = static_cast< double >( idx[i] ) - offset[i];
	}
	if ( m_VectorGradientInterpolator->IsInsideBuffer(cdx) )
	{
		return ( this->m_VectorCast( m_VectorGradientInterpolator->EvaluateAtContinuousIndex(cdx) ) );
	}
	//Just return the default else
	return ( m_GradientImage->GetPixel(idx) );
		}



} // end namespace itk

#endif



