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
 * VascularLevelSetFunction.hxx
 *
 *  Created on: Dec 5, 2014
 *      Author: jmerkow
 */

#ifndef VASCULARLEVELSETFUNCTION_HXX_
#define VASCULARLEVELSETFUNCTION_HXX_

namespace itk{
template< typename TImageType, typename TFeatureImageType >
const typename VascularLevelSetFunction< TImageType, TFeatureImageType >::NeighborhoodSizeValueType
VascularLevelSetFunction< TImageType, TFeatureImageType >
::m_NumVertex = 1 << TImageType::ImageDimension;

template< typename TImageType, typename TFeatureImageType >
const typename VascularLevelSetFunction< TImageType,
TFeatureImageType >::ScalarValueType
VascularLevelSetFunction< TImageType, TFeatureImageType >
::m_DimConst = static_cast< ScalarValueType >( 2.0 / m_NumVertex );

template< typename TImageType, typename TFeatureImageType >
typename VascularLevelSetFunction< TImageType,
TFeatureImageType >::ScalarValueType
VascularLevelSetFunction< TImageType, TFeatureImageType >
::ComputeCurvatureFromNormalVector(const NeighborhoodType &neighborhood) const
 {
	unsigned int  j, k;
	unsigned int  counterN, counterP;
	NeighborhoodSizeValueType positionN,  positionP,
	stride[TImageType::ImageDimension], indicator[TImageType::ImageDimension];

	const NeighborhoodSizeValueType one = 1;
	const NeighborhoodSizeValueType center = neighborhood.Size() / 2;
	const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();

	NormalVectorType normalvector;
	ScalarValueType  curvature;

	for ( j = 0; j < TImageType::ImageDimension; j++ )
	{
		stride[j] = neighborhood.GetStride(j);
		indicator[j] = one << j;
	}
	curvature = NumericTraits< ScalarValueType >::Zero;

	for ( counterN = 0; counterN < m_NumVertex; counterN++ )
	{
		// compute position of normal vector
		positionN = center;
		for ( k = 0; k < TImageType::ImageDimension; k++ )
		{
			if ( counterN & indicator[k] )
			{
				positionN -= stride[k];
			}
		}
		// compute the normal vector
		for ( j = 0; j < TImageType::ImageDimension; j++ ) // derivative axis
		{
			normalvector[j] = NumericTraits< ScalarValueType >::Zero;
			for ( counterP = 0; counterP < m_NumVertex; counterP++ )
			{
				positionP = positionN;
				for ( k = 0; k < TImageType::ImageDimension; k++ )
				{
					if ( counterP & indicator[k] )
					{
						positionP += stride[k];
					}
				}
				if ( counterP & indicator[j] )
				{
					normalvector[j] += neighborhood.GetPixel (positionP) * neighborhoodScales[j];
				}
				else
				{
					normalvector[j] -= neighborhood.GetPixel (positionP) * neighborhoodScales[j];
				}
			} // end counterP
		}   // end derivative axis
		normalvector = normalvector / ( m_MinVectorNorm + normalvector.GetNorm() );
		// add normal to curvature computation
		for ( j = 0; j < TImageType::ImageDimension; j++ ) // derivative axis
		{
			if ( counterN & indicator[j] )
			{
				curvature -= normalvector[j] * neighborhoodScales[j];
			}
			else
			{
				curvature += normalvector[j] * neighborhoodScales[j];
			}
		} // end derivative axis
	}   // end counterN

	curvature *= m_DimConst;

	return curvature;
 }
template< typename TImageType, typename TFeatureImageType >
typename VascularLevelSetFunction< TImageType,
TFeatureImageType >::NormalVectorType
VascularLevelSetFunction< TImageType, TFeatureImageType >
::ComputeNormalVector(const NeighborhoodType &neighborhood) const
 {
	unsigned int  j, k;
	unsigned int  counterP;
	NeighborhoodSizeValueType positionN,  positionP,
	stride[TImageType::ImageDimension], indicator[TImageType::ImageDimension];

	const NeighborhoodSizeValueType one = 1;
	const NeighborhoodSizeValueType center = neighborhood.Size() / 2;

	const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();
	NormalVectorType normalvector;

	for ( j = 0; j < TImageType::ImageDimension; j++ )
	{
		stride[j] = neighborhood.GetStride(j);
		indicator[j] = one << j;
	}
	// compute position of normal vector
	positionN = center;
	for ( k = 0; k < TImageType::ImageDimension; k++ )
	{
		if ( indicator[k] )
		{
			positionN -= stride[k];
		}
	}
	// compute the normal vector
	for ( j = 0; j < TImageType::ImageDimension; j++ ) // derivative axis
	{
		normalvector[j] = NumericTraits< ScalarValueType >::Zero;
		for ( counterP = 0; counterP < m_NumVertex; counterP++ )
		{
			positionP = positionN;
			for ( k = 0; k < TImageType::ImageDimension; k++ )
			{
				if ( counterP & indicator[k] )
				{
					positionP += stride[k];
				}
			}
			if ( counterP & indicator[j] )
			{
				normalvector[j] += neighborhood.GetPixel (positionP) * neighborhoodScales[j];
			}
			else
			{
				normalvector[j] -= neighborhood.GetPixel (positionP) * neighborhoodScales[j];
			}
		} // end counterP
	}   // end derivative axis
	normalvector = normalvector / ( m_MinVectorNorm + normalvector.GetNorm() );

	//}   // end counterN

	return normalvector;
 }



template< typename TImageType, typename TFeatureImageType >
typename VascularLevelSetFunction< TImageType,
TFeatureImageType >::ScalarValueType
VascularLevelSetFunction< TImageType, TFeatureImageType >
::ComputeCurvatureTerm(const NeighborhoodType & neighborhood,
		const FloatOffsetType & offset, GlobalDataStruct *gd){
	if(m_UseNormalVectorCurvature == true)
	{

		return this->ComputeCurvatureFromNormalVector(neighborhood);
	}
	else if ( this->GetUseMinimalCurvature() == false )
	{

		return this->ComputeMeanCurvature(neighborhood, offset, gd);
	}
	else
	{
		if ( ImageDimension == 3 )
		{
			return this->ComputeMinimalCurvature(neighborhood, offset, gd);
		}
		else if ( ImageDimension == 2 )
		{
			std::cout << "Using Mean Curvature: "
					<< this->ComputeMeanCurvature(neighborhood, offset, gd) << std::endl;
			return this->ComputeMeanCurvature(neighborhood, offset, gd);
		}
		else
		{
			return this->ComputeMinimalCurvature(neighborhood, offset, gd);
		}
	}
}


}// namepace





#endif /* VASCULARLEVELSETFUNCTION_HXX_ */
