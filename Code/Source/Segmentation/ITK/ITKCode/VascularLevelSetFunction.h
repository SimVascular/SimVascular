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
 * VascularLevelSetFunction.h
 *
 *  Created on: Nov 25, 2013
 *      Author: jmerkow
 */

/* This Function is meant to be a base for the vasuclar
 * levelset functions to allow an easier interface with simvascular
 * and produce debug images during testing.
 */



#ifndef __VascularLevelSetFunction_H_
#define __VascularLevelSetFunction_H_

#include "itkSegmentationLevelSetFunction.h"
#include "itkVector.h"


namespace itk
{
template< typename TImageType, typename TFeatureImageType = TImageType >
class ITK_EXPORT VascularLevelSetFunction :
public SegmentationLevelSetFunction< TImageType, TFeatureImageType >
{

public:
	// Basic Typedefs
	typedef VascularLevelSetFunction Self;
	typedef SegmentationLevelSetFunction< TImageType, TFeatureImageType >
	Superclass;
	typedef SmartPointer< Self >       Pointer;
	typedef SmartPointer< const Self > ConstPointer;
	typedef TFeatureImageType          FeatureImageType;

	/** Method for creation through the object factory. */
	itkNewMacro(Self);

	/** Run-time type information (and related methods) */
	itkTypeMacro(VascularLevelSetFunction, SegmentationLevelSetFunction);

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

	virtual ~VascularLevelSetFunction() {}
	VascularLevelSetFunction(const Self &); //purposely not
	// implemented
	void operator=(const Self &); //purposely not implemented

	void PrintSelf(std::ostream & os, Indent indent) const
	{
		Superclass::PrintSelf(os, indent);
	}



	void Initialize(const RadiusType & r)
	{
		Superclass::Initialize(r);

		this->SetAdvectionWeight(NumericTraits< ScalarValueType >::One);
		this->SetPropagationWeight(NumericTraits< ScalarValueType >::One);
		this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);
	}




public:

	/***Getters for the Current Images***/
	ImageType * GetCurrentCurvatureImage()
	{return m_CurrentCurvatureImage.GetPointer();}
	ImageType * GetCurrentAdvectionImage()
	{return m_CurrentAdvectionImage.GetPointer();}
	ImageType * GetCurrentUpdateImage()
	{return m_CurrentUpdateImage.GetPointer();}

	/***Allocators for the Current Images***/
	void AllocateCurrentCurvatureImage()
	{
		m_CurrentCurvatureImage->SetRequestedRegion( this->GetFeatureImage()->GetRequestedRegion() );
		m_CurrentCurvatureImage->SetBufferedRegion( this->GetFeatureImage()->GetBufferedRegion() );
		m_CurrentCurvatureImage->SetLargestPossibleRegion( this->GetFeatureImage()->GetLargestPossibleRegion() );
		m_CurrentCurvatureImage->Allocate();
		//this->ClearCurrentCurvatureImage();
	}

	void AllocateCurrentAdvectionImage()
	{
		m_CurrentAdvectionImage->SetRequestedRegion( this->GetFeatureImage()->GetRequestedRegion() );
		m_CurrentAdvectionImage->SetBufferedRegion( this->GetFeatureImage()->GetBufferedRegion() );
		m_CurrentAdvectionImage->SetLargestPossibleRegion( this->GetFeatureImage()->GetLargestPossibleRegion() );
		m_CurrentAdvectionImage->Allocate();
		//this->ClearCurrentAdvectionImage();
	}

	void AllocateCurrentUpdateImage()
	{
		m_CurrentUpdateImage->SetRequestedRegion( this->GetFeatureImage()->GetRequestedRegion() );
		m_CurrentUpdateImage->SetBufferedRegion( this->GetFeatureImage()->GetBufferedRegion() );
		m_CurrentUpdateImage->SetLargestPossibleRegion( this->GetFeatureImage()->GetLargestPossibleRegion() );
		m_CurrentUpdateImage->Allocate();;
		//this->ClearCurrentUpdateImage();
	}

	ScalarValueType ComputeCurvatureFromNormalVector(const NeighborhoodType &neighborhood) const
	{unsigned int  j, k;
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

	NormalVectorType ComputeNormalVector(const NeighborhoodType &neighborhood) const
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

	/***Clear Methods for the Current Images***/
	void ClearCurrentCurvatureImage()
	{
		ImageRegionIterator< ImageType >
		ait( m_CurrentCurvatureImage,
				this->GetFeatureImage()->GetRequestedRegion() );
		for(ait.GoToBegin(); !ait.IsAtEnd(); ++ait)
			ait.Set(0);
	}

	void ClearCurrentAdvectionImage(){
		ImageRegionIterator< ImageType >
		ait( m_CurrentAdvectionImage,
				this->GetFeatureImage()->GetRequestedRegion() );
		for(ait.GoToBegin(); !ait.IsAtEnd(); ++ait)
			ait.Set(0);
	}

	void ClearCurrentUpdateImage()
	{

		ImageRegionIterator< ImageType >
		ait( m_CurrentUpdateImage,
				this->GetFeatureImage()->GetRequestedRegion() );
		for(ait.GoToBegin(); !ait.IsAtEnd(); ++ait)
			ait.Set(0);

	}

protected:

	VascularLevelSetFunction()
{
		m_CurrentCurvatureImage = ImageType::New();
		m_CurrentAdvectionImage = ImageType::New();
		m_CurrentUpdateImage = ImageType::New();
		this->SetAdvectionWeight(NumericTraits< ScalarValueType >::One);
		this->SetPropagationWeight(NumericTraits< ScalarValueType >::One);
		this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);
		m_MinVectorNorm = static_cast< ScalarValueType >( 1.0e-6 );

		this->SetUseMinimalCurvature(false);
		m_Debug = 0;

}



private:
	typename ImageType::Pointer m_CurrentCurvatureImage;
	typename ImageType::Pointer m_CurrentAdvectionImage;
	typename ImageType::Pointer m_CurrentUpdateImage;

	static const NeighborhoodSizeValueType    m_NumVertex;
	static const ScalarValueType              m_DimConst;

	/** The minimum vector norm parameter. */
	ScalarValueType m_MinVectorNorm;

	int m_Debug;


	/* Debug Methods */
public:

	int GetDebug()
	{return m_Debug;}
	void SetDebug(int _arg)
	{
		if ( this->m_Debug != _arg )
			this->m_Debug = _arg;
	}
};

}


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


}// namepace





#endif /* __VascularLevelSetFunction_H_ */
