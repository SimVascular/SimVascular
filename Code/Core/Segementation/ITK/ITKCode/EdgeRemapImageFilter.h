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
 * EdgeRemapImageFilter.h
 *
 *  Created on: Jan 15, 2015
 *      Author: jameson
 */

#ifndef EDGEREMAPIMAGEFILTER_H_
#define EDGEREMAPIMAGEFILTER_H_
#include <iostream>
#include "itkUnaryFunctorImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include <itkSmartPointer.h>

template<typename TInput1, typename TOutput = float>
class EdgeRemapFunctor
{
public:

	inline TOutput operator()(const TInput1 & X) const
	{
		typedef typename itk::NumericTraits< TInput1 >::RealType RealType1;
//		float scale = 1.0f / (m_inputMax);
//		float xNorm = (static_cast<RealType1>(X))*scale;
		return static_cast< TOutput >((1.0 / (1.0 + std::pow(static_cast<RealType1>(X * m_Kappa),static_cast<RealType1>(m_Exponent)))));
	}

	bool operator ==(const EdgeRemapFunctor &other)
	    				{
		return !( *this != other );
	    				}

	bool operator !=(const EdgeRemapFunctor &z)
	    				{ return false; }

	void SetKappa(double kappa)
	{
		m_Kappa = kappa;
	}

	double GetKappa() const
	{
		return m_Kappa;
	}

	void SetExponent(double exponent)
	{
		m_Exponent = exponent;
	}

	double GetExponent() const
	{
		return m_Exponent;
	}
//	void SetInputMax(double inputmax)
//	{
//		m_inputMax = inputmax;
//	}
//
//	double GetInputMax() const
//	{
//		return m_inputMax;
//	}

private:
	float m_Kappa;
	float m_Exponent;
//	float m_inputMax;

};

template <typename TInputImage1,
typename TOutputImage = TInputImage1>
class EdgeRemapImageFilter:
		public itk::UnaryFunctorImageFilter<TInputImage1,TOutputImage,
						EdgeRemapFunctor<
		typename TInputImage1::PixelType,
		typename TOutputImage::PixelType >   >
{
public:
	typedef EdgeRemapImageFilter Self;
	typedef itk::UnaryFunctorImageFilter< TInputImage1, TOutputImage,
			EdgeRemapFunctor<
			typename TInputImage1::PixelType,
			typename TOutputImage::PixelType > > Superclass;

	typedef itk::SmartPointer< Self >       Pointer;
	typedef itk::SmartPointer< const Self > ConstPointer;


	/** Method for creation through the object factory. */
	itkNewMacro(Self);

	/** Runtime information support. */
	itkTypeMacro(EdgeRemapImageFilter,
			UnaryFunctorImageFilter);

	/** Set the second operand as a constant */
	void SetKappa(float kappa)
	{
		if ( kappa == this->GetFunctor().GetKappa() )
		{
			return;
		}
		this->GetFunctor().SetKappa(kappa);
		this->Modified();
	}
	double GetKappa(){return this->GetFunctor().GetKappa();}
	/** Set the Third operand as a constant */
	virtual void SetExponent(float exponent)
	{
		if ( exponent == this->GetFunctor().GetExponent() )
		{
			return;
		}
		this->GetFunctor().SetExponent(exponent);
		this->Modified();
	}
	virtual double GetExponent(){return this->GetFunctor().GetExponent(); }
//	void SetInputMax(float inputmax)
//	{
//		if ( inputmax == this->GetFunctor().GetInputMax() )
//		{
//			return;
//		}
//		this->GetFunctor().SetInputMax(inputmax);
//		this->Modified();
//	}
//	void GetInputMax(){return this->GetInputMax().GetKappa();}

};


#endif /* EDGEREMAPIMAGEFILTER_H_ */
