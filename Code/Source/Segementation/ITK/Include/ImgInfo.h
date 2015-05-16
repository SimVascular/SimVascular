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
 * ImgInfo.h
 *
 *  Created on: May 9, 2014
 *      Author: jameson
 */

#include <sstream>
#include <iostream>
#include <ostream>
#include <string>
typedef std::string string;
typedef std::ostream ostream;

#include "vtkSmartPointer.h"
#include "vtkStructuredPoints.h"
#include "vtkImageData.h"
#include "vtkPolyData.h"

#include "cvMacros.h"

#ifndef IMGINFO_H_
#define IMGINFO_H_


class ImgInfo
{
private:
	int extent[6];
	double origin[3];
	double spacing[3];
	double m_MinValue;
	double m_MaxValue;
public:
	cvSetMacro(MinValue,double);
	cvGetMacro(MinValue,double);
	cvSetMacro(MaxValue,double);
	cvGetMacro(MaxValue,double);

	ImgInfo()
	{
		m_MinValue = 0;
		m_MaxValue = 255;
		int extent[6] = {0,63,0,63,0,0};
		double origin[3] = {0,0,0};
		double spacing[3] = {1,1,1};

		this->SetExtent(extent);
		this->SetOrigin(origin);
		this->SetSpacing(spacing);
	}

	virtual ~ImgInfo(){}

	ImgInfo(int extent[6],double origin[3], double spacing[3])
	{
		m_MinValue = 0;
		m_MaxValue = 255;
		this->SetExtent(extent);
		this->SetOrigin(origin);
		this->SetSpacing(spacing);

	}
	ImgInfo(int vextent[6],double vorigin[3], double vspacing[3],
			double minValue,double maxValue)
	{
		m_MinValue = minValue;
		m_MaxValue = maxValue;
		this->SetExtent(extent);
		this->SetOrigin(origin);
		this->SetSpacing(spacing);
	}
	ImgInfo(vtkStructuredPoints* vtksp)
	{
		m_MinValue = vtksp->GetScalarTypeMin();
		m_MaxValue = vtksp->GetScalarTypeMax();
		int extent[6];
		double spacing[3];
		double origin[3];

		vtksp->GetSpacing(spacing);
		vtksp->GetExtent(extent);
		vtksp->GetOrigin(origin);
		this->SetExtent(extent);
		this->SetOrigin(origin);
		this->SetSpacing(spacing);

	}
	//copy constructor
	ImgInfo(const ImgInfo& other)
	{
		m_MinValue = other.m_MinValue;
		m_MaxValue = other.m_MinValue;
		this->SetExtent(extent);
		this->SetOrigin(origin);
		this->SetSpacing(spacing);
	}

	void SetExtent(int vextent[6])
	{for(int i = 0; i < 6;i++)
		this->extent[i] = vextent[i];}
	void GetExtent(int *dOut)
	{
		const int* extent = this->extent;
		for(int i = 0; i < 6;i++)
			dOut[i] = extent[i];
	}
	int *GetExtent()
	{
		return this->extent;
	}

	void SetOrigin(double vorigin[3])
	{for(int i = 0; i < 3; i++)
		this->origin[i] = vorigin[i];}
	void GetOrigin(double *dOut)
	{
		const double* origin = this->origin;
		for(int i = 0; i < 3;i++)
			dOut[i] = origin[i];
	}
	double *GetOrigin()
	{
		return this->origin;
	}
	void SetSpacing(double vspacing[3])
	{for(int i = 0; i < 3; i++)
		this->spacing[i] = vspacing[i];}

	void GetSpacing(double *dOut)
	{
		const double* spacing = this->spacing;
		for(int i = 0; i < 3;i++)
			dOut[i] = spacing[i];
	}
	double *GetSpacing()
	{
		return this->spacing;
	}
	inline void Print(ostream& os)
	{
		os <<"Extent: {";
		for(int i = 0;i<6;i++)
			os <<this->extent[i] << ", ";
		os <<"}" <<std::endl;

		os <<"Origin: {";
		for(int i = 0;i<3;i++)
			os <<this->origin[i] << ", ";
		os <<"}" <<std::endl;

		os <<"Spacing: {";
		for(int i = 0;i<3;i++)
			os <<this->spacing[i] << ", ";
		os <<"}" <<std::endl;
		os <<"MinValue: " << this->m_MinValue;
		os <<" MaxValue: " << this->m_MaxValue << std::endl;
	}


};


#endif /* IMGINFO_H_ */
