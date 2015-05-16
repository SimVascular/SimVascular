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
 * itkCommandIterationUpdate.h
 *
 *  Created on: Nov 16, 2013
 *      Author: jmerkow
 */


#ifndef _itkCommandLevelSetObserver_h_
#define _itkCommandLevelSetObserver_h_

#include "itkCommand.h"


//Basic and IO Includes
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkTileImageFilter.h"
#include "itkCastImageFilter.h"

#include <iostream>
#include <iomanip>
#include <ostream>
#include <fstream>

//#include "MyUtils.h"

#include "itkImageRegionIterator.h"



namespace itk {
template<class TFilter>
class VascularLevelSetObserver : public Command
{
public:
	typedef VascularLevelSetObserver   Self;
	typedef itk::Command             Superclass;
	typedef itk::SmartPointer<Self>  Pointer;
	typedef typename TFilter::InputImageType ImageType;
	typedef typename TFilter::CurrentImageType CurrentImageType;
	typedef typename CurrentImageType::Pointer CurrentImageTypePointer;
	typedef Image<typename CurrentImageType::PixelType,3> OutputImageType;
	typedef unsigned char UCPixelType;
	typedef itk::Image<UCPixelType,2> UCImageType;
	typedef itk::RGBPixel<UCPixelType> RGBPixelType;
	typedef itk::Image<RGBPixelType> RGBImageType;
	typedef itk::TileImageFilter< CurrentImageType, OutputImageType > TilerType;
	typedef itk::CastImageFilter<CurrentImageType,CurrentImageType> CastType;
	//typedef itk::VascularPhaseTwoLevelSetImageFilter<ImageType,ImageType> VascTwoType;
	//typedef typename VascTwoType::VascularPhaseTwoLevelSetFunctionType::InfoGlobalDataStruct InfoGlobalDataStruct;

	typedef itk::FixedArray< unsigned int, 3 > LayoutType;

	typedef typename itk::ImageFileWriter<CurrentImageType> WriterType;
	//typedef typename itk::RescaleIntensityImageFilter<CurrentImageType,CurrentImageType> RecasterType;
	//typedef typename itk::BinaryThresholdImageFilter<ImageType,ImageType> ThresholdFilterType;
	//typedef typename itk::BinaryContourImageFilter<ImageType,ImageType> ContourFilterType;

	itkNewMacro( Self );
	itkTypeMacro(CommandLevelSetObserver,Superclass);

	unsigned int GetCurrentIteration(){
		return m_ImageNumber;
	}

	void SetMod(unsigned int v)
	{m_Mod = v;}
	void ToggleWrite()
	{m_Write = !m_Write;}

	void WriteEnd(){
		std::cout << "ENDed!!" << std::endl;
		std::cout << m_ImageNumber << std::endl;
	}

	typename TilerType::Pointer GetTiler() const
	{m_Tiler->Update();
	return m_Tiler;}

private:
	unsigned int m_Mod;
	std::ofstream oss;
	std::string m_FilenameCBase;
	std::string m_FilenameABase;
	std::string m_FilenameUBase;
	std::string m_FilenameExt;
	typename WriterType::Pointer m_Writer;
	//typename RecasterType::Pointer m_Recaster;
	//typename ThresholdFilterType::Pointer m_Thresholder;
	typename CastType::Pointer m_Cast;
	typename TilerType::Pointer m_Tiler;

	bool m_Write;

	unsigned int m_ImageNumber;
	std::vector<unsigned int> m_Iterations;
	LayoutType m_Layout;






protected:
	VascularLevelSetObserver() {
		m_Layout[0] = 1;
		m_Layout[1] = 1;
		m_Layout[2] = 0;
		m_Mod = 25;
		m_FilenameCBase = "Curvature";
		m_FilenameABase = "Curvature";
		m_FilenameUBase = "Curvature";
		m_FilenameExt = ".tiff";
		m_ImageNumber = 0;
		m_Writer = WriterType::New();
		m_Tiler = TilerType::New();
		m_Tiler->SetLayout(m_Layout);
		m_Cast = CastType::New();
		m_Write = false;
		//oss = std::cout;
	};
	/*


	 */
public:
	std::string GetFilenameCBase(){return m_FilenameCBase;}
	void SetFilenameCBase(std::string value){m_FilenameCBase = value;}

	std::string GetFilenameABase(){return m_FilenameABase;}
	void SetFilenameABase(std::string value){m_FilenameABase = value;}

	std::string GetFilenameUBase(){return m_FilenameUBase;}
	void SetFilenameUBase(std::string value){m_FilenameUBase = value;}

	std::string GetFilenameExt(){return m_FilenameExt;}
	void SetFilenameExt(std::string value){m_FilenameExt = value;}
	int GetMod(){return m_Mod;}
	void GetMod(int value){m_Mod = value;}

	//void GetOSS(std::ofstream value){oss.swap(value);}

	void Execute(itk::Object *caller, const itk::EventObject & event)
	{
		TFilter * filter =
				dynamic_cast<  TFilter * >( caller );
		if( typeid( event ) != typeid( itk::IterationEvent ) )
		{ return; }



		int iterations = (unsigned int)filter->GetElapsedIterations();
		m_Iterations.push_back(iterations);
		//std::cout << iterations << std::endl;


		if(iterations % m_Mod == 0||iterations < 2){


			try{

				SaveWriteImage(filter->GetCurrentCurvatureImage(),this->GetFilenameCBase());
				SaveWriteImage(filter->GetCurrentAdvectionImage(),this->GetFilenameABase());
				//SaveWriteImage(filter->GetCurrentUpdateImage(),this->GetFilenameUBase());
				SaveWriteImage(filter->GetCurrentOutputImage(),this->GetFilenameUBase());
				m_ImageNumber++;

				//filter->ClearCurrentCurvatureImage();

			}
			catch(itk::ExceptionObject& ex)
			{
				std::cerr << ex <<std::endl;
			}
		}


		// std::cout << filter->GetCurrentParameters() << std::endl;
		//		Execute( (const itk::Object *) caller, event);
	}
	void Execute(const itk::Object * object, const itk::EventObject & event)
	{

	}

	static void DeepCopy(const CurrentImageType* input,CurrentImageTypePointer output)
	{

		output->SetRegions(input->GetLargestPossibleRegion());
			output->Allocate();

			itk::ImageRegionConstIterator<CurrentImageType> inputIterator(input, input->GetLargestPossibleRegion());
			itk::ImageRegionIterator<CurrentImageType> outputIterator(output, output->GetLargestPossibleRegion());

			while(!inputIterator.IsAtEnd())
			{
				outputIterator.Set(inputIterator.Get());
				++inputIterator;
				++outputIterator;
			}
	}

private:
	void	SaveWriteImage(const CurrentImageType * image,std::string FilenameBase)
	{
		std::stringstream sstm;
		sstm  << FilenameBase;
		sstm << std::setw(5) << std::setfill('0') << m_ImageNumber ;
		sstm<< m_FilenameExt;


		m_Writer->SetFileName(sstm.str());



		//std::cout << sstm.str() << std::endl;
		///				filter->GetCurrentCurvatureImage()->UpdateLargestPossibleRegion();
		//inputImageTile = ;
		typename CurrentImageType::Pointer inputImageTile =
				CurrentImageType::New();

		this->DeepCopy(image,inputImageTile);
		inputImageTile->DisconnectPipeline();
		m_Writer->SetInput(inputImageTile);

		m_Tiler->SetInput(m_ImageNumber,image);
		//m_Tiler->Update();

		if(m_Write)
			m_Writer->Update();
	}


};


};

#endif
