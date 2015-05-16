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
#include "MyUtils.h"

#ifndef WIN32
using std::isdigit;
#endif

namespace MyUt
{

void getParams(float params[6],const char* filename)
{
	std::string line;
	std::ifstream infile(filename);
	std::getline(infile, line);

	std::cout << line << std::endl;

	if (std::getline(infile, line))
	{
		std::istringstream iss(line);
		std::string value;
		int i = 0;
		while (std::getline(iss, value,','))
		{
			params[i] = atof(value.c_str());
			i++;
		}
	}

	for(int j = 0;j < 6; j++){
		std::cout << params[j] << "\t\t";
	}
	std::cout << std::endl;
}

std::vector<std::vector<float> > getParams(const char* filename){
	typedef std::vector<std::vector<float> > Vector;
	std::vector<std::vector<float> > params;

	std::string line1;
	std::ifstream infile(filename);

	std::getline(infile, line1);

	int j = 0;
	int i = 0;
	std::vector<float> param;
	std::string line;

	while (std::getline(infile, line))
	{
		param = split <float> (line, ',');
		params.push_back(param);
	}

	std::cout << "Number Configurations Read: " << params.size() << std::endl;
	std::cout << "Each Has " << params[0].size() <<  " Params"<<std::endl;
	std::cout <<line1 <<std::endl;
	for(int jj = 0;jj < params.size(); jj++){
		std::cout << jj << ": ";
		for(int ii = 0;ii < params[jj].size(); ii++){
			std::cout << params[jj][ii] << "\t\t";
		}
		std::cout << std::endl;
	}

	return params;}

std::vector<std::vector<float> > getSeeds(const char* filename){
	typedef std::vector<std::vector<float> > Vector;
	const int seedDim = 3;
	Vector seeds;
	float seedss[3][3];

	std::string line1;
	std::ifstream infile(filename);

	std::getline(infile, line1);

	int j = 0;
	int i = 0;
	std::vector<float> seed;
	std::string line;
	while (std::getline(infile, line))
	{
		seed.resize(seedDim);
		std::istringstream is(line);

		std::string value;
		i = 0;
		while (std::getline(is, value,',') && i < seedDim)
		{
			seed[i] = atof(value.c_str());
			i++;
		}
		seeds.push_back(seed);
		seed.clear();
		j++;
	}

	std::cout << "Number Seeds Read: " << seeds.size() << std::endl;
	std::cout << "Seed Dimension: " << seeds[0].size() << std::endl;
	std::cout <<line1 <<std::endl;
	for(int jj = 0;jj < seeds.size(); jj++){
		std::cout << jj << ": " ;
		for(int ii = 0;ii < seeds[jj].size(); ii++){
			std::cout << seeds[jj][ii] << "\t";
		}
		std::cout << std::endl;
	}

	return seeds;}

std::vector<std::vector<float> > parsePathsFile(const char* filename){
	typedef std::vector<std::vector<float> > Vector;
	Vector points;
	const unsigned int MAX = 50;
	Vector out;
	out.resize(MAX);
	std::string line;
	std::ifstream infile(filename);

	int j = 0;
	while (std::getline(infile, line))
	{

		std::istringstream is(line);

		std::string value;
		int id, idx;

		std::vector<float> pts;


		if(std::getline(is, value,','))
		{
			//std::cout << value << std::endl;
			id = atoi(value.c_str());

		}

		if(std::getline(is, value,':'))
		{
			if(isNumber(value.c_str())){
				idx = atoi(value.c_str());
			}
			else{
				std::cout << value << "skipping.." << std::endl;
				idx = -1;
			}
		}

		std::vector<float> p;
		p.resize(3);
		if(std::getline(is, value))
		{
			pts = split <float> (value, ' ');
			if(idx<MAX && idx >= 0){
				//std::cout << idx << " : ";
				//std::cout << value << std::endl;
				//dim2 is z
				out[idx].push_back(pts[1]);
				out[idx].push_back(pts[3]);
				out[idx].push_back(pts[2]);


				j++;
			}
		}





	}

	//std::cout << j << std::endl;
	return out;}

bool isNumber(const char* s)
{
	int i = 0,  flag;

	while(s[i]){
		//if there is a letter in a string then string is not a number
		if(isalpha(s[i])){
			flag = 0;
			break;
		}
		else flag = 1;
		i++;
	}


	if (flag == 1) return true;
	else return false;


}

bool is_number(const std::string& s)
{
	std::string::const_iterator it = s.begin();
	while (it != s.end() && isdigit(*it)) ++it;
	return !s.empty() && it == s.end();
}

void DeepCopy(Double2dITKImageType::Pointer input,Double2dITKImageType::Pointer output)
{
	output->SetRegions(input->GetLargestPossibleRegion());
	output->Allocate();

	itk::ImageRegionConstIterator<Double2dITKImageType> inputIterator(input, input->GetLargestPossibleRegion());
	itk::ImageRegionIterator<Double2dITKImageType> outputIterator(output, output->GetLargestPossibleRegion());

	while(!inputIterator.IsAtEnd())
	{
		outputIterator.Set(inputIterator.Get());
		++inputIterator;
		++outputIterator;
	}
}



void CopyVTKtoITK(vtkStructuredPoints* in,Double2dITKImageType* out)
{
	typedef itk::VTKImageImport<Double2dITKImageType> itkImportType;
	itkImportType::Pointer itkImporter = itkImportType::New();
	vtkSmartPointer<vtkImageExport> vtkExporter = vtkSmartPointer<vtkImageExport>::New();
	ConnectVTKToITK(vtkExporter.GetPointer(),itkImporter.GetPointer());

	vtkExporter->SetInputData(in);
	vtkExporter->Update();
	itkImporter->Update();
	DeepCopy(itkImporter->GetOutput(),out);
}

void CopyITKtoVTK(const Double2dITKImageType* in,vtkStructuredPoints* out)
{

	typedef itk::VTKImageExport<Double2dITKImageType> itkExportType;
	itkExportType::Pointer itkExporter = itkExportType::New();
	vtkSmartPointer<vtkImageImport> vtkImporter = vtkSmartPointer<vtkImageImport>::New();
	ConnectITKToVTK(itkExporter.GetPointer(),vtkImporter.GetPointer());

	itkExporter->SetInput(in);
	itkExporter->Update();
	vtkImporter->Update();

	out->DeepCopy(vtkImporter->GetOutput());

}


void vtkSetupResliceImage(vtkSmartPointer<vtkImageReslice> reslice,
		vtkSmartPointer<vtkPoints> path,
		int ii){
	std::cout << "Point 1: ";
	double pti[3], pti1[3];
	double axes[16];

	std::cout << "Point 1: ";
	std::cout << "Point 1: " << ii << " : " ;

	path->GetPoint(ii,pti);
	MyUt::printArray(pti,3);

	std::cout << "Point 2: " << ii+1 << " : " ;
	path->GetPoint(ii+1,pti1);
	MyUt::printArray(pti1,3);

	MakeResliceAxesFromPoints(pti,pti1,axes);
	cvVTKNewMacro(vtkMatrix4x4,resliceAxes);
	resliceAxes->DeepCopy(axes);

	reslice->SetResliceAxes(resliceAxes);
	reslice->SetOutputDimensionality(2);
	reslice->SetInterpolationModeToCubic();
	reslice->SetOutputSpacingToDefault();
	reslice->SetOutputOriginToDefault();

}






void vtkResliceImage(vtkSmartPointer<vtkImageImport> importer,
		vtkSmartPointer<vtkImageReslice> reslice,
		double axes[16]){

	int extent[6];
	double spacing[3];
	double origin[3];

	importer->GetDataExtent(extent);
	importer->GetDataSpacing(spacing);
	importer->GetDataOrigin(origin);

	vtkSmartPointer<vtkMatrix4x4> resliceAxes =
			vtkSmartPointer<vtkMatrix4x4>::New();
	resliceAxes->DeepCopy(axes);

	//Connect Pipeline
	reslice->SetInputConnection(importer->GetOutputPort());
	reslice->SetOutputDimensionality(2);
	reslice->SetResliceAxes(resliceAxes);
	reslice->SetInterpolationModeToLinear();


}

void vtkVecToPoints(vtkSmartPointer<vtkPoints> points, std::vector<std::vector<float> > pts){

	std::vector<float> row;

	for(std::vector<float>::size_type i = 0; i != pts.size(); i++){
		row = pts[i];
		points->InsertNextPoint(row[0],row[1],row[2]);

	}

}



std::vector<std::vector<float> > vtkPointsToVec(vtkSmartPointer<vtkPoints> points){
	std::vector<std::vector<float> > pts;
	int numberOfInputPoints = points->GetNumberOfPoints();

	pts.resize(numberOfInputPoints);

	double x[3];
	std::vector<float> v;
	v.resize(3);
	for(int i  = 0;i < numberOfInputPoints;i++){
		points->GetPoint(i,x);
		v[0] = x[0];
		v[1] = x[1];
		v[2] = x[2];
		pts[i] = v;

	}

	return pts;
}

vtkSmartPointer<vtkPoints> vtkInterpolatePath(vtkSmartPointer<vtkPoints> points,int numberOfOutputPoints){

	vtkSmartPointer<vtkPoints> path = vtkSmartPointer<vtkPoints>::New();

	vtkInterpolatePath(points,path, numberOfOutputPoints);

	return path;

}

void vtkInterpolatePath(vtkSmartPointer<vtkPoints> points,vtkSmartPointer<vtkPoints> path,int numberOfOutputPoints){
	vtkSmartPointer<vtkCardinalSpline> splineX =
			vtkSmartPointer<vtkCardinalSpline>::New();
	vtkSmartPointer<vtkCardinalSpline> splineY =
			vtkSmartPointer<vtkCardinalSpline>::New();
	vtkSmartPointer<vtkCardinalSpline> splineZ =
			vtkSmartPointer<vtkCardinalSpline>::New();

	splineX->ClosedOff();
	splineY->ClosedOff();
	splineZ->ClosedOff();

	int numberOfInputPoints = points->GetNumberOfPoints();

	//Adding points to splines
	double x[3];
	for(int i  = 0;i < numberOfInputPoints;i++){
		points->GetPoint(i,x);

		splineX->AddPoint(i,x[0]);
		splineY->AddPoint(i,x[1]);
		splineZ->AddPoint(i,x[2]);
	}

	//std::cout << numberOfOutputPoints << std::endl;
	for(int i = 0;i<numberOfOutputPoints;i++){
		float t = ((numberOfInputPoints-1.0)/(numberOfOutputPoints -1.0))*i;
		float x = splineX->Evaluate(t);
		float y = splineY->Evaluate(t);
		float z = splineZ->Evaluate(t);
		//std::cout << x << " " << y << " " << z << std::endl;
		path->InsertPoint(t,x,y,z);
	}

	//std::cout <<std::endl;


}


void RenderPathSetup(vtkSmartPointer<vtkPoints> points,vtkSmartPointer<vtkPoints> path,
		vtkSmartPointer<vtkActor> profile, vtkSmartPointer<vtkActor> glyph,int ball[],float tubearg){

	int numberOfOutputPoints = points->GetNumberOfPoints();
	int numberOfInputPoints = path->GetNumberOfPoints();
	vtkSmartPointer<vtkSphereSource> balls = vtkSmartPointer<vtkSphereSource>::New();
	vtkSmartPointer<vtkPolyData> inputData = vtkSmartPointer<vtkPolyData>::New();
	vtkSmartPointer<vtkGlyph3D> glyphData = vtkSmartPointer<vtkGlyph3D>::New();
	vtkSmartPointer<vtkPolyDataMapper> glyphMapper = vtkSmartPointer<vtkPolyDataMapper>::New();


	//setup balls
	balls->SetRadius(ball[0]);
	balls->SetPhiResolution(ball[1]);
	balls->SetThetaResolution(ball[2]);

	inputData->SetPoints(points);
	glyphData->SetInputData(inputData);
	glyphData->SetSourceConnection(balls->GetOutputPort());

	glyphMapper->SetInputConnection(glyphData->GetOutputPort());


	glyph->SetMapper(glyphMapper);
	glyph->GetProperty()->SetDiffuseColor(1.0, 0.3882, 0.2784);
	glyph->GetProperty()->SetSpecular(.3);
	glyph->GetProperty()->SetSpecularPower(30);


	vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();
	lines->InsertNextCell(numberOfOutputPoints);
	for(int i = 0;i < numberOfOutputPoints;i++)
	{
		lines->InsertCellPoint(i);
	}

	vtkSmartPointer<vtkPolyData> profileData = vtkSmartPointer<vtkPolyData>::New();

	profileData->SetPoints(points);
	profileData->SetLines(lines);


	vtkSmartPointer<vtkTubeFilter> profileTubes = vtkSmartPointer<vtkTubeFilter>::New();
	profileTubes->SetNumberOfSides(8);
	profileTubes->SetInputData(profileData);
	profileTubes->SetRadius(tubearg);

	vtkSmartPointer<vtkPolyDataMapper> profileMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
	profileMapper->SetInputConnection(profileTubes->GetOutputPort());

	profile->SetMapper(profileMapper);
	profile->GetProperty()->SetDiffuseColor(0.89, 0.81, 0.34);
	profile->GetProperty()->SetSpecular(.3);
	profile->GetProperty()->SetSpecularPower(30);



}

void QuickRenderPolyData(vtkPolyData* front, float radius)
{
	cvVTKNewMacro(vtkRenderWindow,renderWindow);
	cvVTKNewMacro(vtkRenderWindowInteractor,renderWindowInteractor);
	cvVTKNewMacro(vtkRenderer,renderer)
	renderWindowInteractor->SetRenderWindow(renderWindow);
	renderWindow->AddRenderer(renderer);

	vtkSmartPointer<vtkTubeFilter> profileTubes = vtkSmartPointer<vtkTubeFilter>::New();
	profileTubes->SetNumberOfSides(2);
	profileTubes->SetInputData(front);
	profileTubes->SetRadius(radius);

	cvVTKNewMacro(vtkPolyDataMapper,profileMapper);
	cvVTKNewMacro(vtkActor,profile);
	profileMapper->SetInputConnection(profileTubes->GetOutputPort());
	profile->SetMapper(profileMapper);
	profile->GetProperty()->SetDiffuseColor(0.89, 0.81, 0.34);
	profile->GetProperty()->SetSpecular(.3);
	profile->GetProperty()->SetSpecularPower(30);
	renderer->AddActor(profile);

	renderWindowInteractor->Initialize();
	renderWindowInteractor->Start();
}






} //namespace
