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

#include "cvFlowsolverOptions.h"

#if(VER_USE_VTK == 1)

#include <vtkDoubleArray.h>
#include <vtkPoints.h>
#include <vtkPolyData.h>
#include <vtkPointData.h>
#include <vtkSmartPointer.h>
#include <vtkXMLPolyDataReader.h>

#include <iostream>
#include <algorithm>

#ifdef SV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
#define getnumfrombctvtp_ GETNUMFROMBCTVTP
#define getdatafrombctvtp_ GETDATAFROMBCTVTP
#endif

#ifdef __cplusplus
extern "C" {
#endif

void getnumfrombctvtp_(const char* filename, int* ntv, int* nptsmax);
void getdatafrombctvtp_(const char* filename, double x1[],double x2[],double x3[],double v1[],double v2[],double v3[],double ts[],int global_node_id[]);

#ifdef __cplusplus
}
#endif


using namespace std;

void getnumfrombctvtp_(const char* filename, int* ntv, int* nptsmax){

	  vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
	  reader->SetFileName(filename);
	  reader->Update();
	  vtkSmartPointer<vtkPolyData> polydata =  reader->GetOutput();

	  *ntv = polydata->GetNumberOfPoints();
	  *nptsmax=polydata->GetPointData()->GetNumberOfArrays()-1;//excluding the array of GlobalNodeID

}

void getdatafrombctvtp_(const char* filename, double x1[],double x2[],double x3[],double v1[],double v2[],double v3[],double ts[],int global_node_id[]){

	  vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
	  reader->SetFileName(filename);
	  reader->Update();

	  vtkSmartPointer<vtkPolyData> polydata =  reader->GetOutput();

	  // Get the number of points in the polydata
	  vtkIdType ntv = polydata->GetNumberOfPoints();
	  vtkIdType numArrays = polydata->GetPointData()->GetNumberOfArrays();
	  vtkIdType nptsmax =numArrays-1;

	  for(int i=0;i<ntv;i++){
		  double p[3];
		  polydata->GetPoint(i,p);
		  x1[i]=p[0];
		  x2[i]=p[1];
		  x3[i]=p[2];
	  }

	  //string velArrayNames[numArrays-1];
	  string *velArrayNames;
	  velArrayNames = new string[numArrays-1];
	  string nodeIDName="";

	  int m=0;
	  for(int j=0;j<numArrays;j++){
		  string name=polydata->GetPointData()->GetArrayName(j);
		  string str2="velocity";
		  size_t found=name.find(str2);
		  if (found!=string::npos){
			  velArrayNames[m]=name;
			  m++;
		  }
		  str2="GlobalNodeID";
		  found=name.find(str2);
		  if (found!=string::npos){
			  nodeIDName=name;
		  }

	  }

	  if(m==0){
		  fprintf(stderr,"ERROR:  velocity not found in %s.\n", filename);
		  exit(-1);
		  return;
	  }
//	  if(nodeIDName==""){
//		  fprintf(stderr,"ERROR:  Global Node ID not found in %s.\n", filename);
//		  exit(-1);
//		  return;
//	  }

	  //int z = sizeof(velArrayNames)/sizeof(velArrayNames[0]); //Get the array size
	  //sort(velArrayNames,velArrayNames+z);
	  sort(velArrayNames,velArrayNames+numArrays-1);

	  for(int j=0;j<nptsmax;j++){

		  size_t pos = velArrayNames[j].find("_");
		  string tstr = velArrayNames[j].substr (pos+1);
		  ts[j]= atof (tstr.c_str());

		  vtkSmartPointer<vtkDoubleArray> array =
		    vtkDoubleArray::SafeDownCast(polydata->GetPointData()->GetArray(velArrayNames[j].c_str()));

		  if(array){
			  for(int i=0;i<ntv;i++){
			      double vel[3];
			      array->GetTupleValue(i, vel);

			      v1[i*nptsmax+j]=vel[0];
			      v2[i*nptsmax+j]=vel[1];
			      v3[i*nptsmax+j]=vel[2];
		  	  }
	  	  }

	  }

	  delete[] velArrayNames;

	  if(nodeIDName!=""){
		  vtkSmartPointer<vtkIntArray> nodeIDArray =
				  vtkIntArray::SafeDownCast(polydata->GetPointData()->GetArray(nodeIDName.c_str()));

		  if(nodeIDArray){
			  for(int i=0;i<ntv;i++){
				  global_node_id[i]=nodeIDArray->GetValue(i);
			  }
		  }
	  }
}
#endif
