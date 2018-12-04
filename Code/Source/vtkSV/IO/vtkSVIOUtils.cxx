/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 */

#include "vtkSVIOUtils.h"

#include "vtkDataArray.h"
#include "vtkMetaImageWriter.h"
#include "vtkObjectFactory.h"
#include "vtkSmartPointer.h"
#include "vtkSTLReader.h"
#include "vtkSVGlobals.h"
#include "vtkSVPolyDataRawReader.h"
#include "vtkSVRawWriter.h"
#include "vtkSVUnstructuredGridRawReader.h"
#include "vtkUnstructuredGrid.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkXMLStructuredGridWriter.h"
#include "vtkXMLUnstructuredGridReader.h"
#include "vtkXMLUnstructuredGridWriter.h"

#include <sys/types.h>
#include <sys/stat.h>

struct stat info;
// ----------------------
// CheckDirectoryExists
// ----------------------
int vtkSVIOUtils::CheckDirectoryExists(std::string dirname)
{
  if (dirname.empty() || dirname == "" || dirname == "/0")
    return SV_OK;
  if (stat(dirname.c_str(), &info) == 0)
    return SV_OK;

  return SV_ERROR;
}

// ----------------------
// CheckFileExists
// ----------------------
int vtkSVIOUtils::CheckFileExists(std::string filename)
{
  if (stat(filename.c_str(), &info) == 0)
    return SV_OK;

  fprintf(stderr,"File %s does not exist\n", filename.c_str());
  return SV_ERROR;
}

// ----------------------
// IntToString
// ----------------------
/** \details Function to turn an integer into a string. */
std::string vtkSVIOUtils::IntToString(int i)
{
  std::stringstream out;
  out << i;
  return out.str();
}

// ----------------------
// GetPath
// ----------------------
/** \details Function to get the directory from the input File Name
 * For example, /User/Adam.stl returns /User */
std::string vtkSVIOUtils::GetPath(std::string fullName)
{
  std::string pathName;
  int split = fullName.find_last_of("/\\");
  if (split < 0)
    pathName = ".";
  else
    pathName = fullName.substr(0,split);
  return pathName;
}

// ----------------------
// GetRawName
// ----------------------
/** \details Function to get the raw file name from the input File name.
 * For example, Adam.stl returns Adam  */
std::string vtkSVIOUtils::GetRawName(std::string fullName)
{
  std::string rawName;
  unsigned split = fullName.find_last_of("/\\");
  rawName = fullName.substr(split+1);
  rawName.erase(rawName.find_last_of("."),std::string::npos);
  return rawName;
}

// ----------------------
// GetExt
// ----------------------
/** \details Function to get extension from an input string. */
std::string vtkSVIOUtils::GetExt(std::string fullName)
{
  std::string extName;
  unsigned split = fullName.find_last_of(".");
  extName = fullName.substr(split+1);
  return extName;
}

// ----------------------
// ReadSTLFile
// ----------------------
/** Function to read in the STL file, extract the boundaries and pass the input
 * Poly Data information */
int vtkSVIOUtils::ReadSTLFile(std::string inputFilename, vtkPolyData *polydata)
{
  // Check file exists
  if (vtkSVIOUtils::CheckFileExists(inputFilename) != SV_OK)
    return SV_ERROR;

  //Create an STL reader for reading the file
  vtkNew(vtkSTLReader, reader);
  reader->SetFileName(inputFilename.c_str());
  reader->Update();

  //Save the output information from the boundary filter to a Poly Data
  //structure
  polydata->DeepCopy(reader->GetOutput());
  polydata->BuildLinks();

  return SV_OK;
}

// ----------------------
// ReadVTPFile
// ----------------------
int vtkSVIOUtils::ReadVTPFile(std::string inputFilename, vtkPolyData *polydata)
{
  // Check file exists
  if (vtkSVIOUtils::CheckFileExists(inputFilename) != SV_OK)
    return SV_ERROR;

  //Create an STL reader for reading the file
  vtkNew(vtkXMLPolyDataReader, reader);
  reader->SetFileName(inputFilename.c_str());
  reader->Update();

  //Save the output information from the boundary filter to a Poly Data
  //structure
  polydata->DeepCopy(reader->GetOutput());
  polydata->BuildLinks();

  return SV_OK;
}

// ----------------------
// ReadVTUFile
// ----------------------
int vtkSVIOUtils::ReadVTUFile(std::string inputFilename, vtkUnstructuredGrid *grid)
{
  // Check file exists
  if (vtkSVIOUtils::CheckFileExists(inputFilename) != SV_OK)
    return SV_ERROR;

  //Create an STL reader for reading the file
  vtkNew(vtkXMLUnstructuredGridReader, reader);
  reader->SetFileName(inputFilename.c_str());
  reader->Update();

  //Save the output information from the boundary filter to a Poly Data
  //structure
  grid->DeepCopy(reader->GetOutput());
  grid->BuildLinks();

  return SV_OK;
}

// ----------------------
// ReadPolyDataRawFile
// ----------------------
int vtkSVIOUtils::ReadPolyDataRawFile(std::string inputFilename, vtkPolyData *polydata)
{
  // Check file exists
  if (vtkSVIOUtils::CheckFileExists(inputFilename) != SV_OK)
    return SV_ERROR;

  //Create a raw reader for reading the file
  vtkNew(vtkSVPolyDataRawReader, reader);
  reader->SetFileName(inputFilename.c_str());
  reader->Update();

  //Save the output information from the boundary filter to a Poly Data
  //structure
  polydata->DeepCopy(reader->GetOutput());
  polydata->BuildLinks();

  return SV_OK;
}

// ----------------------
// ReadUnstructuredGridRawFile
// ----------------------
int vtkSVIOUtils::ReadUnstructuredGridRawFile(std::string inputFilename, vtkUnstructuredGrid *unstructuredgrid)
{
  // Check file exists
  if (vtkSVIOUtils::CheckFileExists(inputFilename) != SV_OK)
    return SV_ERROR;

  //Create a raw reader for reading the file
  vtkNew(vtkSVUnstructuredGridRawReader, reader);
  reader->SetFileName(inputFilename.c_str());
  reader->Update();

  //Save the output information from the boundary filter to a Poly Data
  //structure
  unstructuredgrid->DeepCopy(reader->GetOutput());
  unstructuredgrid->BuildLinks();

  return SV_OK;
}

// ----------------------
// ReadInputFile
// ----------------------
int vtkSVIOUtils::ReadInputFile(std::string inputFilename, vtkPolyData *polydata)
{
  // Get the extension of the file
  std::string ext = vtkSVIOUtils::GetExt(inputFilename);

  // If it is an stl, read
  if(!strncmp(ext.c_str(),"stl",3))
  {
    if (vtkSVIOUtils::ReadSTLFile(inputFilename, polydata) != SV_OK)
      return SV_ERROR;
  }
  // If it is a vtp, read
  else if(!strncmp(ext.c_str(),"vtp",3))
  {
    if (vtkSVIOUtils::ReadVTPFile(inputFilename, polydata) != SV_OK)
      return SV_ERROR;
  }
  // Other file types are not supported
  else
  {
    std::cout<<"Unrecognized file extension, stl and vtp accepted"<<endl;
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// WriteVTPFile
// ----------------------
int vtkSVIOUtils::WriteVTPFile(std::string outputFilename,vtkPolyData *writePolyData)
{
  // Get directory
  std::string dirName = vtkSVIOUtils::GetPath(outputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(dirName) != SV_OK)
    return SV_ERROR;

  vtkNew(vtkXMLPolyDataWriter, writer);
  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(writePolyData);
#else
  writer->SetInputData(writePolyData);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteVTPFile
// ----------------------
/** \details In this version, the inputFilename is used to get the path and raw name.
 *  The attachName is then attached to the end of the inputFilename
 *  for the ouput filename. */
int vtkSVIOUtils::WriteVTPFile(std::string inputFilename,vtkPolyData *writePolyData,std::string attachName)
{
  std::string rawName, pathName, outputFilename;

  vtkNew(vtkXMLPolyDataWriter, writer);

  pathName = vtkSVIOUtils::GetPath(inputFilename);
  rawName = vtkSVIOUtils::GetRawName(inputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(pathName) != SV_OK)
    return SV_ERROR;

  outputFilename = pathName+"/"+rawName+attachName+".vtp";

  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(writePolyData);
#else
  writer->SetInputData(writePolyData);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteVTUFile
// ----------------------
int vtkSVIOUtils::WriteVTUFile(std::string outputFilename,vtkUnstructuredGrid *writeUnstructuredGrid)
{
  // Get directory
  std::string dirName = vtkSVIOUtils::GetPath(outputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(dirName) != SV_OK)
    return SV_ERROR;

  vtkNew(vtkXMLUnstructuredGridWriter, writer);
  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(writeUnstructuredGrid);
#else
  writer->SetInputData(writeUnstructuredGrid);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteVTUFile
// ----------------------
/** \details In this version, the inputFilename is used to get the path and raw name.
 *  The attachName is then attached to the end of the inputFilename
 *  for the ouput filename. */
int vtkSVIOUtils::WriteVTUFile(std::string inputFilename,vtkUnstructuredGrid *writeUnstructuredGrid,std::string attachName)
{
  std::string rawName, pathName, outputFilename;

  vtkNew(vtkXMLUnstructuredGridWriter, writer);

  pathName = vtkSVIOUtils::GetPath(inputFilename);
  rawName = vtkSVIOUtils::GetRawName(inputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(pathName) != SV_OK)
    return SV_ERROR;

  outputFilename = pathName+"/"+rawName+attachName+".vtu";

  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(writeUnstructuredGrid);
#else
  writer->SetInputData(writeUnstructuredGrid);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteVTSFile
// ----------------------
int vtkSVIOUtils::WriteVTSFile(std::string outputFilename,vtkStructuredGrid *writeStructuredGrid)
{
  // Get directory
  std::string dirName = vtkSVIOUtils::GetPath(outputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(dirName) != SV_OK)
    return SV_ERROR;

  vtkNew(vtkXMLStructuredGridWriter, writer);
  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(writeStructuredGrid);
#else
  writer->SetInputData(writeStructuredGrid);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteVTSFile
// ----------------------
/** \details In this version, the inputFilename is used to get the path and raw name.
 *  The attachName is then attached to the end of the inputFilename
 *  for the ouput filename. */
int vtkSVIOUtils::WriteVTSFile(std::string inputFilename,vtkStructuredGrid *writeStructuredGrid,std::string attachName)
{
  std::string rawName, pathName, outputFilename;

  vtkNew(vtkXMLStructuredGridWriter, writer);

  pathName = vtkSVIOUtils::GetPath(inputFilename);
  rawName = vtkSVIOUtils::GetRawName(inputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(pathName) != SV_OK)
    return SV_ERROR;

  outputFilename = pathName+"/"+rawName+attachName+".vts";

  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(writeStructuredGrid);
#else
  writer->SetInputData(writeStructuredGrid);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteMHDFile
// ----------------------
int vtkSVIOUtils::WriteMHDFile(std::string outputFilename,vtkImageData *image)
{
  // Get directory
  std::string dirName = vtkSVIOUtils::GetPath(outputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(dirName) != SV_OK)
    return SV_ERROR;

  vtkNew(vtkMetaImageWriter, writer);
  writer->SetFileName(outputFilename.c_str());
#if VTK_MAJOR_VERSION <= 5
  writer->SetInput(image);
#else
  writer->SetInputData(image);
#endif

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteRawFile
// ----------------------
int vtkSVIOUtils::WriteRawFile(std::string outputFilename,vtkPolyData *writePolyData)
{
  // Get directory
  std::string dirName = vtkSVIOUtils::GetPath(outputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(dirName) != SV_OK)
    return SV_ERROR;

  vtkNew(vtkSVRawWriter, writer);
  writer->SetFileName(outputFilename.c_str());
  writer->SetInputData(writePolyData);

  writer->Write();
  return SV_OK;
}

// ----------------------
// WriteRawFile
// ----------------------
/** \details In this version, the inputFilename is used to get the path and raw name.
 *  The attachName is then attached to the end of the inputFilename
 *  for the ouput filename. */
int vtkSVIOUtils::WriteRawFile(std::string inputFilename,vtkPolyData *writePolyData,std::string attachName)
{
  std::string rawName, pathName, outputFilename;

  vtkNew(vtkSVRawWriter, writer);

  pathName = vtkSVIOUtils::GetPath(inputFilename);
  rawName = vtkSVIOUtils::GetRawName(inputFilename);

  // Check directory exists
  if (vtkSVIOUtils::CheckDirectoryExists(pathName) != SV_OK)
    return SV_ERROR;

  outputFilename = pathName+"/"+rawName+attachName+".vtp";

  writer->SetFileName(outputFilename.c_str());
  writer->SetInputData(writePolyData);

  writer->Write();
  return SV_OK;
}
