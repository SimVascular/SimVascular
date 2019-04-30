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

#include <Python.h>

#include <map>
#include "sv4gui_PurkinjeNetworkModel.h"
#include <mitkLogMacros.h>

#include <vtkXMLPolyDataWriter.h>

//-------------
// Constructor
//-------------
sv4guiPurkinjeNetworkModel::sv4guiPurkinjeNetworkModel(const std::string name, const std::array<double,3>& firstPoint,
    const std::array<double,3>& secondPoint) : name(name), firstPoint(firstPoint), secondPoint(secondPoint)
{

}

//------------
// Destructor
//------------
sv4guiPurkinjeNetworkModel::~sv4guiPurkinjeNetworkModel()
{
}

//---------------
// SetParameters
//---------------
// Set the Purkinje network generation parameters.
//
// The parameters are read frome the gui and passed in as map.
// The map of parameters are checked and then copied into the
// 'parameterValues' data member.

void sv4guiPurkinjeNetworkModel::SetParameters(std::map<std::string, std::string>& params)
{
  auto msgPrefix = "[sv4guiPurkinjeNetworkModel::SetParameters] ";
  MITK_INFO << msgPrefix;

  std::map<std::string, std::string>::iterator it;
  for (it = params.begin(); it != params.end(); ++it) {
    MITK_INFO << '\t' << it->first << '\t' << it->second;
    if (parameterNames.allNames.find(it->first) == parameterNames.allNames.end()) {
      MITK_ERROR << "Unknown parameter name " << it->first;
      return;
    }
  }

  parameterValues = params;
}

//-----------------
// GenerateNetwork
//-----------------
// Generate a Purkinje network.

bool sv4guiPurkinjeNetworkModel::GenerateNetwork(const std::string outputPath)
{
  std::string msgPrefix = "[sv4guiPurkinjeNetworkModel::GenerateNetwork] ";
  MITK_INFO << msgPrefix << "Output path " << outputPath;

  // Write the surface mesh to a .vtp file.
  auto meshFileName = outputPath + "/" + this->name + ".vtp";
  WriteMesh(meshFileName);
  MITK_INFO << msgPrefix << "Input surface mesh file " << meshFileName;
 
  // Set output file prefix.
  auto outfile = outputPath + "/" + this->name;
  MITK_INFO << msgPrefix << "Output network file " << outfile;

  // Execute the Python command used to generate the Purkinje network. 
  auto cmd = CreateCommand(meshFileName, outfile);
  MITK_INFO << msgPrefix << "Execute cmd " << cmd;
  PyRun_SimpleString(cmd.c_str());
  MITK_INFO << msgPrefix << "Done!";

  // Set the name of the file containing the network of 1D elements.
  this->networkFileName = outputPath + "/" + this->name + ".vtu";
}

//---------------
// CreateCommand
//---------------
// Create the Python command used to generate a Purkinje network.
//
// The command will import the fractal_tree module and execute the 
// fractal_tree.run() function.
//
// The parmeters used by the Python code are stored as a map in 
// the sv4guiPurkinjeNetworkModel data member 'parameterValues'.

std::string sv4guiPurkinjeNetworkModel::CreateCommand(const std::string infile, const std::string outfile)
{
  auto msgPrefix = "[sv4guiPurkinjeNetworkModel::CreateCommand] ";

  // Get the values of the main parameters used by the fractal tree code.
  //
  auto avg_branch_length = parameterValues[parameterNames.AvgBranchLength];
  auto branch_angle = parameterValues[parameterNames.BranchAngle];
  auto branch_seg_length = parameterValues[parameterNames.BranchSegLength];
  auto init_node = parameterValues[parameterNames.FirstPoint];
  auto num_branch_gen = parameterValues[parameterNames.NumBranchGenerations];
  auto repulsive_parameter = parameterValues[parameterNames.RepulsiveParameter];
  auto second_node = parameterValues[parameterNames.SecondPoint];

  // Create the command to generate the network.
  //
  std::string cmd;
  cmd += "import fractal_tree\n";
  cmd += "fractal_tree.run(";
  // Required parameters.
  cmd += "infile='" + infile + "',";
  cmd += "outfile='" + outfile + "',";
  cmd += "init_node='[" + init_node + "]',";
  cmd += "second_node='[" + second_node +  "]',";
  // Optional parameters.
  cmd += "avg_branch_length='" + avg_branch_length +  "',";
  cmd += "branch_angle='" + branch_angle + "',";
  cmd += "branch_seg_length='" + branch_seg_length + "',";
  cmd += "num_branch_gen='" + num_branch_gen + "',";
  cmd += "repulsive_parameter='" + repulsive_parameter + "'";
  cmd += ")\n"; 

  return cmd;
}

//-----------
// WriteMesh 
//-----------
// Write the surface mesh on which to generate the network to a VTK .vtp
// file. This file is read in by the Python fractal tree code.

bool sv4guiPurkinjeNetworkModel::WriteMesh(const std::string fileName)
{
  vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetFileName(fileName.c_str());
  writer->SetInputData(this->meshPolyData);
  writer->Write();
}

//-----------------
// WriteParameters
//-----------------
// Write the parameters used to generate a Purkinje network to a text file.

bool sv4guiPurkinjeNetworkModel::WriteParameters(const std::string fileName, 
    std::map<std::string, std::string>& params)
{

}
