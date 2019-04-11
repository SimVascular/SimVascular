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
#include "sv4gui_SimulationPython1d.h"
#include <mitkLogMacros.h>
#include <QMessageBox>

#include <vtkXMLPolyDataWriter.h>

//-------------
// Constructor
//-------------
sv4guiSimulationPython1d::sv4guiSimulationPython1d()
{

}

//------------
// Destructor
//------------
sv4guiSimulationPython1d::~sv4guiSimulationPython1d()
{
}

//--------------
// GenerateMesh   
//--------------
// Generate a 1D mesh. 
//
// Script arguments: 
//
//     output-directory: The output directory to write the mesh file.
//     centerlines-input-file: The input centerlines geometry (.vtp format). 
//     compute-mesh: Switch to enable computing just the mesh.
//     write-mesh-file: Switch to enable writing a mesh file.
//     mesh-output-file: The name of the mesh file to write (.vtp format).
//
bool sv4guiSimulationPython1d::GenerateMesh(const std::string& outputDir, const std::string& centerlinesFile,
                                            const std::string& meshFile) 
{
  std::string msg = "[sv4guiSimulationPython1d::GenerateMesh] ";
  MITK_INFO << msg << "---------- GenerateMesh ----------";
  sv4guiSimulationPython1dParamNames paramNames;

  // Create the script command.
  auto last = true;
  auto cmd = StartCommand();
  cmd += AddArgument(paramNames.OUTPUT_DIRECTORY, outputDir);
  cmd += AddArgument(paramNames.CENTERLINES_INPUT_FILE, centerlinesFile);
  cmd += AddArgument(paramNames.COMPUTE_MESH, "true");
  cmd += AddArgument(paramNames.WRITE_MESH_FILE, "true");
  cmd += AddArgument(paramNames.MESH_OUTPUT_FILE, meshFile, last);
  MITK_INFO << msg << "Execute cmd " << cmd;
  PyRun_SimpleString(cmd.c_str());
  MITK_INFO << msg << "Done!";


/*
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
*/
}

//--------------
// StartCommand 
//--------------
// Start a command.
//
std::string sv4guiSimulationPython1d::StartCommand()
{
  std::string cmd = "import " + pythonModuleName + "\n";
  cmd += pythonModuleName + ".run(";
  return cmd;
}

//-------------
// AddArgument
//-------------
// Add an argument to a command string.
//
std::string sv4guiSimulationPython1d::AddArgument(const std::string& name, const std::string& value, bool last)
{
    std::string arg = name + "='" + value + "'";
    if (last)  {
        arg += ")\n";
    } else {
        arg += ",";
    }
    return arg;
}

//-----------
// WriteMesh 
//-----------
// Write the surface mesh on which to generate the network to a VTK .vtp
// file. This file is read in by the Python fractal tree code.

bool sv4guiSimulationPython1d::WriteMesh(const std::string fileName)
{
/*
  vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetFileName(fileName.c_str());
  writer->SetInputData(this->meshPolyData);
  writer->Write();
*/
}

//-----------------
// WriteParameters
//-----------------
// Write the parameters used to generate a Purkinje network to a text file.

bool sv4guiSimulationPython1d::WriteParameters(const std::string fileName, 
    std::map<std::string, std::string>& params)
{

}
