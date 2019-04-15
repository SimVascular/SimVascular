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

// The classes defined here provide an interface to the 'generate_1d_mesh' 
// Python script used to generate a 1D mesh from centerlines geometry and 
// create input files to the 1D solver. 
//
#ifndef SV4GUI_SIMULATION_PYTHON1D_H
#define SV4GUI_SIMULATION_PYTHON1D_H

#include "sv4gui_MitkSimJob1d.h"

#include <array>
#include <iostream>
#include <set>
#include <utility>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>

//------------------------------------
// sv4guiSimulationPython1dParamNames
//------------------------------------
// This class defines valid parameter names used by the 'generate_1d_mesh.py' Python script.
//
// The names must match those defined in the 'generate_1d_mesh.py' Args class.
//
class sv4guiSimulationPython1dParamNames
{ 
  public: 
    sv4guiSimulationPython1dParamNames() {
      allNames.insert(BOUNDARY_SURFACE_DIR);
      allNames.insert(CENTERLINES_INPUT_FILE);
      allNames.insert(COMPUTE_MESH);
      allNames.insert(MESH_OUTPUT_FILE);
      allNames.insert(OUTPUT_DIRECTORY);
      allNames.insert(SOLVER_OUTPUT_FILE);
      allNames.insert(WRITE_MESH_FILE);
      allNames.insert(WRITE_SOLVER_FILE);
    }
    const std::string BOUNDARY_SURFACE_DIR = "boundary_surfaces_directory";
    const std::string CENTERLINES_INPUT_FILE = "centerlines_input_file";
    const std::string COMPUTE_MESH = "compute_mesh";
    const std::string MESH_OUTPUT_FILE = "mesh_output_file";
    const std::string OUTPUT_DIRECTORY = "output_directory";
    const std::string SOLVER_OUTPUT_FILE = "solver_output_file";
    const std::string WRITE_MESH_FILE = "write_mesh_file";
    const std::string WRITE_SOLVER_FILE = "write_solver_file";

    std::set<std::string> allNames;
};

//--------------------------
// sv4guiSimulationPython1d
//--------------------------
//
//
class sv4guiSimulationPython1d 
{
  public:
    sv4guiSimulationPython1d();
    ~sv4guiSimulationPython1d(); 

    std::string name; 
    std::string networkFileName; 
    vtkSmartPointer<vtkPolyData> meshPolyData;
    sv4guiSimulationPython1dParamNames parameterNames;
    std::map<std::string, std::string> parameterValues;
    const std::string pythonModuleName = "generate_1d_mesh";

    std::string AddArgument(const std::string& arg, const std::string& value, bool last=false);
    bool GenerateMesh(const std::string& outputDir, const std::string& centerlinesFile, const std::string& meshFile);
    bool GenerateSolverInput(const std::string& outputDir, const std::string& centerlinesFile, const std::string& solverFile,
                             const sv4guiSimJob1d* job);
    std::string StartCommand();
    bool WriteMesh(const std::string fileName);
    bool WriteParameters(const std::string fileName, std::map<std::string, std::string>& params);

};

#endif //SV4GUI_SIMULATION_PYTHON1D_H
