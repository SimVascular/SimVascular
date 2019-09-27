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
#ifndef SV4GUI_SIMULATION_PYTHON_CONVERT_1D_H
#define SV4GUI_SIMULATION_PYTHON_CONVERT_1D_H

#include "sv4gui_MitkSimJob1d.h"

#include <array>
#include <iostream>
#include <set>
#include <utility>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>

//------------------------------------
// sv4guiSimulationPythonConvert1dParamNames
//------------------------------------
// This class defines valid parameter names used by the 'generate_1d_mesh.py' Python script.
//
// The names must match those defined in the 'generate_1d_mesh.py' Args class.
//
class sv4guiSimulationPythonConvert1dParamNames
{ 
  public: 
    sv4guiSimulationPythonConvert1dParamNames() {
      allNames.insert(OUTPUT_DIRECTORY);
    }
    const std::string ALL_SEGMENTS = "all_segments";
    const std::string DATA_NAMES = "data_names";
    const std::string OUTPUT_DIRECTORY = "output_directory";
    const std::string OUTPUT_FILE_NAME = "output_file_name";
    const std::string OUTPUT_FORMAT = "output_format";
    const std::string OUTLET_SEGMENTS = "outlet_segments";
    const std::string RESULTS_DIRECTORY = "results_directory";
    const std::string SOLVER_FILE_NAME = "solver_file_name";
    const std::string TIME_RANGE = "time_range";
    std::set<std::string> allNames;
};

//---------------------------------
// sv4guiSimulationPythonConvert1d
//---------------------------------
//
class sv4guiSimulationPythonConvert1d 
{
  public:
    sv4guiSimulationPythonConvert1d();
    ~sv4guiSimulationPythonConvert1d(); 

    std::string name; 
    std::string networkFileName; 
    vtkSmartPointer<vtkPolyData> meshPolyData;
    sv4guiSimulationPythonConvert1dParamNames m_ParameterNames;
    std::map<std::string, std::string> m_ParameterValues;
    const std::string m_PythonModuleName = "sv_1d_extract_results";

    std::string AddArgument(const std::string& arg, const std::string& value, bool last=false);
    bool AddParameter(const std::string& name, const std::string& value = "");
    bool ConvertResults(const std::string outputDirectory);
    std::string StartCommand();

};

#endif 
