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

// The classes defined here provide an interface for executing 0D and 1D 
// simulation Python scripts used to create solver input files.. 

#ifndef SV4GUI_ROM_SIMULATION_PYTHON_H
#define SV4GUI_ROM_SIMULATION_PYTHON_H

#include "org_sv_gui_qt_romsimulation_Export.h"

#include "sv4gui_MitkROMSimJob.h"

#include <array>
#include <iostream>
#include <set>
#include <utility>

#include <vtkPolyData.h>
#include <vtkSmartPointer.h>

//-------------------------------------
// sv4guiROMSimulationPythonParamNames
//-------------------------------------
// This class defines valid parameter names used by the 'generate_1d_mesh.py' Python script.
//
// The names must match those defined in the 'generate_1d_mesh.py' Args class.
//
class SV_QT_ROMSIMULATION sv4guiROMSimulationPythonParamNames
{ 
  public: 
    sv4guiROMSimulationPythonParamNames() {
      allNames.insert(BOUNDARY_SURFACE_DIR);
      allNames.insert(CENTERLINES_INPUT_FILE);
      allNames.insert(COMPUTE_MESH);
      allNames.insert(DENSITY);
      allNames.insert(ELEMENT_SIZE);
      allNames.insert(INFLOW_INPUT_FILE);
      allNames.insert(MESH_OUTPUT_FILE);
      allNames.insert(MODEL_NAME);
      allNames.insert(MODEL_ORDER);
      allNames.insert(NUM_TIME_STEPS);
      allNames.insert(OUTFLOW_BC_INPUT_FILE);
      allNames.insert(OUTFLOW_BC_TYPE);
      allNames.insert(OUTLET_FACE_NAMES_INPUT_FILE);
      allNames.insert(OUTPUT_DIRECTORY);
      allNames.insert(SAVE_DATA_FREQUENCY);
      allNames.insert(SEG_MIN_NUM);
      allNames.insert(SEG_SIZE_ADAPTIVE);
      allNames.insert(SOLVER_OUTPUT_FILE);
      allNames.insert(TIME_STEP);
      allNames.insert(UNIFORM_BC);
      allNames.insert(UNITS);
      allNames.insert(VISCOSITY);
    }
    const std::string BOUNDARY_SURFACE_DIR = "boundary_surfaces_directory";
    const std::string DENSITY = "density";
    const std::string CENTERLINES_INPUT_FILE = "centerlines_input_file";
    const std::string COMPUTE_MESH = "compute_mesh";
    const std::string INFLOW_INPUT_FILE = "inflow_input_file";
    const std::string ELEMENT_SIZE = "element_size";

    const std::string LINEAR_MATERIAL_EHR = "linear_material_ehr";
    const std::string LINEAR_MATERIAL_PRESSURE = "linear_material_pressure";

    const std::string MATERIAL_MODEL = "material_model";
    const std::string MESH_OUTPUT_FILE = "mesh_output_file";
    const std::string MODEL_NAME = "model_name";
    const std::string MODEL_ORDER = "model_order";
    const std::string NUM_TIME_STEPS = "num_time_steps";

    const std::string OLUFSEN_MATERIAL_K1 = "olufsen_material_k1";
    const std::string OLUFSEN_MATERIAL_K2 = "olufsen_material_k2";
    const std::string OLUFSEN_MATERIAL_K3 = "olufsen_material_k3";
    const std::string OLUFSEN_MATERIAL_EXP = "olufsen_material_exp";
    const std::string OLUFSEN_MATERIAL_PRESSURE = "olufsen_material_pressure";

    const std::string OUTFLOW_BC_INPUT_FILE = "outflow_bc_input_file";
    const std::string OUTFLOW_BC_TYPE = "outflow_bc_type";
    const std::string OUTLET_FACE_NAMES_INPUT_FILE = "outlet_face_names_input_file";
    const std::string OUTPUT_DIRECTORY = "output_directory";

    const std::string SAVE_DATA_FREQUENCY = "save_data_frequency";
    const std::string SEG_MIN_NUM = "seg_min_num";
    const std::string SEG_SIZE_ADAPTIVE = "seg_size_adaptive";
    const std::string SOLVER_OUTPUT_FILE = "solver_output_file";
    const std::string TIME_STEP = "time_step"; 
    const std::string UNIFORM_BC = "uniform_bc";
    const std::string UNITS = "units";
    const std::string VISCOSITY = "viscosity";

    std::set<std::string> allNames;
};

//---------------------------
// sv4guiROMSimulationPython
//---------------------------
// The sv4guiROMSimulationPython class is used as an interface 
// for executing Python scripts used by the SV 0D and 1D solvers.
//
class SV_QT_ROMSIMULATION sv4guiROMSimulationPython 
{
  public:
    sv4guiROMSimulationPython();
    ~sv4guiROMSimulationPython(); 

    std::string name; 
    std::string networkFileName; 
    vtkSmartPointer<vtkPolyData> meshPolyData;
    sv4guiROMSimulationPythonParamNames m_ParameterNames;
    std::map<std::string, std::string> m_ParameterValues;

    // The name of the ROM Python package in SimVascular/Python/site-packages.
    const std::string m_PythonROMSimulationModuleName = "sv_rom_simulation";

    // The 0D solver Python package name.
    const std::string m_PythonZeroDSolverModuleName = "svZeroDSolver.svzerodsolver";
    const std::string m_PythonZeroDSolverFileName = "solver_0d.in";

    std::string AddArgument(const std::string& arg, const std::string& value, bool last=false);
    bool AddParameter(const std::string& name, const std::string& value);
    bool AddParameterList(const std::string& name, const std::vector<std::string>& values);
    bool ExecuteZeroDSimulation(const std::string outputDirectory, const sv4guiROMSimJob* job);
    bool GenerateMesh(const std::string& outputDir, const std::string& centerlinesFile, const std::string& meshFile);
    bool GenerateSolverInput(const std::string outputDirectory, const sv4guiROMSimJob* job);
    std::string StartCommand();
};

#endif 
