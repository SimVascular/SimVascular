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

// Sv4GuiSimXmlWriter is used to write an svMultiPhysics XML solver input file.
//
// Solver parameters are set in the 'Solver Paramters' GUI section of the CFD Simulation Tool. 
//
// The parameter names and types (int, double, enum) are defined in the 
// SimVascular/Code/Source/sv4gui/Plugins/org.sv.gui.qt.simulation/resources/solvertemplate.xml
// file. These parameters are accessed as name/value pairs are stored in the 
// sv4guiSimJob::m_SolverProps map.  
//
// The original implementation did not store parameters according to the sections (e.g. Time Step) 
// so parametes with the same name (e.g. Min iterations) were overwritten. Two addtional 
// m_SolverNonlinearProps and m_SolverLinearProps maps were added to store parameters for those 
// sections.

#include "sv4gui_SimXmlWriter.h"
#include "sv4gui_CapBCWidget.h"

#include <fstream>
#include <sstream>
#include <type_traits>

#include <QString>
#include <QRegularExpression>

//-----------------
// Sv4GuiXmlWriter
//-----------------
//
Sv4GuiSimXmlWriter::Sv4GuiSimXmlWriter()
{
  // Map GUI names to svMultiPhysics supported profile types.
  parameters.profile_names = { 
    {"parabolic", "Parabolic"},
    {"plug", "Flat"}
  };

}

Sv4GuiSimXmlWriter::~Sv4GuiSimXmlWriter()
{
}

//-----------
// add_child
//-----------
// Define a template function for adding a child element to a parent element.
//
template <typename T>
tinyxml2::XMLElement*
Sv4GuiSimXmlWriter::add_child(tinyxml2::XMLElement* parent, const std::string& name, T value)
{
  auto child = doc_.NewElement(name.c_str());
  child->SetText(value);
  parent->InsertEndChild(child);
  return child;
}

// Handle QString that must be converted to std::string.
template<> 
tinyxml2::XMLElement*
Sv4GuiSimXmlWriter::add_child<QString>(tinyxml2::XMLElement* parent, const std::string& name, QString value)
{
  return add_child(parent, name, value.toStdString().c_str());
}

// Handle std::string that must be converted to char*.
template<> 
tinyxml2::XMLElement* 
Sv4GuiSimXmlWriter::add_child<std::string>(tinyxml2::XMLElement* parent, const std::string& name, std::string value)
{
  return add_child(parent, name, value.c_str());
}

//---------------
// add_sub_child
//---------------
// Create a sub-section child element.
//
tinyxml2::XMLElement*
Sv4GuiSimXmlWriter::add_sub_child(tinyxml2::XMLElement* parent, const std::string& name)
{
  auto child = doc_.NewElement(name.c_str());
  parent->InsertEndChild(child);
  return child;
}

//------------------
// add_equation_bcs
//------------------
// Add a `Add_BC` boundary condition section under `Add_equation`.
//
// A boundary condition is created for each face defined from the
// SV Model Tool and stored in the simulation mesh-complete/mesh-surfaces/ 
// directory. 
//
void Sv4GuiSimXmlWriter::add_equation_bcs(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  auto capProps = job->cap_props.GetAll();

  // Process surface faces representing inlet and outlet
  // mesh boundaries (caps).
  //
  auto it = capProps.begin();

  for (auto it = capProps.begin(); it != capProps.end(); ++it) {
    if (it->first == "") {
      continue;
    }

    auto face_name = it->first;
    auto props = it->second;
    auto bc_prop_type = props["BC Type"];

    auto boundary_condition = add_sub_child(equation, "Add_BC");
    boundary_condition->SetAttribute("name", face_name.c_str());

    if (bc_prop_type == sv4guiSimJobBCType::flow) {
      add_velocity_bc(props, job, face_name, boundary_condition);

    } else if (bc_prop_type == sv4guiSimJobBCType::rcr) {
      add_rcr_bc(props, job, boundary_condition);

    } else if (bc_prop_type == sv4guiSimJobBCType::resistance) {
      add_resistance_bc(props, job, boundary_condition);

    } else if (bc_prop_type == sv4guiSimJobBCType::lpm) {
      add_lpm_bc(props, job, boundary_condition);
    }

  }

  // Add wall boundary conditions.
  for (auto face : faces_name_type_) {
    if (face.second != "wall") {
      continue;
    }
    auto boundary_condition = add_sub_child(equation, "Add_BC");
    boundary_condition->SetAttribute("name", face.first.c_str());

    if (cmm_simulation_enabled_) { 
      add_cmm_equation_wall_bc(job, boundary_condition);
    } else {
      add_wall_bc(job, boundary_condition);
    }
  }
}

//------------
// add_lpm_bc
//------------
//
void Sv4GuiSimXmlWriter::add_lpm_bc(GuiProperties& props, sv4guiSimJob* job, 
    tinyxml2::XMLElement* boundary_condition)
{
  if (!zerod_interface_defined_) {
    const char* name;
    boundary_condition->QueryStringAttribute("name", &name);
    auto zerod_interface_props = job->zerod_interface_props.GetAll();
    auto config_file = zerod_interface_props["config_file_name"];
    auto lib_file = zerod_interface_props["library_name"];
    std::string msg = "svZeroDSolver coupled boundary condition for face '" + std::string(name) + "': ";

    if (lib_file == "") {
      throw std::runtime_error(msg + " The svZeroDSolver shared library has not been set."); 

   } else if (config_file == "") {
      throw std::runtime_error(msg + " The svZeroDSolver configuration JSON file has not been set."); 
   }

  }

  add_child(boundary_condition, "Type", "Neumann");
  add_child(boundary_condition, "Time_dependence", "Coupled");
  add_child(boundary_condition, "svZeroDSolver_block", props["lpm_block_name"]);
}

//------------
// add_rcr_bc
//------------
// Add a `Add_BC` RCR boundary condition section under `Add_equation`.
//
void Sv4GuiSimXmlWriter::add_rcr_bc(GuiProperties& props, sv4guiSimJob* job, 
    tinyxml2::XMLElement* boundary_condition)
{
  add_child(boundary_condition, "Type", "Neumann");
  add_child(boundary_condition, "Time_dependence", "RCR");

  std::string str_values = props["Values"];
  std::vector<double> values;
  std::stringstream ss(str_values);
  double value;
  while (ss >> value) {
    values.push_back(value);
  }

  auto rcr_values = add_sub_child(boundary_condition, "RCR_values");
  add_child(rcr_values, "Proximal_resistance", values[0]);
  add_child(rcr_values, "Capacitance", values[1]);
  add_child(rcr_values, "Distal_resistance", values[2]);

  add_child(rcr_values, "Distal_pressure", 0.0);
  add_child(rcr_values, "Initial_pressure", 0.0);
}

//-------------------
// add_resistance_bc
//-------------------
// Add a `Add_BC` resistance boundary condition section under `Add_equation`.
//
void Sv4GuiSimXmlWriter::add_resistance_bc(GuiProperties& props, sv4guiSimJob* job, 
    tinyxml2::XMLElement* boundary_condition)
{
  add_child(boundary_condition, "Type", "Neumann");
  add_child(boundary_condition, "Time_dependence", "Resistance");

  std::string resistance_values = props["Values"];
  add_child(boundary_condition, "Value", resistance_values);
}

//-----------------
// add_velocity_bc
//-----------------
// Add a `Add_BC` velocity inlet boundary condition section under `Add_equation`.
//
// This writes the .flow file.
//
void Sv4GuiSimXmlWriter::add_velocity_bc(GuiProperties& props, sv4guiSimJob* 
    job, const std::string& face_name, tinyxml2::XMLElement* boundary_condition)
{
  add_child(boundary_condition, "Type", "Dirichlet"); 
  add_child(boundary_condition, "Time_dependence", "Unsteady"); 

  // Write flow file.
  //
  std::string file_name = face_name + ".flow";
  std::string flow_values = props["Flow Rate"];
  std::ofstream out(output_dir_ + "/" + file_name);
  out << flow_values;
  out.close();

  add_child(boundary_condition, "Temporal_values_file_path", file_name); 

  auto profile = parameters.profile_names[ props["Analytic Shape"] ];
  add_child(boundary_condition, "Profile", profile ); 
  add_child(boundary_condition, "Impose_flux", parameters.Impose_flux); 
}

//-------------
// add_wall_bc
//-------------
// Add a `Add_BC` no slip boundary condition section under `Add_equation` for a wall face.
//
void Sv4GuiSimXmlWriter::add_wall_bc(sv4guiSimJob* job, tinyxml2::XMLElement* boundary_condition)
{
  add_child(boundary_condition, "Type", "Dirichlet");
  add_child(boundary_condition, "Time_dependence", "Steady");
  add_child(boundary_condition, "Value", 0.0);
}

//--------------
// add_equation
//--------------
// Add an `Add_equation` section of type fluid or cmm.
//
void Sv4GuiSimXmlWriter::add_equation(sv4guiSimJob* job)
{
  #define n_debug_add_equation
  #ifdef debug_add_equation
  std::string msg("[Sv4GuiSimXmlWriter::add_equation] ");
  std::cout << msg << std::endl;
  std::cout << msg << "========== add_equation ==========" << std::endl;
  #endif

  char* eq_type = "fluid";
  //auto wallProps = job->GetWallProps();

  auto equation = add_sub_child(root_, "Add_equation");
  equation->SetAttribute("type", eq_type);

  add_equation_zerod_interface(job, equation);

  add_equation_nl_solver(job, equation);

  auto basicProps = job->basic_props.GetAll();
  add_child(equation, "Density", basicProps["Fluid Density"]);
  auto viscosity = add_sub_child(equation, "Viscosity");
  viscosity->SetAttribute("model", "Constant");
  add_child(viscosity, "Value", basicProps["Fluid Viscosity"]);

  add_equation_solver(job, equation);

  add_equation_output(job, equation);

  add_equation_bcs(job, equation);
}

//-------------------------
// add_cmm_wall_properties
//-------------------------
// Add wall properties for a cmm simulation under the Add_equation section.
//
void Sv4GuiSimXmlWriter::add_cmm_wall_properties(sv4guiSimJob*job, tinyxml2::XMLElement* equation)
{
  auto wall_props = job->wall_props.GetAll();

  auto density = wall_props["Density"];
  add_child(equation, "Solid_density", density);

  auto elastic_modulus = wall_props["Elastic Modulus"];
  add_child(equation, "Elasticity_modulus", elastic_modulus);

  auto poisson_ratio = wall_props["Poisson Ratio"];
  add_child(equation, "Poisson_ratio", poisson_ratio);

  auto shell_thickness = wall_props["Thickness"];
  add_child(equation, "Shell_thickness", shell_thickness);
}

//------------------
// add_cmm_equation
//------------------
// Add an `Add_equation` section of type cmm.
//
void Sv4GuiSimXmlWriter::add_cmm_equation(sv4guiSimJob* job)
{
  #define n_debug_add_cmm_equation
  #ifdef debug_add_cmm_equation
  std::string msg("[Sv4GuiSimXmlWriter::add_cmm_equation] ");
  std::cout << msg << std::endl;
  std::cout << msg << "========== add_cmm_equation ==========" << std::endl;
  #endif

  char* eq_type = "CMM";
  auto wallProps = job->wall_props.GetAll();

  auto equation = add_sub_child(root_, "Add_equation");
  equation->SetAttribute("type", eq_type);

  add_equation_zerod_interface(job, equation);

  // Nonlinear solver paramters.
  add_equation_nl_solver(job, equation);

  // Add physical properties.
  //
  auto basicProps = job->basic_props.GetAll();
  add_child(equation, "Density", basicProps["Fluid Density"]);
  auto viscosity = add_sub_child(equation, "Viscosity");
  viscosity->SetAttribute("model", "Constant");
  add_child(viscosity, "Value", basicProps["Fluid Viscosity"]);

  if (cmm_simulation_initialization_) {
    if (cmm_prestress_simulation_) {
      add_child(equation, "Prestress", "true");
      add_child(equation, "Initialize", "prestress");
    } else {
      add_child(equation, "Initialize", "inflate");
    }
  }

  add_cmm_wall_properties(job, equation);

  add_equation_solver(job, equation);

  add_equation_output(job, equation);

  if (cmm_simulation_initialization_) {
    add_cmm_equation_bf(job, equation);
  } else {
    add_equation_bcs(job, equation);
  }

}

//--------------------------
// add_cmm_equation_wall_bc
//--------------------------
// For a cmm initialization simulation just add 
// a body force on the wall.
//
void Sv4GuiSimXmlWriter::add_cmm_equation_wall_bc(sv4guiSimJob* job, tinyxml2::XMLElement* boundary_condition)
{
  auto cmmProps = job->cmm_props.GetAll();

  add_child(boundary_condition, "Type", "CMM");

  if (cmm_prestress_simulation_) {
    auto prestress_file = cmmProps["Prestress file"];
    add_child(boundary_condition, "Prestress_file_path", prestress_file);

  } else {
    auto disp_file = cmmProps["Displacements file"];
    add_child(boundary_condition, "Initial_displacements_file_path", disp_file);

  }

}

//---------------------
// add_cmm_equation_bf
//---------------------
// Add a body force 'Add_BF' boundary condition for a cmm equation for an 
// initialization simulation.
//
void Sv4GuiSimXmlWriter::add_cmm_equation_bf(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  auto cmmProps = job->cmm_props.GetAll();

  auto boundary_condition = add_sub_child(equation, "Add_BF");

  boundary_condition->SetAttribute("mesh", cmm_wall_name_.c_str());
  
  add_child(boundary_condition, "Type", "traction");
  add_child(boundary_condition, "Time_dependence", "spatial");

  auto traction_file_name = cmmProps["Traction file"];

  if (traction_file_name == "") {
    throw std::runtime_error("Coupled Momentum Method: No traction file has been set.");
  }

  add_child(boundary_condition, "Spatial_values_file_path", traction_file_name);
}

//---------------------
// add_equation_output
//---------------------
// Add an `Output` section under `Add_equation`.
//
void Sv4GuiSimXmlWriter::add_equation_output(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  auto output = add_sub_child(equation, "Output");
  output->SetAttribute("type", "Spatial");
  add_child(output, "Divergence", true); 
  add_child(output, "Pressure", true); 
  add_child(output, "Traction", true); 
  add_child(output, "Velocity", true); 
  add_child(output, "Vorticity", true); 
  add_child(output, "WSS", true); 

  if (cmm_simulation_enabled_) {
    add_child(output, "Displacement", true); 
    add_child(output, "Stress", true); 
  }

  auto boundary_output = add_sub_child(equation, "Output");
  boundary_output->SetAttribute("type", "Boundary_integral");
  add_child(boundary_output, "Velocity", true); 
  add_child(boundary_output, "Pressure", true); 
  add_child(boundary_output, "WSS", true); 
}

//------------------------------
// add_equation_zerod_interface
//------------------------------
// Add the 'svZeroDSolver_interface' section under the equation.
//
void Sv4GuiSimXmlWriter::add_equation_zerod_interface(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  auto zerod_interface_props = job->zerod_interface_props.GetAll();
  auto config_file = zerod_interface_props["config_file_name"];
  auto lib_file = zerod_interface_props["library_name"];

  if ((config_file == "" || lib_file == "")) {
    return;
  }

  auto coupling_type = zerod_interface_props["coupling_type"];
  auto initial_pressure = zerod_interface_props["initial_pressure_value"];
  auto initial_velocity = zerod_interface_props["initial_velocity_value"];

  auto zerod_interface = add_sub_child(equation, "svZeroDSolver_interface");
  add_child(zerod_interface, "Coupling_type", coupling_type); 
  add_child(zerod_interface, "Configuration_file", config_file); 
  add_child(zerod_interface, "Shared_library", lib_file); 
  add_child(zerod_interface, "Initial_flows", initial_velocity); 
  add_child(zerod_interface, "Initial_pressures", initial_pressure); 

  zerod_interface_defined_ = true;
}

//-------------------------
// add_equation_nl_solver
//-------------------------
// Add nonlinear solver paramters section under `Add_equation`..
//
void Sv4GuiSimXmlWriter::add_equation_nl_solver(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  auto nonlinearSolverProps = job->nonlinear_solver_props.GetAll();

  add_child(equation, "Coupled", parameters.Coupled);
  add_child(equation, "Min_iterations", nonlinearSolverProps["Min iterations"]);
  add_child(equation, "Max_iterations", nonlinearSolverProps["Max iterations"]);
  add_child(equation, "Tolerance", nonlinearSolverProps["Tolerance"]); 
  add_child(equation, "Backflow_stabilization_coefficient", nonlinearSolverProps["Backflow stabilization coefficient"]);
}

//---------------------
// add_equation_solver
//---------------------
// Add the linear solver 'LS' section under `Add_equation`.
//
void Sv4GuiSimXmlWriter::add_equation_solver(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  //auto solverProps = job->linear_solver_props.GetAll();
  //auto nonlinearSolverProps = job->GetNonlinearSolverProps();
  auto linearSolverProps = job->linear_solver_props.GetAll();

  auto linear_solver = add_sub_child(equation, "LS");
  auto solver_type = linearSolverProps["Solver"];
  linear_solver->SetAttribute("type", solver_type.c_str());

  auto linear_algebra = add_sub_child(linear_solver, "Linear_algebra");
  linear_algebra->SetAttribute("type", "fsils");
  add_child(linear_algebra, "Preconditioner", parameters.Preconditioner);

  add_child(linear_solver, "Max_iterations", linearSolverProps["Max iterations"]);
  add_child(linear_solver, "Tolerance", linearSolverProps["Tolerance"]); 
  add_child(linear_solver, "Krylov_space_dimension", linearSolverProps["Krylov space dimension"]); 

  if (solver_type == "NS") {
    add_child(linear_solver, "NS_GM_max_iterations", linearSolverProps["NS GM max iterations"]);
    add_child(linear_solver, "NS_GM_tolerance", linearSolverProps["NS GM tolerance"]);

    add_child(linear_solver, "NS_CG_max_iterations", linearSolverProps["NS CG max iterations"]);
    add_child(linear_solver, "NS_CG_tolerance", linearSolverProps["NS CG tolerance"]);
  }
}

//-------------
// add_general
//-------------
// Add data for the 'GeneralSimulationParameters' section.
//
void Sv4GuiSimXmlWriter::add_general(sv4guiSimJob* job)
{
  auto general = add_sub_child(root_, "GeneralSimulationParameters");
  add_child(general, "Continue_previous_simulation", false);
  
  // Time stepping
  auto solverTimeProps = job->solver_time_props.GetAll();
  add_child(general, "Number_of_spatial_dimensions", 3);
  add_child(general, "Number_of_time_steps", solverTimeProps["Number of Timesteps"]);
  add_child(general, "Time_step_size", solverTimeProps["Time Step Size"]);
  add_child(general, "Spectral_radius_of_infinite_time_step", solverTimeProps["Spectral radius of infinite time step"]);

  // Output options
  auto outputProps = job->solver_output_props.GetAll();
  add_child(general, "Increment_in_saving_restart_files", outputProps["Increment in saving restart files"]);
  add_child(general, "Start_saving_after_time_step", outputProps["Start saving after time step"]);

  if (outputProps["Save results in folder"] != "N-procs") {
    add_child(general, "Save_results_in_folder", outputProps["Save results in folder"]);
  }

  add_child(general, "Save_results_to_VTK_format", outputProps["Save results to VTK format"]);
  add_child(general, "Name_prefix_of_saved_VTK_files", outputProps["Name prefix of saved VTK files"]);
  add_child(general, "Increment_in_saving_VTK_files", outputProps["Increment in saving VTK files"]);

  // Misc 
  add_child(general, "Spectral_radius_of_infinite_time_step", solverTimeProps["Spectral radius of infinite time step"]);
}

//----------
// add_mesh
//----------
// Add a 'Add_mesh' section.
//
void Sv4GuiSimXmlWriter::add_mesh(sv4guiSimJob* job)
{
  auto mesh = add_sub_child(root_, "Add_mesh");
  mesh->SetAttribute("name", parameters.Add_mesh_name);
  add_child(mesh, "Mesh_file_path", parameters.Mesh_file_path);

  for (const auto& pair : faces_name_type_) {
    auto face = add_sub_child( mesh, "Add_face");
    auto face_name = pair.first;
    face->SetAttribute("name", face_name.c_str());
    auto face_file = parameters.Face_file_path + "/" + face_name+".vtp";
    add_child(face, "Face_file_path", face_file);
  }
}

//--------------------
// add_cmm_init_mesh
//--------------------
// Add a 'Add_mesh' section for a cmm initializaion stage.
// 
// For the cmm initializaion stage shells are used to model just the mesh surface.
//
void Sv4GuiSimXmlWriter::add_cmm_init_mesh(sv4guiSimJob* job)
{
  #define n_debug_add_cmm_init_mesh
  #ifdef debug_add_cmm_init_mesh
  std::string msg("[Sv4GuiSimXmlWriter::add_cmm_init_mesh] ");
  std::cout << msg << std::endl;
  std::cout << msg << "========== add_cmm_init_mesh ==========" << std::endl;
  #endif
  auto cmmProps = job->cmm_props.GetAll();

  auto mesh = add_sub_child(root_, "Add_mesh");
  mesh->SetAttribute("name", cmm_wall_name_.c_str());

  auto wall_file_name = cmmProps["Wall file"];
  #ifdef debug_add_cmm_init_mesh
  std::cout << msg << "wall_file_name: " << wall_file_name << std::endl;
  #endif

  if (wall_file_name == "") {
    throw std::runtime_error("Coupled Momentum Method: No wall file has been set.");
  }

  add_child(mesh, "Mesh_file_path", wall_file_name); 
  add_child(mesh, "Set_mesh_as_shell", "true"); 
}

//--------------------
// add_cmm_simulation
//--------------------
//
void Sv4GuiSimXmlWriter::add_cmm_simulation(sv4guiSimJob* job)
{
  auto cmmProps = job->cmm_props.GetAll();

  if (cmmProps["Initialize simulation"] == "true") {
    cmm_simulation_initialization_ = true;

    if (cmmProps["Simulation Type"] == "prestress") {
      cmm_prestress_simulation_ = true;
    } else if (cmmProps["Simulation Type"] == "prestress") {
      cmm_prestress_simulation_ = false;
    }
  }

  cmm_wall_name_ = cmmProps["Wall name"];

  if (cmm_wall_name_ == "") {
    throw std::runtime_error("Coupled Momentum Method: No wall name has been set.");
  }

  if (cmm_simulation_initialization_) { 
    add_cmm_init_mesh(job);
  } else {
    add_mesh(job);
  }

  add_cmm_equation(job);
  
}

//----------------------
// check_cmm_simulation
//----------------------
// Check if an CMM simulation is enabled.
//
bool Sv4GuiSimXmlWriter::cmm_simulation_enabled(sv4guiSimJob* job)
{
  auto cmmProps = job->cmm_props.GetAll();
    
  if (cmmProps["Enable cmm simulation"] == "false") {
    cmm_simulation_enabled_ = false;
    return false;
  }

  cmm_simulation_enabled_ = true;
  return true;
}

//-----------------
// create_document
//-----------------
// Create an XML document from an sv4guiSimJob object.
//
// faces_name_type - Stores each mesh face name and its type ('wall' or 'cap').
//
void Sv4GuiSimXmlWriter::create_document(sv4guiSimJob* job, const std::map<std::string,std::string>& faces_name_type, 
    const std::string& output_dir, const std::string& file_name)
{
  output_dir_ = output_dir;
  faces_name_type_ = faces_name_type;

  root_ = doc_.NewElement("svMultiPhysicsFile");
  root_->SetAttribute("version", "1.0");
  doc_.InsertFirstChild(root_);

  add_general(job);

  if (cmm_simulation_enabled(job)) { 

    add_cmm_simulation(job); 

  } else {

    add_mesh(job);
  
    add_equation(job);
  }

  doc_.SaveFile(file_name.c_str());
}

