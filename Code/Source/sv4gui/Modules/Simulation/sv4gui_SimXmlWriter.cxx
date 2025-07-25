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

// The code here is used to write an svMultiPhysics XML solver input file.
//
// Parameters are set in the 'Solver Paramters' GUI section of the CFD Simulation Tool. 
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
  auto capProps = job->GetCapProps();

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

    if (bc_prop_type == "Prescribed Velocities") {
      add_velocity_bc(props, job, face_name, boundary_condition);

    } else if (bc_prop_type == "RCR") {
      add_rcr_bc(props, job, boundary_condition);

    } else if (bc_prop_type == "Resistance") {
      add_resistance_bc(props, job, boundary_condition);
    }
  }

  // Add no slip wall boundary conditions.
  for (auto face : faces_name_type_) {
    if (face.second != "wall") {
      continue;
    }
    auto boundary_condition = add_sub_child(equation, "Add_BC");
    boundary_condition->SetAttribute("name", face.first.c_str());
    add_wall_bc(job, boundary_condition);
  }
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
  add_child(boundary_condition, "Time_dependence", "Resistance");

  std::string str_values = props["Values"];
  std::vector<double> values;
  std::stringstream ss(str_values);
  double value;
  while (ss >> value) {
    values.push_back(value);
  }

  auto rcr_values = add_sub_child(boundary_condition, "RCR_values");
  add_child(rcr_values, "Distal_resistance", values[0]);
  add_child(rcr_values, "Capacitance", values[1]);
  add_child(rcr_values, "Proximal_resistance", values[2]);

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
  bool cmm_equation = false;
  char* eq_type = "fluid";
  
  auto wallProps = job->GetWallProps();
  auto nonlinearSolverProps = job->GetNonlinearSolverProps();

  if (wallProps["Type"] == "deformable") {
    cmm_equation = true;
    eq_type = "cmm";
  }

  auto equation = add_sub_child(root_, "Add_equation");
  equation->SetAttribute("type", eq_type);

  // Nonlinear solver paramters.
  add_child(equation, "Coupled", parameters.Coupled);
  add_child(equation, "Min_iterations", nonlinearSolverProps["Min iterations"]);
  add_child(equation, "Max_iterations", nonlinearSolverProps["Max iterations"]);
  add_child(equation, "Tolerance", nonlinearSolverProps["Tolerance"]); 
  add_child(equation, "Backflow_stabilization_coefficient", nonlinearSolverProps["Backflow stabilization coefficient"]);

  auto basicProps = job->GetBasicProps();
  add_child(equation, "Density", basicProps["Fluid Density"]);
  auto viscosity = add_sub_child(equation, "Viscosity");
  viscosity->SetAttribute("model", "Constant");
  add_child(viscosity, "Value", basicProps["Fluid Viscosity"]);

  add_equation_solver(job, equation);

  add_equation_output(job, equation);

  add_equation_bcs(job, equation);
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
  add_child(output, "Velocity", true); 
  add_child(output, "Vorticity", true); 
  add_child(output, "WSS", true); 
}

//---------------------
// add_equation_solver
//---------------------
// Add the linear solver 'LS' section under `Add_equation`.
//
void Sv4GuiSimXmlWriter::add_equation_solver(sv4guiSimJob* job, tinyxml2::XMLElement* equation)
{
  //auto solverProps = job->GetSolverProps();
  //auto nonlinearSolverProps = job->GetNonlinearSolverProps();
  auto linearSolverProps = job->GetLinearSolverProps();

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
  auto solverProps = job->GetSolverProps();
  add_child(general, "Number_of_spatial_dimensions", 3);
  add_child(general, "Number_of_time_steps", solverProps["Number of Timesteps"]);
  add_child(general, "Time_step_size", solverProps["Time Step Size"]);
  add_child(general, "Spectral_radius_of_infinite_time_step", solverProps["Spectral radius of infinite time step"]);

  // Output options
  add_child(general, "Increment_in_saving_restart_files", solverProps["Increment in saving restart files"]);
  add_child(general, "Start_saving_after_time_step", solverProps["Start saving after time step"]);
  add_child(general, "Save_results_to_VTK_format", solverProps["Save results to VTK format"]);
  add_child(general, "Name_prefix_of_saved_VTK_files", parameters.Name_prefix_of_saved_VTK_files);
  add_child(general, "Increment_in_saving_VTK_files", solverProps["Increment in saving VTK files"]);

  // Misc 
  add_child(general, "Spectral_radius_of_infinite_time_step", solverProps["Spectral radius of infinite time step"]);
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

  add_mesh(job);

  add_equation(job);

  doc_.SaveFile(file_name.c_str());
}

