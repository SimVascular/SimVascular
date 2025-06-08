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

#include "sv4gui_SimXmlWriter.h"

#include <fstream>
#include <type_traits>

#include <QString>
#include <QRegularExpression>

//-----------------
// Sv4GuiXmlWriter
//-----------------
//
Sv4GuiSimXmlWriter::Sv4GuiSimXmlWriter()
{
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
//
void Sv4GuiSimXmlWriter::add_equation_bcs(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation)
{
  auto capProps=job->GetCapProps();
  int velocityCapNumber=0;
  int pressureCapNumber=0;
  auto it = capProps.begin();

  for (auto it = capProps.begin(); it != capProps.end(); ++it) {
    if (it->first == "") {
      continue;
    }

    auto face_name = it->first;
    auto props = it->second;
    auto bc_prop_type = props["BC Type"];
    std::cout << "[add_equation_bcs] face: " << it->first << "  " << "type: " << bc_prop_type << std::endl;

    auto boundary_condition = add_sub_child(xml_equation, "Add_BC");
    boundary_condition->SetAttribute("name", face_name.c_str());

    std::string time_dependence; 
    std::string bc_type; 

    if (bc_prop_type == "Prescribed Velocities") {

    } else if (bc_prop_type == "Resistance") {
      bc_type = "Neumann"; 
      time_dependence = "Resistance"; 
    }

    add_child(boundary_condition, "Type", bc_type.c_str());
    add_child(boundary_condition, "Time_dependence", time_dependence.c_str());
  }

/*

    if (iBc.bcType == "Steady") {
      add_child(boundary_condition, "Value", iBc.g);

    } else if (iBc.bcType == "Unsteady") {
      add_child(boundary_condition, "Temporal_values_file_path", iBc.gtFile);

    } else if (iBc.bcType == "Resistance") {
      add_child(boundary_condition, "Value", iBc.r);

    } else if (iBc.bcType == "General") {
      add_child(boundary_condition, "Temporal_and_spatialvalues_file_path", iBc.gmFile);

    }

    add_child(boundary_condition, "Profile", iBc.profile);

    if (iBc.profile == "User_defined") {
      add_child(boundary_condition, "Spatial_profile_file_path", iBc.gxFile);
    }

    if (iBc.zperm) {
      add_child(boundary_condition, "Zero_out_perimeter", iBc.zperm);
    }

    if (iBc.flux) {
      add_child(boundary_condition, "Impose_flux", iBc.flux);
    }

    if (iBc.imposeIntegral) {
      add_child(boundary_condition, "Impose_on_state_variable_integral", iBc.imposeIntegral);
    }

    if (iBc.effectiveDirection.trimmed() != "") {
      QStringList list = iBc.effectiveDirection.trimmed().split(QRegularExpression("[(),{}-\\s+]"),Qt::SkipEmptyParts);
      // [DaveP] auto list = iBc.effectiveDirection.trimmed().split(QRegExp("[(),{}-\\s+]"),QString::SkipEmptyParts);
      auto dir = "(" + list[0].toStdString() + ", " + list[1].toStdString() + ", " + list[2].toStdString() + ")";
      add_child(boundary_condition, "Effective_direction", dir);
    }

  }

*/

}

//--------------
// add_equation
//--------------
// Add a fluid or cmm equation.
//
void Sv4GuiSimXmlWriter::add_equation(sv4guiSimJob* job)
{
  bool cmm_equation = false;
  char* eq_type = "fluid";
  
  auto wallProps = job->GetWallProps();
  auto solverProps = job->GetSolverProps();

  if (wallProps["Type"] == "deformable") {
    cmm_equation = true;
    eq_type = "cmm";
  }

  auto xml_equation = add_sub_child(root_, "Add_equation");
  xml_equation->SetAttribute("type", eq_type);

  add_child(xml_equation, "Coupled", parameters.Coupled);
  add_child(xml_equation, "Min_iterations", parameters.Min_iterations);
  add_child(xml_equation, "Max_iterations", parameters.Max_iterations);
  add_child(xml_equation, "Tolerance", parameters.Tolerance);

  add_child(xml_equation, "Backflow_stabilization_coefficient", solverProps["Backflow Stabilization Coefficient"]);

  // Add fluid properties.
  //
  auto basicProps = job->GetBasicProps();
  add_child(xml_equation, "Density", basicProps["Fluid Density"]);
  auto xml_viscosity = add_sub_child(xml_equation, "Viscosity");
  xml_viscosity->SetAttribute("model", "Constant");

  add_equation_solver(job, xml_equation);

  //add_equation_output(job, xml_equation);

  add_equation_bcs(job, xml_equation);
}

//---------------------
// add_equation_output
//---------------------
//
#ifdef Sv4GuiSimXmlWriter_use
void Sv4GuiSimXmlWriter::add_equation_output(sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation)
{
  auto output = add_sub_child(xml_equation, "Output");
  output->SetAttribute("type", "Spatial");

  foreach ( auto& outName , eq.getOutputNames() ) {
    add_child(output, outName.toStdString(), true); 
  }
 
}
#endif

//---------------------
// add_equation_solver
//---------------------
// Add the linear solver section to an equation section.
//
void Sv4GuiSimXmlWriter::add_equation_solver(sv4guiSimJob* job, tinyxml2::XMLElement* xml_equation)
{
  auto solverProps = job->GetSolverProps();

  auto linear_solver = add_sub_child(xml_equation, "LS");
  auto solver_type = solverProps["svLS Type"];
  linear_solver->SetAttribute("type", solver_type.c_str());

  auto linear_algebra = add_sub_child(linear_solver, "Linear_algebra");
  linear_algebra->SetAttribute("type", "fsils");
  add_child(linear_algebra, "Preconditioner", parameters.solver.Preconditioner);

  add_child(linear_solver, "Max_iterations", solverProps["Maximum Number of Iterations for svLS NS Solver"]);
  add_child(linear_solver, "Tolerance", parameters.solver.Tolerance);
  add_child(linear_solver, "Krylov_space_dimension", parameters.solver.Krylov_space_dimension);

  if (solver_type == "NS") {
    add_child(linear_solver, "NS_GM_max_iterations", parameters.solver.NS_GM_max_iterations);
    add_child(linear_solver, "NS_GM_tolerance", parameters.solver.NS_GM_tolerance);

    add_child(linear_solver, "NS_CG_max_iterations", parameters.solver.NS_CG_max_iterations);
    add_child(linear_solver, "NS_CG_tolerance", parameters.solver.NS_CG_tolerance);
  }

}

//------------------
// add_fsi_equation
//------------------
// Add an FSI equation.
//
#ifdef Sv4GuiSimXmlWriter_use
void Sv4GuiSimXmlWriter::add_fsi_equation(sv4guisvFSIJob* job, const sv4guisvFSIeqClass& eq, 
    tinyxml2::XMLElement* xml_equation)
{
  // Add fluid domain 1.
  //
  // This seems to have ID 1 ?
  //
  auto domain_1 = add_sub_child(xml_equation, "Domain");
  domain_1->SetAttribute("id", 0);

  add_child(domain_1, "Equation", "fluid");
  add_child(domain_1, "Density", eq.getPropValue(0));

  auto viscosity = add_sub_child(domain_1, "Viscosity");
  viscosity->SetAttribute("model", "Constant");
  add_child(viscosity, "Value", eq.getPropValue(1));

  add_child(domain_1, "Backflow_stabilization_coefficient", eq.backflowStab);

  // Add struct domain 2.
  //
  auto domain_2 = add_sub_child(xml_equation, "Domain");
  domain_2->SetAttribute("id", 1);

  add_child(domain_2, "Equation", "struct");

  // For Constitutive_model with no sub-elements we need to use
  // 'add_child()' with an empty value.
  //
  auto Constitutive_model = add_child(domain_2, "Constitutive_model", "");
  Constitutive_model->SetAttribute("type", eq.constitutiveModel.toStdString().c_str());

  add_child(domain_2, "Density", eq.getPropValue(2));
  add_child(domain_2, "Elasticity_modulus", eq.getPropValue(3));
  add_child(domain_2, "Poisson_ratio", eq.getPropValue(4));

  if (eq.remesher != "None") {
    add_remeshing(job, eq, xml_equation);
  }
 
}
#endif

//-------------
// add_general
//-------------
// Add data for the 'GeneralSimulationParameters' section.
//
void Sv4GuiSimXmlWriter::add_general(sv4guiSimJob* job)
{
  auto general = add_sub_child(root_, "GeneralSimulationParameters");

  auto solverProps = job->GetSolverProps();
  
  // Time stepping
  //
  add_child(general, "Number_of_spatial_dimensions", 3);
  add_child(general, "Number_of_time_steps", solverProps["Number of Timesteps"]);
  add_child(general, "Time_step_size", solverProps["Time Step Size"]);

#ifdef use

  // Restart files.
  //
  add_child(general, "Restart_file_name", job->restartFileName);
  add_child(general, "Increment_in_saving_restart_files", job->restartInc);
  add_child(general, "Start_saving_after_time_step", job->startSavingStep);
  add_child(general, "Continue_previous_simulation", job->continuePrevious);
  add_child(general, "Save_averaged_results", job->saveAvgResult);

  // VTK output.
  //
  add_child(general, "Save_results_to_VTK_format", job->vtkSaveResults);
  add_child(general, "Name_prefix_of_saved_VTK_files", job->vtkFileName);
  add_child(general, "Increment_in_saving_VTK_files", job->vtkInc);

  // Misc 
  //
  add_child(general, "Spectral_radius_of_infinite_time_step", job->rhoInf);
  add_child(general, "Searched_file_name_to_trigger_stop", job->stopFileName);
  add_child(general, "Simulation_requires_remeshing", job->remeshing);

  add_child(general, "Verbose", job->verbose);
  add_child(general, "Warning", job->warn);
  add_child(general, "Debug", job->debug);
#endif
}

//----------
// add_mesh
//----------
//
#ifdef Sv4GuiSimXmlWriter_use
void Sv4GuiSimXmlWriter::add_mesh(const sv4guisvFSIJob* job, sv4guisvFSIDomain& domain, const int domain_id)
{
  auto mesh = add_sub_child(root_, "Add_mesh");
  mesh->SetAttribute("name", domain.name.c_str());

  auto mesh_file = domain.folderName + path_sep_ + domain.fileName;
  add_child(mesh, "Mesh_file_path", mesh_file);

  for ( auto& face_name : domain.faceNames ) {
    auto face = add_sub_child( mesh, "Add_face");
    face->SetAttribute("name", face_name.c_str());
    auto face_file = domain.folderName + path_sep_ + domain.faceFolderName + path_sep_ + face_name+".vtp";
    add_child(face, "Face_file_path", face_file);
  }

  add_child( mesh, "Domain", domain_id);
}
#endif

//----------------
// add_projection
//----------------
// Add projection between fluid-solid surfaces.
//
#ifdef Sv4GuiSimXmlWriter_use
void Sv4GuiSimXmlWriter::add_projection(const sv4guisvFSIJob* job)
{
  for (auto& eq: job->m_Eqs) {
    if (eq.physName != "FSI") {
      break;
    }

    for (auto& fbc : eq.faceBCs ) {
      auto& iBc = fbc.second;

      if (iBc.bcType == "Projection") {
        auto projection = add_sub_child(root_, "Add_projection");
        projection->SetAttribute("name", iBc.faceName.toStdString().c_str());
        add_child(projection, "Project_from_face", iBc.projectionFaceName);
      }
    }
  }
}
#endif

//---------------
// add_remeshing
//---------------
//
#ifdef Sv4GuiSimXmlWriter_use
void Sv4GuiSimXmlWriter::add_remeshing(const sv4guisvFSIJob* job, const sv4guisvFSIeqClass& eq, 
    tinyxml2::XMLElement* xml_equation)
{
  auto remesher  = add_sub_child(xml_equation, "Remsher");
  remesher->SetAttribute("type", eq.remesher.toStdString().c_str());

  for (auto& pair : job->m_Domains) {
    auto domainName = pair.first;
    auto& domain = pair.second;
    auto Max_edge_size = add_sub_child(xml_equation, "Max_edge_size");
    Max_edge_size->SetAttribute("name", domainName.c_str()); 
    Max_edge_size->SetAttribute("value", domain.edgeSize);
  }

  add_child(remesher, "Min_dihedral_angle", eq.rmMinAngle); 
  add_child(remesher, "Max_radius_ratio", eq.rmMaxRadiusRatio); 
  add_child(remesher, "Remesh_frequency", eq.rmFrequency); 
  add_child(remesher, "Frequency_for_copying_data", eq.rmCopyFrequency); 
}
#endif

//-----------------------------
// add_single_physics_equation
//-----------------------------
// Add an equation for a single physics simulation.
//
#ifdef Sv4GuiSimXmlWriter_use
void Sv4GuiSimXmlWriter::add_single_physics_equation(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation)
{
  for (int i = 0; i < eq.getPropCount(); i++) {
    auto name = eq.getPropName(i).toStdString();
    std::replace(name.begin(), name.end(), ' ', '_');
    add_child(xml_equation, name, eq.getPropValue(i));
  }
}
#endif

//-----------------
// create_document
//-----------------
// Create an XML document from an sv4guiSimJob object.
//
void Sv4GuiSimXmlWriter::create_document(sv4guiSimJob* job, const std::string& file_name)
{
  root_ = doc_.NewElement("svMultiPhysicsFile");
  root_->SetAttribute("version", "1.0");
  doc_.InsertFirstChild(root_);

  // Add data for the GeneralSimulationParameters section.
  add_general(job);

#ifdef use
  // If there are two domains set the first domain to the fluid domain.
  auto domains = sort_domains(job);

  // Add mesh data for the Add_mesh sections.
  int domain_id = 0;
  for (auto& domain : domains) {
    add_mesh(job, domain, domain_id);
    domain_id += 1;
  }

  // Add projection between fluid-solid surfaces.
  add_projection(job);

#endif

  add_equation(job);

  doc_.SaveFile(file_name.c_str());
}

//--------------
// sort_domains
//--------------
//
// If there are two domains set the first domain to the fluid domain.
//
#ifdef Sv4GuiSimXmlWriter_use
std::vector<sv4guisvFSIDomain> 
Sv4GuiSimXmlWriter::sort_domains(const sv4guisvFSIJob* job)
{
  std::vector<sv4guisvFSIDomain> domains;

  if (job->m_Domains.size() ==1 ){
    for (auto& pair : job->m_Domains) {
      domains.push_back(pair.second);
    }

  } else if (job->m_Domains.size() == 2){
    domains.resize(2);

    for (auto& pair : job->m_Domains) {
      if (pair.second.type == "fluid") {
        domains[0] = pair.second;
      } else {
        domains[1] = pair.second;
      }
    }
  }

  return domains;
}
#endif

