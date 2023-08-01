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

#include "sv4gui_XmlWriter.h"

#include <fstream>
#include <type_traits>

//-----------------
// Sv4GuiXmlWriter
//-----------------
//
Sv4GuiXmlWriter::Sv4GuiXmlWriter()
{
}

Sv4GuiXmlWriter::~Sv4GuiXmlWriter()
{
}

//-----------
// add_child
//-----------
// Define a template function for adding a child element to a parent element.
//
template <typename T>
tinyxml2::XMLElement*
Sv4GuiXmlWriter::add_child(tinyxml2::XMLElement* parent, const std::string& name, T value)
{
  auto child = doc_.NewElement(name.c_str());
  child->SetText(value);
  parent->InsertEndChild(child);
  return child;
}

// Handle QString that must be converted to std::string.
template<> 
tinyxml2::XMLElement*
Sv4GuiXmlWriter::add_child<QString>(tinyxml2::XMLElement* parent, const std::string& name, QString value)
{
  return add_child(parent, name, value.toStdString().c_str());
}

// Handle std::string that must be converted to char*.
template<> 
tinyxml2::XMLElement* 
Sv4GuiXmlWriter::add_child<std::string>(tinyxml2::XMLElement* parent, const std::string& name, std::string value)
{
  return add_child(parent, name, value.c_str());
}

//---------------
// add_sub_child
//---------------
// Create a sub-section child element.
//
tinyxml2::XMLElement*
Sv4GuiXmlWriter::add_sub_child(tinyxml2::XMLElement* parent, const std::string& name)
{
  auto child = doc_.NewElement(name.c_str());
  parent->InsertEndChild(child);
  return child;
}

//---------------
// add_equations
//---------------
// Add equations.
//
void Sv4GuiXmlWriter::add_equations(const sv4guisvFSIJob* job)
{
  for (auto& eq : job->m_Eqs) {
    auto xml_equation = add_sub_child(root_, "Add_equation");
    xml_equation->SetAttribute("type", eq.physName.toStdString().c_str());

    add_child(xml_equation, "Coupled", eq.coupled);
    add_child(xml_equation, "Min_iterations", eq.minItr);
    add_child(xml_equation, "Max_iterations", eq.maxItr);
    add_child(xml_equation, "Tolerance", eq.tol);

    if (eq.physName == "fluid") {
      add_child(xml_equation, "Backflow_stabilization_coefficient", eq.backflowStab);
    }

    if (eq.getPhysName() == "FSI") {
      add_fsi_equation(eq, xml_equation);

    } else {
      add_single_physics_equation(eq, xml_equation);
    }

    if (eq.physName != "mesh") {
      add_equation_solver(eq, xml_equation);
    }
  }
}

//---------------------
// add_equation_solver
//---------------------
// Add the linear solver section to an equation section.
//
void Sv4GuiXmlWriter::add_equation_solver(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation)
{
  auto linear_solver = add_sub_child(xml_equation, "LS");
  linear_solver->SetAttribute("type", eq.lsType.toStdString().c_str());

  add_child(linear_solver, "Preconditioner", eq.lsPreconditioner); 
  add_child(linear_solver, "Max_iterations", eq.lsMaxItr); 
  add_child(linear_solver, "Tolerance", eq.tol);
  add_child(linear_solver, "Krylov_space_dimension", eq.lsKrylovDim);

  if (eq.lsType == "NS") {
    add_child(linear_solver, "NS_GM_max_iterations", eq.lsNSGMMaxItr);
    add_child(linear_solver, "NS_GM_tolerance", eq.lsNSGMTol);

    add_child(linear_solver, "NS_CG_max_iterations", eq.lsNSCGMaxItr);
    add_child(linear_solver, "NS_CG_tolerance", eq.lsNSCGTol); 
  }

}

//------------------
// add_fsi_equation
//------------------
// Add an FSI equation.
//
void Sv4GuiXmlWriter::add_fsi_equation(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation)
{
  // Add fluid domain 1.
  //
  // This seems to have ID 1 ?
  //
  auto domain_1 = add_sub_child(xml_equation, "Domain");
  domain_1->SetAttribute("id", 1);

  add_child(domain_1, "Equation", "fluid");
  add_child(domain_1, "Density", eq.getPropValue(0));

  auto viscosity = add_sub_child(domain_1, "Viscosity");
  viscosity->SetAttribute("model", eq.getPropValue(1));

  add_child(domain_1, "Backflow_stabilization_coefficient", eq.backflowStab);

  // Add struct domain 2.
  //
  auto domain_2 = add_sub_child(xml_equation, "Domain");
  domain_2->SetAttribute("id", 2);

  add_child(domain_2, "Equation", "struct");
  auto Constitutive_model = add_sub_child(domain_2, "Constitutive_model");
  Constitutive_model->SetAttribute("model", eq.constitutiveModel.toStdString().c_str());

  add_child(domain_2, "Density", eq.getPropValue(2));
  add_child(domain_2, "Elasticity_modulus", eq.getPropValue(3));
  add_child(domain_2, "Poisson_ratio", eq.getPropValue(4));
}

//-------------
// add_general
//-------------
// Add data for the 'GeneralSimulationParameters' section.
//
void Sv4GuiXmlWriter::add_general(const sv4guisvFSIJob* job)
{
  auto general = add_sub_child(root_, "GeneralSimulationParameters");

  // Time stepping
  //
  add_child(general, "Number_of_spatial_dimensions", job->nsd);
  add_child(general, "Number_of_time_steps", job->timeSteps);
  add_child(general, "Time_step_size", job->stepSize.c_str());

  // Restart files.
  //
  add_child(general, "Continue_previous_simulation", job->continuePrevious);
  add_child(general, "Restart_file_name", job->restartFileName);
  add_child(general, "Increment_in_saving_restart_files", job->restartInc);
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
}

//----------
// add_mesh
//----------
//
void Sv4GuiXmlWriter::add_mesh(const sv4guisvFSIJob* job, sv4guisvFSIDomain& domain, const int domain_id)
{
  auto mesh = add_sub_child(root_, "Add_mesh");
  mesh->SetAttribute("name", domain.name.c_str());

  add_child(mesh, "Mesh_file_path", domain.folderName);

  for ( auto& face_name : domain.faceNames ) {
    auto face = add_sub_child( mesh, "Add_face");
    face->SetAttribute("name", face_name.c_str());
    auto face_file = domain.folderName + path_sep_ + domain.faceFolderName + path_sep_ + face_name+".vtp";
    add_child(face, "Face_file_path", face_file);
  }

  add_child( mesh, "Domain", domain_id);
}

//----------------
// add_projection
//----------------
// Add projection between fluid-solid surfaces.
//
void Sv4GuiXmlWriter::add_projection(const sv4guisvFSIJob* job)
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

//-----------------------------
// add_single_physics_equation
//-----------------------------
// Add an equation for a single physics simulation.
//
void Sv4GuiXmlWriter::add_single_physics_equation(const sv4guisvFSIeqClass& eq, tinyxml2::XMLElement* xml_equation)
{
  for (int i = 0; i < eq.getPropCount(); i++) {
    auto name = eq.getPropName(i).toStdString();
    std::replace(name.begin(), name.end(), ' ', '_');
    add_child(xml_equation, name, eq.getPropValue(i));
  }
}

//-----------------
// create_document
//-----------------
// Create an XML document from the sv4guisvFSIJob object.
//
void Sv4GuiXmlWriter::create_document(const sv4guisvFSIJob* job, const std::string& file_name)
{
  root_ = doc_.NewElement("svFSIFile");
  root_->SetAttribute("version", "1.0");
  doc_.InsertFirstChild(root_);

  // Add data for the GeneralSimulationParameters section.
  add_general(job);

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

  // Add equations.
  add_equations(job);

  doc_.SaveFile(file_name.c_str());
}

//--------------
// sort_domains
//--------------
//
// If there are two domains set the first domain to the fluid domain.
//
std::vector<sv4guisvFSIDomain> 
Sv4GuiXmlWriter::sort_domains(const sv4guisvFSIJob* job)
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

