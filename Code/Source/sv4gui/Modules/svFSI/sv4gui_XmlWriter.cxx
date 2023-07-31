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
// Add a child element to a parent.
//
tinyxml2::XMLElement*
Sv4GuiXmlWriter::add_child(tinyxml2::XMLDocument& doc, tinyxml2::XMLElement* parent, const std::string& name)
{
  auto child = doc.NewElement(name.c_str());
  parent->InsertEndChild(child);
  return child;
}

//-------------
// add_general
//-------------
// Add data for the 'GeneralSimulationParameters' section.
//
tinyxml2::XMLElement*
Sv4GuiXmlWriter::add_general(const sv4guisvFSIJob* job, tinyxml2::XMLDocument& doc)
{
  auto general = doc.NewElement("GeneralSimulationParameters");

  // Time stepping
  //
  auto Number_of_spatial_dimensions = add_child(doc, general, "Number_of_spatial_dimensions");
  Number_of_spatial_dimensions->SetText(job->nsd);

  auto Number_of_time_steps = add_child(doc, general, "Number_of_time_steps");
  Number_of_time_steps->SetText(job->timeSteps);

  auto Time_step_size = add_child(doc, general, "Time_step_size");
  Time_step_size->SetText(job->stepSize.c_str());

  // Restart files.
  //
  auto Continue_previous_simulation = add_child(doc, general, "Continue_previous_simulation");
  Continue_previous_simulation->SetText(job->continuePrevious);

  auto Restart_file_name = add_child(doc, general, "Restart_file_name");
  Restart_file_name->SetText(job->restartFileName.c_str());

  auto Increment_in_saving_restart_files = add_child(doc, general, "Increment_in_saving_restart_files");
  Increment_in_saving_restart_files->SetText(job->restartInc);

  auto Save_averaged_results = add_child(doc, general, "Save_averaged_results");
  Save_averaged_results->SetText(job->saveAvgResult);

  // VTK output.
  //
  auto Save_results_to_VTK_format = add_child(doc, general, "Save_results_to_VTK_format");
  Save_results_to_VTK_format->SetText(job->vtkSaveResults);

  auto Name_prefix_of_saved_VTK_files = add_child(doc, general, "Name_prefix_of_saved_VTK_files");
  Name_prefix_of_saved_VTK_files->SetText(job->vtkFileName.c_str());

  auto Increment_in_saving_VTK_files = add_child(doc, general, "Increment_in_saving_VTK_files");
  Increment_in_saving_VTK_files->SetText(job->vtkInc);

  // Misc 
  //
  auto Spectral_radius_of_infinite_time_step = add_child(doc, general, "Spectral_radius_of_infinite_time_step");
  Spectral_radius_of_infinite_time_step->SetText(job->rhoInf);

  auto Searched_file_name_to_trigger_stop = add_child(doc, general, "Searched_file_name_to_trigger_stop");
  Searched_file_name_to_trigger_stop->SetText(job->stopFileName.c_str());

  auto Simulation_requires_remeshing = add_child(doc, general, "Simulation_requires_remeshing");
  Simulation_requires_remeshing->SetText(job->remeshing);

  auto Verbose = add_child(doc, general, "Verbose");
  Verbose->SetText(job->verbose);

  auto Warning = add_child(doc, general, "Warning");
  Warning->SetText(job->warn);

  auto Debug = add_child(doc, general, "Debug");
  Debug->SetText(job->debug);

  return general;
}

//-----------
// add_meshe
//-----------
//
tinyxml2::XMLElement*
Sv4GuiXmlWriter::add_mesh(const sv4guisvFSIJob* job, sv4guisvFSIDomain& domain,
    tinyxml2::XMLDocument& doc)
{
  auto mesh = doc.NewElement("Add_mesh");
  mesh->SetAttribute("name", domain.name.c_str());

  auto Mesh_file_path = add_child(doc, mesh, "Mesh_file_path");
  Mesh_file_path->SetText(domain.folderName.c_str());

  for ( auto& face_name : domain.faceNames ) {
    auto face = add_child(doc, mesh, "Add_face");
    //auto face = doc.NewElement("Add_face");
    face->SetAttribute("name", face_name.c_str());
    auto Face_file_path = add_child(doc, face, "Face_file_path");
    auto face_file = domain.folderName + path_sep_ + domain.faceFolderName + path_sep_ + face_name+".vtp";
    Face_file_path->SetText(face_file.c_str());
  }

  return mesh;
}

//-----------------
// create_document
//-----------------
// Create an XML document from the sv4guisvFSIJob object.
//
void Sv4GuiXmlWriter::create_document(const sv4guisvFSIJob* job, const std::string& file_name)
{
  tinyxml2::XMLDocument doc;
  tinyxml2::XMLElement* root = doc.NewElement("svFSIFile");
  root->SetAttribute("version", "1.0");
  doc.InsertFirstChild(root);

  // Add data for the GeneralSimulationParameters section.
  auto general = add_general(job, doc);
  root->InsertEndChild(general);

  // If there are two domains set the first domain to the fluid domain.
  auto domains = sort_domains(job);

  // Add mesh data for the Add_mesh sections.
  for (auto& domain : domains) {
    auto mesh = add_mesh(job, domain, doc);
    root->InsertEndChild(mesh);
  }

  doc.SaveFile(file_name.c_str());
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



